#' Library of auxiliary functions to support large RastaStash generation
#' 
#' @export
#' @description
#' Create a new `RastaStash` object from test files provided with the package.
#' 
#' @param raster_folder A valid path to the test GTiff files.
#' @param epithet A string used to identify the species files used to populate the
#' `RastaStash` object. Must be either "radians", "siderea" or "stellata".
#' @param cores Number of cores used to parallelize file processing.
#'
#' @return A `RastaStash` object.
#'
#' @details Download the test files from the website and unzip them in the chosen
#' folder before passing it as an argument to the function. It is highly recommended
#' to parallelize object creation for it will greatly decrease execution time:
#' object construction is quite expensive due to the large number of raster files
#' per species (900) and expensive sanity checks. Use `parallel::detectCores()`
#' to verify how many cores are available. Use the maximum possible number for
#' better performance.
#'
#' @examples
#' \dontrun{ # Return a`RastaStash` object for the "radians" dataset using all
#' # but one of available cores in your machine
#' radians_rs <- get_test_rasta_stash( raster_folder = "/path/to/test/files/",
#' epithet = "radians", cores = parallel::detectCores() - 1 )
#' radians_rs$get_rep_count() # Should return 100 }

get_test_rasta_stash <- function( raster_folder, epithet, cores = 1L )
{
    base::stopifnot( base::is.character( raster_folder ),
                     base::is.character( epithet ),
                     assertthat::is.number( cores ) )

    cores <- .parse_cores( cores )
    
    if( ! base::dir.exists( raster_folder ) )
    {
        stop( base::paste( raster_folder, "is not a valid folder." ) )
    }    

    if( ! epithet %in% c( "atl", "brz", "crb" ) )
    {
        stop( base::paste0( "Invalid class '", epithet, "'; must be either
              'atl', 'brz' or 'crb'." ) )
    }    
    
    file_list <- base::list.files( path = raster_folder, full.names = TRUE,
                                   pattern = base::paste0( epithet, ".+\\.tif$" ) )

    if( base::length( file_list ) == 0 )
    {
        stop( base::paste0( "No files prefixed with  '", epithet, "' found in ",
              raster_folder ) )
    }

    get_large_rasta_stash( file_list = file_list, rep_count = 100, cores ) %>%
        return(.)
}

#' Get large rasta stash
#' 
#' @noRd

get_large_rasta_stash <- function( file_list, rep_count = 100, cores = 1L )
{
    base::stopifnot( base::is.character( file_list ),
                     assertthat::is.number( rep_count ),
                     assertthat::is.number( cores ) )

    base::paste( "Assembling RastaStash using", cores,
                 "cores - please be patient..." ) %>%
        message(.)

    doFuture::registerDoFuture()
    future::plan( future::multisession, workers = .parse_cores( cores ) )
    
    chunks <- .get_chunks( file_list, cores )
    total_size <- base::length( file_list )

    progressr::handler_progress( format = "[:bar] :percent | ETA: :eta | :message",
                                 width = 80,
                                 clear = FALSE ) %>% progressr::handlers(.) 

    progressr::with_progress({
        p <- progressr::progressor( steps = total_size )        

        rasta_stash_list <- foreach::foreach( file_chunk = chunks,
            .options.future = list( seed = TRUE, 
            .packages = c( "magrittr", "raster", "Rasta" ) ) ) %dofuture%
        {
            rasta_stash <- RastaStash$new()

            for( file_path in file_chunk )
            {
                if( ! base::file.exists( file_path ) )
                {
                    stop( base::paste( file_path, "does not exist" ) )
                }
                
                file_name <- base::basename( file_path )
                
                tokens <- base::strsplit( file_name , "[_\\.]" ) %>%
                    base::unlist(.)

                spat_stack <- terra::rast( file_path )

                for( i in base::seq_len( rep_count ) )
                {
                    my_raster <- raster::raster( spat_stack[[ i ]] )

                    rasta_land <- RastaLand$new( class = tokens[1],
                                                method = tokens[2],
                                                rep = i,
                                                rep_count = rep_count,
                                                raster_obj = my_raster )

                    rm( my_raster )
                    rasta_stash$add( rasta_land )
                    rm( rasta_land )

                    p()
                }
            }
            base::gc()
            return( rasta_stash )
        }
    })
         
    future::plan( future::sequential )

    return( RastaStash$new( stash = rasta_stash_list ) )
}

#' Chunk list for parallelization
#' 
#' @noRd

.get_chunks <- function( my_list, cores = 1L )
{
    base::stopifnot( base::is.character( my_list ),
                     assertthat::is.number( cores ) )

    if( base::length( my_list ) == 0 )
    {
        stop( base::paste0( "Cannot proceed because object list is empty." ) )
    }

    cores <- .parse_cores( cores )

    if( cores == 1 )
    {
        return( my_list )
    }
    else if( base::length( my_list ) < cores )
    {
        return( my_list )
    }
    else
    {
        chunks <- base::seq_along( my_list ) %>%
            base::cut( ., breaks = cores, labels = FALSE  ) %>%
            base::split( my_list, . ) %>%
            return(.)
    }
}

#' Parses number of cores
#' 
#' @noRd

.parse_cores <- function( cores )
{
    base::stopifnot( assertthat::is.number( cores ) )

    if( cores < 1 )
    {
        return( 1L )
    }
    else if( cores > parallel::detectCores() )
    {
        return( parallel::detectCores() - 1 )
    }
    else
    {
        return( base::as.integer( cores )  )
    }
}
