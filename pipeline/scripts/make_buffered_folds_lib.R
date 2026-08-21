main <- function()
{
    blockCV_version <- utils::packageVersion( "blockCV" )
    
    if( blockCV_version != '3.2.0' )
        { stop( "Needs blockCV >= 3.2.0, got version ", blockCV_version ) }

    base::lapply( CUTOUT_CODES, save_folds_for_each_cutout )

    message( "DONE!" )
}

save_folds_for_each_cutout <- function( code )
{
    message( "Processing data for '", code, "' please wait... " )

    # Use closure to facilitate parallelization
    folds_builder <- get_folds_builder( code = code )
    n_cores <- get_num_cores( N_CORES )
    reps <- base::seq_len( N_REPS )

    if ( n_cores > 1L ) # Parallelize
    {
        # Create backend with progress bar support
        future::plan( strategy = "multisession", workers = n_cores )

        # "cli" style progress bar
        progressr::handler_cli() %>%
            progressr::handlers(.)

        progressr::with_progress({
            p <- progressr::progressor( steps = N_REPS )
            
            folds_tb_list <- future.apply::future_lapply( reps,
                                                         function(i) { p(); folds_builder(i) },
                                                         future.seed = TRUE,
                                                         future.packages = c( "magrittr" ) )
        })
    }
    else # Serial processing
    {
        folds_tb_list <- cli::cli_progress_along( reps, "Processing" ) %>%
            base::lapply( ., folds_builder )
    }

    # Combine all replicates into one data.frame and save it as RDS
    folds_tb <- base::do.call( dplyr::bind_rows, folds_tb_list ) %>%
        save_rds( ., "folds_tb", code, OUTPUT_ENV )
}

# Return closure
get_folds_builder <- function( code )
{
    # Get environmental SpatStack from disk
    env_stack <- get_spat_stack( "env_stack", code )

    # Assemble mask of cells (= SpatRaster) that have data for all environmental variables
    full_mask <- get_full_mask( env_stack )

    # Generate SpatVec from full mask
    complete_vect <- get_complete_vect( full_mask )

    # Drop deep water records and save occurrences
    cropped_occ <- crop_occ_to_raster( complete_vect, code )
    save_rds( cropped_occ, 'cropped_occ', code, OUTPUT_ENV )
    
    # List with N_REPS sets of thinned occurrences
    thin_list <- thin_occurrences( cropped_occ )

    # Get cutout environmental range (meters) from file
    range_m <- get_range_m( code )

    # Get pa_cells around buffered cropped occurrences
    ok_pa_sf <- get_ok_pa_sf( full_mask, cropped_occ )

    # Wrap env_stack to prevent pointer error during parallelization
    env_stack <- terra::wrap( env_stack )
    
    return ( function( rep_id )
    {
        # Single data.frame from spThin for this replicate; enforce preferred projection
        thin_occ_sf <- sf::st_as_sf( thin_list[[ rep_id ]],
                                     coords = c( "Longitude", "Latitude" ),
                                     crs = PREFERRED_CRS )
        
        # Make occurrence spatial folds
        occ_folds <- make_spatial_folds( my_sf = thin_occ_sf,
                                         range_m = range_m,
                                         k = SPATIAL_FOLDS,
                                         seed = BASE_SEED + rep_id )

        # Sample pseudo-absences around buffered occurrences
        pa_sf <- sample_pa_sf( ok_pa_sf = ok_pa_sf,
                               seed = BASE_SEED + 1000L + rep_id )

        # Make pseudo-absences spatial folds
        pa_folds <- make_spatial_folds( my_sf = pa_sf,
                                        range_m = range_m,
                                        k = SPATIAL_FOLDS,
                                        seed = BASE_SEED + 2000L + rep_id )

        # Generate folds tibble
        occ_tb <- get_folds_tibble( thin_occ_sf,
                                    env_stack,
                                    code,
                                    rep_id,
                                    occ_folds$folds_ids,
                                    1L )

        pa_tb <- get_folds_tibble( pa_sf,
                                   env_stack,
                                   code,
                                   rep_id,
                                   pa_folds$folds_ids,
                                   0L )

        folds_tb <- dplyr::bind_rows( occ_tb, pa_tb )

        return( folds_tb )
    } )
}

thin_occurrences <- function( occ_sf )
{
    # occ_sf: sf with geometry; assumes a single species per cutout
    # spThin works with data.frames and expects column names LAT, LONG, SPEC
    # (replaced by the CUTOUT code )
    # Extract coordinates in WGS84 long/lat
    coords <- sf::st_coordinates( occ_sf )
    
    df <- base::data.frame( LAT = coords[, "Y"],
                            LONG = coords[, "X"],
                            SPEC = occ_sf$cutout )
    
    base::set.seed( BASE_SEED )
    
    spThin::thin( loc.data = df,
                  lat.col = "LAT",
                  long.col = "LONG",
                  spec.col = "SPEC",
                  thin.par = THIN_DIST_KM,
                  reps = N_REPS,
                  locs.thinned.list.return = TRUE,
                  write.files = FALSE,
                  verbose = FALSE ) %>%
        return(.)
}

get_ok_pa_sf <- function( full_mask, cropped_occ )
{
    # Set buffering CRS
    buffer_crs <- "+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"

    # Work in a projected CRS to enforce distances in meters
    template <- terra::project( full_mask, buffer_crs )
    template[ template < 1 ] <- NA            
    
    occ_proj <- sf::st_transform( cropped_occ, buffer_crs )

    # Find all non-NA cells in template (full environmental data mask)
    vals <- terra::values( template, na.rm = FALSE )
    cand_cells <- base::which( ! base::is.na( vals ) )
    cand_xy <- terra::xyFromCell( template, cand_cells )

    # Build buffer mask around occurrences
    # THIN_DIST_KM: min distance from presences (km)
    occ_vect <- terra::vect( occ_proj )
    occ_buf <- terra::buffer( occ_vect, width = THIN_DIST_KM * 1000 )
    buf_rast <- terra::rasterize( occ_buf, template, field = 1 )

    # Find pseudo-absence candidate cells outside buffered area
    buf_vals <- terra::values( buf_rast, na.rm = FALSE )
    ok_pa_cells <- base::is.na( buf_vals[cand_cells] ) %>%
        cand_cells[.]

    # Resets number of PA points if larger than number of OK cells
    if ( base::length( ok_pa_cells ) < N_PA_POINTS )
    {
        warning( "Fewer candidate pseudo-absence cells than requested; sampling all available." )
        N_PA_POINTS <- base::length( ok_cells )
    }

    # Reprojects OK cells onto "sf" with preferred CRS and returns "sf"
    xy <- terra::xyFromCell( template, ok_pa_cells )

    base::as.data.frame( xy ) %>%
        sf::st_as_sf( x = ., coords = c( "x", "y" ), crs = buffer_crs ) %>%
        sf::st_transform( ., PREFERRED_CRS ) %>%
        return(.)
}

# Get mask with full set of environmental measurements
get_full_mask <- function( env_stack )
{
    terra::app( env_stack, fun = function(...)
            { base::as.integer( base::all( ! base::is.na( c(...) ) ) ) } ) %>%
        return(.)
}

# Generates SpatVector with cell coordinates for the full mask
get_complete_vect <- function( full_mask )
{
    # Selects coordinates for valid cells in "full_mask"
    complete_cells <- base::which( terra::values( full_mask ) == 1 )
    complete_xy <- terra::xyFromCell( full_mask, complete_cells )

    terra::vect( data.frame( x = complete_xy[ ,1],
                             y = complete_xy[ ,2]),
                             geom = c( "x", "y" ),
                             crs  = terra::crs( full_mask ) ) %>%
        return(.)
}

get_spat_stack <- function( tag, code )
{
    base::paste0( tag, "_", code, ".grd" ) %>%
        read_spat_stack( ., OUTPUT_ENV, PREFERRED_CRS ) %>%
        return(.)
}

# Reproject snapped occurrences onto raster CRS for each cutout
crop_occ_to_raster <- function( complete_vect, code )
{
    # Occurrence data
    occ_sf <- sf::st_as_sf( load_occ_points(),
                            coords = c( "lon", "lat" ),
                            crs = PREFERRED_CRS )

    # Select coords corresponding to the cutout
    occ_sub <- occ_sf[ occ_sf$cutout == code, ]

    # If buggy "sf", attempts to fix it
    occ_ll <- base::as.data.frame( occ_sub ) %>%
        sf::st_as_sf( . ) %>%
        sf::st_make_valid( . ) %>%         
        sf::st_zm( ., drop = TRUE, what = "ZM" )

    if ( base::nrow( occ_ll ) == 0 )
        { stop( "No usable occurrences found for cutout ", code ) }

    # Turns "complete_vect" into an "sf" object in the same projection as occ_ll
    complete_sf <- sf::st_as_sf( complete_vect,
                                 crs = PREFERRED_CRS )

    # Index of nearest complete_sf row for each occ_sf row
    idx <- sf::st_nearest_feature( occ_ll, complete_sf )  
    nearest_sf <- complete_sf[ idx, ]

    # Build a new "sf" object with corrected coordinates but original attributes
    new_sf <- base::cbind( sf::st_drop_geometry( occ_ll ),
                           sf::st_coordinates( nearest_sf ) )

    snapped_sf <- sf::st_as_sf( new_sf,
                                coords = c( "X", "Y" ),
                                crs = PREFERRED_CRS )

    # Crop valid occurrences to shallow areas
    shallow_stack <- get_spat_stack( "shallow_stack", code )
    snapped_pts <- terra::vect( snapped_sf )
    g <- terra::geom( snapped_pts )
    shallow_cells <- terra::cellFromXY( shallow_stack[[ "btm" ]], g[ , c( "x", "y" ) ] )
    valid <- ! base::is.na( shallow_cells )

    return( snapped_sf[ valid, ] )
}

# Get cutout environmental range (meters) from file
get_range_m <- function( code )
{
    range_tbl <- read_rds( "autocorr_range.rds", OUTPUT_ENV )
    
    range_m <- range_tbl$range[ range_tbl$cutout == code ]

    if ( base::length( range_m ) != 1L )
    {
        stop( "Range not found or not unique for cutout: ", code )
    }

    return( range_m )
}

make_spatial_folds <- function( my_sf, range_m, k = 5L, seed = NULL )
{
    # my_sf: sf with geometry, should have a response column (dummy is fine)
    # range_m: block/cell size in metres.
    if ( ! "occ" %in% base::names( my_sf ) )
        { my_sf$occ <- 1L }

    if ( ! base::is.null( seed ) )
        { base::set.seed( seed ) }
    
    # Random blocking, hexagonal grid, random fold assignment.
    # You can vary offset via 'offset' if you want additional variation in origin.
    folds <- blockCV::cv_spatial( x = my_sf,
                                  column = "occ",
                                  size = range_m,
                                  k = k,
                                  selection = "random",
                                  hexagon = TRUE,
                                  iteration = BLOCK_CV_ITER,
                                  plot = FALSE,
                                  progress = FALSE,
                                  report = FALSE )
    
    return( folds )
}

sample_pa_sf <- function( ok_pa_sf, seed )
{
    base::set.seed( seed )

    sample_sfc <- sf::st_sample( ok_pa_sf,
                                 size = N_PA_POINTS,
                                 type = "random",
                                 exact = TRUE )

    sf::st_sf( sample_sfc, crs = PREFERRED_CRS ) %>%
        sf::st_cast( ., "POINT" ) %>%
        return(.)
}

get_folds_tibble <- function( my_sf, env_stack, code, rep_id, folds_ids, bin_pa )
{
    coords <- sf::st_coordinates( my_sf )
    env_vars <- get_raster_values( env_stack, my_sf )
    
    tibble::tibble( rep_id = rep_id,
                    fold_id = folds_ids,
                    resp_var = bin_pa,
                    lon = coords[ ,1],
                    lat = coords[ ,2] ) %>%
        dplyr::bind_cols( ., env_vars ) %>%
        return(.)    
}

load_occ_points <- function()
{
    occ_points <- readr::read_delim( OCC_FILE,
                                     delim = ";",
                                     show_col_types = FALSE )

    if ( ! all ( c( "lon", "lat", "cutout" ) %in% base::names( occ_points ) ) )
    {
        stop( "Occurrence table must contain columns: lon, lat, cutout." )
    }

    occ_points %<>% dplyr::select( c( "lon", "lat", "cutout" ) ) %>%
        dplyr::mutate_at( c( "lon", "lat" ), base::as.numeric ) %>%
        dplyr::mutate_at( c( "cutout" ), base::as.factor ) %>%
        stats::na.omit(.) %>%
        dplyr::mutate( lat = tibble::num( occ_points$lat, digits = SIG_DIG ),
                       lon = tibble::num( occ_points$lon, digits = SIG_DIG ) )

    return( occ_points ) 
}

get_raster_values <- function( env_stack, my_sf )
{
    env_stack <- terra::unwrap( env_stack )
    
    terra::extract( env_stack, my_sf, df = TRUE, ID = FALSE ) %>%
        return(.)
}

base::file.path( SCRIPT_ENV, "aux_lib.R" ) %>%
    source(.)
