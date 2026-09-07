get_filename <- function( obj_name, code, obj_env, ext )
{
    base::paste0( obj_name, "_", code, ".", ext ) %>%
        base::file.path( obj_env, . ) %>%
        return(.)
}

get_log_con <- function( filename )
{
    base::file.path( BASE_ENV, filename ) %>%
        base::file( ., open = "wt" ) %>%
        return(.)
}

get_model_fun <- function( model_name, tag )
{
    base::paste0( "get_", model_name, "_", tag ) %>%
        base::do.call( ., base::list() ) %>%
        return(.)
}

get_num_cores <-function( n_cores )
{
    max_cores <- parallel::detectCores() - 1

    if( n_cores > max_cores )
    {
        message( "Resetting number of cores to ", max_cores )

        return( max_cores )
    }
    else if( n_cores <= 1L )
    {
        return( 1L )
    }
    else
    {
        base::floor( n_cores ) %>%
            return(.)
    }
}

get_warning_handler <- function( log_con, code )
{
    return( function( w )
    {
        base::Sys.time() %>%
            base::format( ., "%Y-%m-%d %H:%M:%S" ) %>%
            base::paste0( "Log for' ", code,
                         " 'cutout generated on: ", . ) %>%
            c( . , "---", base::conditionMessage( w ), "---" ) %>%
            base::writeLines( ., con = log_con )

        invokeRestart( "muffleWarning" )
    })
}

is_positive_integer <- function( x )
{
    if( ! is_positive_numeric( x ) ) return( FALSE )

    base::all( x == as.integer( x ) ) %>%
        return(.)
}

is_positive_numeric <- function( x )
{
    if( ! is_numeric( x ) ) return( FALSE )

    base::all( x > 0 ) %>%
        return(.)
}

is_numeric <- function( x )
{
    if( ! base::is.numeric(x) ) return( FALSE )
    
    return( base::is.finite( x ) %>% base::all( . ) &
            ! base::is.na( x ) %>% base::any( . ) )
}

process_codes <- function( log_file_name )
{
    base::stopifnot( base::is.character( log_file_name ) )

    for( code in CUTOUT_CODES )
    {
        log_con <- base::paste0( log_file_name, code, ".log" ) %>%
            get_log_con(.)

        handle_warnings <- get_warning_handler( log_con, code )
        
        message( "Processing '", code, "' data" )
        
        withCallingHandlers(
        {
            process_cutout( code = code )
        },
        warning = handle_warnings )

        message( "DONE!" )
        message()

        base::close( log_con )
    }

    message( "Cleaning up" )
    if( H2O_WAS_RUN ) h2o::h2o.shutdown( prompt = FALSE )

    base::invisible( NULL )
}

read_rds <- function( filename, obj_env )
{
    base::file.path( obj_env, filename ) %>%
        base::readRDS(.) %>%
        return(.)
}

read_spat_stack <- function( filename, obj_env, my_crs )
{
    base::file.path( obj_env, filename ) %>%
        terra::rast(.) %>%
        terra::project( ., my_crs ) %>%
        return(.)
}

sanitize_fn <- function( my_function, function_name, model_name, message = "" )
{
    sanitize_tags( c( function_name, model_name, message ) )
    my_type <- base::typeof( my_function )
    
    if ( my_type != "closure" )
    {
        stop( "`", function_name, "` must be a function; got ",
              my_type, " instead from `",
              model_name, "`",
              message  )
    }

    base::invisible( NULL )
}

sanitize_tb <- function( my_tibble, tibble_name, model_name, message = "" )
{
    sanitize_tags( c( tibble_name, model_name, message ) ) 

    if ( ! tibble::is_tibble( my_tibble ) )
    {
        stop( "`", tibble_name, "` must be a tibble; got ",
              base::typeof( my_tibble ), " instead for model `",
              model_name, "` ",
              message  )
    }

    base::invisible( NULL )
}

sanitize_tags <- function( vector )
{
    base::stopifnot( base::is.character( vector ) )
    base::invisible( NULL )
}
    
save_rds <- function( obj, obj_name, code, obj_env )
{
    filename <- get_filename( obj_name, code, obj_env, 'rds' )

    base::saveRDS( obj, file = filename )

    message( "Saved ", filename )

    return( TRUE )
}

