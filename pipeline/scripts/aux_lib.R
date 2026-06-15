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

save_rds <- function( obj, obj_name, code, obj_env )
{
    filename <- get_filename( obj_name, code, obj_env, 'rds' )

    base::saveRDS( obj, file = filename )

    message( "Saved ", filename )

    return( TRUE )
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
