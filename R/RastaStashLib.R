#' Library of auxiliary functions for `RastaStash`

#' Test for null parameter
#' 
#' @noRd
test_null <- function( x )
{
    base::ifelse( base::is.null( x ), 0, x ) %>%
        return(.)
}

#' Infinitesimal value to prevent log(0)
#' 
#' @noRd
inf_value <- function()
{
    return( 1e-07 )
}
#' Get indices of cells in between zero and maximum number of votes
#' 
#' @noRd
get_calc_state_idx <- function( cell_states )
{
    base::stopifnot( test_class( cell_states, 'list' ) )

    base::lapply( 1:base::length( cell_states ), function(i)
        { base::ifelse( base::length( cell_states[[ i ]] ) > 1, TRUE,
          base::ifelse( cell_states[[ i ]] < 0, TRUE, FALSE ) ) } ) %>%
          base::unlist(.) %>%
          base::which(.) %>%
          return(.)
}
#' Formats replicate numbers
#' 
#' @noRd
get_list_rep <- function( rep )
{
    base::stopifnot( base::is.character( rep ) || is_integer( rep ) )

    if( is_integer( rep ) )
    {
        base::paste0( 'rep', rep ) %>% return(.)
    }
    else if( base::is.character( rep ) )
    {
        return( rep )
    }
    else
    {
        stop( base::paste( "'rep' must be either an integer or string, got ",
              base::typeof( rep ), "instead" ) )
    }
}
#' Get numeric values from rasters
#' 
#' @noRd
get_num_values <- function( raster_to_process )
{
    base::stopifnot( test_class( raster_to_process, 'RasterLayer' ) )

    raster_to_process %>%
        raster::getValues(.) %>%
        stats::na.omit(.) %>%
        base::as.numeric(.) %>%
        return(.)
}
#' Get sum of raster values
#' 
#' @noRd
sum_raster <- function( raster_to_process )
{
    raster_to_process %>%
        get_num_values(.) %>%
        base::sum(.) %>%
        return(.)
}
#' Get mean of raster values
#' 
#' @noRd
get_raster_mean <- function( raster_to_process )
{
    raster_to_process %>%
        get_num_values(.) %>%
        base::mean(.) %>%
        return(.)
}
#' Get mean of raster values
#' 
#' @noRd
get_valid_dec_ids <- function( rasta_state )
{
    base::stopifnot( test_class( rasta_state, 'RastaState' ) )
    
    method_count <- rasta_state$get_method_count()
    all_dec_ids <- base::seq( from = 1, to = 2**method_count, by = 1 )
    invalid_dec_ids <- 2**c( 0:method_count )

    base::setdiff( all_dec_ids, invalid_dec_ids ) %>%
        return(.)
}
#' Check if raster values are binary
#' 
#' @noRd
isnt_binary_raster <- function( rasta_land )
{
    base::stopifnot( test_class( rasta_land, 'RastaLand' ) )

    for ( px in ( rasta_land$get_raster_obj() %>% raster::getValues(.) ) )
    {
        if( ! base::is.na( px ) && px != 1 && px != 0 )
            { return( TRUE ) }
    }

    return( FALSE )
}
#' Check if raster has values
#' 
#' @noRd
raster_has_values <- function( rasta_land )
{
    base::stopifnot( test_class( rasta_land, 'RastaLand' ) )

    return( rasta_land$get_raster_obj() %>%
            raster::getValues(., mat = FALSE ) %>%
            base::length(.) > 0 )
}
# Pareto front functions
#' Get matrix with ensemble codes
#' 
#' @noRd
get_methods_matrix <- function( rasta_state, n_methods )
{
    base::stopifnot( test_class( rasta_state, 'RastaState' ) )
    base::stopifnot( is_integer( n_methods ) )

    if( n_methods == 3 )
        { return( matrix() ) }

    col_max <- base::choose( n_methods, base::floor( n_methods / 2 ) )
    m <- base::matrix( base::rep( NA, col_max * ( n_methods - 1 ) ),
        nrow = n_methods - 1 )
    unpaired_count <- 1

    for ( dec_id in 1:( 2**n_methods - 1 ) ) 
    {
        ensemble_size <- rasta_state$get_bin_vec( dec_id ) %>% 
            base::sum(.)

        if( base::log2( dec_id ) == base::trunc( base::log2( dec_id ) ) )
        { 
            next
        }
        else if( ensemble_size == n_methods | ensemble_size == n_methods - 1  )
        {
            i <- base::ifelse( (-1)**unpaired_count < 0, 1, n_methods - 1 )
            unpaired_count <- unpaired_count + 1
        }
        else
        {
            i <- ensemble_size 
        }

        j <- stats::na.omit( m[ i, ] ) %>% base::length(.)
        m[ i, j + 1 ] <- dec_id
    }

    return( m )
}
# Pareto front functions
#' Get list of numeric pairs
#' 
#' @noRd
get_pair_list <- function( m )
{
    base::stopifnot( base::is.matrix( m ) )

    if( base::ncol( m ) == 1 )
        { return( base::list( c( 3, 5 ), c( 6, 7 ) ) ) }

    row_max <- base::ceiling( base::nrow( m ) / 2 )
    j <- 1
    list_elem <- 0
    pair_list <- base::list()

    for ( i in 1:row_max )
    {
        for( j in 1:base::ncol( m ) )
        {
            if( i == base::nrow( m ) + 1 - i )
            { 
                pair_vec <- c( m[ i, j ], NA ) 
            }
            else
            { 
                pair_vec <- c( m[ i, j ], m[ base::nrow( m ) + 1 - i, j ] ) 
            }
             
            if( stats::na.omit( pair_vec ) %>% base::length(.) > 0 )
            { 
                list_elem <- list_elem + 1
                pair_list[[ list_elem ]] <- pair_vec
            }
            else
            {
                break
            }
        }
    }

    return( pair_list )
}
