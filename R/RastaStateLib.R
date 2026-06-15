#' Library of auxiliary functions for `RastaState`

#' Sanitize various method calls
#' 
#' @noRd
sanitize_methods <- function( methods )
{
    for( i in 1:base::length( methods ) )
    {
        if( ! assertthat::is.string( methods[i] ) )
           { return( FALSE ) }

        if( base::suppressWarnings( ! base::as.numeric( methods[i] ) %>%
                base::is.na(.) ) )
           { return( FALSE ) }
    }

    return( TRUE )
}
#' Sanitize decimal index identifying ensemble
#' 
#' @noRd
sanitize_dec_index <- function( dec_index )
{
    if( ! is_integer( dec_index ) )
    {
        error_msg <- base::paste( "Expected an integer 'dec_index', got",
            base::typeof( dec_index ), "instead" )
        
        if( base::length( dec_index ) > 0 )
            { error_msg <- base::paste0( error_msg, " - Value:", dec_index ) }

        stop( error_msg )        
    }

    return( TRUE )
}
#' Test properly formatted bin state
#' 
#' @noRd
test_bin_state <- function( bin_state )
{
    base::grep( 'bin[0-9]+', bin_state, perl = TRUE ) %>%
        base::length(.) > 0 %>%
        return(.)
}
#' Get state list for methods passed as parameters
#' 
#' @noRd
get_state_list <- function( methods )
{
    base::stopifnot( sanitize_methods( methods ) )

    state_vec <- initialize_state_vec( methods )
    states_list <- update_list( list(), state_vec, methods )
    for( i in 1:base::length( methods ) )
    {
        method_comb <- utils::combn( methods, i )
        for( j in 1:base::ncol( method_comb ) )
        {
            comb_vec <- state_vec
            comb_vec[ method_comb[ ,j] ] = 1 
            states_list <- update_list( states_list, comb_vec, methods )
        }
    }

    return( states_list )
}
#' Update state list
#' 
#' @noRd
update_list <- function( states_list, state_vec, methods )
{
    base::stopifnot( base::is.list( states_list ),
                     base::is.numeric( state_vec ),
                     sanitize_methods( methods ) )

    state <- get_dec_state( state_vec )
    dec_state <- base::paste0( 'state', state )
    bin_key <- base::paste( state_vec, collapse = '' ) %>%
        base::paste0( 'bin', . )

    states_list[[ bin_key ]] <- state
    states_list[[ dec_state ]] <- get_attrbs( state_vec, methods )
    states_list[[ dec_state ]]$state <- state


    return( states_list )
}
#' Build `RastaState` attrbs
#' 
#' @noRd
get_attrbs <- function( state_vec, methods )
{
    base::stopifnot( base::is.numeric( state_vec ),
                     sanitize_methods( methods ) )

    attrbs <- list( bin_vec = state_vec,
                    consensus = get_consensus( state_vec ),
                    consensus_type = get_consensus_type( state_vec ),
                    ensemble_agreement = get_agreement( state_vec ),
                    votes = base::sum( state_vec ) )

    base::names( attrbs$ensemble_agreement ) <- methods
    
    return( attrbs )
}
#' Get decimal state for state vector
#' 
#' @noRd
get_dec_state <- function( state_vec )
{
    base::stopifnot( base::is.numeric( state_vec ) )

    places <- length( state_vec )
    state <- sapply( 1:places, function( x ) { state_vec[x]*2**( places - x ) } )
    
    return( sum( state ) )
}
#' Get ensemble agreement
#' 
#' @noRd
get_agreement <- function( state_vec )
{
    base::stopifnot( base::is.numeric( state_vec ) )

    if( get_consensus( state_vec ) == 0 )
    {
        return( base::ifelse( state_vec == 0, 1, 0 ) )
    }
    else
    {
        return( state_vec )
    }
}
#' Get ensemble consensus
#' 
#' @noRd
get_consensus <- function( state_vec )
{
    base::stopifnot( base::is.numeric( state_vec ) )

    base::ifelse( base::sum( state_vec ) < base::length( state_vec ) / 2,
        return( 0 ),
        return( 1 ) )
}
#' Get ensemble consensus type
#' 
#' @noRd
get_consensus_type <- function( state_vec )
{
    base::stopifnot( base::is.numeric( state_vec ) )

    agreement <- get_agreement( state_vec )
    sum_agreement <- base::sum( agreement )

    return( base::paste( sum_agreement, base::length( state_vec ) -
        sum_agreement, sep = 'x' ) )
}
#' Initialize state vector
#' 
#' @noRd
initialize_state_vec <- function( methods )
{
    base::stopifnot( sanitize_methods( methods ) )
   
    state_vec <- base::rep( 0, base::length( methods ) )
    base::names( state_vec ) <- methods

    return( state_vec )
}
