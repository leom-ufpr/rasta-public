#' Library of auxiliary functions
#' @keywords internal
#' @name RastaTools
#' @docType package
#' @importFrom assertthat is.number is.string
#' @importFrom doFuture registerDoFuture %dofuture%
#' @importFrom dplyr arrange bind_rows 
#' @importFrom envnames get_fun_name
#' @importFrom future plan multisession sequential
#' @importFrom foreach foreach
#' @importFrom magrittr %>%
#' @importFrom parallel detectCores
#' @importFrom progressr handler_progress progressor with_progress 
#' @importFrom R6 R6Class
#' @importFrom raster raster brick calc freq getValues nlayers raster stack setValues 
#' @importFrom Rfast colsums colTabulate Table
#' @importFrom terra app rast
#' @importFrom tibble tibble
"_PACKAGE"
utils::globalVariables( c(".", "file_chunk", "path_list" ) )

#' Check if all elements of vector are numeric
#' 
#' @noRd

isnot_alnum <- function( x )
{
    if( base::is.integer( x ) )
    {
        return( FALSE )
    } 
    if( base::class( x ) != "numeric" && base::class( x ) != "character" )
    {
        return( TRUE )
    } 
    else if( base::is.na( x ) %>% base::sum( . ) > 0 )
    {
        return( TRUE )
    } 
    else if( base::is.numeric( x ) )
    {
        return( FALSE )
    } 
    else if( assertthat::is.string( x ) &
             base::grep( '[[:alnum:]]', x, perl = TRUE ) %>%
                 base::length( . ) > 0  )
    {
        return( FALSE )
    }
    else
    {
        return( TRUE )
    }
}

#' Check if argument is integer
#' 
#' @noRd

is_integer <- function( num )
{
    if( base::length( num ) == 0 )
    {
        return ( FALSE )
    }
    else if ( ! base::is.numeric( num ) )
    {
        return ( FALSE )
    }
    else if ( base::is.matrix( num ) )
    {
        return ( FALSE )
    }
    else if ( base::is.vector( num ) && base::length( num ) > 1 )
    {
        return ( FALSE ) 
    }
    else if ( base::round( num ) != num )
    {
        return ( FALSE )
    }
    else
    {
        return ( TRUE )
    }
}

#' Convert dec states between ensembles
#' 
#' @noRd

get_to_dec_state_fun <- function( from_rasta_state, to_rasta_state, method_filter )
{
    base::stopifnot( test_class( from_rasta_state, "RastaState" ),
                     test_class( to_rasta_state, "RastaState" ),
                     base::is.logical( method_filter ) )

    return( function( from_dec_state )
    {
        from_bin_vec <- from_rasta_state$get_bin_vec( from_dec_state ) 

        c( 'bin', from_bin_vec[ method_filter ] ) %>%
            base::paste( ., collapse = '' ) %>%
            to_rasta_state$get_dec_state( . ) %>%
            return( . )
    } )
}

#' Issue message
#' 
#' @noRd

get_var_val_type_msg <- function( var_name, var_val, client, type )
{
    stopifnot( assertthat::is.string( var_name ),
               assertthat::is.string( client ),
               assertthat::is.string( type ) )
    
    return( base::paste0( "Wrong '", var_name, "' type passed to ", client,
        ": got '", base::typeof( var_val ), "' expected '", type, "' instead." ) )
}

#' Test object class
#' 
#' @noRd

test_class <- function( my_object, my_class )
{
    base::stopifnot( base::is.character( my_class ) )

    classes <- base::class( my_object )
    
    return( classes[ 1 ] == my_class )
}
