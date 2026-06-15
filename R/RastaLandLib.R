#' Library of auxiliary functions for `RastaLand`

#' Get message for attribute verification
#' 
#' @noRd
get_attrb_type_message <- function( attrb_name, attrb_type, type )
{
    base::stopifnot( assertthat::is.string( attrb_name ),
        assertthat::is.string( type ) )

    return( base::paste0( "Wrong '", attrb_name,
        "' value passed to constructor: got ", attrb_type, " expected ", type,
        " instead ", get_usage_message() ) )
}
#' Get usage message
#' 
#' @noRd
get_usage_message <- function()
{
    return( base::paste( " - try something like: RastaLand$new( class = 'Homo sapiens'," ,
        "method = 'maxent', rep = 1, rep_count = 100, raster_obj = raster_obj )" ) )
}
#' Sanitize various arguments
#' 
#' @noRd
sanitize_arg <- function( attrb_name, attrb_val, type )
{
    base::stopifnot( assertthat::is.string( attrb_name ),
        assertthat::is.string( type ) )
    
    base::stopifnot( type == 'character' || type == 'numeric' )

    type_message <- base::typeof( attrb_val ) %>%
        get_attrb_type_message( attrb_name, . , type )

    empty_message <- 'no argument' %>%
        get_attrb_type_message( attrb_name, . , type )

    if( base::length( attrb_val ) == 0 )
    {
        stop( empty_message )
    }
    else if( type == 'character' )
    {
        if( assertthat::is.string( attrb_val ) && isnot_alnum( attrb_val ) )
        {
            stop( empty_message )
        }
        else if( ! assertthat::is.string( attrb_val )  )
        {
            stop( type_message )
        }
        else
        {
            return( TRUE )
        }            
    }
    else if( type == 'numeric' ) 
    {
        if( ! assertthat::is.number( attrb_val ) && isnot_alnum( attrb_val ) )
        {
            stop( empty_message )
        }
        else if( ! assertthat::is.number( attrb_val ) )
        {
            stop( type_message )
        }
        else
        {
            return( TRUE )
        }
    }
}
