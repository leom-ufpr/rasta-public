#' Library of auxiliary functions for `RastaFront`

#' Sanitize various parameters
#' 
#' @noRd
sanitize_pars = function( par_val, par_desc )
{
    check_null_par( par_val, par_desc )
    check_not_num( par_val, par_desc )

    return( TRUE )
}
#' Sanitize null parameters
#' 
#' @noRd
check_null_par = function( par_val, par_desc )
{
    base::stopifnot( base::is.character( par_desc ) ) 

    if( base::is.null( par_val ) )
    {
        base::paste( "RastaFront's constructor requires", par_desc ) %>%
            stop(.)
    }

    return( TRUE )
}
#' Sanitize numeric parameters
#' 
#' @noRd
check_not_num = function( par_val, par_desc )
{
    if( base::is.null( par_val ) )
       { return( TRUE ) }

    base::stopifnot( base::is.character( par_desc ) )
    error_msg <- base::paste( par_desc, "must be all numeric." )

    if( ! base::is.numeric( par_val ) | base::any( base::is.na( par_val ) ) )
    {
        stop( error_msg )
    }
    else
    {
        return( TRUE )
    }
}
