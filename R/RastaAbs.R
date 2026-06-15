#' Abstract base class inherited by other `Rasta` classes
#'
#' Methods and attributes inherited by `RastaStash` and `RastaFront`
#'
RastaAbs <- R6::R6Class( "RastaAbs",
    public = list(

        #' @description
        #' Placeholder function
        #' 
        #' @param ... Any parameters passed to the constructor will be ignored
        #' 
        #' @noRd

        initialize = function(...)
        {
            if ( base::identical( base::class( self )[1L], 'RastaAbs' ) )
            {
                stop( 'RastaAbs is abstract; do not instantiate directly.' )
            }
        },

        #' @description
        #' Get cells converted to Shannon entropy terms
        #' 
        #' @param state_mx The internally set state matrix
        #' 
        #' @noRd

        get_cell_values = function( state_mx )
        {
            cell_values <- self$initialize_cell_values( 0, 0 )

            if( ! base::is.null( private$get_state_idx() ) )
            {
                freq_state_mx <- state_mx + 1
                freq_mx <- Rfast::colTabulate( freq_state_mx, max_number =
                    base::max( freq_state_mx ) )
                p_mx <- freq_mx / self$get_rep_count()
                p_log_mx <- p_mx
                p_log_mx[ p_mx == 0 ] <- 1
                cell_values[ private$state_idx ] <- Rfast::colsums( p_mx * 
                    base::log2( p_log_mx ) ) * ( -1 )
            }
            return( cell_values )
        },

        #' @description
        #' Get Shannon diversity
        #' 
        #' @noRd

        get_diversity = function()
        {
            gamma_freqs <- self$get_gamma_freqs()

            n <- base::sum( gamma_freqs )
            p <- gamma_freqs / n %>% base::as.numeric(.)

            return( - base::sum( p * base::log2( p ) ) )
        },

        #' @description
        #' Get entropy raster
        #' 
        #' @noRd

        get_entropy_raster = function()
        {
            private$get_state_mx() %>%
                self$get_cell_values(.) %>%
                self$initialize_raster(.) %>%
                return(.)
        },

        #' @description
        #' Get gamma frequencies
        #' 
        #' @noRd

        get_gamma_freqs = function()
        {
            max_fill <- 2**self$get_method_count() - 1

            gamma_freqs <- c( private$get_init_freqs( 'get_zero_idx', 0 ),
                              private$get_init_freqs( 'get_max_idx', max_fill ) )

            if( ! base::is.null( private$get_state_idx() ) )
                { gamma_freqs <- c( gamma_freqs, 
                    Rfast::Table( private$get_state_mx() ) ) }

            return( gamma_freqs[ ! base::is.na( base::names( gamma_freqs ) ) ] )
        },

        #' @description
        #' Get uncertainty
        #' 
        #' @noRd

        get_uncertainty = function()
        {
            if( ! self$has_state_mx() )
                { return( 0 ) }

            self$get_entropy_raster() %>%
                sum_raster(.) %>%
                return(.)
        },

        #' @description
        #' Check if method is in ensemble
        #' 
        #' @param method The method name
        #' 
        #' @noRd

        has_method = function( method )
        {
            base::stopifnot( base::is.character( method ) )
            base::ifelse( private$unknown_method( method ),
                return( FALSE ), return( TRUE ) )
        },

        #' @description
        #' Check if state matrix was built
        #' 
        #' @noRd

        has_state_mx = function()
        {
            if( base::is.null( private$get_state_mx() ) )
                return( FALSE )

            if( base::length( private$get_state_mx() ) == 0 )
                return( FALSE )
            
            if( base::is.na( private$get_state_mx() ) %>% any( . ) )
                return( FALSE )

            return( TRUE )
        },

        #' @description
        #' Initialize cell values for raster generation
        #' 
        #' @param zero_fill The zero value  used to fill the cells
        #' @param max_fill The maximum value  used to fill the cells
        #' 
        #' @noRd

	initialize_cell_values = function( zero_fill, max_fill )
        {
            base::stopifnot( assertthat::is.number( zero_fill ),
                             assertthat::is.number( max_fill ) )
        
            cell_values <- private$get_base_values() 
            
            if( ! base::is.null( private$get_zero_idx() ) )
                { cell_values[ private$zero_idx ] <- zero_fill } 

            if( ! base::is.null( private$get_max_idx() ) )
                { cell_values[ private$max_idx ] <- max_fill }
            
            return( cell_values )
        },

        #' @description
        #' Initialize raster
        #'
        #' @param cell_values Cell values used to fill the raster
        #' 
        #' @noRd

	initialize_raster = function( cell_values )
        {
            base::stopifnot( base::is.numeric( cell_values ) )

            private$get_base_raster() %>%
                raster::setValues( ., cell_values ) %>%
                return(.)
        },

        #' @description
        #' Test functions in the private interfaces
        #'
        #' @param name The function name
        #' @param args The list of arguments used in the function
        #' 
        #' @noRd

        test_priv_fun = function( name = NULL, args = list() )
        {
            base::stopifnot( base::is.character( name ) )

            if( name %in% base::names( private ) )
            {
                base::do.call( private[[ name ]], args ) %>%
                    return(.)
            }
            else 
            {
                warning( base::paste( "Method", name, "not found" ) )
                return( NULL )
            }
         } ),
    private = list(
        get_init_freqs = function( fun_name, fill_value )
        {
            base::stopifnot( base::is.character( fun_name ),
                             base::is.numeric( fill_value ) )

            if( base::is.null( private[[ fun_name ]]() ) )
                { return( c() ) }

            gamma_freq <- c( private[[ fun_name ]]() %>% 
                base::length(.) * self$get_rep_count() )

            names( gamma_freq ) <- fill_value
               
            return( gamma_freq ) 
        } ) )
