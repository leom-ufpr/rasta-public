#' R6 Class to instantiate `RastaFront` objects
#'
#' Classe internally used by `RastaStash`. Do not access these methods directly
#' 
RastaFront <- R6::R6Class( "RastaFront",
    inherit = RastaAbs,                      
    public = base::list(
        #' @field methods A vector with the method names that make up the 
        #' ensemble
        methods = base::vector(), 
        #' @field rep_count The total number of replicates in the stash
        rep_count = base::numeric(),
        #' @field dec_id The decimal ID of the ensemble
        dec_id = base::numeric(),
        #' @description
        #' Method constructor
        #' 
        #' @param base_raster The initial raster
        #' @param base_values Starting values for the raster
        #' @param dec_id The decimal ID that identifies the ensemble
        #' @param from_rasta_state RastaState object to be converted from
        #' @param max_idx Maximum index for state matrix
        #' @param rep_count Maximum number of replicates
        #' @param state_idx Vector with indices of variable pixels
        #' @param state_mx The state matrix from the stash
        #' @param zero_idx Vector with indices of pixels with zero values
        #' 
        #' @noRd
        initialize = function( base_raster = NULL, base_values = NULL, 
            dec_id = NULL, from_rasta_state = NULL, max_idx = NULL, 
            rep_count = NULL, state_idx = NULL, state_mx = NULL,
            zero_idx = NULL )
        {
            sanitize_pars( rep_count,
                'Count of replicates (rep_count)' )
            sanitize_pars( dec_id,
                'Decimal identity of method ensemble (dec_id)' )
            check_not_num( max_idx,
                'Vector of maximum votes indices (max_idx)' )
            check_not_num( state_idx, 
                'Vector of microstate indices (state_idx)' )
            check_not_num( state_mx, 
                'Matrix of microstates (state_mx)' )
            check_not_num( zero_idx, 
                'Vector of zero vote indices (zero_idx)' )

            if( ! test_class( base_raster, 'RasterLayer' ) )
            {
                get_var_val_type_msg( 'base_raster', base_raster, 
                    'constructor', 'RasterLayer' ) %>%
                base::stop(.)
            }

            if( ! base::is.vector( base_values ) )
            {
                get_var_val_type_msg( 'base_values', base_values,
                    'constructor', 'vector' ) %>%
	        base::stop(.)
            }

            check_null_par( from_rasta_state, 
                'RastaState object (from_rasta_state)' )
        
            if( ! test_class( from_rasta_state, 'RastaState' ) )
            {
                get_var_val_type_msg( 'from_rasta_state', 
                    from_rasta_state, 'constructor', 'RastaState' ) %>%
		base::stop(.)
            }

            self$rep_count <- rep_count
            self$dec_id <- dec_id

            private$max_idx <- max_idx
            private$state_idx <- state_idx
            private$zero_idx <- zero_idx

            private$base_raster <- base_raster
            private$base_values <- base_values

            private$from_rasta_state <- from_rasta_state
            private$to_rasta_state <- from_rasta_state$get_dec_id_method_list( dec_id ) %>%
                RastaState$new( methods = . )

            self$methods <- private$to_rasta_state$get_methods()

            if( base::is.null( state_mx ) ) 
            {
                bin_id_vec <- from_rasta_state$get_bin_vec( self$dec_id )
                private$method_filter <- bin_id_vec == 1
                self$methods <- base::names( bin_id_vec[ private$method_filter ] )
                private$bin_id_vec <- bin_id_vec

                private$state_mx <- base::rep( NA, self$get_rep_count() *
                        base::length( private$get_state_idx() ) ) %>%
                    base::matrix( ., nrow = self$get_rep_count() )
                
                method_count <- 2**private$from_rasta_state$get_method_count()
                private$dec_map <- base::rep( NA, method_count )
                base::names( private$dec_map ) <- 0:( method_count - 1 )
            }
            else
            {
                private$state_mx <- state_mx

                front_state_sum <- Rfast::colsums( state_mx )
                max_state_sum <- self$get_rep_count() *
                    ( 2**self$get_method_count() - 1 )

                private$get_front_idx( front_state_sum, max_state_sum ) %>%
                    private$update_indices( 'max_idx', . )

                private$get_front_idx( front_state_sum, 0 ) %>%
                    private$update_indices( 'zero_idx', . )

                if( base::length( private$state_mx ) == 0 )
                    { private$state_mx <- NULL }
            }
        },
        # Accessors
        #' @description
        #' Get decimal ID corresponding to the ensemble
        #' 
        #' @noRd
        get_dec_id = function()
        {
            return( self$dec_id )
        },
        #' @description
        #' Get number of methods in the ensemble
        #' 
        #' @noRd
        get_method_count = function()
        {
            return( base::length( self$methods ) )
        },
        #' @description
        #' Get replicate count
        #' 
        #' @noRd
        get_rep_count = function()
        {
            return( self$rep_count )
        },
        # Builder
        #' @description
        #' Build state matrix
        #' 
        #' @param row_num The row number
        #' @param dec_vec The vector of decimal values
        #' 
        #' @noRd
        build_state_mx = function( row_num, dec_vec )
        {
            stopifnot( is_integer( row_num ),
                       base::is.numeric( dec_vec ) )
            
            private$state_mx[ row_num, ] <- base::lapply( dec_vec,
                    private$map_dec ) %>%
                base::unlist(.) %>%
                base::as.vector(.)
            
            return( TRUE )
        } ),
    private = list( base_values = NULL, base_raster = NULL, bin_id_vec = NULL,
        dec_map = NULL, from_rasta_state = NULL, max_idx = NULL,
        method_filter = NULL, state_idx = NULL, state_mx = NULL,
        to_rasta_state = NULL, zero_idx = NULL,
        # Accessors
        check_idx = function( accessor )
        {
            base::stopifnot( base::is.character( attribute ) )

            base::ifelse( base::is.null( private[[ base::paste0( 'get_',
                    attribute ) ]]() ),
                return( c() ),
                return( private[[ base::paste0( 'get_', attribute ) ]] ) )
        },
        get_base_raster = function()
        {
            return( private$base_raster )
        },
        get_base_values = function()
        {
            return( private$base_values )
        },
        get_bin_id_vec = function()
        {
            return( private$bin_id_vec )
        },
        get_dec_map = function()
        {
            return( private$dec_map )
        },
        get_from_rasta_state = function()
        {
            return( private$from_rasta_state )
        },
        get_front_idx = function( front_state_sum, match_value )
        {
            base::stopifnot( base::is.numeric( front_state_sum ),
                base::is.numeric( match_value ) )
            
            match_cond <- front_state_sum == match_value
            focus_idx <- base::which( match_cond ) 

            base::ifelse( base::length( focus_idx ) > 0, return( focus_idx ),
                return( c() ) )
        },
        get_init_freqs = function( vec_name, fill_value )
        {
            base::stopifnot( base::is.character( vec_name ),
                             assertthat::is.number( fill_value ) )
            
            if( base::is.null( private[[ vec_name ]] ) )
                { stop( base::paste( vec_name,
                    "was not set; can't get initial frequencies" ) ) }

            return( super$get_init_freqs( vec_name, fill_value ) )
        },
        get_max_idx = function()
        {
            return( private$max_idx )
        },
        get_method_filter = function()
        {
            return( private$method_filter )
        },
        get_state_idx = function()
        {
            return( private$state_idx )
        },
        get_state_mx = function()
        {
            return( private$state_mx )
        },
        get_to_rasta_state = function()
        {
            return( private$to_rasta_state )
        },
        get_zero_idx = function()
        {
            return( private$zero_idx )
        },
        update_indices = function( attribute, idx_vector )
        {
            base::stopifnot( attribute %in% c( 'max_idx', 'zero_idx' ) )
            
            if( base::length( idx_vector > 0 ) )
            {
                private[[ attribute ]] <- c( private[[ attribute ]],
                    private$state_idx[ c( idx_vector ) ] )

                updated_state_idx <- private$state_idx[ -c( idx_vector ) ]

                if( base::length( updated_state_idx ) > 0 )
                {
                    private$state_idx <- updated_state_idx
                    private$state_mx <- private$state_mx[ , -c( idx_vector ) ]
                }
                else
                {
                    private$state_idx <- NULL
                    private$state_mx <- NULL
                }
            }

            return( TRUE )
        },

        # Core methods
        map_dec = function( from_dec_state )
        {
            base::stopifnot( base::length( private$dec_map ) > 0 )
            
            if( ! base::is.na( private$dec_map[ from_dec_state + 1 ] ) )
                { return( private$dec_map[ from_dec_state + 1 ] ) }

            to_dec_state <- private$get_to_dec_state( from_dec_state )
            
            private$dec_map[ from_dec_state + 1 ] <- to_dec_state

            return( to_dec_state )
        },
        get_to_dec_state = function( from_dec_state )
        {
            base::stopifnot( is_integer( from_dec_state ) )
            
            from_rasta_state <- private$get_from_rasta_state()
            to_rasta_state <- private$get_to_rasta_state()

            f <- get_to_dec_state_fun( from_rasta_state,
                to_rasta_state, private$method_filter )

            to_dec_state <- f( from_dec_state )

            return( to_dec_state )
        } ) )
