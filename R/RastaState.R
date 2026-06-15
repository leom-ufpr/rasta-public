#' R6 Class to instantiate `RastaState` objects
#'
#' Classe internally used by `RastaStash`. Do not access these methods directly
#' 
RastaState <- R6::R6Class( "RastaState",
    public = base::list(
        #' @description
        #' Method constructor
        #'                
        #' @param methods A character vector with the method names.
        #' @param weights A numeric vector with the method weights.
        #'
        #' @noRd
        initialize = function( methods = vector(), weights = vector() )
        {
            if( ! base::is.character( methods ) ||
                ! sanitize_methods( methods ) )
                stop( base::paste( "Constructor requires string vector with
                    method names - e.g.", "RastaState$new( methods = c( 'ann',
                    'svm', maxent' ) )" ) )
            
            private$methods <- base::sort( methods )
            private$set_state_list()

            if( base::length( weights ) > 0 )
                { private$set_method_weights( weights ) }
        },
        #' @description
        #' Adds entropy weights to the object.
        #'
        #' @param weights A numeric vector with the method weights.
        #' 
        #' @noRd
        add_weights = function( weights )
        {
            private$set_method_weights( weights )

            dec_keys <- self$get_state_list() %>%
                base::names(.) %>%
                base::grep( 'state', ., value = TRUE, fixed = TRUE )
            
            for( dec_key in dec_keys )
            {
                if( ! base::is.null( private$state_list[[ dec_key ]][[ 'wgt_votes' ]] ) )
                {
                    private$state_list[[ dec_key ]][[ 'wgt_votes' ]] <- NULL
                }
            }

            return( TRUE )
        },
        #' @description
        #' Get binary vector corresponding to the decimal state passed as
        #' argument to the method.
        #'
        #' @param dec_state An integer that identifies the ensemble state.
        #' 
        #' @noRd
        get_bin_vec = function( dec_state )
        {
            return( private$get_attribute( dec_state ) )
        },
        #' @description
        #' Get majority rule binary state for the decimal state passed as
        #' argument to the method.
        #'
        #' @param dec_state An integer that identifies the ensemble state.
        #' 
        #' @noRd
        get_consensus = function( dec_state )
        {
            return( private$get_attribute( dec_state ) )
        },
        #' @description
        #' Get consensus type corresponding to the decimal state passed as
        #' argument to the method.
        #'
        #' @param dec_state An integer that identifies the ensemble state.
        #' 
        #' @noRd
        get_consensus_type = function( dec_state )
        {
            return( private$get_attribute( dec_state ) )
        },
        #' @description
        #' Get the binary state corresponding to the decimal state passed as
        #' argument to the method.
        #'
        #' @param bin_state A string that identifies the ensemble state.
        #' 
        #' @noRd
        get_dec_state = function( bin_state )
        {
            if( ! test_bin_state( bin_state ) )
            {
                stop( base::paste( "Incorrectly formatted 'bin_state'", 
                                   "expected something like 'bin0001', got",
                                   bin_state ) )
            } 

            if( base::is.null( private$state_list[[ bin_state ]] ) )
            {
                base::paste0( "No 'dec_state' correponding to '", bin_state,
                        "' was found" ) %>%
                    stop(.)
            } 
            
            return( private$state_list[[ bin_state ]] )
        },
        #' @description
        #' Get the ensemble agreements corresponding to the decimal state
        #' passed as argument to the method.
        #'
        #' @param dec_state An integer that identifies the ensemble state.
        #' 
        #' @noRd
        get_ensemble_agreement = function( dec_state )
        {
            sanitize_dec_index( dec_state )
            
            return( private$get_attribute( dec_state ) )
        },
        #' @description
        #' Get the method agreements corresponding to the decimal state
        #' passed as argument to the method.
        #'
        #' @param dec_state An integer that identifies the ensemble state.
        #' @param method A string that identifies the method.
        #' 
        #' @noRd
        get_method_agreement = function( dec_state, method )
        {
            sanitize_dec_index( dec_state )
            
            if( ! assertthat::is.string( method ) )
            {
                base::paste( "Function 'get_agreement' requires method as
                        string; got", method ) %>%
                    stop(.)
            }

            if( ! method %in% private$methods )
            {
                base::paste0( "Method '", method, "'not in list passed to
                        the constructor" ) %>%
                    stop(.)
            }

            ensemble_agreement <- self$get_ensemble_agreement( dec_state )

            return( ensemble_agreement[[ method ]] )
        },
        #' @description
        #' Get the count of methods used to construct the `RastaState`
        #' object.
        #' 
        #' @noRd
        get_method_count = function()
        {
            return( base::length( private$methods ) )
        },
        #' @description
        #' Get the power of the binary digit corresponding to the method  
        #' name passed as argument.
        #'
        #' @param method A string that identifies the method.
        #' 
        #' @noRd
        get_method_power = function( method )
        {
            if( ! base::is.character( method ) )
                { stop( base::paste( "Method must be character; got", method,
                    "instead" ) ) }

            if( private$set_method_power() )
                { return( private$method_power[[ method ]] ) }

            stop( "Can't set 'method_power' attribute" )
        },
        #' @description
        #' Accessor to the algorithm weights, if present in the RastaState
        #' object.
        #' 
        #' @noRd
        get_method_weights = function()
        {
            if( base::length( private$weights ) > 0 )
                { return( private$weights ) }

            stop( "Weights were not set" )
        },
        #' @description
        #' Accessor to the vector of methods used to construct the `RastaState`
        #' object.
        #' 
        #' @noRd
        get_methods = function()
        {
            return( private$methods )
        },
        #' @description
        #' Get the method count corresponding to the decimal ID that identifies 
        #' the method ensemble.
        #'
        #' @param dec_id An integer that identifies the ensemble.
        #' 
        #' @noRd
        get_dec_id_method_count = function( dec_id )
        {
            sanitize_dec_index( dec_id )

            method_count <- self$get_dec_id_method_list( dec_id ) %>%
                base::length(.) %>%
                base::as.integer(.) %>%
                return(.)
        },
        #' @description
        #' Get the list of methods corresponding to the decimal ID that
        #' identifies the method ensemble.
        #'
        #' @param dec_id An integer that identifies the ensemble.
        #' 
        #' @noRd
        get_dec_id_method_list = function( dec_id )
        {
            sanitize_dec_index( dec_id )

            method_vec <- self$get_bin_vec( dec_id )

            return( method_vec[ method_vec == 1 ] %>% base::names(.) )
        },
        #' @description
        #' Get the exhaustive list of binary states that fully describe all 
        #' method combinarions in the ensemble.
        #' 
        #' @noRd
        get_state_list = function()
        {
            return( private$state_list )
        },
        #' @description
        #' Get vote count corresponding to the decimal state passed as
        #' argument to the method.
        #'
        #' @param dec_state An integer that identifies the ensemble state.
        #' 
        #' @noRd
        get_votes = function( dec_state )
        {
            sanitize_dec_index( dec_state )
            
            return( private$get_attribute( dec_state ) )
        },
        #' @description
        #' Get the dot product of the weights and binary vector corresponding to
        #' the decimal state passed as argument to the method.
        #'
        #' @param dec_state An integer that identifies the ensemble state.
        #' 
        #' @noRd
        get_wgt_prod = function( dec_state )
        {
            base::stopifnot( base::is.numeric( dec_state ) )
            
            self$get_method_weights() * self$get_bin_vec( dec_state ) %>%
                return(.)
        },
        #' @description
        #' Get weighted votes corresponding to the decimal state and function
        #' passed as arguments to the method.
        #'
        #' @param dec_state An integer that identifies the ensemble state.
        #' @param f A character that indicates what type of voting to compute.
        #' 
        #' @noRd
        get_wgt_votes = function( dec_state, f )
        {
            sanitize_dec_index( dec_state )
            base::stopifnot( is.character( f ), f %in% c( "mrule", "sum" ) )

            dec_key <- private$get_dec_key( dec_state )
            
            if( ! is.null( private$state_list[[ dec_key ]][[ 'wgt_votes' ]] ) )
                { return( private$state_list[[ dec_key ]][[ 'wgt_votes' ]] ) }

            wgt_sum <- base::sum( self$get_wgt_prod( dec_state ) )
            
            if( f == "sum" )
            {
                private$state_list[[ dec_key ]][[ 'wgt_votes' ]] <- wgt_sum
            }
            else
            {
                private$state_list[[ dec_key ]][[ 'wgt_votes' ]] <-
                    ifelse( wgt_sum > 0.5, 1, 0 )
            }

            return( private$state_list[[ dec_key ]][[ 'wgt_votes' ]] )
         } ),
    private = list( methods = vector(), method_power = vector(),
                    state_list = list(), weights = vector(),
        get_dec_key = function( dec_state )
        {
            base::stopifnot( is.numeric( dec_state ) )

            return( base::paste0( 'state', dec_state ) )
        },
        set_state_list = function()
        {
            private$state_list <- get_state_list( private$methods )
        },
        set_method_power = function()
        {
            if( base::length( private$method_power ) > 0 )
                { return( TRUE ) }
            
            private$method_power <- c( ( base::length( private$methods ) - 1 ):0 )
            base::names( private$method_power ) <- private$methods

            return( TRUE )
        },
        set_method_weights = function( weights )
        {
            if( ! base::is.vector( weights ) || ! base::is.numeric( weights ) )
                { paste( "Expected numeric vector, got", weights ) %>% stop(.) }

            if( base::length( weights ) != 1 &
                base::length( weights ) != base::length( self$get_methods() ) )
                    { stop( "Weight and method vector have different lengths" ) }

            private$weights <- weights

            return( TRUE )
        },
        get_attribute = function( dec_state )
        {
            if( base::is.na( dec_state ) )
                { return( NA ) }

            attrb_name <- base::gsub( 'get_', '',
                envnames::get_fun_name( n = 1 ) )

            if( ! base::is.numeric( dec_state ) )
                { stop( base::paste0( "Non-numeric State was passed to get_",
                    attrb_name ) ) }

            dec_key <- private$get_dec_key( dec_state )

            return( private$state_list[[ dec_key ]][[ attrb_name ]] )
        } ) )
