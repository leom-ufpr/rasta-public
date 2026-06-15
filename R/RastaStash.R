#' R6 Class to instantiate `RastaStash` objects
#'
#' Workhorse class that instantiates rich objects with all the
#' necessary methods to implement ensemble learning in ENM using information
#' theory.
#' 
#' @export
RastaStash <- R6::R6Class( "RastaStash",
    inherit = RastaAbs,                      
    public = base::list(

        #' @description
        #' Create a new `RastaStash` object.
        #' 
        #' @param stash A list of `RastaLand` or 'RastaStash` objects. If 
        #' omitted, the constructor returns an empty `RastaStash` object that 
        #' may be subsequently populated using the method `add`.
        #' @param .enforce_binary A logical scalar. Unless you are a developer, 
        #' do not set this parameter. Default: TRUE.
        #' @param .profile A logical scalar. Unless you are a developer, do not 
        #' set this parameter. Default: FALSE.
        #' @param .verbosity An integer. Unless you are a developer, do not set 
        #' this parameter. Default: 0.
        #'
        #' @return A `RastaStash` object.
        #'
        #' @details You may pass a mixed list of `RastaStash` and `RastaLand`
        #' objects to the constructor.  However, make sure that all objects
        #' have the same `class` attribute and that the number of pseudo-replicates
        #' per method at the end of object construction is identical.
        #'
        #' @examples
        #' \dontrun{ # Returns an empty `RastaStash` object
        #' rs <- RastaStash$new()
        #' # Returns a populated `RastaStash` using a list of `RastaLand` objects
        #' rs <- RastaStash$new( stash = list( rl1, rl2, rl3 ) )
        #' # Returns a populated `RastaStash` using a list of `RastaStash` objects
        #' rs <- RastaStash$new( stash = list( rs1, rs2, rs3 ) ) }

        initialize = function( stash = list(), .enforce_binary = TRUE, 
            .profile = FALSE, .verbosity = 0, .threads = 1L )
        {
            if( ! base::is.list( stash ) )
            {
                stop( get_var_val_type_msg( 'stash', stash, 
                    'constructor', 'RastaStash'  ) )
            }

            if( ! base::is.logical( .enforce_binary ) )
            {
                stop( get_var_val_type_msg( 'enforce_binary', .enforce_binary,
                    'constructor', 'logical' ) )
            }

            if( ! base::is.logical( .profile ) )
            {
                stop( get_var_val_type_msg( 'profile', .profile,
                    'constructor', 'logical' ) )
            }

            if( ! is_integer( .verbosity ) )
            {
                stop( get_var_val_type_msg( 'verbosity', .verbosity,
                    'constructor', 'integer' ) )
            }

            private$enforce_binary <- .enforce_binary
            private$profile <- .profile
            private$verbosity <- .verbosity
            self$set_threads( .threads )

            if( base::length( stash ) > 0 )
            {
                for ( rasta_object in stash ) self$add( rasta_object )
            }
        },

        #' @description
        #' Populates a `RastaStash` object with `RastaLand` objects or other
        #' `RastaStash` objects. 
        #'
        #' @param rasta_object A valid `RastaLand` or `RastaStash` object.
        #' See `RastaLand` documentation for details on construction.
        #'
        #' @return TRUE if the `Rasta` object was correctly added to `RastaStash`.
        #'
        #' @details This method is internally called if `RastaStash` objects are
        #' built using the `stash` parameter. You may add a mix of `RastaStash` and
        #' `RastaLand` objects to the stash. However, make sure that all objects
        #' have the same `class` attribute and that the number of pseudo-replicates
        #' per method at the end of object construction is identical.
        #'
        #' @examples
        #' \dontrun{ rs <- RastaStash$new() # Creates an empty `RastaStash` object 
        #' rs$add( rasta_land ) # Adds first `RastaLand` to the stash
        #' rs$add( another_rasta_land ) # Adds another `RastaLand` to the stash
        #' 
        #' rs <- RastaStash$new() # Creates an empty `RastaStash` object 
        #' rs$add( rasta_stash ) # Adds first `RastaStash` to the stash
        #' rs$add( another_rasta_stash ) # Adds another `RastaStash` to the stash }

        add = function( rasta_object )
        {
            if( test_class( rasta_object, 'RastaLand' ) )
            {
                private$add_rasta_land( rasta_object )
            }
            else if( test_class( rasta_object, 'RastaStash' ) )
            {
                private$add_rasta_stash( rasta_object )
            }
            else
            {
                message <- get_var_val_type_msg( 'object', rasta_object, 'add',
                    'RastaLand or RastaStash' )

                stop( message )
            }
        },

        # Metadata methods
        #' @description
        #' Accessor to `RastaStash` object's class.
        #' 
        #' @return `RastaStash`'s object class. The class is a label such as the
        #' taxon name. If called on an empty `RastaStash`, will return NULL.
        #'
        #' @examples
        #' \dontrun{ my_class <- rs$get_class() }

        get_class = function()
        {
            return( private$class )
        },

        # Metadata methods
        #' @description
        #' Accessor to the `RastaLand` stash store in the `RastaStash` object.
        #'
        #' @return A list with all `RastaLand` objects stored in the `RastaStash`
        #' object. If called on an empty `RastaStash`, will return an empty list.
        #' 
        #' @examples
        #' \dontrun{ my_stash <- rs$get_all() }

        get_all = function()
        {
            return( private$stash )
        },

        #' @description
        #' Accessor to the list counting the number of `RastaLand` objects that
        #' share the same method (SDM algorithm) in the stash.
        #' 
        #' @return A list indexed by methods whose values are the `RastaLand`
        #' object counts. If called on an empty `RastaStash`, will return an empty list.
        #' 
        #' @examples
        #' \dontrun{ my_method_count <- rs$get_method_count_list() }

        get_method_count_list = function()
        {
            return( private$method_count_list )
        },

        #' @description
        #' Lists all methods (SDM algorithms) contained in the stash.
        #' 
        #' @return A vector with the method names in alphabetical order or an
        #' empty vector if `RastaStash` object is empty.
        #'
        #' @examples
        #' \dontrun{ my_methods <- rs$get_methods() }

        get_methods = function()
        {
            if( base::length( self$get_method_count_list() ) > 0 )
            {
                self$get_method_count_list() %>%
                    base::unlist(.) %>%
                    base::names(.) %>%
                    base::sort(.) %>%
                    return(.)
            }
            else
            {
                return( c() )
            }
        },

        #' @description
        #' Number of methods contained in the stash.
        #' 
        #' @return A positive integer or 0 if `RastaStash` object is empty.
        #' 
        #' @examples
        #' \dontrun{ my_method_count <- rs$get_method_count() }

        get_method_count = function()
        {
            if( base::length( self$get_methods() ) > 0 )
            {
                self$get_methods() %>%
                    base::length(.) %>%
                    base::as.integer(.) %>%
                    return(.)
            }
            else
            {
                return( 0 )
            }
        },

        #' @description
        #' Number of replicates used to generate the `RastaLand` stash.
        #' 
        #' @return A positive integer or 0 if `RastaStash` object is empty.
        #' 
        #' @examples
        #' \dontrun{ my_rep_count <- rs$get_rep_count() }

        get_rep_count = function()
        {
            if( self$get_method_count() == 0 )
            {
                return( 0 )
            }
            else
            {
                private$process_reps( max ) %>%
                    return(.)
            }
        },

        #' @description
        #' Accessor to the `RastaLand` objects using their order in the stash.
        #' 
        #' @param i Positive integer corresponding to the object index in the 
        #' stash.
        #' 
        #' @return A `RastaLand` object or NULL if `RastaStash` object is empty.
        #' 
        #' @examples
        #' \dontrun{ first_rasta_land <- rs$get_by_index( 1 )
        #' second_rasta_land <- rs$get_by_index( 2 ) }

        get_by_index = function( i )
        {
            if( ! self$has_stash() )
            {
                return( NULL )
            }
            else
            {
                private$sanitize_index( i ) %>%
                    private$stash[[.]] %>%
                    return(.)
            }
        },

        #' @description
        #' Accessor to the `RastaLand` objects generated using the method passed 
        #' as an argument to the method.
        #' 
        #' @param method A string representing a valid method name.
        #' 
        #' @return A list of `RastaLand` objects.
        #' 
        #' @examples
        #' \dontrun{ ann_rasta_land_list <- rs$get_by_method( "ann" ) }

        get_by_method = function( method )
        {
            private$assert_stash_and_reps( 'get_by_method' )
            private$set_check_lists( 'get_by_method' )

            if( private$unknown_method( method ) )
            {
                base::paste( "Method", method, "does not exist" ) %>% stop(.)
            }

            base::lapply( c( 1:self$get_rep_count() ), function( rep )
            { private$by_method_list[[ method ]][[ rep ]] %>%
                    self$get_by_index(.) } ) %>%
                return(.)
        },

        #' @description
        #' Accessor to the `RastaLand` objects corresponding to the replicate
        #' number passed as arguments, regardless of the SDM algorithm used to
        #' generate them.
        #' 
        #' @param i An integer corresponding to a valid replicate number.
        #' 
        #' @return A list of `RastaLand` objects.
        #' 
        #' @examples
        #' \dontrun{ first_rep_rasta_land_list <- rs$get_by_rep( 1 ) }

        get_by_rep = function( i )
        {
            private$assert_stash_and_reps( 'get_by_rep' )
            private$set_check_lists( 'get_by_rep' )

            rep <- private$sanitize_index( i ) %>% get_list_rep(.)

            base::lapply( self$get_methods(), function( method )
            { private$by_rep_list[[ rep ]][[ method ]] %>%
                  self$get_by_index(.) } ) %>%
                return(.)
        },

        #' @description
        #' Accessor to the number of `RastaLand` objects in the stash.
        #' 
        #' @return An integer greater than 0 for a populated `RastaStash` object,
        #' 0 if object is empty.
        #' 
        #' @examples
        #' \dontrun{ rasta_land_count <- rs$get_rasta_land_count() }

        get_rasta_land_count = function()
        {
            return( base::length( private$stash ) )
        },

        #' @description
        #' Accessor to the raster size (in non-NA pixels) in the stash. 
        #' 
        #' @return An integer for a populated `RastaStash` object, NULL if
        #' object is empty.
        #' 
        #' @examples
        #' \dontrun{ raster_size <- rs$get_raster_size() }

        get_raster_size = function()
        {
            return( private$raster_size_control )
        },

        #' @description
        #' Returns TRUE if object was populated with `RastaLand` objects or
        #' FALSE if `RastaStash` is empty.
        #' 
        #' @return A logical value.
        #' 
        #' @examples
        #' \dontrun{ rs <- RastaStash$new() # Create an empty stash
        #' rs$has_stash() # Returns FALSE
        #' rs <- RastaStash$new( stash = list( rs1, rs2 ) ) # Create a populated stash
        #' rs$has_stash() # Returns TRUE }
        
        has_stash = function()
        {
            if( base::length( private$stash ) == 0 )
            {
                return( FALSE )
            }
            else
            {
                return( TRUE )
            }
        },

        #' @description
        #' Returns TRUE if object was populated with `RastaLand` objects and
        #' list of methods was internally assembled and FALSE otherwise
        #' 
        #' @return A logical value.
        #' 
        #' @examples
        #' \dontrun{ rs <- RastaStash$new() # Create an empty stash
        #' rs$has_methods() # Returns FALSE
        #' rs <- RastaStash$new( stash = list( rs1, rs2 ) ) # Create a populated stash
        #' rs$has_methods() # Returns TRUE }
        
        has_methods = function()
        {
            if( self$get_method_count() == 0 )
            {
                return( FALSE )
            }
            else
            {
                return( TRUE )
            }
        },
        
        # Data methods
        #' @description
        #' Bootstraps uncertainty. 
        #' 
        #' @param bs_reps An integer corresponding to number of desired
        #' pseudo-replicates. If omitted, will default to the number of
        #' replicates in the stash.
        #' @param bs_matrix A design matrix used to enforce the repeated
        #' measures design. If bs_matrix is passed to this method, bs_reps is
        #' ignored. See details for more information.  
        #' 
        #' @return A list with two entries: a vector with the bootstrapped
        #' uncertainty value for each pseudo-replicate and the design matrix.
        #' 
        #' @details Uncertainty is calculated as the sum of the entropy of each
        #' pixel in the uncertainty raster (surface). It is boostrapped by
        #' random re-sampling (with replacement) the binary rasters generated,
        #' for instance, by a cross-validation procedure. If the user employed a
        #' repeated measures design i.e., each training and validation set are
        #' subjected to all algorithms in the ensemble, passing the design
        #' matrix as a parameter when bootstrapping uncertainty for another
        #' algorithm will ensure that the same experiments are also used across
        #' algorithms. 
        #' 
        #' @examples
        #' \dontrun{ # ANN uncertainty will be bootstrapped using 1000
        #' # pseudo-replicates
        #' ann_bs_list <- ann_rs$bootstrap_uncertainty( bs_reps = 1000 )
        #' ann_bs_uncertainty <- ann_bs_list$bs_uncertainty
        #' # Uses the same matrix to enforce the repeated measures design
        #' # when computing the bootstrapped uncertainty for GLM
        #' glm_bs_list <- glm_rs$bootstrap_uncertainty( bs_matrix =
        #'     ann_bs_list$bs_matrix ) }

        bootstrap_uncertainty = function( bs_reps = self$get_rep_count(),
            bs_matrix = base::matrix() )
        {
            if( ! is_integer( bs_reps ) )
            {
                stop ( get_var_val_type_msg( 'bs_reps', bs_reps, 
                    'bootstrap_uncertainty', 'integer'  ) )
            }

            if( ! base::is.matrix( bs_matrix ) )
            {
                stop ( get_var_val_type_msg( 'bs_matrix', bs_matrix, 
                    'bootstrap_uncertainty', 'matrix'  ) )
            }
            
            if( bs_reps < 1 )
            {
                stop( base::paste( "Either 'RastaStash' object has no 'RastaLand'",
                                   "stash or selected 'bs_reps' is smaller than 1;",
                                   "can't bootstrap uncertainty" ) )
            }

            private$assert_stash_and_reps( 'bootstrap_uncertainty' )

            message( "Bootstrapping uncertainty, please wait..." )

            bs_reps <- base::trunc( bs_reps )
            bs_uncertainty <- base::rep( 0, bs_reps )

            if( ! super$has_state_mx() )
            {
	        return( bs_uncertainty )
            }
            else if( base::nrow( bs_matrix ) == 1 )
	    {
	        rep_count <- self$get_rep_count()
                bs_matrix <- base::matrix( rep( 0, bs_reps * rep_count ),
                    nrow = bs_reps )

                for( bs_rep in 1:bs_reps )
                {
                    bs_matrix[ bs_rep, ]  <- base::sample( c( 1:rep_count ),
                        size = rep_count, replace = TRUE )

                    bs_uncertainty[ bs_rep ] <- bs_matrix[ bs_rep, ] %>%
		        private$assemble_uncertainty(.)
                }
            }
            else
	    {
                for( bs_rep in 1:bs_reps )
                {
                    bs_uncertainty[ bs_rep ] <- bs_matrix[ bs_rep, ] %>%
		        private$assemble_uncertainty(.)
                }
	    }

            return( list( bs_uncertainty = bs_uncertainty,
                bs_matrix = bs_matrix ) )
        },

        #' @description
        #' Not fully tested, do not use.
        #' 
        #' @keywords internal

        get_diversity = function()
        {
            private$assert_stash_and_reps( 'get_diversity' )
            private$check_matrices( 'get_diversity' )
            
            return( super$get_diversity() )
        },

        #' @description
        #' Not fully tested, do not use.
        #' 
        #' @keywords internal

        get_gini_raster = function()
        {
            private$assert_stash_and_reps( 'get_gini_raster' )
            total_votes <- self$get_rasta_land_count()

            p1 <- raster::calc( self$get_vote_raster(),
                function( x ) { ( x / total_votes ) ** 2 } )
            p0 <- raster::calc( self$get_vote_raster(),
                function( x ) { ( ( total_votes - x ) / total_votes ) ** 2 } )
            
            return( 1 - ( p0 + p1 )  )
        },

        #' @description
        #' Get uncertainty as the sum of the entropies of every pixel in the
        #' uncertainty raster.
        #' 
        #' @return A numeric value.
        #' 
        #' @examples
        #' \dontrun{ algo_uncertainty <- rs$get_uncertainty() }

        get_uncertainty = function()
        {
            private$assert_stash_and_reps( 'get_uncertainty' )
            private$check_matrices( 'get_uncertainty' )

            return( super$get_uncertainty() )
        },

        #' @description
        #' Get uncertainty raster.
        #' 
        #' @return A `RasterLayer` object.
        #' 
        #' @examples
        #' \dontrun{ uncertainty_raster <- rs$get_uncertainty_raster() }

        get_uncertainty_raster = function()
        {
            private$assert_stash_and_reps( 'get_uncertainty_raster' )
            private$check_matrices( 'get_uncertainty_raster' )

            return( private$set_uncertainty_raster() )
        },
        
        #' @description
        #' Not fully tested, do not use.
        #' 
        #' @keywords internal

        get_normalized_entropy_raster = function()
        {
            private$assert_stash_and_reps( 'get_normalized_entropy_raster' )

            f <- function( x ) { x / self$get_method_count() }

            return( raster::calc( self$get_entropy_raster(), f ) )
        }, 

        #' @description
        #' Not fully tested, do not use
        #' 
        #' @keywords internal

        get_pareto_front = function() 
        {
            private$assert_stash_and_reps( 'get_pareto_front' )

            if( self$get_method_count() < 3 )
            {
                warning( "Can't build a Pareto front with less than 3 methods" )
                return( NULL )
            } 

            rasta_state <- self$get_rasta_state()
            valid_dec_ids <- get_valid_dec_ids( rasta_state )

            pareto_tibble <- base::lapply( valid_dec_ids,
                function( dec_id ) { private$process_pair( dec_id, rasta_state ) } ) %>%
                dplyr::bind_rows(.) %>%
                dplyr::arrange( dec_id ) %>%
            return(.)
        },

        #' @description
        #' Not fully tested, do not use
        #' 
        #' @keywords internal

        get_rasta_state = function()
        {
            private$set_attribute( 'rasta_state' ) %>%
                return(.)
        },

        #' @description
        #' Get the mean value of the efficiency raster.
        #' 
        #' @return A numeric value.
        #' 
        #' @examples
        #' \dontrun{ algo_efficiency <- rs$get_efficiency() }

        get_efficiency = function()
        {
            private$assert_stash_and_reps( 'get_efficiency' )

            self$get_efficiency_raster() %>%
                get_raster_mean(.) %>%
                return(.)
        },

        #' @description
        #' Get the efficiency raster.
        #' 
        #' @return A raster object.
        #' 
        #' @examples
        #' \dontrun{ efficiency_raster <- rs$get_efficiency_raster() }

        get_efficiency_raster = function()
        {
            private$set_attribute( 'efficiency_raster' ) %>%
                return(.)
        },

        #' @description
        #' Get the raster corresponding to the sum of all binary rasters in the
        #' stash.
        #' 
        #' @return A raster object.
        #' 
        #' @examples
        #' \dontrun{ vote_raster <- rs$get_vote_raster() }

        get_vote_raster = function()
        {
            private$set_attribute( 'vote_raster' ) %>%
                return(.)
        },

        #' @description
        #' Not fully tested, do not use
        #' 
        #' @keywords internal

        get_mrule_raster = function()
        {
            private$set_attribute( 'mrule_raster' ) %>%
                return(.)
        },

        #' @description
        #' Get the unweighted probability raster.
        #' 
        #' @return A raster object.
        #' 
        #' @examples
        #' \dontrun{ prob_raster <- rs$get_prob_raster() }

        get_prob_raster = function()
        {
            private$assert_stash_and_reps( 'get_prob_raster' )

            return( self$get_vote_raster() / self$get_rasta_land_count() )
        },

        #' @description
        #' Get the number of current threads (logical processors)
        #' 
        #' @return An integer corresponding to the number of threads
        #' 
        #' @examples
        #' \dontrun{ thread_count <- rs$get_threads() }
        get_threads = function()
        {
            return( private$threads )
        },
        
        #' @description
        #' Get a stack of rasters, each corresponding to the weights computed
        #' for each algorithm in the stash.
        #' 
        #' @return A stack object.
        #' 
        #' @examples
        #' \dontrun{ weight_stack <- rs$get_weight_stack() }
        
        get_weight_stack = function()
        {
            private$set_attribute( 'weight_stack' ) %>%
                return(.)
        },
        
        #' @description
        #' Not fully tested, do not use
        #' 
        #' @keywords internal

        get_wgt_mrule_raster = function()
        {
            private$set_attribute( 'wgt_mrule_raster' ) %>%
                return(.)
        },

        #' @description
        #' Get the weighted probability raster.
        #' 
        #' @return A raster object.
        #' 
        #' @examples
        #' \dontrun{ weighted_prob_raster <- rs$get_wgt_prob_raster() }

        get_wgt_prob_raster = function()
        {
            private$set_attribute( 'wgt_prob_raster' ) %>%
                return(.)
        },

        #' @description
        #' Set the number of threads to be used in some paralellized operations.
        #' Thread number will be internally set to the minimum value, either the argument
        #' "threads" or the number of logical cores. Negative values will be ignored.
        #' 
        #' @return Nothing
        #' 
        #' @examples
        #' \dontrun{ rs$set_threads( threads = 6L ) }
        set_threads = function( threads = 1L )
        {
            if( ! is_integer( threads ) )
            {
                stop( get_var_val_type_msg( 'threads', threads,
                    'set_threads', 'integer' ) )
            }

            threads <- base::max( c( threads, 1L ) )
            threads <- base::min( c( threads, parallel::detectCores() ) )

            private$threads <- threads
        }
    ),
    private = list( base_values = NULL, base_raster = NULL,
                    by_method_list = list(), by_rep_list = list(), class = NULL,
                    disable_mx = FALSE, efficiency_raster = NULL, enforce_binary = TRUE,
                    entropy_raster = NULL, map_mx = NULL, max_idx = NULL,
                    method_count_list = list(), method_power = double(),
                    profile = FALSE, rasta_state = NULL, raster_size_control = NULL,
                    rep_method_mx = NULL, snr_raster = NULL, stash = list(),
                    state_idx = NULL, state_mx = NULL, mrule_raster = NULL,
                    prob_raster = NULL, uncertainty_raster = NULL, threads = integer(),
                    vote_raster = NULL, verbosity = NULL, weight_stack = NULL,
                    wgt_mrule_raster = NULL, wgt_prob_raster = NULL, zero_idx = NULL,
        add_rasta_land = function( rasta_land )
        {
            base::stopifnot( test_class( rasta_land, 'RastaLand' ) )

            if( private$enforce_binary && isnt_binary_raster( rasta_land ) )
            {
                message <- base::paste0( 'Replicate #', rasta_land$get_rep(),
                    ' generated using "', rasta_land$get_method(),
                    '" has non-binary cells' )

                stop( message )
            }

            private$check_class( rasta_land$get_class() )
            private$update_method_list( rasta_land$get_method() )

            raster_size <- rasta_land$get_raster_obj() %>%
                raster::getValues(.) %>%
                stats::na.omit(.) %>%
                base::length(.) %>%
                private$update_raster_size(.)
            
            private$stash <- c( private$stash, rasta_land )
            
            return( TRUE )
        },
        add_rasta_stash = function( rasta_stash )
        {
            base::stopifnot( test_class( rasta_stash, 'RastaStash' ) )

            private$check_class( rasta_stash$get_class() )
            private$update_raster_size( rasta_stash$get_raster_size() )
            method_list <- rasta_stash$get_method_count_list()
            
            for ( method in base::names( method_list ) )
            {
                qty <- method_list[[ method ]]
                private$update_method_list( method, qty ) 
            }
            
            private$stash <- c( private$stash, rasta_stash$get_all() )

            return( TRUE )
        },
        assert_stash_and_reps = function( fun_name )
        {
	    base::stopifnot( base::is.character( fun_name ) )

            if( ! self$has_stash() )
            {
                stop( base::paste( "'RastaStash' object has no 'RastaLand' stash;",
                                   "can't execute", fun_name ) )
            }
            else if( ! self$has_methods() )
            {
                stop( base::paste( "'RastaStash' object has no internally built",
                                   "method list; can't execute", fun_name ) )
            }
            else if( ! private$check_reps() )
            {
                stop( base::paste( "Found different replicate counts across",
                                   "'RastaStash' methods; can't execute", fun_name ) )
            }
            else
            {
                return( TRUE )
            }
        },
        check_matrices = function( fun_name )
        {
            base::stopifnot( base::is.character( fun_name ) )

            if( ! self$has_stash() )
            {
                if( base::is.null( private$get_zero_idx() ) &&
                    base::is.null( private$get_max_idx() ) )
                {
                    stop( base::paste( "Can't build 'state_mx', needed to execute",
                        fun_name,  "please instantiate a new 'RastaStash' object" ) )
                }
            }

            return( TRUE )
        },
	assemble_uncertainty = function( rows )
	{
	    base::stopifnot( base::is.numeric( rows ) )
	    
            private$state_mx[ rows, ] %>%
                super$get_cell_values(.) %>%
                super$initialize_raster(.) %>%
                sum_raster(.) %>%
		return(.)
        },
        check_reps = function()
        {
            if( ! self$has_methods() )
            {
                return( FALSE )
            }
            else if( private$process_reps( min ) != private$process_reps( max ) )
            {
                return( FALSE )
            }
            else
            {
                return( TRUE )
            }
        },
        get_base_raster = function()
        {
            private$set_attribute( 'base_raster' ) %>%
                return(.)
        },
        get_base_values = function()
        {
            private$set_attribute( 'base_values' ) %>%
                return(.)
        },
        get_built_rasta_front = function( dec_id )
        {
            base::stopifnot( is_integer( dec_id ) )

            base_args <- private$get_rf_base_args( dec_id )

            rasta_front <- base::do.call( RastaFront$new, c( base_args,
                base::list( max_idx = private$get_max_idx(),
                state_idx = private$get_state_idx(),
                zero_idx = private$get_zero_idx() ) ) )

            base::lapply( 1:base::nrow( private$get_state_mx() ), function( i )
                { rasta_front$build_state_mx( i, private$state_mx[ i, ] ) } )

            return( rasta_front )
        },
        get_bin_state_mx = function( i, pixel_count )
        {
            private$rep_method_mx[ i, ] %>%
                private$stash[ . ] %>%
                base::lapply( ., function( rasta_land )
                { private$state_idx %>%
                      rasta_land$get_raster_obj()[ . ] } ) %>%
                base::unlist(.) %>%
                base::matrix( ., ncol = pixel_count, byrow = TRUE ) %>%
                return(.)
        },
        get_d_raster = function( h_stack ) # Weighted votes functions
        {
            base::stopifnot( test_class( h_stack, 'RasterStack' ) )

            sum_raster <- raster::calc( h_stack, fun = base::sum )

            return( self$get_method_count() - sum_raster )
        },
        get_front_state_mx = function( dec_id )
        {
            base::stopifnot( is_integer( dec_id ), private$has_map_mx() )

            if( base::is.null( private$get_state_idx() ) )
                { return( NULL ) }

            row_count <- base::nrow( private$get_state_mx() )
            col_count <- base::ncol( private$state_mx )

            front_state_mx <- base::rep( 0, row_count * col_count ) %>%
                base::matrix( ., nrow = row_count )

            for( i in 1:row_count )
            {
                rep_idx <- base::which( private$state_mx[ i, ] > 0 )
                front_state_mx[ i, rep_idx ] <- private$map_mx[ dec_id,
                    private$state_mx[ i, ] ]
            }

            return( front_state_mx )
        },
        get_h_stack = function()
        {
            base::lapply( self$get_methods(), function( x )
                { private$get_h_raster( x ) } ) %>%
                raster::stack(.) %>%
                return(.)
        },
        get_h_raster = function( method )
        {
            base::stopifnot( base::is.character( method ) )

            method_stash <- RastaStash$new( stash = self$get_by_method( method ) )

            return( method_stash$get_entropy_raster() )
        },
        get_init_freqs = function( vec_name, fill_value )
        {
            base::stopifnot( base::is.character( vec_name ),
                assertthat::is.number( fill_value ) )

            if( base::is.null( private[[ vec_name ]] ) )
                return( c() )

            return( super$get_init_freqs( vec_name, fill_value ) )            
        },
        get_max_idx = function()
        {
            private$set_attribute( 'max_idx' ) %>%
                return(.)
        },
        get_map_mx = function()
        {
            return( private$map_mx )
        },
        get_mx_rasta_front = function( dec_id )
        {
            base::stopifnot( is_integer( dec_id ) )

            if( base::is.null( private$get_state_idx() ) &&
                base::is.null( private$get_zero_idx() ) )
                { return( NULL ) }

            if( base::is.null( private$get_state_idx() ) &&
                base::is.null( private$get_max_idx() ) )
                { return( NULL ) }

            base_args <- private$get_rf_base_args( dec_id )
            
            rasta_front <- base::do.call( RastaFront$new, c( base_args,
                list( max_idx = private$get_max_idx(),
                      state_idx = private$get_state_idx(),
                      state_mx = private$get_front_state_mx( dec_id ),
                      zero_idx = private$get_zero_idx() ) ) )

            return( rasta_front )
        },
        get_raster = function( f, zero_fill, max_fill )
        {
            base::stopifnot( test_class( f, 'function' ),
                       assertthat::is.number( zero_fill ),
                       assertthat::is.number( max_fill ) ) 

            private$get_cell_values( zero_fill, max_fill, f ) %>%
                super$initialize_raster(.) %>%
                return(.)
        },
        get_rep_method_mx = function()
        {
            private$set_attribute( 'rep_method_mx' ) %>%
                return(.)
        },
        get_rf_base_args = function( dec_id )
        {
            base::stopifnot( is_integer( dec_id ) )
            
            return( base::list( rep_count = self$get_rep_count(),
                                dec_id = dec_id,
                                base_raster = private$get_base_raster(),
                                base_values = private$get_base_values(),
                                from_rasta_state = self$get_rasta_state() ) )
        },
        get_state_idx = function()
        {
            private$set_attribute( 'state_idx' ) %>%
                return(.)
        },
        get_state_mx = function()
        {
            private$set_attribute( 'state_mx' ) %>%
                return(.)
        },
        get_vote_values = function()
        {
            vote_raster <- self$get_vote_raster()
            
            return( vote_raster[ , ] %>% base::as.vector(.) )
        },
        get_weight_mx = function()
        {
            if( base::is.null( private$get_state_idx() ) )
                { return( matrix() ) }

            weight_mx <- base::rep( NA, self$get_method_count() *
                base::ncol( private$get_state_mx() ) ) %>%
                base::matrix( . , nrow = self$get_method_count() )

            weight_stack <- self$get_weight_stack()

            for( i in 1:self$get_method_count() )
            {
                weight_mx[ i, ] <- weight_stack[[ i ]][ private$get_state_idx() ] %>%
                    base::unlist(.) %>%
                    base::as.vector(.)
            }

            return( weight_mx )
        },
        get_wgt_votes_calc = function( weight_mx, criterion )
        {
            base::stopifnot( test_class( weight_mx, 'matrix' ) )
            base::stopifnot( base::is.character( criterion ),
                             criterion %in% c( "mrule", "sum" ) )

            rasta_state <- self$get_rasta_state()
            
            wgt_votes_calc <- function( j )
            {
                rasta_state$add_weights( weight_mx[, j ] )

                base::lapply( private$state_mx[, j ], function( x )
                    { rasta_state$get_wgt_votes( x, criterion ) } ) %>%
                    base::unlist(.) %>%
                    base::sum(.) %>%
                    return(.)
            }

            return( wgt_votes_calc )
        },
        get_wgt_votes_raster = function( criterion,
            weight_mx = private$init_weight_mx() )
        {
            base::stopifnot( assertthat::is.string( criterion ) &&
                             criterion %in% c( 'mrule', 'sum' ) )
            base::stopifnot( base::is.matrix( weight_mx ) )

            wgt_votes_calc <- private$get_wgt_votes_calc( weight_mx, criterion )
            cell_values <- super$initialize_cell_values( 0, self$get_rep_count() )

            if( ! base::is.null( private$get_state_idx() ) )
            {
                cell_values[ private$state_idx ] <-
                    base::lapply( 1:base::ncol( private$get_state_mx() ),
                        wgt_votes_calc ) %>%
                    base::unlist(.)
            }
            
            return( super$initialize_raster( cell_values ) / self$get_rep_count() )
        },
        get_zero_idx = function()
        {
            private$set_attribute( 'zero_idx' ) %>%
                return(.)
        },
        has_map_mx = function()
        {
            if ( private$disable_mx )
                { return( FALSE ) }

            mx_file <- base::paste0( 'map_mx_', self$get_method_count(), '.bin' )
            map_path <- system.file( 'extdata', mx_file, package = 'Rasta' )

            if ( base::file.exists( map_path ) )
            {
                base::load( map_path )
                private$map_mx <- map_mx
                return( TRUE )
            }
            else
            {
                return( FALSE )
            }
        },
        init_unw_rasta_state = function()
        {
            rasta_state <- self$get_rasta_state()
            method_wt <- 1 / self$get_method_count()
            rep( method_wt, self$get_method_count() ) %>%
                rasta_state$add_weights(.)

            return( rasta_state )
        },
        init_weight_mx = function()
        {
            if( base::is.null( private$get_state_idx() ) )
            {
                return( matrix() )
            }
            else
            {
                return( private$get_weight_mx() )
            }
        },
        process_pair = function( dec_id, rasta_state )
	{
            base::stopifnot( test_class( rasta_state, "RastaState" ) )
        
            if( base::is.na( dec_id ) )
                { return( tibble() ) }

            base::stopifnot( is_integer( dec_id ) )
        
            if( private$has_map_mx() ) 
            {
                rasta_front <- private$get_mx_rasta_front( dec_id )
            } 
            else 
            {
                rasta_front <- private$get_built_rasta_front( dec_id )
            }

            tibble::tibble( dec_id = dec_id, 
                method_count = rasta_state$get_dec_id_method_count( dec_id ),
                diversity = base::ifelse( base::is.null( rasta_front ), 0,
                    rasta_front$get_diversity() ),
                uncertainty = base::ifelse( base::is.null( rasta_front ), 0,
                    rasta_front$get_uncertainty() ) ) %>%
                return(.)
	},
        process_reps = function( f )
        {
            base::stopifnot( test_class( f, 'function' ) )

            if( ! self$has_methods() )
            {
                stop( "'RastaStash' object has no methods, can't process reps." )
            }
            else
            {
                self$get_method_count_list() %>%
                    base::as.integer(.) %>%
                    f(.) %>%
                    return(.)
            }
        },
        set_attribute = function( fun_name )
        {
            base::stopifnot( base::is.character( fun_name ) )

            attrb_name <- base::gsub( 'get_', '', fun_name )
            set_method <- base::paste0( 'set_', attrb_name )

            if( base::is.null( private[[ attrb_name ]] ) )
            {
                base::tryCatch(
                {
                    if( private$profile && private$verbosity > 0 )
                        start_time <- base::proc.time()

                    if ( base::isTRUE( private[[ set_method ]]() ) )
                    {
                        attribute <- private[[ attrb_name ]]
                        
                        if( private$profile && private$verbosity > 0 )
                        {
                            message( base::paste( "Setting", attrb_name, "took",
                                base::sprintf( "%.6f", base::proc.time() -
                                    start_time ) ) )
                        }
                            
                        return( attribute )
                    }
                },
                error = function( e )
                {
                    message( base::paste0( "Failed when attempting to use method '",
                        set_method, "', and attribute name '", attrb_name, "'" ) )
                    message( e )
                } )
            }
            else
            {
                return( private[[ attrb_name ]] )
            }            
        },
        set_b_raster = function()
        {
            f <- function( x ) { ( x / self$get_rasta_land_count() ) }

            private$prob_raster <- raster::calc( self$get_vote_raster(), f )

            return( TRUE )
        },
        set_base_values = function()
        {
            seed_rasta <- self$get_by_index( 1 )

            private$base_values <- seed_rasta$get_raster_obj() %>%
                raster::getValues(.) %>%
                base::as.vector(.) %>%
                base::length(.) %>%
                base::rep( NA, . )

            return( TRUE )
        },
        set_base_raster = function()
        {
            private$set_check_lists( 'set_base_raster' )
            rasta_land <- self$get_by_index( 1 )
            private$base_raster <- rasta_land$get_raster_obj()

            return( TRUE )
        },
        set_check_lists = function( fun_name )
        {
            if( base::length( private$by_rep_list ) > 0 &&
                base::length( private$by_method_list ) > 0 )
                { return( TRUE ) }

            base::stopifnot( base::is.character( fun_name ) )
            private$assert_stash_and_reps( fun_name )

            max_rep = 0
            for( i in 1:self$get_rasta_land_count() )
            {
                rasta_land <- private$stash[[ i ]]
                list_rep <- get_list_rep( rasta_land$get_rep() )
                private$by_rep_list[[ list_rep ]][[ rasta_land$get_method() ]] <- i
                private$by_method_list[[ rasta_land$get_method() ]][[ list_rep ]] <- i

                if( rasta_land$get_rep() > max_rep )
                    { max_rep = rasta_land$get_rep() }
            }

            if( max_rep != self$get_rep_count() )
            {
                stop( base::paste( "Found a total rep count of", max_rep,
                    "replicates in RastaLand objects; RastaStash was set with a count of",
                    self$get_rep_count(), "reps - died after calling", fun_name ) )
            }

            rep_count = 0
            for( method in base::names( private$by_method_list ) )
            {
                if( rep_count == 0 )
                {
                    rep_count = base::length( private$by_method_list[[ method ]] )
                }
                else
                {
                    next
                }
            }

            return( TRUE )
        },
        set_disable_mx = function()
        {
            private$disable_mx = TRUE

        },
        set_efficiency_raster = function()
        {
            private$assert_stash_and_reps( 'get_efficiency_raster' )

            message( "Computing efficiency raster, please wait..." )
            
            cell_values <- self$initialize_cell_values( 1, 1 )

            if( ! base::is.null( private$get_state_idx() ) )
            {
	        weight_mx <- private$get_weight_mx() + inf_value()
	        p_matrix <- weight_mx * base::log2( weight_mx ) * ( -1 )
		h_cells <- Rfast::colsums( p_matrix )
		h_max <- base::log2( self$get_method_count() )
	        
	        cell_values[ private$state_idx ] <- h_cells / h_max
            }
            
            private$efficiency_raster <- super$initialize_raster( cell_values )

            return( TRUE )
        },
        set_idx = function( match_value )
        {
            base::stopifnot( assertthat::is.number( match_value ) )

            vote_values <- private$get_vote_values()
            match_cond <- vote_values == match_value
            focus_idx <- base::which( match_cond )

            base::ifelse( base::length( focus_idx ) > 0, return( focus_idx ),
                return( NULL ) ) 
        },
        set_max_idx = function()
        {
            private$max_idx <- self$get_rasta_land_count() %>%
                private$set_idx(.)

            return( TRUE )            
        },
        set_mrule_raster = function()
        {
            private$assert_stash_and_reps( 'get_mrule_raster' )
            
            if( base::is.null( private$get_state_idx() ) )
            {
                weight_mx <- base::matrix()
            }
            else
            {
                weight_mx <- base::rep( 1 / self$get_method_count(),
                        base::ncol( private$get_state_mx() ) ) %>%
                    base::matrix( ., nrow = 1 )
            }
            
            private$mrule_raster <- private$get_wgt_votes_raster( 'mrule',
                                                                  weight_mx )

            return( TRUE )
        },
        set_rasta_state = function()
        {
            private$assert_stash_and_reps( 'get_rasta_state' )

            private$rasta_state <- self$get_methods() %>%
                RastaState$new(.)

            return( TRUE )
        },
        set_rep_method_mx = function()
        {
            private$set_check_lists( 'set_rep_method_mx' )
            
            m <- base::matrix( rep( 0, self$get_method_count() *
                self$get_rep_count() ), ncol = self$get_method_count() )

            base::colnames( m ) <- self$get_methods()

            for( method in base::names( private$by_method_list ) )
            {
                for( rep in base::names( private$by_method_list[[ method ]] ) )
                {
                    num_rep <- base::gsub( 'rep', '', rep ) %>%
                        base::as.numeric(.)
                    m[ num_rep, method ] <- private$by_method_list[[ method ]][[ rep ]]
                }
            }

            private$rep_method_mx <- m

            return( TRUE )
        },
        set_state_idx = function()
        {
            vote_values <- private$get_vote_values()
            vote_idx <- 1:base::length( vote_values )
            spare_idx <- c( private$get_max_idx(),
                            private$get_zero_idx(),
                            base::is.na( vote_values ) %>% base::which(.) )

            if( base::length( spare_idx ) > 0 )
            {
                state_idx <- vote_idx[ -c( spare_idx ) ]

                if( base::length( state_idx ) > 0 )
                {
                    private$state_idx <- state_idx
                }
                else
                {
                    private$state_idx <- NULL
                }
            }
            else
            {
                private$state_idx <- vote_idx
            }

            return( TRUE )
        },
        set_state_mx = function()
        {
            if( base::is.null( private$get_state_idx() ) )
                { return( TRUE ) } 
            
            pixel_count <- private$get_state_idx() %>% base::length(.)
            method_count <- self$get_method_count()
            rasta_state <- self$get_rasta_state()
            methods_powers <- base::sapply( self$get_methods(),
                rasta_state$get_method_power )
            
            private$state_mx <- base::rep( 0, self$get_rep_count() *
               pixel_count ) %>%
                base::matrix( . , ncol = pixel_count )

            # Candidato vetorização
            for( i in 1:base::nrow( private$get_rep_method_mx() ) )
            {
                bin_state_mx <- private$get_bin_state_mx( i, pixel_count )
                private$state_mx[ i, ] <- Rfast::colsums( bin_state_mx *
                    2**methods_powers )
            }

            return( TRUE )
        },
        set_uncertainty_raster = function()
        {
            base::stopifnot( private$set_check_lists( 'set_uncertainty_raster' ) )

            if( ! base::is.null( private$uncertainty_raster ) )
            {
                return( private$uncertainty_raster )
            }
            else
            {
                message( "Computing uncertainty raster, please wait..." )
                private$uncertainty_raster <- super$get_entropy_raster()
                return( private$uncertainty_raster )
            }
        },
        set_vote_raster = function()
        {
            private$assert_stash_and_reps( 'get_vote_raster' )

            message( "Computing vote raster, please wait..." )
                
            list_of_rasters <- base::lapply( self$get_all(), function( x ) { terra::rast( x$get_raster_obj() ) } )

            spat_stack <- terra::rast( list_of_rasters )
            r_sum <- terra::app( spat_stack, fun = sum, cores = private$threads )

            private$vote_raster <- raster::raster( r_sum )
                
            return( TRUE )
        },
        set_weight_stack = function()
        {
            private$assert_stash_and_reps( 'get_weight_stack' )

            message( "Computing weight stack, please wait..." )
            
            h_stack <- private$get_h_stack()
            d_raster <- private$get_d_raster( h_stack )
            weight_stack <- ( 1 - h_stack ) / d_raster
            base::names( weight_stack ) <- self$get_methods()

            private$weight_stack <- weight_stack

            return( TRUE )
        },
        set_wgt_mrule_raster = function()
        {
            private$assert_stash_and_reps( 'get_wgt_mrule_raster' )
            
            private$wgt_mrule_raster <- private$get_wgt_votes_raster( 'mrule' )

            return( TRUE )
        },
        set_wgt_prob_raster = function()
        {
            private$assert_stash_and_reps( 'get_wgt_prob_raster' )

            message( "Computing weighted probability raster, please wait..." )
            
            private$wgt_prob_raster <- private$get_wgt_votes_raster( 'sum' )
            
            return( TRUE )
        },
        set_zero_idx = function()
        {
            private$zero_idx <- private$set_idx( 0 )

            return( TRUE )            
        },
        sanitize_index = function( i )
        {
            private$assert_stash_and_reps( 'sanitize_index' )

            if( ! assertthat::is.number( i ) && ! is_integer( i ) )
            {
                message <- get_var_val_type_msg( 'index', i,
                    envnames::get_fun_name( n = 1 ), 'numeric' )
                stop( message )
            }
            else if( i < 1 )
            {
                message <- base::paste( "Index must be an integer greater than 1;
                    got", i )
                stop( message )
            }

            stash_length <- self$get_rasta_land_count()
            
            if( i > stash_length )
            {
                message <- base::paste( "Index is outside of stash bounds;
                    maximum is", stash_length )
                stop( message )
            }
                
            base::as.integer( i ) %>%
                return(.)
        },
        check_class = function( class )
        {
            base::stopifnot( base::is.character( class ) )

            if( base::is.null( private$class ) )
            {
                private$class <- class
            }
            else if( private$class != class )
            {
                message <- base::paste( "Attempt to build 'RastaStash' with 
                    objects of different classes:", private$class, "and", class )

                stop( message )
            }

            return( TRUE )
        },        
        update_method_list = function( method, qty = 1L )
        {
            base::stopifnot( base::is.character( method ),
                             is_integer( qty ) )

            if( base::is.null( private$method_count_list[[ method ]] ) )
            {
                private$method_count_list[[ method ]] <- qty
            }
            else
            {
                private$method_count_list[[ method ]] <-
                    private$method_count_list[[ method ]] + qty
            }

            return( TRUE )
        },
        update_raster_size = function( raster_size )
        {
            base::stopifnot( is_integer( raster_size ) )

            if( base::is.null( private$raster_size_control ) )
            {
                private$raster_size_control <- raster_size
            }
            else if( private$raster_size_control != raster_size )
            {
                message <- base::paste( "Attempt to build 'RastaStash' with
                    rasters of different lengths: expected",
                    private$raster_size_control, "got", raster_size ) 

                stop( message )
            }

            return( TRUE )
        },
        unknown_method = function( method )
        {
            base::stopifnot( base::is.character( method ) )
            
            private$assert_stash_and_reps( 'unknown_method' )

            if( method %in% self$get_methods() )
            {
                return( FALSE )
            }
            else
            {
                return( TRUE )
            }
        } ) )
