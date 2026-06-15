#' R6 Class to instantiate RastaLand objects
#'
#' Wrapper class to raster layer objects used in ENM ensemble learning employing
#' information theory.
#'
#' @export
RastaLand <- R6::R6Class( "RastaLand",
    public = base::list(
        #' @description
        #' Create a new `RastaLand` object.
        #' 
        #' @param class A string that identifies the class of the raster layer.
        #' The class is a label such as the taxon name.
        #' @param method The ENM algorithm used to generate the wrapped 
        #' RasterLayer
        #' @param rep The replicate number of the randomization experiment used
        #' to generate the raster layer.
        #' @param rep_count The total number of replicates in the randomization 
        #' procedure.
        #' @param raster_obj Binary raster layer object.
        #'
        #' @return A `RastaLand` object.
        #'
        #' @examples
        #' \dontrun{ # Returns a `RastaLand` object corresponding to the first
        #' # replicate of the 100 randomization procedure used to generate all
        #' # the binarized raster layers computed using ANN as the ENM algorithm.
        #' rl <- RastaLand$new( class = "Some species", rep = 1, rep_count = 100,
        #' method = "ann", raster_obj = first_ann_raster_layer ) }
        initialize = function( class = character(), method = character(), 
            rep = NULL, rep_count = NULL, raster_obj = NULL )
        {
            sanitize_arg( 'class', class, 'character' )
            sanitize_arg( 'method', method, 'character' )
            sanitize_arg( 'rep', rep, 'numeric' )
            sanitize_arg( 'rep_count', rep_count, 'numeric' )

            if( base::is.null( raster_obj ) )
                { stop( get_attrb_type_message( 'raster_obj', 
                    'no argument', 'RasterLayer' ) ) }

            if( base::class( raster_obj )[1] != 'RasterLayer' )
                { stop( get_attrb_type_message( 'raster_obj', 
                    base::class( raster_obj )[1], 'RasterLayer' ) ) }

            private$raster_obj <- raster_obj
            private$method <- method
            private$class <- class
            private$rep <- rep
            private$rep_count <- rep_count
        },
        #' @description
        #' Accessor to `RastaLand` object's class. 
        #' 
        #' @return A string.
        #'
        #' @examples
        #' \dontrun{ my_class <- rl$get_class() }
        get_class = function()
        {
            return( private$class )
        },
        #' @description
        #' Accessor to method (ENM algorithm) used to create the raster layer
        #' object wrapped in the `RastaLand` object.
        #' 
        #' @return A string.
        #'
        #' @examples
        #' \dontrun{ my_method <- rl$get_method() }
        get_method = function()
        {
            return( private$method )
        },
        #' @description
        #' Accessor to the replicate number.
        #' 
        #' @return Numeric value.
        #'
        #' @examples
        #' \dontrun{ my_rep <- rl$get_rep() }
        get_rep = function()
        {
            return( private$rep )
        },
        #' @description
        #' Accessor to total number of replicates
        #'
        #' @return Numeric value.
        #'
        #' @examples
        #' \dontrun{ my_rep_count <- rl$get_rep_count() }
        get_rep_count = function()
        {
            return( private$rep_count )
        },
        #' @description
        #' Accessor to the raster layer wrapped in the `RastaLand` object.
        #' 
        #' @return Numeric value.
        #'
        #' @examples
        #' \dontrun{ my_rep_count <- rl$get_rep_count() }
        get_raster_obj = function()
        {
            return( private$raster_obj )
        },
        #' @description
        #' Internally used
        #' 
        #' @keywords internal
        get_state_freqs = function()
        {
            if( base::length( private$state_freqs ) == 0 )
                { private$set_state_freqs() }

            return( private$state_freqs )
        },
        #' @description
        #' Internally used
        #' 
        #' @keywords internal
        get_states = function()
        {
            f <- self$get_state_freqs() %>%
                base::names(.) %>%
                base::as.numeric(.) %>%
                return(.)
        },
        #' @description
        #' Internally used
        #' 
        #' @keywords internal
        get_entropy = function()
        {
            f <- self$get_state_freqs()
            n <- base::sum( f )
            p <- f / n

            return( - base::sum( p * base::log2( p ) ) )
        } ),
    private = base::list( class = base::character(), method = base::character(),
        rep = base::numeric(), rep_count = base::numeric(),
        raster_obj = raster::raster(), state_freqs = vector(),
        set_state_freqs = function()
        {
            f <- self$get_raster_obj() %>%
                 raster::getValues(.) %>%
                 Rfast::Table(.)

            private$state_freqs <- f[ ! is.na( names( f ) ) ]
            
            return( TRUE )
        } ) )
