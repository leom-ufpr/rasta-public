' R6 Class to instantiate `Rasta` objects
#'
#'#' @description
#' `Rasta` applies information theory to ensemble learning in ENM by combining
#' outcomes from several ENM models. Data are stored as multilayer `terra`
#' `SpatRaster` objects that can be provided at initialization or added later
#' via the `add()` method.
#'
#' @details
#' The `Rasta` class expects:
#' - All rasters must be aligned (same extent, resolution, and CRS)
#' - All SpatRasters must have exactly `replicates` layers
#' - `intervals` must be an integer between 2 and floor(replicates/2)
#'
#' @export
Rasta <- R6::R6Class(
  "Rasta",
  public = list(
    #' @field class The broad category defining the data (e.g., species name)
    class = NULL,

    #' @field replicates Number of layers per suitability stack; must be equal
    #' across all methods
    replicates = NULL,

    #' @field intervals Number of intervals used to bin suitability scores
    intervals = NULL,

    #' @field methods Vector of ENM method names corresponding to stacks
    methods = NULL,

    #' @field threads Number of threads for parallelized operations
    threads = NULL,

    #' @field entropy_stack Stack with Rao's (Q) entropy rasters
    entropy_stack = c(),

    #' @description
    #' Initialize a new Rasta object
    #'
    #' @param class Character string. The broad category defining the data
    #'   (e.g., species, population). Mandatory, no default.
    #' @param replicates Integer. Number of layers per SpatRaster. Mandatory,
    #'   must be >= 2, no default.
    #' @param intervals Integer. Number of intervals for discretized suitability
    #'   scores. Must be between 2 and floor(replicates/2). Default is 10L.
    #' @param stacks List of terra SpatRaster objects. Optional, can be NULL
    #'   for empty initialization. If provided, must match length of `methods`.
    #' @param methods Character vector of ENM method names. Must match length
    #'   of `stacks`. Default is NULL or empty character vector.
    #' @param threads Integer. Number of threads for parallelization.
    #'   Default is 1L. Maximum should not exceed available cores.
    #' @param .skip_checks Logical. If TRUE, skip internal checks on SpatRasters.
    #'   Default is FALSE.
    #'
    #' @return A new Rasta object invisibly.
    initialize = function( class, replicates, intervals = 10L, methods = NULL,
                           stacks = NULL, threads = 1L, .skip_checks = FALSE )
    {
        # Parameter validation
        if( missing( class ) )
        {
            stop( "`class` attribute is mandatory and must be provided." )
        }
        if( missing( replicates ) )
        {
            stop( "`replicates` attribute is mandatory and must be provided." )
        }
        if( ! is.character( class ) || length( class ) != 1 )
        {
            stop( "`class` must be a single character string." )
        }
        if( ! private$is_positive_integer( replicates ) )
        {
            stop( "`replicates` must be numeric." )
        }

        replicates <- as.integer( replicates )

        if( replicates < 2L )
        {
            stop( "`replicates` must be at least 2." )
        }

        if( ! private$is_positive_integer( intervals ) )
        {
            stop( "`intervals` must be numeric." )
        }

        intervals <- as.integer(intervals)

        if( intervals < 2L || intervals > floor(replicates / 2 ) )
        {
            stop( "`intervals` must be between 2 and floor(replicates/2). ",
                 "Provided: ", intervals,
                 ", max allowed: ", floor(replicates / 2 ) )
        }

        if( ! private$is_positive_integer( threads ) )
        {
            stop( "`threads` must be numeric." )
        }

        threads <- as.integer( threads )

        if( threads < 1L )
        {
            warning( "`Threads` will be reset to ", 1 )
            threads <- 1
        }

        max_threads <- parallel::detectCores()

        if( threads > max_threads )
        {
            warning( "`Threads` will be reset to ", max_threads )
            threads <- max_threads
        }

        # Validate .skip_checks argument
        if ( ! is.logical( .skip_checks ) || length( .skip_checks ) != 1L )
        {
            stop( ".skip_checks must be a single logical value." )
        }

        # Handle methods and stacks pairing
        if( is.null( methods ) )
        {
            methods <- character( 0 )
        }
        if( ! is.character( methods ) )
        {
            stop( "`methods` must be a character vector." )
        }
        if( is.null( stacks ) )
        {
            stacks <- list()
        }
        if( ! is.list( stacks ) )
        {
            stop( "`stacks` must be a list of SpatRaster objects." )
        }

        if( length( methods ) != length( stacks ) )
        {
            stop( "`methods` and `stacks` must have the same length. ",
                  "Got: methods=", length( methods ),
                  ", stacks=", length( stacks ) )
        }

        # Store public fields
        self$class <- class
        self$replicates <- replicates
        self$intervals <- intervals
        self$threads <- threads
        self$.skip_checks <- .skip_checks
        
        private$breaks <- seq( 0, 1, length.out = intervals + 1L )
        private$set_bins_dist_matrix()
        private$set_max_q()
        
        private$suit_stack_list <- stacks
        names( private$stacks ) <- methods

        # Run sanity checks on rasters if .skip_checks is FALSE
        if( length( stacks ) > 0L && ! self$.skip_checks )
        {
            private$check_rasters()
        }

        invisible( self )
    },

    #' @description
    #' Get the entropy raster for a given method
    #'
    #' @param method Character string. Name of the ENM method.
    #'
    #' @return A SpatRaster object with Rao's entropy values.
    get_entropy_raster = function( method )
    {
        private$check_method( method )

        if ( ! check_stack( private$entropy_stack, method ) )
        {
            p <- private$get_p_stack( method )
            d <- private$dist_matrix
            m <- private$max_q

            Qr <- terra::app (
                p_stack,
                fun = function(x)
                {
                    t( x ) %*% dist_matrix %*% x %>%
                        as.numeric(.) / m
                }
            )

            private$entropy_stack <- private$add_stack( private$entropy_stack,
                                                        Qr,
                                                        method )
        }

        return( private$entropy_raster_list[[ method ]] )
    },

    #' @description
    #' Get the weight raster for a given method
    #'
    #' @param method Character string. Name of the ENM method.
    #'
    #' @return A SpatRaster with raster weights.
    get_weight_raster = function( method )
    {
        private$check_method( method )
        
        if ( ! check_stack( private$weight_stack, method ) )
        {
            weight_numerator <- 1 - private$get_entropy_raster( method )
            weight_denominator <- private$get_weight_denominator()
            
            weigth_raster <- weight_numerator/weight_denominator
            
            private$weight_stack <- private$add_stack( private$weight_stack,
                                                       weight_raster,
                                                       method )
        }

        return( private$weight_stack[[ method ]] )
    },

    #' @description
    #' Get the raster with weighted mean ensemble suitabilities.
    #'
    #' @return A `SpatRaster` with weighted mean ensemble suitabilities.
    get_wgt_ensemble_raster = function()
    {
        if ( is.null( private$wgt_ensemble_raster ) )
        {
            wgt_ensemble_raster <- NULL
            
            for( method in self$methods )
            {
                mean_suit_raster <- self$get_mean_suit_raster( method )
                weight_raster <- self$get_weight_raster( method )
                wgt_method_raster <- mean_suit_raster * weight_raster

                if( is.null( wgt_ensemble_raster ) )
                {
                    wgt_ensemble_raster <- wgt_method_raster
                }
                else
                {
                    wgt_ensemble_raster <- wgt_ensemble_raster + wgt_method_raster
                }
            }

            private$wgt_ensemble_raster <- wgt_ensemble_raster
        }

        return( private$wgt_ensemble_raster )
    },

    #' @description
    #' Get the efficiency raster, i.e., the raster with the normalized Shannon
    #' entropy computed using weights
    #'
    #' @return A `SpatRaster` with efficiency values.
    get_efficiency_raster = function()
    {
        efficiency_raster <- NULL
        max_entropy <- log( length( self$methods ) )
        
        for( method in self$methods )
        {
            weight_raster <- self$get_weight_raster( method )
            summand <- weight_raster * log( weight_raster ) * (-1)
            
            if( is.null( efficiency_raster ) )
            {
                efficiency_raster <- summand
            }
            else
            {
                efficiency_raster <- efficiency_raster + summand
            }

            private$efficiency_raster <- efficiency_raster / max_entropy
        }

        return( private$efficiency_raster )
    },

    #' @description
    #' Get the mean entropy for a given method
    #'
    #' @param method Character string. Name of the ENM method.
    #'
    #' @return A single value representing the mean raster entropy.
    get_mean_entropy = function( method )
    {
        private$check_method( method )

        entropy_raster <- self$get_entropy_raster( method )
        mean_entropy <- terra::global( entropy_raster, fun = "mean", na.rm = TRUE )

        return( mean_entropy )
    },

    #' @description
    #' Bootstrap the entropy for a given method
    #'
    #' @param method Character string. Name of the ENM method.
    #' @param reps Integer. Number of bootstrap pseudo-replicates.
    #' @param seed Numeric. Seed value for random number generation.
    #'
    #' @return A tibble with columns: method, bs_replicate, and log_mean_entropy.
    bootstrap_entropy = function( method, reps, seed = 1L )
    {
        private$check_method( method )

        if ( ! private$is_positive_integer( reps ) || reps < 2L )
        {
            stop( "`reps` must be a positive integer greater than or equal to 2." )
        }

        if ( ! is.numeric( seed ) )
        {
            stop( "`seed` must be numeric." )
        }

        results <- tibble::tibble( method = factor(),
                                   bs_replicate = integer(),
                                   log_mean_entropy = numeric() )

        for ( i in 1:reps )
        {
            set.seed( seed + ( i - 1 ) * 10 )

            indices <- sample( 1:self$replicates,
                               size = self$replicates,
                               replace = TRUE )

            bootstrapped_stack <- terra::subset( private$suit_stack_list[[ method ]],
                                                 indices )

            boot_rasta <- Rasta$new( class = self$class,
                                     replicates = self$replicates,
                                     intervals = self$intervals,
                                     stacks = list( bootstrapped_stack ),
                                     methods = method,
                                     threads = self$threads,
                                     .skip_checks = TRUE )

            mean_entropy <- boot_rasta$get_mean_entropy(method)

            results <- tibble::add_row( results,
                                        method = method,
                                        bs_replicate = i,
                                        log_mean_entropy = log10( mean_entropy ) )
        }

        return( results )
    }
  ),

  private = list(
      # breaks Vector with breaks used to compute Q
      breaks = vector(),

      # bins Matrix with intervals used to bin suitability values
      bins = matrix(),
      
      # distance_matrix Vector with midpoint deltas used to compute Q
      dist_matrix = matrix(),

      # max_q Maximum possible Q value
      max_q = 0,

      # suit_stack_list List that stores suitability `SpatRaster`stacks
      suit_stack_list = list(),

      # count_stack_list List that stores `SpatRaster`stacks with
      # suitability binned counts
      count_stack_list = list(),

      # p_stack_list List that stores `SpatRaster`stacks with
      # proportional suitability counts
      p_stack_list = list(),
      
      # mean_suit_stack Stack that stores lazily built rasters with mean
      # suitability values for each method
      mean_suit_stack = c(),

      # weight_stack Stack that stores weight rasters used in
      # weighted mean suitability computation
      weight_stack = c(),

      # weight_denominator Raster that stores denominator used in weight
      # computation
      weight_denominator = NULL,

      # efficiency_raster Efficiency raster
      efficiency_raster = c(),
      
      # wgt_ensemble_raster Weighted ensemble suitability raster
      wgt_ensemble_raster = c(),
      
      # Check if argument is a positive integer
      is_positive_integer = function( x )
      {
          numeric <- is.numeric( x ) & is.finite( x ) & length( x ) == 1

          if( ! numeric ) return( FALSE )

          pos_integer <- x > 0 & x == as.integer( x )

          if( ! pos_integer ) return( FALSE )

          return( TRUE )
      },
      
      # Check alignment within each `SpatRaster` and consistency across all 
      check_rasters = function()
      {
          for ( method in names( private$suit_stack_list ) )
          {
              stack <- private$suit_stack_list[[ method ]]

              # Round values to 3 decimal digits
              stack <- terra::round(stack, digits = 3)

              limits <- terra::global( stack,
                                       fun = c( "min", "max" ),
                                       na.rm = TRUE )

              out_of_bounds <- any( limits[ "min", ] < 0 |
                                    limits[ "max", ] > 1,
                                    na.rm = TRUE )

              if ( out_of_bounds )
              {
                  stop( "Stack has suitability value outside [0, 1].")
              }

              if( ! methods::is( stack, " SpatRaster" ) )
              {
                  stop( "Object associated with method `", method,
                        "` is not a terra SpatRaster." )
              }

              n_layers <- terra::nlyr( stack )

              if( n_layers != self$replicates )
              {
                  stop( method, " SpatRaster ",
                       " has ", n_layers,
                       " layers but expected ",
                       self$replicates, "." )
              }

              private$check_alignment_within( stack, method )
          }

          if( length( private$suit_stack_list ) > 1L )
          {
              private$check_alignment_between()
          }

          invisible( NULL )
      },

      # Check that all layers within a SpatRaster are aligned
      check_alignment_within = function( stack, method )
      {
          if( terra::nlyr( stack ) > 1L )
          {
              ext <- terra::ext( stack )
              res <- terra::res( stack )
              crs <- terra::crs( stack )

              # All layers in a SpatRaster are inherently aligned within the
              # same object. This check ensures the raster object itself is
              # valid
              tryCatch(
              {
                  terra::ncol( stack )
                  terra::nrow( stack )
              },
              error = function(e)
              {
                  stop( method, "SpatRaster ",
                        " has invalid structure: ", e$message )
              } )
          }

          invisible( NULL )
      },

      # Check alignment among all SpatRasters in stacks
      check_alignment_between = function()
      {
          if( length( private$suit_stack_list ) < 2L )
          {
              return( invisible( NULL ) )
          }

          methods <- self$methods
          method_count <- length( self$methods )

          ref_stack <- private$suit_stack_list[[ methods[1] ]]
          ref_ext <- terra::ext( ref_stack )
          ref_res <- terra::res( ref_stack )
          ref_crs <- terra::crs( ref_stack )

          for ( method in methods[ 2L:method_count ] )
          {
              stack <- private$suit_stack_list[[ method ]]

              if( ! identical( terra::ext( stack ), ref_ext ) )
              {
                  stop( method, " SpatRaster ",
                        " has different extent than SpatRaster 1." )
              }

              if( ! identical( terra::res( stack ), ref_res ) )
              {
                  stop( method, " SpatRaster ",
                        " has different resolution than SpatRaster 1." )
              }

              if( terra::crs( stack ) != ref_crs )
              {
                  stop( method, " SpatRaster ", i,
                        " has different CRS than SpatRaster 1." )
              }
          }

          invisible( NULL )

      },

      # Check that a new stack is aligned with existing stacks
      check_alignment_with_existing = function( stack )
      {
          if( length( private$suit_stack_list ) == 0L)
          {
              return( invisible( NULL ) )
          }

          ref_stack <- private$suit_stack_list[[ 1L ]]
          ref_ext <- terra::ext( ref_stack )
          ref_res <- terra::res( ref_stack )
          ref_crs <- terra::crs( ref_stack )

          if( ! identical( terra::ext( stack ), ref_ext ) )
          {
              stop("New stack has different extent than existing stacks.")
          }

          if( ! identical( terra::res( stack ), ref_res ) )
          {
              stop("New stack has different resolution than existing stacks.")
          }

          if( terra::crs( stack ) != ref_crs )
          {
              stop( "New stack has different CRS than existing stacks." )
          }

          invisible( NULL )
      },

      # Check if method is known
      check_method = function( method )
      {
          if ( ! is.character( method ) )
          {
              stop( paste( "Expected string, got", typeof( method ),
                           "instead" ) )
          }

          if ( ! method %in% names( private$suit_stack_list ) )
          {
              stop( paste( "Unknown method:", method ) )
          }

          return( TRUE )
      },

      # Check if stack has `SpatRaster` labeled with `name`
      has_raster = function( stack, name )
      {
          return( name %in% names( stack ) )
      },

      # Check if stack has `SpatRaster` labeled with `name`
      add_to_stack = function( stack, raster, name = NULL )
      {
          if( ! is.null( name ) ) names( raster ) <- name

          return( c( stack, raster ) )
      },
      
      # Get/set `SpatRasters` with mean suitability values
      get_mean_suit_raster = function( method )
      {
          private$check_method( method )

          if ( ! has_raster( private$mean_suit_stack, method ) )
          {
              suit_stack <- private$suit_stack[[ method ]]

              mean_suit_raster <- terra::app( suit_stack,
                                              fun = "mean",
                                              na.rm = TRUE )
                                             
              private$mean_suit_stack <- private$add_stack( private$mean_suit_stack,
                                                            mean_suit_raster,
                                                            method )
          }

          return( private$mean_suit_stack[[ method ]] )
      },

      # Get/set list of `SpatRasters` with per interval counts of suitability
      # values
      get_count_stack = function( method )
      {
          private$check_method( method )

          if ( is.null( private$count_stack_list[[ method ]] ) )
          {
              suit_stack <- private$suit_stack[[ method ]]
              b <- private$bins
              count_stack <- c()

              for( l in 1:nrow( b ) )
              {
                  count_raster <- terra::app(
                      suit_stack,
                      function( x )
                      {
                          sum( x >= b[ l, 1 ] & x < b[ l, 2 ], na.rm = TRUE )
                      }
                  )

                  count_stack <- c( count_stack, count_raster )
              }
              
              private$count_stack_list[[ method ]] <- count_stack
          }

          return( private$count_stack_list[[ method ]] )
      },

      # Get/set list of `SpatRasters` with proportional counts of suitability
      # values
      get_p_stack = function( method )
      {
          private$check_method( method )

          if ( is.null( private$p_stack_list[[ method ]] ) )
          {
              count_stack <- private$count_stack_list[[ method ]]
              private$p_stack_list[[ method ]] <- count_stack / self$replicates
          }

          return( private$p_stack_list[[ method]] )
      },

      # Get/set list of `SpatRasters` with proportional counts of suitability
      # values
      get_weight_denominator = function()
      {
          if( is.null( private$weight_denominator ) )
          {
              Qs <- c()

              for( method in self$methods() )
              {
                  Qs <- c( Qs, self$get_entropy_raster( method ) )
              }

              M <- length( self$methods )
              private$weight_denominator <- M - terra::app( Qs,
                                                            fun = "sum",
                                                            na.rm = TRUE )
          }

          return( private$weight_denominator )
      },
      
      # Set midpoints attribute
      set_bins_dist_matrix = function()
      {
          bk <- private$breaks
          K <- length( bk )
          bk[ K ] <- bk[ K ] + .Machine$double.eps 
          
          i <- 1:( K-1 )
          j <- 2:K

          private$bins <- matrix( c( bk[ i ], bk[ j ] ), ncol = 2 )

          midpoints <- ( bk[ i ] + bk[ j ] ) / 2
          N <- length( midpoints )
          
          grid <- tidyr::expand_grid( i = c( 1:N ), j = c( 1:N ) )
          dist_vector <- abs( midpoints[ grid$j ] - midpoints[ grid$i ] )
                    
          private$dist_matrix <- matrix( dist_vector, ncol = N )

          invisible( NULL )
      },

      # Set maximum theoretical Q value
      set_max_q = function()
      {
          private$max_q <- round( max( private$dist_matrix ) / 2, 2 )

          invisible( NULL )
      }
   )
)
