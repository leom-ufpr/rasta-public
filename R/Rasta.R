#' R6 Class to instantiate `Rasta` objects
#' @description
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
    
    #' @field .skip_checks Used for internal object instatiation.
    .skip_checks = NULL,
    
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
    #' @return A new `Rasta` object.
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
        if( ! is.character( class ) | length( class ) != 1 )
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

        if( intervals < 2L )
        {
            stop( "`intervals` must greater or equal to 2 - provided:",
                  intervals )
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
        if ( ! is.logical( .skip_checks ) | length( .skip_checks ) != 1L )
        {
            stop( ".skip_checks must be a single logical value." )
        }

        # Handle methods and stacks pairing
        if( is.null( methods ) )
        {
            stop( "`methods` must be provided." )
        }
        if( ! is.character( methods ) | length( methods ) == 0 )
        {
            stop( "A character vector with the `methods` names is a required ",
                  "argument" )
        }
        if( is.null( stacks ) )
        {
            stacks <- list()
        }
        if( ! is.list( stacks ) | length( stacks ) == 0 )
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
        self$intervals <- intervals
        self$methods <- methods
        self$replicates <- replicates
        self$threads <- threads
        self$.skip_checks <- .skip_checks
        
        private$breaks <- seq( 0, 1, length.out = intervals + 1L )
        private$set_bins_dist_matrix()
        private$set_max_q()
        
        private$suit_stack_list <- stacks
        names( private$suit_stack_list ) <- methods

        # Run sanity checks on rasters if .skip_checks is FALSE
        if( length( stacks ) > 0L && ! self$.skip_checks )
        {
            private$check_rasters()
        }

        invisible( self )
    },

    #' @description
    #' Get the Rao's entropy raster for a given method
    #'
    #' @param method Character string corresponding to the name of the ENM method.
    #'
    #' @return A `SpatRaster`.
    get_entropy_raster = function( method )
    {
        private$check_method( method )

        if ( ! private$has_raster( "entropy_stack", method ) )
        {
            p <- private$get_p_stack( method )
            d <- private$dist_matrix
            m <- private$max_q

            Qr <- terra::app(
                p,
                fun = function( x )
                {
                    if ( anyNA( x ) )
                    {
                        return( NA_real_ )
                    }
                    else
                    {
                        t( x ) %*% d %*% x %>%
                            as.numeric(.) / m %>%
                            return(.)
                    }
                }
            )

            private$add_to_stack( "entropy_stack", Qr, method )
        }

        return( private$entropy_stack[[ method ]] )
    },
    
    #' @description
    #' Get the mean suitability raster, averaged across the multilayer suitability
    # `SpatRasters` for the  method passed as argument to the function.
    #'
    #' @param method Character string corresponding to the name of the ENM method.
    #'  
    #' @return A `SpatRaster`.
    get_mean_suitability_raster = function( method )
    {
        private$check_method( method )

        add_mean_suitability_raster <- length( private$mean_suitability_stack ) == 0 |
            ! private$has_raster( "mean_suitability_stack", method )

        if ( add_mean_suitability_raster )
        {
            Ss <- private$suit_stack_list[[ method ]]

            private$mean_suitability_stack[[ method ]] <- terra::app( Ss,
                                                                      fun = "mean",
                                                                      na.rm = TRUE )
        }

        return( private$mean_suitability_stack[[ method ]] )
    },

    #' @description
    #' Get the per-cell means of the ensemble suitabilities, i.e., computed
    #' across ALL suitability `SpatRasters`.
    #'  
    #' @return A `SpatRaster`.
    get_mean_ensemble_raster = function()
    {
        MSr <- 0

        if( is.null( private$mean_ensemble_raster ) )
        {
            for ( method in self$methods )
            {
                MSr <- ( MSr + self$get_mean_suitability_raster( method ) )
            }

            private$mean_ensemble_raster <- MSr / length( self$methods )
        }
        
        return( private$mean_ensemble_raster )
    },

    #' @description
    #' Get the raster with the per-cell ensemble weights of a method.
    #'
    #' @param method Character string corresponding to the name of the ENM method.
    #'
    #' @return A `SpatRaster`.
    get_weight_raster = function( method )
    {
        private$check_method( method )

        if ( ! private$has_raster( "weight_stack", method ) )
        {
            Wd <- private$get_weight_denominator()
            Wr <- ( 1 - self$get_entropy_raster( method ) ) / Wd
            
            private$add_to_stack( "weight_stack", Wr, method )
        }

        return( private$weight_stack[[ method ]] )
    },

    #' @description
    #' Get the raster with the per-cell weighted means of ensemble suitabilities.
    #'
    #' @return A `SpatRaster`.
    get_wgt_ensemble_raster = function()
    {
        if ( is.null( private$wgt_ensemble_raster ) )
        {
            WEr <- 0

            for( method in self$methods )
            {
                MSr <- self$get_mean_suitability_raster( method )
                Wr <- self$get_weight_raster( method )
                WEr <- WEr + MSr * Wr
            }

            private$wgt_ensemble_raster <- WEr
        }

        return( private$wgt_ensemble_raster )
    },

    #' @description
    #' Get the efficiency raster, i.e., the raster with the normalized Shannon
    #' entropy, computed using weights as proportions.
    #'
    #' @return A `SpatRaster`.
    get_efficiency_raster = function()
    {
        Hr <- 0

        if( is.null( private$efficiency_raster ) )
        {
            for( method in self$methods )
            {
                Wr <- self$get_weight_raster( method )
                Hr <- Hr + ( Wr * log( Wr + .Machine$double.eps ) )
            }
            
            max_h <- log( length( self$methods ) )        
            private$efficiency_raster <- Hr / max_h * (-1)
        }

        return( private$efficiency_raster )
    },

    #' @description
    #' Get the mean Rao's entropy for a given method.
    #'
    #' @param method Character string with the name of the ENM method.
    #'
    #' @return A numeric value representing the global raster entropy for the
    #' raster.
    get_mean_entropy = function( method )
    {
        private$check_method( method )

        Qr <- self$get_entropy_raster( method ) 
        mean_entropy <- terra::global( Qr,
                                       fun = "mean",
                                       na.rm = TRUE )

        return( as.numeric( mean_entropy ) )
    },

    #' @description
    #' Bootstrap the entropy for a given method
    #'
    #' @param method Character string. Name of the ENM method.
    #' @param reps Integer. Number of bootstrap pseudo-replicates.
    #' @param seed Numeric. Seed value for random number generation.
    #'
    #' @return A `tibble` with columns `method`, `bs_replicate`, and `mean_entropy`.
    bootstrap_entropy = function( method, reps = 100, seed = 1L )
    {
        private$check_method( method )

        if( is.null( private$bs_tibble[[ method ]] ) )
        {
            if ( ! private$is_positive_integer( reps ) | reps < 2L )
            {
                stop( "`reps` must be a positive integer greater than or equal to 2." )
            }

            if ( ! is.numeric( seed ) )
            {
                stop( "`seed` must be numeric." )
            }

            bs_tibble <- tibble::tibble( method = factor(),
                                         bs_replicate = integer(),
                                         mean_entropy = numeric() )

            msg <- paste0( "Bootstrapping`", method ,"`entropy" )

            bin_matrix <- private$get_binned_matrix( method )
            
            compute_entropy <- private$get_entropy_fun()

            for ( i in cli::cli_progress_along( 1:reps, name = msg ) )
            {
                set.seed( seed + ( i - 1 ) * 10 )

                indices <- sample( 1:self$replicates,
                                   size = self$replicates,
                                   replace = TRUE )

                rep_mat <- bin_matrix[ indices, ]

                n_bins <- nrow( private$bins )

                count_matrix <- vapply( seq_len( n_bins ),
                                        function( b ) Rfast::colsums( rep_mat == b ),
                                        numeric( ncol( rep_mat ) ) )

                Qv <- apply( t( count_matrix ), 2L, compute_entropy )

                bs_tibble <- tibble::add_row( bs_tibble,
                                              method = method,
                                              bs_replicate = i,
                                              mean_entropy = mean( Qv ) )
            }

            private$bs_tibble[[ method ]] <- bs_tibble

            message()
        }

        return( private$bs_tibble[[ method ]] )
    },

    #' @description
    #' Function that allows access to private attributes and functions for testing.
    #' DO NOT USE unless you are a developer.
    #' 
    #' @param name Character string. Name of private attribute or function.
    #' @param tag Character. Tag identifying if attribute or function is being
    #' called.
    #' @param args List. List of arguments to be passed to private function.
    #'
    #' @return Attribute or function result.
    test_private = function( name = NULL, tag = "attrb", args = list() )
    {
        base::stopifnot( base::is.character( name ) )
        base::stopifnot( base::is.character( tag ) )
        base::stopifnot( base::is.list( args ) )
        base::stopifnot( tag %in% c( "attrb", "function" ) )

        if( name %in% names( private ) )
        {
            if( tag == "attrb" )
            {
                return( private[[ name ]] )
            }
            else
            {
                return( base::do.call( private[[ name ]], args ) )
            }
        }
        else 
        {
            warning( base::paste( "Method", name, "not found" ) )
        }

        invisible( NULL )        
    }
  ),

  private = list(
      # breaks Vector with breaks used to compute Q
      breaks = vector(),

      # bins Matrix with intervals used to bin suitability values
      bins = matrix(),

      # bs_tibble Tibble with bootstrapped entropy values
      bs_tibble = list(),
      
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

      # entropy_stack Stack that stores lazily built rasters with entropy
      # values for each method
      entropy_stack = NULL,
      
      # mean_suitability_stack Stack that stores lazily built rasters with mean
      # suitability values for each method
      mean_suitability_stack = NULL,

      # mean_ensemble_raster Lazily built rasters with mean suitability values
      # for the whole ensemble
      mean_ensemble_raster = NULL,
      
      # weight_stack Stack that stores weight rasters used in
      # weighted mean suitability computation
      weight_stack = NULL,

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
          numeric <- is.numeric( x ) &
              is.finite( x ) &
              length( x ) == 1 &
              ! is.na( x )

          if( ! numeric ) return( FALSE )

          pos_integer <- x > 0 &
              x == as.integer( x )

          if( ! pos_integer ) return( FALSE )

          return( TRUE )
      },
      
      # Check alignment within each `SpatRaster` and consistency across all 
      check_rasters = function()
      {
          methods <- names( private$suit_stack_list )
          msg <- "Checking rasters"
          
          for ( method_idx in cli::cli_progress_along( methods, name = msg ) )
          {
              method <- methods[[ method_idx ]]
              stack <- private$suit_stack_list[[ method ]]
              
              # Round values to 3 decimal digits
              stack <- terra::round( stack, digits = 3 )
              
              limits <- terra::global( stack,
                                       fun = c( "min", "max" ),
                                       na.rm = TRUE )

              out_of_bounds <- base::any( limits$min < 0 ) |
                  base::any( limits$max > 1 ) 

              if ( out_of_bounds )
              {
                  stop( "Stack has suitability value outside [0, 1].")
              }

              if( ! is( stack, "SpatRaster" ) )
              {
                  stop( "Object associated with method `", method,
                        "` is not a terra SpatRaster; got ", class( stack ) )
              }

              n_layers <- terra::nlyr( stack )

              if( n_layers != self$replicates )
              {
                  stop( method, " SpatRaster",
                       " has ", n_layers,
                       " layers but expected ",
                       self$replicates, "." )
              }
              
              private$suit_stack_list[[ method ]] <- stack
          }

          if( length( private$suit_stack_list ) > 1L )
          {
              private$check_alignment_between()
          }

          message()
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

          ref_spat <- private$suit_stack_list[[ methods[1] ]]

          for ( method in methods[ 2L:method_count ] )
          {
              tgt_spat <- private$suit_stack_list[[ method ]]
              private$check_raster_geom( ref_spat, tgt_spat, method )
          }

      },

      # Check raster geometry
      check_raster_geom = function( ref_spat, tgt_spat, method )
      {
          stopifnot( is( ref_spat, "SpatRaster" ) )
          stopifnot( is( tgt_spat, "SpatRaster" ) )
          stopifnot( is.character( method ) )
          
          tryCatch(
          {
              terra::compareGeom( ref_spat,
                                  tgt_spat,
                                  lyrs = TRUE,
                                  crs = TRUE,
                                  ext = TRUE,
                                  rowcol = TRUE,
                                  res = TRUE,
                                  stopOnError = TRUE )
          },
          error = function(e)
          {
              stop( method,
                    " SpatRasters are not aligned with reference: ",
                    e$message )
          } )

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

      # Message function
      print_message = function( fun_name, method = NULL, msg = FALSE )
      {
          if( isTRUE( msg ) )
          {
              stopifnot( is.character( fun_name ) )
              
              if( ! is.null( method ) )
                  { stopifnot( is.character( method ) ) }

              msg_header <- paste0( "Executing `", fun_name, "`" )
              
              if( ! is.null( method ) )
                  { paste( msg_header, "for", method ) }

              message( msg_header )
          }

          invisible( NULL )
      },
      
      # Check if stack has `SpatRaster` labeled with `name`
      has_raster = function( stack, method )
      {
          if ( is.null( private[[ stack ]] ) )
          {
              return( FALSE )
          }
          else
          {
              return( method %in% names( private[[ stack ]] ) )
          }
      },

      # Add `SpatRaster` to stack
      add_to_stack = function( stack, raster, method )
      {
          names( raster ) <- method

          if( is.null( private[[ stack ]] ) )
          {
              private[[ stack ]] <- raster
          }
          else
          {
              terra::add( private[[ stack ]] ) <- raster
          }

          invisible( NULL )
      },

      # Get/set list of `SpatRasters` with per interval counts of suitability
      # values
      get_count_stack = function( method )
      {
          private$check_method( method )

          if ( is.null( private$count_stack_list[[ method ]] ) )
          {
              suit_stack <- private$suit_stack_list[[ method ]]
              b <- private$bins
              count_stack <- c()

              msg <- paste0( "Binning `", method, "` suitbalility" ) 
              
              for( l in cli::cli_progress_along( c( 1:nrow( b ) ), name = msg ) )
              {
                  count_raster <- terra::app(
                      suit_stack,
                      function( x )
                      {
                          if ( anyNA( x ) )
                          {
                              return( NA_real_ )
                          }
                          else
                          {
                              sum( x >= b[ l, 1 ] & x < b[ l, 2 ] ) %>%
                                  return(.)
                          }
                      }
                  )

                  count_stack <- c( count_stack, count_raster )
              }
              message()
              
              private$count_stack_list[[ method ]] <- terra::rast( count_stack )
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
              count_stack <- private$get_count_stack( method )
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

              for( method in self$methods )
              {
                  Qs <- c( Qs, self$get_entropy_raster( method ) )
              }

              M <- length( self$methods )
              private$weight_denominator <- M - terra::app( terra::rast(Qs),
                                                            fun = "sum",
                                                            na.rm = TRUE )
          }

          return( private$weight_denominator )
      },

      # Get matrix of binned suitability values corresponding to the method
      # passed as argument
      get_binned_matrix = function( method )
      {
          private$check_method( method )

          method_raster <- private$suit_stack_list[[ method ]]
          
          suit_mat <- t( terra::values( method_raster, mat = TRUE ) )
          suit_mat <- unname( suit_mat )
          value_columns <- ! is.na( suit_mat[ 1, ] )
          bin_matrix <- suit_mat[ , value_columns ]

          bins <- private$bins

          for( l in 1:nrow( bins ) )
          {
              idx_mat <- bin_matrix < bins[ l, 2 ]
              bin_matrix[ idx_mat ] <- l + 1
          }
          
          bin_matrix <- bin_matrix - 1
          storage.mode( bin_matrix ) <- "integer"

          return( bin_matrix )
      },

      # Return function to compute Rao's entropy for vectors passed as arguments
      get_entropy_fun = function()
      {
            d <- private$dist_matrix
            r <- nrow( d )
            b <- seq_len( r )
            n <- self$replicates
            m <- private$max_q

            return( function( x )
            {
                stopifnot( is.vector( x ) )
                
                idx <- b %in% ( names( x ) %>% as.numeric() )
                p <- rep( 0, r )
                p[ idx ] <- unlist( x ) %>% unname(.)
                p <- p / n

                t( p ) %*% d %*% p %>%
                    as.numeric(.) / m %>%
                    return(.)
            } )
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
