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

    #' @field replicates Number of layers per SpatRaster
    replicates = NULL,

    #' @field intervals Number of intervals for discretized suitability scores
    intervals = NULL,

    #' @field methods Vector of ENM method names corresponding to stacks
    methods = NULL,

    #' @field threads Number of threads for parallelized operations
    threads = NULL,

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
    #'
    #' @return A new Rasta object invisibly.
    initialize = function( class, replicates, intervals = 10L, methods = NULL,
                           threads = 1L )
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

        if( private$is_positive_integer( threads ) )
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
        
        private$breaks <- seq( 0, 1, length.out = intervals + 1L )
        private$set_midpoints()
        
        private$stack_list <- stacks
        names( private$stacks ) <- methods


        # Run sanity checks on rasters if any are provided
        if( length( stacks ) > 0L )
        {
            private$check_rasters()
        }

        invisible( self )
    },

    #' @description
    #' Add a new SpatRaster to the Rasta object
    #'
    #' @param raster A terra SpatRaster object with exactly `replicates` layers
    #' @param method Character string. Name of the ENM method used to generate
    #'   this raster.
    #'
    #' @return Invisibly returns self for method chaining.
    add = function( raster, method )
    {
      if( missing( raster ) || missing( method ) )
      {
        stop( "`raster` and `method` are both required." )
      }

      if( ! methods::is( raster, "SpatRaster" ) )
      {
        stop("`raster` must be a terra SpatRaster object.")
      }

      if( ! is.character( method ) || length( method ) != 1 )
      {
        stop( "`method` must be a single character string." )
      }

      n_layers <- terra::nlyr( raster )

      if( n_layers != self$replicates )
      {
        stop( "Raster must have exactly ",
              self$replicates,
              " layers. ",
              "Got: ",
              n_layers )
      }

      if( length( self$stacks ) > 0L )
      {
        private$check_alignment_with_existing(raster)
      }

      self$stacks[[ length( self$stacks ) + 1L ]] <- raster
      self$methods <- c( self$methods, method )

      invisible( self )
    }
  ),

  private = list(
      #' @field breaks Vector with breaks used to compute Rao's entropy
      breaks = vector(),

      #' @field midpoints Vector with midpoints used to compute Rao's entropy
      midpoints = vector(),
      
      #' @field mean_suit_raster List that stores lazily build raster with mean
      #'suitability values for each method stack
      mean_suit_list = list(),

      #' @field quantile_stack_list List that stores `SpatRaster` stacks with
      #' count value within each interval.
      quantile_stack_list = list(),

      #' @field stack_list List that stores suitability `SpatRaster`stacks
      stack_list = list(),

      #' @field entropy_raster Raster with Rao's entropeis
      entropy_raster = list(),
      
      #' @description
      #' Check alignment within each SpatRaster and consistency across all
      is_positive_integer = function( x )
      {
          numeric <- is.numeric( x ) & is.finite( x ) & length( x ) == 1

          if( ! numeric ) return( FALSE )

          pos_integer <- x > 0 & x == as.integer( x )

          if( ! pos_integer ) return( FALSE )

          return( TRUE )
      }
      
      #' @description
      #' Check alignment within each SpatRaster and consistency across all
      check_rasters = function()
      {
          for ( method in names( private$stack_list ) )
          {
              stack <- private$stack_list[[ method ]]

              limits <- terra::global( stack,
                                       fun = c( "min", "max" ),
                                       na.rm = TRUE )

              out_of_bounds <- any( limits[ "min", ] < 0 |
                                    limits[ "max", ] > 1,
                                    na.rm = TRUE )

              if ( out_of_bounds )
              {
                  stop( "Satck has suitability value outside [0, 1].")
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

          if( length( private$stack_list ) > 1L )
          {
              private$check_alignment_between()
          }
      },

      #' @description
      #' Check that all layers within a SpatRaster are aligned
      check_alignment_within = function( stack, method )
      {
          # For terra::SpatRaster, we check that all layers have the same
          # extent, resolution, and CRS
          if( terra::nlyr( stack ) > 1L )
          {
              ext <- terra::ext( stack )
              res <- terra::res( stack )
              crs <- terra::crs( stack )

          # All layers in a SpatRaster are inherently aligned within the
          #same object
          # This check ensures the raster object itself is valid
              tryCatch(
              {
                  # Attempt to access basic properties to ensure integrity
                  terra::ncol( stack )
                  terra::nrow( stack )
              },
              error = function(e)
              {
                  stop( method, "SpatRaster ",
                        " has invalid structure: ", e$message )
              } )
          }
      },

      #' @description
      #' Check alignment among all SpatRasters in stacks
      check_alignment_between = function()
      {
          if( length( private$stack_list ) < 2L )
          {
              return( invisible( NULL ) )
          }

          methods <- self$methods
          method_count <- length( self$methods )

          # Get reference properties from first raster
          ref_stack <- private$stack_list[[ methods[1] ]]
          ref_ext <- terra::ext( ref_stack )
          ref_res <- terra::res( ref_stack )
          ref_crs <- terra::crs( ref_stack )

          # Compare all other stacks to the reference
          for ( method in methods[ 2L:method_count ] )
          {
              stack <- private$stack_list[[ method ]]

              # Check extent
              if( ! identical( terra::ext( stack ), ref_ext ) )
              {
                  stop( method, " SpatRaster ",
                        " has different extent than SpatRaster 1." )
              }

              # Check resolution
              if( ! identical( terra::res( stack ), ref_res ) )
              {
                  stop( method, " SpatRaster ",
                        " has different resolution than SpatRaster 1." )
              }

              # Check CRS
              if( terra::crs( stack ) != ref_crs )
              {
                  stop( method, " SpatRaster ", i,
                        " has different CRS than SpatRaster 1." )
              }
          }
      },

      #' @description
      #' Check that a new stack is aligned with existing stacks
      check_alignment_with_existing = function( stack )
      {
          if( length( private$stack_list ) == 0L)
          {
              return( invisible( NULL ) )
          }

          ref_stack <- private$stack_list[[ 1L ]]
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
      },

      #' @description
      #' Check if method is known
      check_method = function( method )
      {
          if ( ! is.character( method ) )
          {
              stop( paste( "Expected string, got", typeof( method ),
                           "instead" ) )
          }

          if ( ! method %in% names( private$stack_list ) )
          {
              stop( paste( "Unknown method:", method ) )
          }

          return( TRUE )
      },


      #' @description
      #' Get/set list of `SpatRasters` with mean suitability values
      get_mean_suit_list = function( method )
      {
          private$check_method( method )

          if ( is.null( private$mean_suit_raster[[ method ]] ) )
          {
              suit_stack <- private$stack_list[[ method ]]

              mean_suit_raster <- terra::app( suit_stack,
                                              function(x) mean( x, na.rm = TRUE ),
                                              cores = self$cores )
          }

          private$mean_suit_list[[ method ]] <- mean_suit_raster

          return( private$mean_suit_list[[ method ]] )
      },

      #' @description
      #' Get/set list of `SpatRasters` with per interval counts of suitability
      #' values
      get_count_stack = function( method )
      {
          private$check_method( method )

          if ( is.null( private$count_stack_list[[ method ]] ) )
          {
              suit_stack <- self$stack_list[[ method ]]
              q <- self$intervals

              count_stack <- terra::app( suit_stack, function( x )
              {
                  cut( x,
                      breaks = private$breaks,
                      include.lowest = TRUE,
                      right = TRUE,
                      labels = FALSE )
              },
              cores = self$threads )
              
              private$count_stack_list[[ method ]] <- count_stack
          }

          return( private$count_stack_list[[ method]] )
      },

      #' @description
      #' Get/set list of `SpatRasters` with mean suitability values
      set_midpoints = function()
      {
          lower <- 1:length( private$breaks ) - 1
          upper <- 2:length( breaks )

          private$midpoints <- ( private$breaks[ lower ] +
                                 private$breaks[ upper ] ) / 2
      }
  )
)
