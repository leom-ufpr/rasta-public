library(testthat)

library(magrittr)
library(R6)
library(terra)

# Helper function to create a single layer SpatRaster
create_raster <- function(vector, n_cols = 10, n_rows = 10) {
    mat <- base::matrix(vector, nrow = n_rows, ncol = n_cols)
    return(terra::rast(mat))
}

# Helper function to create a multiayer SpatRaster
create_stack <- function(n_layers, raster) {
  rasters <- vector("list", n_layers)
  for (i in seq_len(n_layers)) {
    rasters[[i]] <- raster
  }
  return(terra::rast(rasters))
}

# Helper function to create standard toy SpatRaster
create_toy_spatraster <- function(n_layers, n_cols = 10, n_rows = 10) {
  # Create a vector of random values with specific ranges
  vec <- c(stats::runif(30, min = 0.00, max = 0.09),
           stats::runif(50, min = 0.10, max = 0.19),
           stats::runif(20, min = 0.20, max = 0.29))

  # Raster from vector  
  raster <- create_raster(vector = vec, n_cols = n_cols, n_rows = n_rows)

  # Return stack with given number of identical layers  
  return(create_stack(n_layers, raster))
}

new_from_template <- function(ref_raster, vector) {
  raster <- terra::deepcopy(ref_raster)
  values(raster) <- vector
  return(raster)
}

compare_raster_data <- function(raster_1, raster_2)
{
    expect_equal(terra::values(raster_1) %>% as.numeric(),
                 terra::values(raster_2) %>% as.numeric())
}

test_that("Rasta constructor initializes correctly with valid arguments", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Initialize Rasta object
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Check if object is initialized correctly
  expect_is(obj, "Rasta")
  expect_equal(obj$class, "test_class")
  expect_equal(obj$replicates, 5)
  expect_equal(obj$intervals, 10L)
  expect_equal(obj$methods, c("ANN", "GLM"))
  expect_equal(obj$threads, 1L)
})

test_that("Rasta constructor fails with invalid class argument", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  expect_error(Rasta$new(
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  ), "`class` attribute is mandatory and must be provided.")
})

test_that("Rasta constructor fails with invalid replicates argument", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  expect_error(Rasta$new(
    class = "test_class",
    intervals = 10,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    replicates = "1L",
    .skip_checks = FALSE
  ), "`replicates` must be numeric.")
})

test_that("Rasta constructor fails with low replicates argument", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  expect_error(Rasta$new(
    class = "test_class",
    intervals = 10,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    replicates = 1L,
    .skip_checks = FALSE
  ), "`replicates` must be at least 2.")
})

test_that("Rasta constructor fails with invalid stacks argument", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = c(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  ), "`stacks` must be a list of SpatRaster objects.")
})

test_that("Rasta constructor fails with invalid methods argument", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1, stack_2),
    threads = 1L,
    .skip_checks = FALSE
  ), "`methods` must be provided.")
})

test_that("Rasta constructor fails with invalid methods argument", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    methods = list("ANN", "GLM"),
    stacks = list(stack_1, stack_2),
    threads = 1L,
    .skip_checks = FALSE
  ), "A character vector with the `methods` names is a required argument" )
})

test_that("Rasta constructor fails with mismatched methods and stacks lengths", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  ), "`methods` and `stacks` must have the same length.")
})

test_that("Rasta constructor fails with invalid threads argument", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = "a",
    .skip_checks = FALSE
  ), "`threads` must be numeric.")
})

test_that("Rasta constructor fails with invalid skip_checks argument", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = "TRUE"
  ), ".skip_checks must be a single logical value.")
})

test_that("Rasta constructor fails with SpatRasters having different number of layers", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(6)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  ), "GLM SpatRaster has 6 layers but expected 5." )
})

test_that("Rasta constructor fails with SpatRasters having different extents", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- terra::crop(stack_1, terra::ext(stack_1)/2)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
   ), regexp = "\\[compareGeom\\] extents do not match" )
})

test_that("Rasta constructor fails with SpatRasters having different resolutions", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- terra::resample(stack_1, terra::res(stack_1) * 2)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  ), regexp = "\\[compareGeom\\] number of rows and\\/or columns do not match" )
})

test_that("Rasta constructor fails with SpatRasters having different CRS", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster( 5 )
  terra::crs( stack_1 ) <- "EPSG:4326"
    
  stack_2 <- create_toy_spatraster( 5 )
  terra::crs( stack_2 ) <- "EPSG:3857"
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  ),regexp = "\\[compareGeom\\] CRS do not match" )
})

test_that("Rasta constructor fails with suitability values outside [0, 1]", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  terra::values( stack_1[[ 1 ]] ) <- c(runif(99), -1)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1),
    methods = c("ANN"),
    threads = 1L,
    .skip_checks = FALSE
  ), regexp = "value outside \\[0, 1\\]")
})

test_that("Rasta constructor fails with suitability values outside [0, 1] within the same stack", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  terra::values( stack_1[[ 1 ]] ) <- c(runif(99), -1)
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
    stacks = list(stack_1),
    methods = c("ANN"),
    threads = 1L,
    .skip_checks = FALSE
  ), regexp = "value outside \\[0, 1\\]")
})

# New tests for variation between multilayer SpatRasters with NA values

test_that("Rasta constructor works with NAs in different layers", {
  # Create toy SpatRasters with NAs in different layers
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Introduce NAs in the same cells for both stacks
  coords <- terra::xyFromCell(stack_1, sample(1:terra::ncell(stack_1), size = 5) )
  stack_1[ coords ] <- NA
  stack_2[ coords ] <- NA
  
  expect_is(Rasta$new(
    class = "test_class",
    replicates = 5,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  ), "Rasta")
})

test_that("Rasta constructor fails with NAs in the same layer", {
  # Create toy SpatRasters with NAs in the same layer
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Introduce NAs in the same cells for both stacks in the same layer
  coords <- terra::xyFromCell(stack_1, sample(1:terra::ncell(stack_1), size = 5) )
  stack_1[[ 1 ]][ coords ] <- NA
  stack_2[[ 2 ]][ coords ] <- NA

  expect_is(Rasta$new(
    class = "test_class",
    replicates = 5,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  ), "Rasta" )
})

# New test to verify if cell values were rounded down to 3 decimal places

test_that("Rasta constructor rounds cell values to 3 decimal places", {
  # Create toy SpatRasters with high precision values
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Set values to a high precision number
  coords <- terra::xyFromCell(stack_1, sample(1:terra::ncell(stack_1), size = 5) )
  stack_1[ coords ] <- runif(5, min = 0.123456789, max = 0.987654321)
  stack_2[ coords ] <- runif(5, min = 0.123456789, max = 0.987654321)

  # Initialize Rasta object
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Extract values from the SpatRaster
  suit_stack_list <- obj$test_private("suit_stack_list")
  values <- terra::values(suit_stack_list[[1]])

  # Check if values are rounded to 3 decimal places
  expect_true(base::all(round(values, 3) == values,na.rm = TRUE))

  values <- terra::values(suit_stack_list[[2]])
  expect_true(base::all(round(values, 3) == values,na.rm = TRUE))
})

# New test for get_mean_suit_raster

test_that("get_mean_suit_raster returns a SpatRaster with mean suitability values", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Initialize Rasta object
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Get mean suitability raster for method "ANN"
  mean_suit_raster <- obj$get_mean_suit_raster("ANN")
  
  # Check if the returned object is a SpatRaster
  expect_is(mean_suit_raster, "SpatRaster")
  
  # Check if the number of layers in the mean suitability raster is 1
  expect_equal(terra::nlyr(mean_suit_raster), 1)
  
  # Check if the extent and resolution of the mean suitability raster match
  # those of the input stacks
  round_stack <- terra::round(stack_1, digits = 3)
  expect_equal(terra::values(terra::app(round_stack, fun = mean, na.rm=TRUE)),
               terra::values(mean_suit_raster),
               stopOnError = FALSE)
})

# New test to verify if private$bins is correctly instantiated

test_that("private$bins is correctly instantiated with default intervals", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Initialize Rasta object with default intervals (10L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Get private$bins using the accessor function
  bins <- obj$test_private("bins")
  
  # Check if bins is a matrix with 2 columns and 11 rows
  expect_is(bins, "matrix")
  expect_equal(ncol(bins), 2)
  expect_equal(nrow(bins), 10)
  
  # Check the values in the first row
  expect_equal(bins[1, ], c(0, 0.1))
  
  # Check the values in the last row
  expect_equal(bins[nrow(bins), ], c(0.9, 1 + .Machine$double.eps))
})

test_that("private$bins is correctly instantiated with custom intervals", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Initialize Rasta object with custom intervals (5L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 5L,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Get private$bins using the accessor function
  bins <- obj$test_private("bins")
  
  # Check if bins is a matrix with 2 columns and 6 rows
  expect_is(bins, "matrix")
  expect_equal(ncol(bins), 2)
  expect_equal(nrow(bins), 5)
  
  # Check the values in the first row
  expect_equal(bins[1, ], c(0, 0.2))
  
  # Check the values in the last row
  expect_equal(bins[nrow(bins), ], c(0.8, 1 + .Machine$double.eps))
})

# New test to verify if private$dist_matrix is correctly instantiated

test_that("private$dist_matrix is correctly instantiated with default intervals", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Initialize Rasta object with default intervals (10L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Get private$bins using the accessor function
  bins <- obj$test_private("bins")
  
  # Calculate expected midpoints
  midpoints <- (bins[,1] + bins[,2]) / 2
  
  # Calculate expected dist_matrix
  K <- length(midpoints)
  grid <- tidyr::expand_grid(i = c(1:K), j = c(1:K))
  dist_vector <- abs(midpoints[grid$j] - midpoints[grid$i])
  expected_dist_matrix <- matrix(dist_vector, ncol = K)
  
  # Get private$dist_matrix using the accessor function
  dist_matrix <- obj$test_private("dist_matrix")
  
  # Check if dist_matrix is a matrix with correct dimensions
  expect_is(dist_matrix, "matrix")
  expect_equal(ncol(dist_matrix), K)
  expect_equal(nrow(dist_matrix), K)
  
  # Check if diagonal elements are zero
  diag_elements <- diag(dist_matrix)
  expect_true(all(diag_elements == 0))
  
  # Check if off-diagonal elements match expected values
  expect_equal(dist_matrix, expected_dist_matrix)

  # Check max_q
  expect_equal(obj$test_private("max_q"), 0.45)

})

test_that("private$dist_matrix is correctly instantiated with custom intervals", {
  # Create toy SpatRasters
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Initialize Rasta object with custom intervals (5L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 5L,
    stacks = list(stack_1, stack_2),
    methods = c("ANN", "GLM"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Get private$bins using the accessor function
  bins <- obj$test_private("bins")
  
  # Calculate expected midpoints
  midpoints <- (bins[,1] + bins[,2]) / 2
  
  # Calculate expected dist_matrix
  K <- length(midpoints)
  grid <- tidyr::expand_grid(i = c(1:K), j = c(1:K))
  dist_vector <- abs(midpoints[grid$j] - midpoints[grid$i])
  expected_dist_matrix <- matrix(dist_vector, ncol = K)
  
  # Get private$dist_matrix using the accessor function
  dist_matrix <- obj$test_private("dist_matrix")
  
  # Check if dist_matrix is a matrix with correct dimensions
  expect_is(dist_matrix, "matrix")
  expect_equal(ncol(dist_matrix), K)
  expect_equal(nrow(dist_matrix), K)
  
  # Check if diagonal elements are zero
  diag_elements <- diag(dist_matrix)
  expect_true(all(diag_elements == 0))
  
  # Check if off-diagonal elements match expected values
  expect_equal(dist_matrix, expected_dist_matrix)

  # Check max_q
  expect_equal(obj$test_private("max_q"), 0.40)
})

# New test to verify if private$get_count_stack is correctly instantiated

test_that("private$get_count_stack and private$get_p_stack returns correct count stack with default intervals", {
  # Create a SpatRaster with these specific values
  stack_1 <- create_toy_spatraster(5)
  
  # Initialize Rasta object with default intervals (10L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    stacks = list(stack_1),
    methods = c("ANN"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Get private$count_stack using the accessor function
  count_stack <- obj$test_private( "get_count_stack",
                                   tag = "function",
                                   args = list("ANN"))

  # Check if count_stack is a SpatRaster with correct dimensions
  expect_is(count_stack, "SpatRaster")
  expect_equal(terra::nlyr(count_stack), obj$intervals)

  raster <- vector(mode = "list", length = obj$intervals)
  raster[[1]] <- new_from_template(ref_raster = stack_1[[1]],
                                   vector = c(rep(5,30), rep(0,70)))
  raster[[2]] <- new_from_template(ref_raster = raster[[1]],
                                   vector = c(rep(0,30), rep(5,50), rep(0,20)))
  raster[[3]] <- new_from_template(ref_raster = raster[[1]],
                                   vector = c(rep(0,80), rep(5,20)))
  
  # Check if the first pixel has correct counts in each layer
  for( i in 4:10L ) {
      raster[[i]] <- new_from_template(raster[[1]], c(rep(0,100)))
  }      

  exp_stack <- terra::rast(raster)
  expect_equal(exp_stack, count_stack)

  # Initialize Rasta object with default intervals (10L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    stacks = list(stack_1),
    methods = c("ANN"),
    threads = 1L,
    .skip_checks = FALSE
   )

  p_stack <- obj$test_private( "get_p_stack",
                               tag = "function",
                               args = list("ANN"))

  expect_equal(exp_stack/obj$replicates, p_stack)
})

test_that("private$get_count_stack and private$get_p_stack return correct count stack with intervals=5L", {
  # Create a SpatRaster with these specific values
  stack_1 <- create_toy_spatraster(5)
  
  # Initialize Rasta object with default intervals (10L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 5,
    stacks = list(stack_1),
    methods = c("ANN"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Get private$count_stack using the accessor function
  count_stack <- obj$test_private( "get_count_stack",
                                   tag = "function",
                                   args = list("ANN"))

  # Check if count_stack is a SpatRaster with correct dimensions
  expect_is(count_stack, "SpatRaster")
  expect_equal(terra::nlyr(count_stack), obj$intervals)

  raster <- vector(mode = "list", length = obj$intervals)
  raster[[1]] <- new_from_template(ref_raster = stack_1[[1]],
                                   vector = c(rep(5,80), rep(0,20)))
  raster[[2]] <- new_from_template(ref_raster = raster[[1]],
                                   vector = c(rep(0,80), rep(5,20)))
  
  # Check if the first pixel has correct counts in each layer
  for( i in 3:5 ) {
      raster[[i]] <- new_from_template(raster[[1]], c(rep(0,100)))
  }      

  exp_stack <- terra::rast(raster)
  expect_equal(exp_stack, count_stack)

  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 5,
    stacks = list(stack_1),
    methods = c("ANN"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  p_stack <- obj$test_private("get_p_stack",
                              tag = "function",
                              args = list("ANN"))

  expect_equal(exp_stack/obj$replicates, p_stack)
})

test_that("self$get_entropy_raster is all 0 for standard object", {
  # Create a SpatRaster with these specific values
  stack_1 <- create_toy_spatraster(5)
  stack_2 <- create_toy_spatraster(5)
  
  # Initialize Rasta object with default intervals (10L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 5,
    stacks = list(stack_1,stack_2),
    methods = c("ANN","GLM"),
    threads = 1L,
    .skip_checks = FALSE
  )

  exp_raster <- new_from_template(stack_1[[1]], c(rep(0,100)))
  names(exp_raster) <- "ANN"
  q_raster <- obj$get_entropy_raster("ANN")
  compare_raster_data(exp_raster, q_raster)

  exp_raster <- new_from_template(stack_1[[2]], c(rep(0,100)))
  names(exp_raster) <- "GLM"
  q_raster <- obj$get_entropy_raster("GLM")
  compare_raster_data(exp_raster, q_raster)
})

test_that("self$get_entropy_raster varies as expected for custom rasters", {

  vec <- c(stats::runif(30, min = 0.00, max = 0.09),
           stats::runif(30, min = 0.10, max = 0.19),
           rep(NA, 10),
           stats::runif(30, min = 0.20, max = 0.29))

  raster <- create_raster(vec)
  stack_1 <- create_stack(n_layers = 3, raster = raster)
  
  vec <- c(stats::runif(30, min = 0.90, max = 1.00),
           stats::runif(30, min = 0.80, max = 0.89),
           rep(NA, 10),
           stats::runif(30, min = 0.20, max = 0.29))

  raster <- create_raster(vec)
  terra::add(stack_1) <- create_stack(n_layers = 3, raster = raster)
    
  # Initialize Rasta object with default intervals (10L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = 6,
    stacks = list(stack_1),
    methods = c("ANN"),
    threads = 1L,
    .skip_checks = FALSE
  )

  p_count_obs <- obj$test_private("get_count_stack",
                                  tag = "function",
                                  args = list("ANN"))

  #1
  p_count_exp <- create_raster(c(rep(3, 30),
                                 rep(0, 30),
                                 rep(NA, 10),
                                 rep(0, 30)))

  #2
  terra::add(p_count_exp) <- create_raster(c(rep(0, 30),
                                             rep(3, 30),
                                             rep(NA, 10),
                                             rep(0, 30)))

  #3
  terra::add(p_count_exp) <- create_raster(c(rep(0, 30),
                                             rep(0, 30),
                                             rep(NA, 10),
                                             rep(6, 30)))

  for( i in 4:8 )
  {
      terra::add(p_count_exp) <- create_raster(c(rep(0, 60),
                                                 rep(NA, 10),
                                                 rep(0, 30)))
  }
  
  #9   
  terra::add(p_count_exp) <- create_raster(c(rep(0, 30),
                                             rep(3, 30),
                                             rep(NA, 10),
                                             rep(0, 30)))
  #10
  terra::add(p_count_exp) <- create_raster(c(rep(3, 30),
                                             rep(0, 30),
                                             rep(NA, 10),
                                             rep(0, 30)))

  expect_equal(terra::values(p_count_obs),
               terra::values(p_count_exp))

  max_q <- obj$test_private("max_q")
  
  exp_Q <- c(rep(1, 30),
             rep(0.5*0.70/max_q, 30),
             rep(NA, 10),
             rep(0, 30))
  
  exp_raster <- create_raster(exp_Q)
  q_raster <- obj$get_entropy_raster("ANN")
  names(exp_raster) <- "ANN"
  compare_raster_data(exp_raster, q_raster)
  expect_equal(obj$get_mean_entropy("ANN"), mean(stats::na.omit(exp_Q)))
  
  # Initialize Rasta object with 5L intervals
  obj <- Rasta$new(
    class = "test_class",
    replicates = 6,
    intervals = 5L,
    stacks = list(stack_1),
    methods = c("ANN"),
    threads = 1L,
    .skip_checks = FALSE
  )

  max_q <- obj$test_private("max_q")
  
  exp_Q <- c(rep(1, 60),
             rep(NA, 10),
             rep(0, 30))
  
  exp_raster <- create_raster(exp_Q)
  q_raster <- obj$get_entropy_raster("ANN")
  names(exp_raster) <- "ANN"
  compare_raster_data(exp_raster, q_raster)
  expect_equal(obj$get_mean_entropy("ANN"), mean(stats::na.omit(exp_Q)))
})


test_that("Weight related attribute work", {

    #ANN - 30/0 - 30/0.25 -10/NA - 30/0.25
    vec <- c(stats::runif(30, min = 0.00, max = 0.09),
             stats::runif(30, min = 0.10, max = 0.19),
             rep(NA, 10),
             stats::runif(30, min = 0.20, max = 0.29))

    raster <- create_raster(vec)
    stack_1 <- create_stack(n_layers = 3,
                            raster = raster)
    
    vec <- c(stats::runif(30, min = 0.90, max = 1.00),
             stats::runif(30, min = 0.80, max = 0.89),
             rep(NA, 10),
             stats::runif(30, min = 0.20, max = 0.29))

    raster <- create_raster(vec)
    terra::add(stack_1) <- create_stack(n_layers = 3,
                                        raster = raster)
    
    #GLM - 30/0 - 30/0.25 -10/NA - 30/0.25
    vec <- c(stats::runif(30, min = 0.00, max = 0.09),
             stats::runif(30, min = 0.10, max = 0.19),
             rep(NA, 10),
             stats::runif(30, min = 0.20, max = 0.29))

    raster <- create_raster(vec)
    stack_2 <- create_stack(n_layers = 3,
                            raster = raster)
    
    vec <- c(stats::runif(30, min = 0.90, max = 1.00),
             stats::runif(30, min = 0.80, max = 0.89),
             rep(NA, 10),
             stats::runif(30, min = 0.20, max = 0.29))

    raster <- create_raster(vec)
    terra::add(stack_2) <- create_stack(n_layers = 3,
                                        raster = raster)

    #GOWER - 30/0.5 - 30/0.25 -10/NA - 30/0.25
    vec <- c(stats::runif(30, min = 0.00, max = 0.09),
             stats::runif(30, min = 0.10, max = 0.19),
             rep(NA, 10),
             stats::runif(30, min = 0.20, max = 0.29))

    raster <- create_raster(vec)
    stack_3 <- create_stack(n_layers = 3,
                            raster = raster)
    
    vec <- c(stats::runif(30, min = 0.00, max = 0.09),
             stats::runif(30, min = 0.80, max = 0.89),
             rep(NA, 10),
             stats::runif(30, min = 0.20, max = 0.29))

    raster <- create_raster(vec)
    terra::add(stack_3) <- create_stack(n_layers = 3,
                                        raster = raster)

    #SVM - 30/0.5 - 30/0.25 -10/NA - 30/1/0.25
    vec <- c(stats::runif(30, min = 0.00, max = 0.09),
             stats::runif(30, min = 0.10, max = 0.19),
             rep(NA, 10),
             stats::runif(30, min = 0.20, max = 0.29))

    raster <- create_raster(vec)
    stack_4 <- create_stack(n_layers = 3, raster = raster)
    
    vec <- c(stats::runif(30, min = 0.00, max = 0.09),
             stats::runif(30, min = 0.80, max = 0.89),
             rep(NA, 10),
             stats::runif(30, min = 0.20, max = 0.29))

    raster <- create_raster(vec)
    terra::add(stack_4) <- create_stack(n_layers = 3,
                                        raster = raster)

    obj <- Rasta$new(
      class = "test_class",
      replicates = 6,
      stacks = list(stack_1,stack_2,stack_3,stack_4),
      methods = c("ANN","GLM","GOWER","SVM"),
      threads = 1L,
      .skip_checks = FALSE
    )

    # Entropy
    expect_is(obj, "Rasta")
    max_q <- obj$test_private("max_q")

    exp_Q <- c(rep(1, 30),
               rep(0.5*0.70/max_q, 30),
               rep(NA, 10),
               rep(0, 30))
    
    exp_raster <- create_raster(exp_Q)
    names(exp_raster) <- "ANN"
    q_raster <- obj$get_entropy_raster("ANN")
    
    exp_raster <- create_raster(exp_Q)
    names(exp_raster) <- "GLM"
    q_raster <- obj$get_entropy_raster("GLM")
    compare_raster_data(exp_raster, q_raster)

    exp_Q <- c(rep(0, 30),
               rep(0.5*0.70/max_q, 30),
               rep(NA, 10),
               rep(0, 30))
    
    exp_raster <- create_raster(exp_Q)
    names(exp_raster) <- "GOWER"
    q_raster <- obj$get_entropy_raster("GOWER")
    compare_raster_data(exp_raster, q_raster)

    exp_raster <- create_raster(exp_Q)
    names(exp_raster) <- "SVM"
    q_raster <- obj$get_entropy_raster("SVM")
    compare_raster_data(exp_raster, q_raster)

    obj <- Rasta$new(
      class = "test_class",
      replicates = 6,
      stacks = list(stack_1,stack_2,stack_3,stack_4),
      methods = c("ANN","GLM","GOWER","SVM"),
      threads = 1L,
      .skip_checks = FALSE
    )
    
    # Denominator
    exp_D <- c(rep(2, 30),
               rep(0.5*0.70/max_q*4, 30),
               rep(NA, 10),
               rep(0, 30))

    exp_raster <- create_raster(4-exp_D)
    expect_equal(c("ANN", "GLM", "GOWER", "SVM"), obj$methods)
    d_raster <- obj$test_private(name = "get_weight_denominator",
                                 tag = "function")
    compare_raster_data(exp_raster, d_raster)

    obj <- Rasta$new(
      class = "test_class",
      replicates = 6,
      stacks = list(stack_1,stack_2,stack_3,stack_4),
      methods = c("ANN","GLM","GOWER","SVM"),
      threads = 1L,
      .skip_checks = FALSE
    )
    
    # Weights
    exp_W <- c(rep(0.00, 30),
               rep(0.25, 30),
               rep(NA, 10),
               rep(0.25, 30))
    
    exp_raster <- create_raster(exp_W)

    w_raster <- obj$get_weight_raster("ANN")
    compare_raster_data(exp_raster, w_raster)

    exp_W <- c(rep(0.00, 30),
               rep(0.25, 30),
               rep(NA, 10),
               rep(0.25, 30))

    exp_raster <- create_raster(exp_W)
    w_raster <- obj$get_weight_raster("GLM")
    compare_raster_data(exp_raster, w_raster)

    exp_W <- c(rep(0.50, 30),
               rep(0.25, 30),
               rep(NA, 10),
               rep(0.25, 30))

    exp_raster <- create_raster(exp_W)
    w_raster <- obj$get_weight_raster("GOWER")
    compare_raster_data(exp_raster, w_raster)

    exp_W <- c(rep(0.50, 30),
               rep(0.25, 30),
               rep(NA, 10),
               rep(0.25, 30))

    exp_raster <- create_raster(exp_W)
    w_raster <- obj$get_weight_raster("SVM")
    compare_raster_data(exp_raster, w_raster)

    # Efficiency
    obj <- Rasta$new(
      class = "test_class",
      replicates = 6,
      stacks = list(stack_1,stack_2,stack_3,stack_4),
      methods = c("ANN","GLM","GOWER","SVM"),
      threads = 1L,
      .skip_checks = FALSE
    )

    exp_H <- c(rep(0.50, 30),
               rep(1, 30),
               rep(NA, 10),
               rep(1, 30))
    
    exp_raster <- create_raster(exp_H)
    h_raster <- obj$get_efficiency_raster()
    compare_raster_data(exp_raster, h_raster)
})
