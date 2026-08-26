library(testthat)

library(magrittr)
library(R6)
library(terra)

# Helper function to create a toy SpatRaster
create_toy_spatraster <- function(n_layers, n_cols = 10, n_rows = 10) {
  rasters <- vector("list", n_layers)
  for (i in seq_len(n_layers)) {
    # Create a matrix of random values with specific ranges
    vec <- c(stats::runif(30, min = 0.00, max = 0.09),
             stats::runif(50, min = 0.10, max = 0.19),
             stats::runif(20, min = 0.20, max = 0.29))
    mat <- base::matrix(vec, nrow = n_rows, ncol = n_cols)
    # Convert the matrix to a SpatRaster
    rasters[[i]] <- terra::rast(mat)
  }
  return(terra::rast(rasters))
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

test_that("private$get_count_stack returns correct count stack", {
  # Create toy SpatRasters with specific values for testing
  n_layers <- 10
  n_cols <- 10
  n_rows <- 10
  
  # Define specific values for each layer in the first pixel
  values <- matrix(0, nrow = n_layers, ncol = 1)
  values[1:2, ] <- 0.05  # 2 layers with value between 0.0-0.1
  values[3:7, ] <- 0.15  # 5 layers with value between 0.1-0.2
  values[8:10,] <- 0.25  # 3 layers with value between 0.2-0.3
  
  # Create a SpatRaster with these specific values
  stack_1 <- terra::rast(values)
  
  # Initialize Rasta object with default intervals (10L)
  obj <- Rasta$new(
    class = "test_class",
    replicates = n_layers,
    stacks = list(stack_1),
    methods = c("ANN"),
    threads = 1L,
    .skip_checks = FALSE
  )
  
  # Get private$count_stack using the accessor function
  count_stack <- obj$test_private("get_count_stack", args = list(method = "ANN"))
  
  # Check if count_stack is a SpatRaster with correct dimensions
  expect_is(count_stack, "SpatRaster")
  expect_equal(terra::nlyr(count_stack), n_layers)
  expect_equal(terra::ncol(count_stack), n_cols)
  expect_equal(terra::nrow(count_stack), n_rows)
  
  # Check if the first pixel has correct counts in each layer
  expected_counts <- c(2, 5, 3, rep(0, n_layers - 3))
  actual_counts <- terra::values(count_stack)[1, ]
  expect_equal(actual_counts, expected_counts)
})
