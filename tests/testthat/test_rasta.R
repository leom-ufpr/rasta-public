library(testthat)

library(magrittr)
library(R6)
library(terra)

# Helper function to create a toy SpatRaster
create_toy_spatraster <- function(n_layers, n_cols = 10, n_rows = 10) {
  rasters <- vector("list", n_layers)
  for (i in seq_len(n_layers)) {
    # Create a matrix of random values with some NA values
    mat <- matrix(runif(n_cols * n_rows), nrow = n_rows, ncol = n_cols)
    mat[sample(1:(n_cols * n_rows), size = 5)] <- NA
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
  
  expect_error(Rasta$new(
    class = "test_class",
    replicates = 5,
    intervals = 10,
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
  
  # Check if the extent and resolution of the mean suitability raster match those of the input stacks
  expect_true(terra::compareGeom(stack_1, mean_suit_raster, stopOnError = FALSE)$equal)
})
