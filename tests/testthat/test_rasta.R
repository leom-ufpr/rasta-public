library(testthat)
library(R6)
library(terra)

# Helper function to create a toy SpatRaster
create_toy_spatraster <- function(n_layers, n_cols = 10, n_rows = 10) {
  rasters <- vector("list", n_layers)
  for (i in seq_len(n_layers)) {
    # Create a matrix of random values
    mat <- matrix(runif(n_cols * n_rows), nrow = n_rows, ncol = n_cols)
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
