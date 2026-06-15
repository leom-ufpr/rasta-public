test_private_attribs <- function( rasta_front, rasta_stash, attrb )
{
    base::stopifnot( test_class( rasta_front, 'RastaFront' ),
        test_class( rasta_stash, 'RastaStash' ) )
    
    expect_identical( rasta_front$test_priv_fun( attrb ),
        rasta_stash$test_priv_fun( attrb ) )
}

get_exp_mx <- function( vec )
{
    base::stopifnot( is.vector( vec ) )

    base::matrix( rep( vec, 2 ), nrow = 2, byrow = TRUE ) %>%
        return( . )
}

get_valid_rasta_front <- function( rasta_stash, my_dec_id )
{
    base::stopifnot( test_class( rasta_stash, 'RastaStash' ),
               base::is.numeric( my_dec_id ) )

    RastaFront$new( rep_count = rasta_stash$get_rep_count(),
                    dec_id = my_dec_id, max_idx = rasta_stash$test_priv_fun( 'get_max_idx' ),
                    state_idx = rasta_stash$test_priv_fun( 'get_state_idx' ),
                    zero_idx = rasta_stash$test_priv_fun( 'get_zero_idx' ),
                    base_raster = rasta_stash$test_priv_fun( 'get_base_raster' ),
                    base_values = rasta_stash$test_priv_fun( 'get_base_values' ),
                    from_rasta_state = rasta_stash$get_rasta_state() ) %>%
        return( . )
}

test_pareto_tibble <- function( rasta_stash )
{
    testthat::expect_that( rasta_stash, testthat::is_a( 'RastaStash' ) )

    pareto_tibble <- rasta_stash$get_pareto_front()

    pareto_tibble_18 <- pareto_tibble %>% dplyr::filter( dec_id == 18 )
    testthat::expect_true( pareto_tibble_18$diversity == 1 )
    testthat::expect_true( pareto_tibble_18$uncertainty == 16 )

    pareto_tibble_13 <- pareto_tibble %>% dplyr::filter( dec_id == 13 )
    testthat::expect_true( pareto_tibble_13$diversity > 0 )
    testthat::expect_true( pareto_tibble_13$uncertainty < 16 )

    pareto_tibble_9 <- pareto_tibble %>% dplyr::filter( dec_id == 9 )
    testthat::expect_true( pareto_tibble_9$diversity > 0 )
    testthat::expect_true( pareto_tibble_9$uncertainty < 16 )

    pareto_tibble_9 <- pareto_tibble %>% dplyr::filter( dec_id == 12 )
    testthat::expect_true( pareto_tibble_9$diversity == 0 )
    testthat::expect_true( pareto_tibble_9$uncertainty == 0 )
}

get_pareto_state_5_mx <- function()
{
    return( base::matrix( c( base::rep( 12, 16 ), base::rep( 22, 16 ), base::rep( 28, 16 ), base::rep( 7, 16 ),
                       base::rep( 10, 16 ), base::rep( 1, 16 ) ), nrow = 6, byrow = TRUE ) )
}

get_map_mx_5_3 <- function()
{
    return( base::matrix( c( base::rep( 0, 16 ), base::rep( 2, 16 ), base::rep( 0, 16 ), base::rep( 3, 16 ),
                       base::rep( 2, 16 ), base::rep( 1, 16 ) ), nrow = 6, byrow = TRUE ) )
}

get_map_mx_5_14 <- function()
{
    return( base::matrix( c( base::rep( 6, 16 ), base::rep( 3, 16 ), base::rep( 6, 16 ), base::rep( 3, 16 ),
                       base::rep( 5, 16 ), base::rep( 0, 16 ) ), nrow = 6, byrow = TRUE ) )
}

get_map_mx_5_27 <- function()
{
    return( base::matrix( c( base::rep( 4, 16 ), base::rep( 10, 16 ), base::rep( 12, 16 ), base::rep( 3, 16 ),
                       base::rep( 6, 16 ), base::rep( 1, 16 ) ), nrow = 6, byrow = TRUE ) )
}

get_pareto_state_7_mx <- function()
{
    return( base::matrix( c( base::rep( 56, 16 ), base::rep( 78, 16 ), base::rep( 92, 16 ), base::rep( 43, 16 ),
                       base::rep( 22, 16 ), base::rep( 37, 16 ) ), nrow = 6, byrow = TRUE ) )
}

get_map_mx_7_65 <- function()
{
    return( base::matrix( c( base::rep( 0, 16 ), base::rep( 2, 16 ), base::rep( 2, 16 ), base::rep( 1, 16 ),
                       base::rep( 0, 16 ), base::rep( 1, 16 ) ), nrow = 6, byrow = TRUE ) )
}

get_map_mx_7_59 <- function()
{
    return( base::matrix( c( base::rep( 28, 16 ), base::rep( 6, 16 ), base::rep( 12, 16 ), base::rep( 23, 16 ),
                       base::rep( 10, 16 ), base::rep( 17, 16 ) ), nrow = 6, byrow = TRUE ) )
}

test_not_zero_pareto_tibble <- function( rasta_stash )
{
    base::stopifnot( test_class( rasta_stash, 'RastaStash' ) )

    pareto_tibble <- rasta_stash$get_pareto_front()
    testthat::expect_true( tibble::is_tibble( pareto_tibble ) )

    max_diversity <- base::nrow( pareto_tibble ) * rasta_stash$get_method_count()
    max_uncertainty <- max_diversity * 16

    total_diversity <- base::sum( pareto_tibble$diversity )
    total_uncertainty <- base::sum( pareto_tibble$uncertainty )

    testthat::expect_true( total_diversity > 0 & total_diversity < max_diversity )
    testthat::expect_true( total_uncertainty > 0 & total_uncertainty < max_uncertainty )
}

test_that( 'Sanitization',
{
    base::source( "helper_rasta_stash_lib.R" )
    base::source( "helper_rasta_land_lib.R" )
    testthat::expect_true( check_null_par( 'par_val', 'par_desc' ) )
    testthat::expect_error( check_null_par( NULL, 'par_desc' ),
                 'Rasta.+par_desc' )

    testthat::expect_true( check_not_num( c( 1, 3, 4, 5 ), 'par_desc' ) )
    testthat::expect_error( check_not_num( 'par_val', 'par_desc' ),
                 'par_desc.+numeric' )
    testthat::expect_error( check_not_num( c( 1, 2, 3, 'a' ), 'par_desc' ),
                  'par_desc.+numeric' )
    testthat::expect_error( check_not_num( c( 1, 2, 3, NA ), 'par_desc' ),
                  'par_desc.+numeric' )
    testthat::expect_error( check_not_num( c( 1, 2, 3, base::list( a = 3 ) ), 'par_desc' ),
                  'par_desc.+numeric' )
    testthat::expect_error( check_not_num( c( 1, 2, 3, c( 'a', 0 ) ), 'par_desc' ),
                  'par_desc.+numeric' )
})

test_that( 'Interface',
{
    rasta_stash <- RastaStash$new( stash = get_no_warning_stash_0() )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) ) 
    rasta_state <- rasta_stash$get_rasta_state()
    testthat::expect_equal( get_valid_dec_ids( rasta_state ), 
                  c( 3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15 ) )

    pareto_tibble <- rasta_stash$get_pareto_front()
    testthat::expect_equal( base::sum( pareto_tibble$diversity ), 0 )
    testthat::expect_equal( base::sum( pareto_tibble$uncertainty ), 0 )

    rasta_stash <- RastaStash$new( stash = get_pareto_mx_2_methods() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_warning( rasta_stash$get_pareto_front(), "Pareto front" )

    rasta_stash <- RastaStash$new( stash = get_state_mx_stash_NA() )

    testthat::expect_error( RastaFront$new(), 'rep_count' )
    testthat::expect_error( RastaFront$new( rep_count = rasta_stash$get_rep_count() ), 'dec_id' )
    testthat::expect_error( RastaFront$new( rep_count = rasta_stash$get_rep_count(), dec_id = 5 ), 'base_raster' )
    testthat::expect_error( RastaFront$new( rep_count = rasta_stash$get_rep_count(),
                                  dec_id = 5, max_idx = rasta_stash$test_priv_fun( 'get_max_idx' ) ),
                                  'base_raster' )
    testthat::expect_error( RastaFront$new( rep_count = rasta_stash$get_rep_count(),
                                  dec_id = 5, max_idx = rasta_stash$test_priv_fun( 'get_max_idx' ),
                                  state_idx = rasta_stash$test_priv_fun( 'get_state_idx' ) ),
                                  'base_raster' )
    testthat::expect_error( RastaFront$new( rep_count = rasta_stash$get_rep_count(),
                                  dec_id = 5, max_idx = rasta_stash$test_priv_fun( 'get_max_idx' ),
                                  state_idx = rasta_stash$test_priv_fun( 'get_state_idx' ),
                                  zero_idx = rasta_stash$test_priv_fun( 'get_zero_idx' ),
                                  base_raster = rasta_stash$test_priv_fun( 'get_base_raster' ) ),
                                  'base_values' )
    testthat::expect_error( RastaFront$new( rep_count = rasta_stash$get_rep_count(),
                                  dec_id = 5, max_idx = rasta_stash$test_priv_fun( 'get_max_idx' ),
                                  state_idx = rasta_stash$test_priv_fun( 'get_state_idx' ),
                                  zero_idx = rasta_stash$test_priv_fun( 'get_zero_idx' ),
                                  base_raster = rasta_stash$test_priv_fun( 'get_base_raster' ),
                                  base_values = rasta_stash$test_priv_fun( 'get_base_values' ) ),
                                  'from_rasta_state' )
   testthat::expect_error( RastaFront$new( rep_count = rasta_stash$get_rep_count(),
                                 dec_id = 5, max_idx = c( 1, 2, 4, 'c', 6 ),
                                 state_idx = rasta_stash$test_priv_fun( 'get_state_idx' ),
                                 zero_idx = rasta_stash$test_priv_fun( 'get_zero_idx' ),
                                 base_raster = rasta_stash$test_priv_fun( 'get_base_raster' ),
                                 base_values = rasta_stash$test_priv_fun( 'get_base_values' ),
                                 from_rasta_state = rasta_stash$get_rasta_state(),
                                 'max_idx' ) )
    testthat::expect_error( RastaFront$new( rep_count = rasta_stash$get_rep_count(),
                                 dec_id = 5, max_idx = rasta_stash$test_priv_fun( 'get_max_idx' ),
                                 state_idx = rasta_stash$test_priv_fun( 'get_state_idx' ),
                                 zero_idx = c( 'a', 2, 4, 5, 6 ),
                                 base_raster = rasta_stash$test_priv_fun( 'get_base_raster' ),
                                 base_values = rasta_stash$test_priv_fun( 'get_base_values' ),
                                 from_rasta_state = rasta_stash$get_rasta_state(),
                                 'zero_idx' ) )

    rasta_front <- get_valid_rasta_front( rasta_stash, 5 )
    testthat::expect_that( rasta_front, testthat::is_a( 'RastaFront' ) )
    testthat::expect_false( rasta_front$has_state_mx() )
    
    testthat::expect_identical( rasta_front$get_rep_count(), rasta_stash$get_rep_count() )
    testthat::expect_identical( rasta_front$get_dec_id(), 5 )

    test_private_attribs( rasta_front, rasta_stash, 'get_base_raster' )
    test_private_attribs( rasta_front, rasta_stash, 'get_base_values' ) 
    test_private_attribs( rasta_front, rasta_stash, 'get_max_idx' ) 
    test_private_attribs( rasta_front, rasta_stash, 'get_state_idx' ) 
    test_private_attribs( rasta_front, rasta_stash, 'get_zero_idx' )
    
    testthat::expect_equal( rasta_front$test_priv_fun( 'get_from_rasta_state' ),
                  rasta_stash$get_rasta_state() )

    testthat::expect_equal( rasta_front$test_priv_fun( 'get_to_rasta_state' ),
                  RastaState$new( methods = c( 'glm', 'svm' ) ) )

    expected_bin_id_vec <- c( 0, 1, 0, 1 )
    names( expected_bin_id_vec ) <- c( 'ann', 'glm', 'rf', 'svm' )

    testthat::expect_identical( rasta_front$test_priv_fun( 'get_bin_id_vec' ),
                      expected_bin_id_vec )

    expected_method_filter <- c( FALSE, TRUE, FALSE, TRUE )
    names( expected_method_filter ) <- c( 'ann', 'glm', 'rf', 'svm' )
    
    testthat::expect_identical( rasta_front$test_priv_fun( 'get_method_filter' ),
                      expected_method_filter )
    testthat::expect_identical( rasta_front$test_priv_fun( 'get_state_mx'),
                      base::matrix( base::rep( NA, 30 ), nrow = 2 ) )

    expected_dec_map <- base::rep( NA, 16 )
    names( expected_dec_map ) <- base::rep( 0:15 )
    
    testthat::expect_identical( rasta_front$test_priv_fun( 'get_dec_map' ),
                      expected_dec_map )

    from_rasta_state <- rasta_front$test_priv_fun( 'get_from_rasta_state' )
    to_rasta_state <- rasta_front$test_priv_fun( 'get_to_rasta_state' )

    expected_bin_vec <- c( 0, 1, 0, 1 )
    names( expected_bin_vec ) <- c( 'ann', 'glm', 'rf', 'svm' )

    testthat::expect_identical( from_rasta_state$get_bin_vec( 5 ), expected_bin_vec )
    testthat::expect_identical( from_rasta_state$get_bin_vec( 5 ), expected_bin_vec )
    testthat::expect_identical( rasta_front$test_priv_fun( 'map_dec', base::list( 5 ) ), 3 )

    expected_dec_map <- c( base::rep( NA, 5 ), 3, base::rep( NA, 10 ) )
    names( expected_dec_map ) <- base::rep( 0:15 )
    
    testthat::expect_identical( rasta_front$test_priv_fun( 'get_dec_map' ), expected_dec_map )

    testthat::expect_identical( rasta_front$test_priv_fun( 'map_dec', base::list( 1 ) ), 1 )    
    testthat::expect_identical( rasta_front$test_priv_fun( 'map_dec', base::list( 2 ) ), 0 )
    testthat::expect_identical( rasta_front$test_priv_fun( 'map_dec', base::list( 4 ) ), 2 )    
    testthat::expect_identical( rasta_front$test_priv_fun( 'map_dec', base::list( 8 ) ), 0 )
    testthat::expect_identical( rasta_front$test_priv_fun( 'map_dec', base::list( 9 ) ), 1 )
    testthat::expect_identical( rasta_front$test_priv_fun( 'map_dec', base::list( 10 ) ), 0 )

    from_state_mx <- get_exp_mx( c( 1, 2, 4, 8, 3, 5, 9, 6, 10, 12, 7, 11, 13, 14, 15 ) )

    base::lapply( 1:base::nrow( from_state_mx ), function( i )
        { rasta_front$build_state_mx( i, from_state_mx[ i, ] ) } )
    
    to_state_mx <- get_exp_mx( c( 1, 0, 2, 0, 1, 3, 1, 2, 0, 2, 3, 1, 3, 2, 3 ) )
    testthat::expect_identical( rasta_front$test_priv_fun( 'get_state_mx' ), to_state_mx )

    rasta_front <- get_valid_rasta_front( rasta_stash, 7 )
    testthat::expect_that( rasta_front, testthat::is_a( 'RastaFront' ) )
    
    base::lapply( 1:base::nrow( from_state_mx ), function( i )
        { rasta_front$build_state_mx( i, from_state_mx[ i, ] ) } )

    to_state_mx <- get_exp_mx( c( 1, 2, 4, 0, 3, 5, 1, 6, 2, 4, 7, 3, 5, 6, 7 ) )
    testthat::expect_identical( rasta_front$test_priv_fun( 'get_state_mx' ), to_state_mx )

    rasta_stash <- RastaStash$new( stash = get_pareto_6_reps() )
    testthat::expect_that( rasta_stash, testthat::is_a( 'RastaStash' ) )
    testthat::expect_true( base::is.matrix( rasta_stash$test_priv_fun( 'get_state_mx' ) ) )
    testthat::expect_false( base::is.null( rasta_stash$test_priv_fun( 'get_state_mx' ) ) )

    rasta_front <- rasta_stash$test_priv_fun( 'get_built_rasta_front', base::list( 18 ) ) 
    testthat::expect_that( rasta_front, testthat::is_a( 'RastaFront' ) )
    testthat::expect_false( base::is.null( rasta_front$test_priv_fun( 'get_state_mx' ) ) )
    testthat::expect_false( base::ncol( rasta_front$test_priv_fun( 'get_state_mx' ) ) == 0 )
    testthat::expect_false( base::is.na( rasta_front$test_priv_fun( 'get_state_mx' ) ) %>% any( . ) )
        
    testthat::expect_true( rasta_front$has_state_mx() )
    testthat::expect_true( rasta_front$get_uncertainty() > 0 )
    testthat::expect_true( rasta_front$get_diversity() > 0 )

    rasta_front <- rasta_stash$test_priv_fun( 'get_built_rasta_front', base::list( 26 ) ) 
    testthat::expect_true( rasta_front$get_uncertainty() == 16 )
    testthat::expect_true( rasta_front$get_diversity() == 1 )
})

test_that( 'Pareto Built Base::Matrix',
{
    rasta_stash <- RastaStash$new( stash = get_pareto_6_reps() )
    rasta_stash$test_priv_fun( 'set_disable_mx' )

    test_pareto_tibble( rasta_stash )
})


test_that( 'Pareto Stored Base::Matrix',
{
    rasta_stash <- RastaStash$new( stash = get_pareto_mx_5_methods() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_method_count(), 5 )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_mx' ),
                  get_pareto_state_5_mx() )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_front_state_mx', base::list( 3 ) ),
                  get_map_mx_5_3() )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_front_state_mx', base::list( 14 ) ),
                  get_map_mx_5_14() )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_front_state_mx', base::list( 27 ) ),
                  get_map_mx_5_27() )

    rasta_stash <- RastaStash$new( stash = get_pareto_mx_5_methods() )
    test_not_zero_pareto_tibble( rasta_stash )

    rasta_stash <- RastaStash$new( stash = get_pareto_mx_7_methods() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_method_count(), 7 )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_mx' ),
                  get_pareto_state_7_mx() )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_front_state_mx', base::list( 65 ) ),
                  get_map_mx_7_65() )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_front_state_mx', base::list( 59 ) ),
                  get_map_mx_7_59() )

    rasta_stash <- RastaStash$new( stash = get_pareto_mx_7_methods() )
    test_not_zero_pareto_tibble( rasta_stash ) 
    
    rasta_stash <- RastaStash$new( stash = get_pareto_mx_6_100_50() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_method_count(), 6 )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )
    testthat::expect_true( base::is.null( rasta_stash$test_priv_fun( 'get_max_idx' ) ) )
    testthat::expect_true( base::is.null( rasta_stash$test_priv_fun( 'get_zero_idx' ) ) )
    testthat::expect_equal( base::length( rasta_stash$test_priv_fun( 'get_state_idx' ) ), 16 )

    # ANN e BIOMOD
    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 48 ) )
    testthat::expect_that( rasta_front, testthat::is_a( "RastaFront" ) )
    testthat::expect_true( rasta_front$has_state_mx() )

    state_mx <- rasta_front$test_priv_fun( 'get_state_mx' )
    testthat::expect_true( base::is.matrix( state_mx ) )
    front_state_sum <- Rfast::colsums( state_mx )
    testthat::expect_equal( front_state_sum, base::rep( 6, 16 ) )

    rep_count <- rasta_front$get_rep_count()
    testthat::expect_equal( rep_count, 6 )

    max_state_sum <- rasta_front$get_rep_count() * ( 2**rasta_front$get_method_count() - 1 )
    testthat::expect_equal( max_state_sum, 18 )

    testthat::expect_identical( rasta_front$test_priv_fun( 'get_front_idx', base::list( state_mx, 0 ) ), c() )
    testthat::expect_identical( rasta_front$test_priv_fun( 'get_front_idx', base::list( state_mx, max_state_sum ) ), c() )

    testthat::expect_true( rasta_front$test_priv_fun( 'update_indices', base::list( 'zero_idx', c() ) ) )
    testthat::expect_true( rasta_front$test_priv_fun( 'update_indices', base::list( 'max_idx', c() ) ) )

    testthat::expect_true( base::is.null( rasta_front$test_priv_fun( 'get_max_idx' ) ) )
    testthat::expect_true( base::is.null( rasta_front$test_priv_fun( 'get_zero_idx' ) ) )

    testthat::expect_identical( rasta_front$test_priv_fun( 'get_state_idx'), c( 1:16 ) )

    testthat::expect_true( rasta_front$has_state_mx() )
    testthat::expect_identical( rasta_front$get_diversity(), 0 )
    testthat::expect_identical( rasta_front$get_uncertainty(), 0 )
    
    # ANN e GLM
    state_mx <- rasta_stash$test_priv_fun( 'get_front_state_mx', base::list( 36 ) )
    testthat::expect_true( base::is.matrix( state_mx ) )
    
    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 36 ) )
    testthat::expect_false( rasta_front$has_state_mx() )

    # BIOMOD e RF
    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 17 ) )
    testthat::expect_false( rasta_front$has_state_mx() )

    # ANN e GAM
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_front_state_mx', base::list( 40 ) ),
                  base::matrix( base::rep( c( 0, 0, 1, 1 ), 24 ), nrow = 6, byrow = TRUE ) )
    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 40 ) )
    testthat::expect_that( rasta_front, testthat::is_a( "RastaFront" ) )
    testthat::expect_true( base::is.null( rasta_front$test_priv_fun( 'get_max_idx' ) ) )
    testthat::expect_equal( rasta_front$test_priv_fun( 'get_state_idx' ), c( 3, 4, 7, 8, 11, 12, 15, 16 ) )
    testthat::expect_equal( rasta_front$test_priv_fun( 'get_zero_idx' ), c( 1, 2, 5, 6, 9, 10, 13, 14 ) )
    testthat::expect_equal( rasta_front$test_priv_fun( 'get_state_mx' ),
                 base::matrix( base::rep( 1, 48 ), nrow = 6, byrow = TRUE ) )
    testthat::expect_identical( rasta_front$get_diversity(), 1 )
    testthat::expect_identical( rasta_front$get_uncertainty(), 0 )

    # BIOMOD e GAM
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_front_state_mx', base::list( 24 ) ),
                  base::matrix( base::rep( c( 2, 2, 3, 3 ), 24 ), nrow = 6, byrow = TRUE ) )
    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 24 ) )
    testthat::expect_that( rasta_front, testthat::is_a( "RastaFront" ) )
    testthat::expect_true( base::is.null( rasta_front$test_priv_fun( 'get_zero_idx' ) ) )
    testthat::expect_equal( rasta_front$test_priv_fun( 'get_max_idx' ), c( 3, 4, 7, 8, 11, 12, 15, 16 ) )
    testthat::expect_equal( rasta_front$test_priv_fun( 'get_state_idx' ), c( 1, 2, 5, 6, 9, 10, 13, 14 ) )
    testthat::expect_equal( rasta_front$test_priv_fun( 'get_state_mx' ),
                 base::matrix( base::rep( 2, 48 ), nrow = 6, byrow = TRUE ) )
    testthat::expect_identical( rasta_front$get_diversity(), 1 )
    testthat::expect_identical( rasta_front$get_uncertainty(), 0 )

    rasta_stash <- RastaStash$new( stash = get_pareto_mx_6_100_50() )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )
    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 17 ) )
    testthat::expect_true( base::is.null( rasta_front$test_priv_fun( 'get_state_idx' ) ) )
    testthat::expect_true( base::is.null( rasta_front$test_priv_fun( 'get_state_mx' ) ) )

    # PARETO SHIFT
    rasta_stash <- RastaStash$new( stash = get_pareto_shift() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_method_count(), 4 )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )
    testthat::expect_equal( base::length( rasta_stash$test_priv_fun( 'get_max_idx' ) ), 4 )
    testthat::expect_equal( base::length( rasta_stash$test_priv_fun( 'get_zero_idx' ) ), 5 )
    testthat::expect_equal( base::length( rasta_stash$test_priv_fun( 'get_state_idx' ) ), 7 )

    # ANN e GAM
    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 9 ) )
    testthat::expect_that( rasta_front, testthat::is_a( "RastaFront" ) )
    testthat::expect_equal( base::length( rasta_front$test_priv_fun( 'get_max_idx' ) ), 6 )
    testthat::expect_equal( base::length( rasta_front$test_priv_fun( 'get_zero_idx' ) ), 6 )
    testthat::expect_equal( base::length( rasta_front$test_priv_fun( 'get_state_idx' ) ), 4 )

    rasta_stash <- RastaStash$new( stash = get_pareto_shift_9() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )

    testthat::expect_equal( rasta_stash$get_diversity(), rasta_front$get_diversity() )

    testthat::expect_equal( rasta_stash$get_uncertainty(), rasta_front$get_uncertainty() )

    rasta_stash <- RastaStash$new( stash = get_pareto_shift() )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )
    
    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 14 ) )
    testthat::expect_that( rasta_front, testthat::is_a( "RastaFront" ) )
    
    rasta_stash <- RastaStash$new( stash = get_pareto_shift_14() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )

    testthat::expect_equal( rasta_stash$test_priv_fun( "get_max_idx" ) %>% base::sort( . ),
                  rasta_front$test_priv_fun( "get_max_idx" ) %>% base::sort( . ) )

    testthat::expect_equal( rasta_stash$test_priv_fun( "get_zero_idx" ), rasta_front$test_priv_fun( "get_zero_idx" ) )
    testthat::expect_equal( rasta_stash$test_priv_fun( "get_state_idx" ), rasta_front$test_priv_fun( "get_state_idx" ) )

    testthat::expect_equal( rasta_stash$get_diversity(), rasta_front$get_diversity() )
    testthat::expect_equal( rasta_stash$get_uncertainty(), rasta_front$get_uncertainty() )
    
    rasta_stash <- RastaStash$new( stash = get_pareto_mx_6_100_50() )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )
    
    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 30 ) )
    testthat::expect_that( rasta_front, testthat::is_a( "RastaFront" ) )
    
    rasta_stash <- RastaStash$new( stash = get_pareto_mx_4_100_50() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_true( rasta_stash$test_priv_fun( 'has_map_mx' ) )

    testthat::expect_equal( rasta_stash$test_priv_fun( "get_max_idx" ),  
                  rasta_front$test_priv_fun( "get_max_idx" ) )

    testthat::expect_equal( rasta_stash$test_priv_fun( "get_zero_idx" ),  
                  rasta_front$test_priv_fun( "get_zero_idx" ) )

    testthat::expect_equal( rasta_stash$test_priv_fun( "get_state_idx" ),  
                  rasta_front$test_priv_fun( "get_state_idx" ) )

    testthat::expect_equal( rasta_stash$get_diversity(),
                  rasta_front$get_diversity() )

    testthat::expect_equal( rasta_stash$get_uncertainty(),
                  rasta_front$get_uncertainty() )

    rasta_stash <- RastaStash$new( stash = get_pareto_zero_cells() )
    testthat::expect_true( rasta_stash$get_pareto_front() %>% tibble::is_tibble( . ) )

    rasta_front <- rasta_stash$test_priv_fun( 'get_mx_rasta_front', base::list( 15 ) )
    testthat::expect_equal( rasta_stash$get_diversity(),
                  rasta_front$get_diversity() )

    testthat::expect_equal( rasta_stash$get_uncertainty(),
                  rasta_front$get_uncertainty() )
})
