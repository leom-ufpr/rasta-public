no_stash_test <- function( rasta_stash, fun_name )
{
    base::stopifnot( base::class( rasta_stash )[1] == "RastaStash",
                     base::is.character( fun_name ) )

    testthat::expect_error( rasta_stash[[ fun_name ]](), base::paste0( "'RastaLand'.+", fun_name ) )
}

unequal_stash_test <- function( rasta_stash, fun_name )
{
    base::stopifnot( base::class( rasta_stash )[1] == "RastaStash",
                     base::is.character( fun_name ) )

    testthat::expect_error( rasta_stash[[ fun_name ]](), base::paste0( "replicate.+", fun_name ) )
}

test_that( "Auxiliary subs",
{
    source( "helper_rasta_land_lib.R" )
    source( "helper_rasta_stash_lib.R" )
    testthat::expect_match( get_var_val_type_msg( 'stash', raster::raster(), 'constructor', 'RastaStash' ),
                  'stash.+constructor.+S4.+RastaStash',
                  perl = TRUE )

    testthat::expect_true( test_class( raster::raster(), 'RasterLayer' ) )
    testthat::expect_false( test_class( 'a', 'SpatRaster' ) )

    testthat::expect_identical( get_list_rep( 'a' ), 'a' )
    testthat::expect_identical( get_list_rep( 1 ), 'rep1' )

    rasta_land <- RastaLand$new( class = 'test_half',
                                 method = 'none',
                                 rep = 1,
                                 rep_count = 1,
                                 raster_obj = get_raster_layer_0_50() )
    
    expected_freq <- c( 8, 8 )
    base::names( expected_freq ) <- c( 0, 1 )
    testthat::expect_equal( rasta_land$get_state_freqs(), expected_freq )
    testthat::expect_false( isnt_binary_raster( rasta_land ) )

    rasta_land <- RastaLand$new( class = 'test_non_binary',
                                 method = 'none',
                                 rep = 1,
                                 rep_count = 1,
                                 raster_obj = get_raster_layer_16_max() )

    testthat::expect_true( isnt_binary_raster( rasta_land ) )
    testthat::expect_equal( get_num_values( get_raster_layer_1_100() ), base::rep( 1, 16 ) )
    testthat::expect_equal( get_num_values( get_raster_layer_1_100_NA() ), base::rep( 1, 15 ) )
    testthat::expect_equal( sum_raster( get_raster_layer_1_100() ), 16 )
    testthat::expect_equal( sum_raster( get_raster_layer_1_100_NA() ), 15 )
    testthat::expect_equal( get_raster_mean( get_raster_layer_1_100() ), 1 )
    testthat::expect_equal( get_raster_mean( get_raster_layer_1_100_NA() ), 1 )
})

test_that( "Class interface and elementary methods",
{
    testthat::expect_error( RastaStash$new( stash = 0 ), "Wrong.+constructor" )
    testthat::expect_error( RastaStash$new( stash = NA ), "Wrong.+constructor" )
    testthat::expect_error( RastaStash$new( stash = NULL ), "Wrong.+constructor" )
    testthat::expect_error( RastaStash$new( stash = raster::raster() ), "Wrong.+constructor" )
    testthat::expect_error( RastaStash$new( stash = c() ), "Wrong.+constructor" )
    testthat::expect_error( RastaStash$new( stash = c( 0, 'a' ) ), "Wrong.+constructor" )
    testthat::expect_error( RastaStash$new( stash = base::list( NA ) ), "Wrong.+add" )
    testthat::expect_error( RastaStash$new( stash = base::list ( NULL ) ), "Wrong.+add" )
    testthat::expect_error( RastaStash$new( stash = base::list( base::numeric() ) ), "Wrong.+add" )
    testthat::expect_error( RastaStash$new( stash = base::list( base::character() ) ), "Wrong.+add" )
    testthat::expect_error( RastaStash$new( stash = base::list( a = 1, b = 2 ) ), "Wrong.+add" )
    testthat::expect_error( RastaStash$new( stash = base::list( a = 'c', b = 'd' ) ), "Wrong.+add" )
    testthat::expect_that( RastaStash$new( stash = base::list() ), is_a( "RastaStash" ) )
    testthat::expect_error( RastaStash$new( stash = base::list(), .enforce_binary = 1 ), ".enforce_binary.+logical" )
    testthat::expect_error( RastaStash$new( stash = base::list(), .enforce_binary = "OK" ), ".enforce_binary.+logical" )
    testthat::expect_error( RastaStash$new( stash = base::list(), .profile = 1L ), ".profile.+logical" )
    testthat::expect_error( RastaStash$new( stash = base::list(), .profile = "TRUE" ), ".profile.+logical" )
    testthat::expect_error( RastaStash$new( stash = base::list(), .verbosity = TRUE ), ".verbosity.+integer" )
    testthat::expect_error( RastaStash$new( stash = base::list(), .verbosity = 1.2 ), ".verbosity.+integer" )
    testthat::expect_that( RastaStash$new( stash = base::list(), .verbosity = 1L ), is_a( "RastaStash" ) )
    testthat::expect_that( RastaStash$new( stash = base::list(), .verbosity = 1 ), is_a( "RastaStash" ) )

    rasta_stash <- RastaStash$new( stash = base::list() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_error( rasta_stash$add( 0 ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( NA ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( NULL ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( c() ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( c( 0, 'a' ) ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( raster::raster() ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( base::list() ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( base::list( NA ) ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( base::list( NULL ) ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( base::list( a = 1, b = 2 ) ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( base::list( a = 'c', b = 'd' ) ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( base::list( base::character() ) ), "Wrong.+object.+add" )
    testthat::expect_error( rasta_stash$add( base::list( base::numeric() ) ), "Wrong.+object.+add" )
    testthat::expect_warning( rasta_stash$test_priv_fun( "get_whatever" ), "Method" )
    
    testthat::expect_true( get_rasta_land( get_raster_layer_1_100, 'ann', 1, 4 ) %>%
                 rasta_stash$add(.) )
    testthat::expect_true( get_rasta_land( get_raster_layer_0_100, 'ann', 2, 4 ) %>%
                 rasta_stash$add(.) )
    testthat::expect_true( get_rasta_land( get_raster_layer_0_50, 'ann', 3, 4 ) %>%
                 rasta_stash$add(.) )
    testthat::expect_true( get_rasta_land( get_raster_layer_0_50_inter, 'ann', 4, 4 ) %>%
                 rasta_stash$add(.) )
    testthat::expect_identical( rasta_stash$get_class(), 'S. siderea' )
    testthat::expect_equal( rasta_stash$get_method_count_list(), base::list( ann = 4 ) )
    testthat::expect_identical( rasta_stash$get_method_count(), base::as.integer( 1 ) )
    testthat::expect_identical( rasta_stash$get_methods(), 'ann' )
    testthat::expect_identical( rasta_stash$get_rep_count(), base::as.integer( 4 ) )
    testthat::expect_error( get_rasta_land( get_raster_layer_16_max, 'ann', 1, 4 ) %>%
                  rasta_stash$add(.), "Replicate.+non-binary" )

    rasta_stash <- RastaStash$new( stash = base::list(), .enforce_binary = FALSE )
    testthat::expect_that( rasta_stash, testthat::is_a( 'RastaStash' ) )

    rasta_stash <- RastaStash$new( stash = base::list() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_method_count_list(), base::list()  )
    testthat::expect_equal( rasta_stash$get_method_count(), 0 )
    testthat::expect_equal( rasta_stash$get_methods(), c() )
    testthat::expect_true( get_rasta_land( get_raster_layer_1_100, 'ann', 1 ) %>%
                 rasta_stash$add(.) )
    testthat::expect_true( get_rasta_land( get_raster_layer_0_100, 'rf', 1 ) %>%
                 rasta_stash$add(.) )
    testthat::expect_true( get_rasta_land( get_raster_layer_0_50, 'svm', 1 ) %>%
                 rasta_stash$add(.) )
    testthat::expect_true( get_rasta_land( get_raster_layer_0_50_inter, 'glm', 1 ) %>%
                 rasta_stash$add(.) )
    testthat::expect_equal( rasta_stash$get_method_count_list(),
                      base::list( ann = 1, rf = 1, svm = 1, glm = 1 ) )
    testthat::expect_identical( rasta_stash$get_method_count(), base::as.integer( 4 ) )
    testthat::expect_identical( rasta_stash$get_methods(), c( 'ann', 'glm', 'rf', 'svm' ) )
    testthat::expect_identical( rasta_stash$get_rep_count(), base::as.integer( 1 ) )

    rasta_stash <- RastaStash$new( stash = base::list() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_true( get_rasta_land( get_raster_layer_1_100, 'ann', 1, 4, ) %>%
                 rasta_stash$add(.) )
    testthat::expect_error( get_rasta_land( get_raster_layer_1_100, 'ann', 1, 4, 'S. stellata' ) %>%
                  rasta_stash$add(.), "S. siderea and S. stellata" )

    rasta_stash <- RastaStash$new( stash = base::list() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    for( rasta_land in get_no_warning_stash_1() )
    {
        testthat::expect_true( rasta_stash$add( rasta_land ) )
    }
    testthat::expect_equal( rasta_stash$get_all(), get_no_warning_stash_1() )
    testthat::expect_equal( rasta_stash$get_method_count_list(), base::list( ann = 2, rf = 2, svm = 2, glm = 2 ) )
    testthat::expect_identical( rasta_stash$get_method_count(), base::as.integer( 4 ) )
    testthat::expect_identical( rasta_stash$get_methods(), c( 'ann', 'glm', 'rf', 'svm' ) )
    testthat::expect_true( rasta_stash$has_method( 'ann' ) )
    testthat::expect_true( rasta_stash$has_method( 'rf' ) )
    testthat::expect_false( rasta_stash$has_method( 'gam' ) )
    testthat::expect_identical( rasta_stash$get_rep_count(), base::as.integer( 2 ) )
    testthat::expect_that( RastaStash$new( stash = get_no_warning_stash_1() ), testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_method_count_list(), base::list( ann = 2, rf = 2, svm = 2, glm = 2 ) )
    testthat::expect_identical( rasta_stash$get_method_count(), base::as.integer( 4 ) )
    testthat::expect_identical( rasta_stash$get_methods(), c( 'ann', 'glm', 'rf', 'svm' ) )
    testthat::expect_identical( rasta_stash$get_rep_count(), base::as.integer( 2 ) )

    rasta_stash <- RastaStash$new( stash = get_warning_stash() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_error( rasta_stash$get_vote_raster(), "replicate.+get_vote_raster" )

    rasta_stash <- RastaStash$new( stash = base::list() )
    testthat::expect_true( rasta_stash$add( get_rasta_land_0_50( 1, 'ann', 1 ) ) )
    testthat::expect_error( rasta_stash$add( get_rasta_land_0_50_NA( 2, 'ann', 2 ) ), "different lengths" )
    
    rasta_stash <- RastaStash$new( stash = get_no_warning_stash_1() )
    testthat::expect_that( RastaStash$new( stash = get_no_warning_stash_1() ), testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_all(), get_no_warning_stash_1() )
    testthat::expect_error( rasta_stash$get_by_index( NA ), "Wrong.+index.+numeric" )
    testthat::expect_error( rasta_stash$get_by_index( NULL ), "Wrong.+index.+numeric" )
    testthat::expect_error( rasta_stash$get_by_index( base::list( numeric() ) ), "Wrong.+index.+numeric" )
    testthat::expect_error( rasta_stash$get_by_index( 0 ), "Index.+greater" )
    testthat::expect_error( rasta_stash$get_by_index( 9 ), "Index.+bounds" )
    for( i in c( 1:8 ) )
    {        
        testthat::expect_that( rasta_stash$get_by_index( i ), testthat::is_a( "RastaLand" ) )
    }
    testthat::expect_equal( rasta_stash$get_by_rep( 1 ), get_by_rep_stash( 1, get_power_list() ) )
    testthat::expect_equal( rasta_stash$get_by_rep( 2 ), get_by_rep_stash( 2, get_power_list() ) )
    testthat::expect_equal( rasta_stash$get_by_index( 1 ), get_no_warning_stash_1()[[ 1 ]] )
    testthat::expect_equal( rasta_stash$get_by_index( 4 ), get_no_warning_stash_1()[[ 4 ]] )
    testthat::expect_equal( rasta_stash$get_by_index( 5 ), get_no_warning_stash_1()[[ 5 ]] )
    testthat::expect_equal( rasta_stash$get_by_index( 8 ), get_no_warning_stash_1()[[ 8 ]] )

    rasta_stash <- RastaStash$new()
    testthat::expect_null( rasta_stash$get_class() )
    testthat::expect_true( base::is.list( rasta_stash$get_all() ) )
    testthat::expect_equal( base::length( rasta_stash$get_all() ), 0 )
    testthat::expect_true( base::is.list( rasta_stash$get_method_count_list() ) )
    testthat::expect_equal( base::length( rasta_stash$get_method_count_list() ), 0 )
    testthat::expect_equal( rasta_stash$get_methods(), c() )
    testthat::expect_equal( rasta_stash$get_method_count(), 0 )
    testthat::expect_equal( rasta_stash$get_rep_count(), 0 )
    testthat::expect_null( rasta_stash$get_by_index() )
    testthat::expect_equal( rasta_stash$get_rasta_land_count(), 0 )
    testthat::expect_null( rasta_stash$get_raster_size() )
    testthat::expect_false( rasta_stash$has_stash() )
    testthat::expect_false( rasta_stash$has_methods() )
    testthat::expect_error( rasta_stash$get_by_rep( 1 ), "'RastaLand'.+get_by_rep" )
    testthat::expect_error( rasta_stash$get_by_method( 'ann' ), "'RastaLand'.+get_by_method" )
    
    rs_unequal <- RastaStash$new( stash = get_warning_stash() )
    testthat::expect_true( rs_unequal$has_stash() )
    testthat::expect_true( rs_unequal$has_methods() )
    testthat::expect_error( rs_unequal$get_by_rep( 1 ), "replicate.+get_by_rep" )
    testthat::expect_error( rs_unequal$get_by_method( 'ann' ), "replicate.+get_by_method" )
        
    for( public_method in c( 'get_diversity', 'get_efficiency', 'get_efficiency_raster',
        'get_gini_raster', 'get_uncertainty', 'get_uncertainty_raster',
        'get_normalized_entropy_raster', 'get_pareto_front', 'get_rasta_state',
        'get_vote_raster', 'get_mrule_raster', 'get_prob_raster', 'get_weight_stack',
        'get_wgt_mrule_raster', 'get_wgt_prob_raster' ) )
    {
        no_stash_test( rasta_stash, public_method )
        unequal_stash_test( rs_unequal, public_method )
    }

    rasta_stash <- RastaStash$new( stash = get_no_warning_stash_1() )
    rep_method_mx <- base::matrix( c( 1, 4, 2, 3, 5, 8, 6, 7 ), nrow = 2, byrow = TRUE )
    base::colnames( rep_method_mx ) <- rasta_stash$get_methods()
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_rep_method_mx' ), rep_method_mx )

    rasta_stash <- RastaStash$new( stash = get_single_rep_stash() )
    testthat::expect_error( rasta_stash$test_priv_fun( 'set_check_lists', list( 'get_single_rep_stash' ) ), 'count.+1.+count.+2 reps' )

    rasta_stash <- RastaStash$new( stash = get_merge_1_stash() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_rasta_land_count(), 4 )

    rs_2 <- RastaStash$new( stash = get_merge_2_stash() )
    testthat::expect_that( rs_2, testthat::is_a( "RastaStash" ) )

    rs_3 <- RastaStash$new( stash = get_merge_3_stash() )
    testthat::expect_that( rs_3, testthat::is_a( "RastaStash" ) )

    rs_4 <- RastaStash$new( stash = get_merge_4_stash() )
    testthat::expect_that( rs_4, testthat::is_a( "RastaStash" ) )

    rs_5 <- RastaStash$new( stash = get_merge_5_stash() )
    testthat::expect_that( rs_5, testthat::is_a( "RastaStash" ) )

    rs_NA <- RastaStash$new( stash = get_merge_NA() )
    testthat::expect_that( rs_NA, testthat::is_a( "RastaStash" ) )
    testthat::expect_error( rasta_stash$new( stash = base::list( rs_NA, rs_2, rs_3, rs_4, rs_5 ), "rasters.+different lengths" ) )

    rs_unequal <- RastaStash$new( stash = get_merge_unequal() )
    testthat::expect_that( rs_unequal, testthat::is_a( "RastaStash" ) )
    testthat::expect_error( rasta_stash$new( stash = base::list( rs_unequal, rs_2, rs_3, rs_4, rs_5 ), "Unequal.+SDM methods" ) )

    rs_weird_class <- RastaStash$new( stash = get_weird_stash() )
    testthat::expect_that( rs_weird_class, testthat::is_a( "RastaStash" ) )
    testthat::expect_error( rasta_stash$new( stash = base::list( rs_weird_stash, rs_2, rs_3, rs_4, rs_5 ), "different classes" ) )

    rs_1 <- RastaStash$new( stash = get_merge_1_stash() )
    testthat::expect_that( rs_1, testthat::is_a( "RastaStash" ) )

    rasta_stash <- RastaStash$new( stash = base::list( rs_1, rs_2, rs_3, rs_4, rs_5 ) )
    testthat::expect_equal( rasta_stash$get_rasta_land_count(), 20 )

    rasta_stash <- RastaStash$new()
    for( rs in base::list( rs_1, rs_2, rs_3, rs_4, rs_5 ) ) rasta_stash$add( rs )
    testthat::expect_equal( rasta_stash$get_rasta_land_count(), 20 )
})

test_that( "Vote raster and derived",
{
    no_warning_stash <- get_no_warning_stash_1()

    rasta_stash <- RastaStash$new( stash = no_warning_stash )
    testthat::expect_equal( rasta_stash$get_rasta_land_count(), 8 )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_true( rasta_stash$test_priv_fun( "set_vote_raster" ) )

    rasta_stash <- RastaStash$new( stash = no_warning_stash )
    testthat::expect_that( rasta_stash$test_priv_fun( "set_attribute", base::list( "vote_raster" ) ), testthat::is_a( "RasterLayer" ) )
    
    prob_raster <- rasta_stash$get_prob_raster()
    testthat::expect_that( prob_raster, testthat::is_a( "RasterLayer" ) )
    testthat::expect_equal(get_vec_val( prob_raster ), base::rep( 1, 16 ) )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_base_values' ), base::rep( NA, 16 ) )

    max_votes <- rasta_stash$get_rep_count() * rasta_stash$get_method_count()
    testthat::expect_equal( rasta_stash$test_priv_fun( 'set_idx', base::list( max_votes ) ), c( 1:16 ) )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'set_idx', base::list( 0 ) ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_max_idx' ), c( 1:16 ) )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_zero_idx' ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_idx' ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_mx' ), NULL )

    rasta_stash <- RastaStash$new( stash = get_no_warning_stash_0() )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_max_idx' ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_zero_idx' ), c( 1:16 ) )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_idx' ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_mx' ), NULL )

    rasta_stash <- RastaStash$new( stash = get_max_h_stash_2rep_shuff() )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_max_idx' ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_zero_idx' ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_idx' ), c( 1:16 ) )

    rasta_stash <- RastaStash$new( stash = get_max_h_stash_2rep_NA() )
    testthat::expect_identical( base::length( rasta_stash$get_all() ), rasta_stash$get_rasta_land_count() )
    testthat::expect_identical( rasta_stash$get_rasta_land_count(), rasta_stash$get_method_count() * rasta_stash$get_rep_count() )

    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_max_idx' ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_zero_idx' ), NULL )
    testthat::expect_true( rasta_stash$test_priv_fun( 'get_state_idx' ) %>% base::length(.) > 0 )
    testthat::expect_true( rasta_stash$test_priv_fun( 'get_state_idx' ) %>% base::length(.) < 16 )

    rasta_stash <- RastaStash$new( stash = get_state_mx_stash_1() )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_max_idx' ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_zero_idx' ), NULL )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_idx' ), c( 1:16 ) )

    rep_method_mx <- base::matrix( c( 1, 4, 2, 3, 5, 8, 6, 7 ),
                             nrow = rasta_stash$get_rep_count(),
                             byrow = TRUE )

    base::colnames( rep_method_mx ) <- rasta_stash$get_methods() 
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_rep_method_mx' ), rep_method_mx )

    rasta_state <- rasta_stash$get_rasta_state() 
    testthat::expect_that( rasta_state, testthat::is_a( "RastaState" ) )

    methods_powers <-c( 3, 2, 1, 0 )
    base::names( methods_powers ) <- rasta_stash$get_methods()
    testthat::expect_equal( sapply( rasta_stash$get_methods(), rasta_state$get_method_power ), methods_powers )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_idx' ) %>% base::length(.), 16 )
    bin_state_mx_1 <- base::matrix( c( base::rep( 0,16 ), base::rep( 1,16 ), base::rep( 1,16 ), base::rep( 0,16 ) ), nrow = 4, byrow = TRUE )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_bin_state_mx', base::list( 1, 16 ) ), bin_state_mx_1 )
    bin_state_mx_2 <- base::matrix( c( base::rep( 1,16 ), base::rep( 1,16 ), base::rep( 1,16 ), base::rep( 0,16 ) ), nrow = 4, byrow = TRUE )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_bin_state_mx', base::list( 2, 16 ) ), bin_state_mx_2 )

    state_mx <- base::matrix( c( base::rep( 6, 16 ), base::rep( 14, 16 ) ), nrow = 2, byrow = TRUE )
    testthat::expect_equal( Rfast::colsums( bin_state_mx_1 * 2**methods_powers ), state_mx[ 1, ] )
    testthat::expect_equal( Rfast::colsums( bin_state_mx_2 * 2**methods_powers ), state_mx[ 2, ] )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_mx' ), state_mx )

    rasta_stash <- RastaStash$new( stash = get_state_mx_stash_NA() )
    testthat::expect_that( rasta_stash$test_priv_fun( 'get_rep_method_mx' ), testthat::is_a( "matrix" ) )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_idx' ), c( 1:15 ) )

    bin_state_mx_1 <- base::matrix( c( get_raster_layer_0_50_NA() %>% get_binary_vector(.),
                                 get_raster_layer_1_50_NA() %>% get_binary_vector(.),
                                 get_raster_layer_1_50_NA() %>% get_binary_vector(.),
                                 get_raster_layer_0_50_NA() %>% get_binary_vector(.) ), nrow = 4, byrow = TRUE )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_bin_state_mx', base::list( 1, 15 ) ), bin_state_mx_1 )

    bin_state_mx_2 <- base::matrix( c( get_raster_layer_1_50_NA() %>% get_binary_vector(.),
                                 get_raster_layer_1_50_NA() %>% get_binary_vector(.),
                                 get_raster_layer_1_50_NA() %>% get_binary_vector(.),
                                 get_raster_layer_0_50_NA() %>% get_binary_vector(.) ), nrow = 4, byrow = TRUE)
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_bin_state_mx', base::list( 2, 15 ) ), bin_state_mx_2 )

    state_mx <- base::matrix( c( 6, 6, 9, 9, 6, 6, 9, 9, 6, 6, 9, 9, 6, 6, 9, 14, 14, 1, 1, 14, 14, 1, 1, 14, 14, 1, 1, 14, 14, 1 ), nrow = 2, byrow = TRUE )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_mx' ), state_mx )
    
    rasta_stash <- RastaStash$new( stash = base::list() )
    rasta_stash$add( get_na_votes_rasta_land( get_raster_layer_1_100_NA, 1 ) )
    rasta_stash$add( get_na_votes_rasta_land( get_raster_layer_1_100_NA, 2 ) )
    testthat::expect_identical( get_vec_val( rasta_stash$get_vote_raster() ), get_na_votes() %>%
                                                                                                    raster::getValues(.) )
})

test_that( "Raster entropy - H = 0",
{
    rasta_stash <- RastaStash$new( stash = get_no_warning_stash_1() )
    testthat::expect_that( rasta_stash, testthat::is_a( 'RastaStash' ) )
    testthat::expect_that( rasta_stash$get_rasta_state(), testthat::is_a( 'RastaState' ) )

    rasta_state <- RastaState$new( methods = names( get_method_list() ) ) 
    testthat::expect_that( rasta_state, testthat::is_a( "RastaState" ) )
    power_list <- get_power_list()

    vote_raster <- rasta_stash$get_vote_raster()
    testthat::expect_identical( get_vec_val( vote_raster ), base::rep( 8, 16 ) )
    testthat::expect_equal( rasta_stash$get_method_count() * rasta_stash$get_rep_count(), 8 )
    testthat::expect_equal( 2*rasta_stash$get_method_count(), 8 )
    
    rasta_stash <- RastaStash$new( stash = get_no_warning_stash_1() )
    vote_raster <- rasta_stash$get_vote_raster()
    testthat::expect_identical( get_vec_val( vote_raster ), base::rep( 8, 16 ) )

    entropy_raster <- rasta_stash$get_uncertainty_raster()
    testthat::expect_that( entropy_raster, testthat::is_a( "RasterLayer" ) )

    raster_layer_0_100 <- get_raster_layer_0_100()
    testthat::expect_identical( get_vec_val( entropy_raster ),
                      raster_layer_0_100 %>% raster::getValues(.) )
    
    testthat::expect_equal( rasta_stash$get_gamma_freqs(), get_1_100_gamma_freqs() )
    testthat::expect_identical( rasta_stash$get_diversity(), 0 )

    gini_raster <- rasta_stash$get_gini_raster()
    raster_layer_0_100 <- get_raster_layer_0_100()
    testthat::expect_identical( get_vec_val( gini_raster ),
                      raster_layer_0_100 %>% raster::getValues(.) )
})

test_that( "Raster entropy - max H, 2 rep",
{
    rasta_stash <- RastaStash$new( stash = get_max_h_1_stash() )
    exp_gamma_freqs <- c( 8, 8 )
    base::names( exp_gamma_freqs ) <- c( 7, 8 )
    testthat::expect_equal( rasta_stash$get_gamma_freqs(), exp_gamma_freqs )

    rasta_stash <- RastaStash$new( stash = get_max_h_2_1_stash() )
    exp_gamma_freqs <- c( 8, 8 )
    base::names( exp_gamma_freqs ) <- c( 6, 9 )
    testthat::expect_equal( rasta_stash$get_gamma_freqs(), exp_gamma_freqs )

    rasta_stash <- RastaStash$new( stash = get_max_h_stash_2rep() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_method_count(), 4 ) 
    testthat::expect_equal( rasta_stash$get_rep_count(), 2 ) 
    testthat::expect_equal( rasta_stash$get_methods(), c( 'ann', 'glm', 'rf', 'svm' ) )
    testthat::expect_equal( rasta_stash$get_method_count_list(), base::list( ann = 2, glm = 2 , rf = 2, svm = 2 ) )
    testthat::expect_that( rasta_stash$get_by_index( 1 ), testthat::is_a( 'RastaLand' ) )

    rasta_state <- rasta_stash$get_rasta_state()
    testthat::expect_that( rasta_state, testthat::is_a( "RastaState" ) )
    testthat::expect_true( rasta_stash$get_by_rep( 1 ) %>% base::is.list(.) )
    
    rasta_list <- rasta_stash$get_by_rep( 1 )
    power_list <- get_power_list()
    test_methods <- base::names( power_list )
    testthat::expect_equal( rasta_stash$get_methods(), test_methods )

    i <- base::length( rasta_stash$get_methods() )
    testthat::expect_true( i == 4 )

    max_h_stash <- get_max_h_stash_2rep()
    for( rasta_land in rasta_list )
    {
        testthat::expect_that( rasta_land, testthat::is_a( "RastaLand" ) )
        test_method <- test_methods[ 5 - i ]
        testthat::expect_identical( rasta_land$get_method(), test_method )

        lib_method_power <- rasta_land$get_method() %>% rasta_state$get_method_power(.)
        testthat::expect_equal( lib_method_power, i - 1 )
        testthat::expect_equal( lib_method_power, power_list[[ test_method ]] )

        test_rasta_land <- max_h_stash[[ 5 - i ]]
        testthat::expect_that( test_rasta_land, testthat::is_a( 'RastaLand' ) )

        test_raster_layer <- test_rasta_land$get_raster_obj()
        testthat::expect_that( test_raster_layer, testthat::is_a( 'RasterLayer' ) )

        test_powered_raster <- test_raster_layer * 2**( i - 1 )
        testthat::expect_that( test_powered_raster, testthat::is_a( 'RasterLayer' ) )

        lib_powered_raster <- rasta_land$get_raster_obj() * 2**lib_method_power
        testthat::expect_that( lib_powered_raster, testthat::is_a( 'RasterLayer' ) )
        testthat::expect_equal( lib_powered_raster[ , ], test_powered_raster[ , ] )

        i <- i - 1
    }

    testthat::expect_true( isS4( rasta_stash$get_uncertainty_raster() ) )
    testthat::expect_equal( rasta_stash$get_uncertainty(), 16 * base::log( 2, base = 2 ) )
    testthat::expect_equal( rasta_stash$get_gamma_freqs(), get_max_h_gamma_freqs() )

    testthat::expect_equal( rasta_stash$get_diversity(), 2 )
    testthat::expect_true( rasta_stash$get_prob_raster() %>% get_raster_sum(.) > 0 )
    testthat::expect_true( rasta_stash$get_prob_raster() %>% get_raster_sum(.) < 16 * 100 )
    testthat::expect_true( rasta_stash$get_gini_raster() %>% get_raster_sum(.) > 0 )
})

test_that( "Raster entropy - max H, 2 equal rep",
{
    max_h_stash <- get_max_h_50_stash( get_rasta_land_0_50, get_rasta_land_1_50 )
    rasta_stash <- RastaStash$new( stash = max_h_stash )
    
    testthat::expect_equal( rasta_stash$get_diversity(), 1 )
    testthat::expect_identical(get_vec_val( rasta_stash$get_uncertainty_raster() ), c( base::rep( 1, 16 ) ) )
    testthat::expect_identical( rasta_stash$get_uncertainty(), 16 )
    testthat::expect_identical(get_vec_val( rasta_stash$get_prob_raster() ),
                               get_vec_val( rasta_stash$get_gini_raster() ) )
})

test_that( "Raster entropy - max H, 2 equal rep NA",
{
    max_h_stash <- get_max_h_50_stash( get_rasta_land_0_50_NA, get_rasta_land_1_50_NA )

    rasta_stash <- RastaStash$new( stash = max_h_stash )

    testthat::expect_equal( rasta_stash$get_diversity(), 1 )
    testthat::expect_identical(get_vec_val( rasta_stash$get_uncertainty_raster() ), c( base::rep( 1, 15 ), NA ) )
    testthat::expect_identical( rasta_stash$get_uncertainty(), 15 )
    testthat::expect_identical(get_vec_val( rasta_stash$get_prob_raster() ),
                               get_vec_val( rasta_stash$get_gini_raster() ) )
})

test_that( "Raster entropy - max H, 2 unequal rep",
{
    rasta_stash <- RastaStash$new( stash = get_max_h_stash_2rep() )
    
    testthat::expect_equal( rasta_stash$get_diversity(), 2 )

    testthat::expect_identical(get_vec_val( rasta_stash$get_uncertainty_raster() ), 
                      get_raster_layer_1_100() %>% raster::getValues(.) )

    testthat::expect_identical(get_vec_val( rasta_stash$get_uncertainty_raster() ),
                      get_raster_layer_1_100() %>% raster::getValues(.) )

    testthat::expect_identical( rasta_stash$get_uncertainty(),
                      get_raster_layer_1_100() %>% raster::getValues(.) %>% sum(.) )
    
    testthat::expect_identical(get_vec_val( rasta_stash$get_prob_raster() ),
                      get_max_h_stash_2rep_votes( get_max_h_stash_2rep ) )

    testthat::expect_identical(get_vec_val( rasta_stash$get_gini_raster() ),
                      c( base::rep( 0.46875, 16 ) ) )
})

test_that( "Raster entropy - 2 rep NA",
{
    rasta_stash <- RastaStash$new( stash = get_max_h_stash_2rep_NA() )

    testthat::expect_true( rasta_stash$get_diversity() < 2 )
    testthat::expect_true( rasta_stash$get_diversity() > 1.99 )
    
    testthat::expect_identical( get_vec_val( rasta_stash$get_uncertainty_raster() ),
                      get_raster_layer_1_100_NA() %>% raster::getValues(.) )

    testthat::expect_identical( rasta_stash$get_uncertainty(), 15 )

    testthat::expect_identical( get_vec_val( rasta_stash$get_prob_raster() ),
                      get_max_h_stash_2rep_votes( get_max_h_stash_2rep_NA ) )

    testthat::expect_identical( get_vec_val( rasta_stash$get_gini_raster() ),
                      c( base::rep( 0.46875, 15 ), NA ) )
})

test_that( "Raster entropy - max H, 2 rep shuff",
{
    rasta_stash <- RastaStash$new( stash = get_max_h_stash_2rep_shuff() )
    testthat::expect_equal( rasta_stash$get_uncertainty_raster() %>% get_raster_sum(.), 16 * log( 2, base = 2 ) )
    testthat::expect_equal( rasta_stash$get_diversity(), 2 )
    testthat::expect_true( rasta_stash$get_prob_raster() %>% get_raster_sum(.) > 0 )
    testthat::expect_true( rasta_stash$get_prob_raster() %>% get_raster_sum(.) < 16 * 100 )
    testthat::expect_true( rasta_stash$get_gini_raster() %>% get_raster_sum(.) > 0 )
})

test_that( "Raster entropy - 0 stash",
{
    rasta_stash <- RastaStash$new( stash = get_no_warning_stash_0() )
    testthat::expect_equal( rasta_stash$get_uncertainty(), get_raster_layer_0_100() %>% raster::getValues(.) %>% sum(.)  )
    testthat::expect_equal( rasta_stash$get_diversity(), 0 )
    testthat::expect_equal( get_vec_val( rasta_stash$get_vote_raster() ),
                            get_raster_layer_0_100() %>% raster::getValues(.) )
    testthat::expect_true( rasta_stash$get_gini_raster() %>% get_raster_sum(.) == 0 )
})

test_that( "Raster entropy - inter H",
{
    rasta_stash <- RastaStash$new( stash = get_inter_h_stash_5rep() )
    testthat::expect_true( rasta_stash$get_uncertainty() < 16 * base::log( 5, base = 2 ) )
    testthat::expect_true( rasta_stash$get_diversity() < base::log( 16, base = 2 ) )
    testthat::expect_true( rasta_stash$get_prob_raster() %>% get_raster_sum(.) > 0 )
    testthat::expect_true( rasta_stash$get_prob_raster() %>% get_raster_sum(.) < 16 )
    testthat::expect_true( rasta_stash$get_gini_raster() %>% get_raster_sum(.) > 0 )
    testthat::expect_true( rasta_stash$get_gini_raster() %>% get_raster_sum(.) < 8 )
})

test_that( "Raster entropy - max H single method",
{
    rasta_stash <- RastaStash$new( stash = get_max_h_stash_single_method() )
    testthat::expect_equal( rasta_stash$get_rep_count(), 4 )
    testthat::expect_equal( rasta_stash$get_method_count(), 1 )
    testthat::expect_equal( rasta_stash$get_by_rep( 1 ) %>% base::length(.), 1 )

    rasta_state <- rasta_stash$get_rasta_state()
    testthat::expect_equal( rasta_state$get_method_power( 'ann' ), 0 )
    
    testthat::expect_identical( rasta_stash$get_uncertainty(), 16 )
    testthat::expect_identical( rasta_stash$get_diversity(), 1 )
    testthat::expect_identical( rasta_stash$get_prob_raster() %>% get_raster_sum(.), 8 )
    testthat::expect_identical( rasta_stash$get_gini_raster() %>% get_raster_sum(.), 8 )
})

test_that( "Raster entropy - H = 0, single method",
{
    rasta_stash <- RastaStash$new( stash = get_0_0_h_stash_single_method() )
    testthat::expect_equal( rasta_stash$get_rep_count(), 4 )
    testthat::expect_equal( rasta_stash$get_method_count(), 1 )
    testthat::expect_equal( rasta_stash$get_by_rep( 1 ) %>% base::length(.), 1 )
    
    testthat::expect_identical( rasta_stash$get_uncertainty_raster() %>% get_raster_sum(.), 0 )
    testthat::expect_identical( rasta_stash$get_diversity(), 0 )
    testthat::expect_identical( rasta_stash$get_prob_raster() %>% get_raster_sum(.), 0 )
    testthat::expect_identical( rasta_stash$get_gini_raster() %>% get_raster_sum(.), 0 )

    rasta_stash <- RastaStash$new( stash = get_1_0_h_stash_single_method() )
    testthat::expect_identical( rasta_stash$get_uncertainty(), 0 )
    testthat::expect_identical( rasta_stash$get_diversity(), 0 )
    testthat::expect_identical( rasta_stash$get_vote_raster() %>% get_raster_sum(.), 64 )
    testthat::expect_identical( rasta_stash$get_gini_raster() %>% get_raster_sum(.), 0 )
})

test_that( "Raster entropy - 80/20, single method",
{
    rasta_stash <- RastaStash$new( stash = get_20_80_stash() )
    testthat::expect_equal( rasta_stash$get_rep_count(), 10 )
    testthat::expect_equal( rasta_stash$get_method_count(), 1 )

    exp_h <- ( 0.2*base::log( 0.2, base = 2 ) + 0.8*base::log( 0.8, base = 2 ) ) * ( -1 )
    
    testthat::expect_equal( rasta_stash$get_uncertainty(), 16 * exp_h )
    testthat::expect_equal( rasta_stash$get_diversity(), exp_h )
    testthat::expect_identical( rasta_stash$get_prob_raster() %>% get_raster_sum(.), 0.20 * 16 )

    gini_index <- 1 - ( 0.2**2 + 0.8**2 )
    testthat::expect_equal( rasta_stash$get_gini_raster() %>% get_raster_sum(.), gini_index * 16 )
})

test_that( "Raster weighted votes - 6 reps",
{
    rasta_stash <- RastaStash$new( stash = get_votes_6rep() )
    testthat::expect_that( rasta_stash, testthat::is_a( "RastaStash" ) )
    testthat::expect_equal( rasta_stash$get_method_count(), 4 ) 
    testthat::expect_equal( rasta_stash$get_rep_count(), 6 ) 
    testthat::expect_equal( rasta_stash$get_methods(), c( 'ann', 'glm', 'rf', 'svm' ) )
    testthat::expect_equal( rasta_stash$get_method_count_list(), base::list( ann = 6, glm = 6 , rf = 6, svm = 6 ) )
    testthat::expect_that( rasta_stash$get_by_index( 1 ), testthat::is_a( 'RastaLand' ) )

    for( method in get_power_list() %>% base::names(.) )
    {
        by_method <- rasta_stash$get_by_method( method )
        testthat::expect_true( base::is.list( by_method ) )
        testthat::expect_equal( base::length( by_method ), 6 )
        for( rasta_land in by_method )
        {
            testthat::expect_that( rasta_land, testthat::is_a( 'RastaLand' ) )
        }
    }

    rasta_stash <- RastaStash$new( stash = get_votes_stable() )
    h_stack <- rasta_stash$test_priv_fun( 'get_h_stack' )
    testthat::expect_that( h_stack, testthat::is_a( 'RasterStack' ) )
    testthat::expect_equal( raster::nlayers( h_stack ), 5 )
    testthat::expect_that( h_stack[[ 1 ]], testthat::is_a( 'RasterLayer' ) )
    testthat::expect_equal( get_vec_val( h_stack[[ 1 ]] ), base::rep( 0, 16 ) )
    testthat::expect_equal( get_vec_val( h_stack[[ 5 ]] ), base::rep( 0, 16 ) )

    raster_sum <- raster::calc( h_stack, fun = sum )
    testthat::expect_that( raster_sum, testthat::is_a( "RasterLayer" ) )
    testthat::expect_equal(get_vec_val( raster_sum ), base::rep( 0, 16 ) )

    d_raster <- rasta_stash$test_priv_fun( 'get_d_raster', base::list( h_stack ) )
    testthat::expect_that( d_raster, testthat::is_a( "RasterLayer" ) )
    testthat::expect_equal( get_vec_val( d_raster ), base::rep( 5, 16 ) )

    weight_stack <- rasta_stash$get_weight_stack()
    testthat::expect_that( weight_stack, testthat::is_a( "RasterBrick" ) )

    for( i in 1:raster::nlayers( weight_stack ) )
    {
        testthat::expect_equal(get_vec_val( weight_stack[[ i ]] ), base::rep( 1/5, 16 ) )
    }

    testthat::expect_that( rasta_stash$get_wgt_mrule_raster(), testthat::is_a( 'RasterLayer' ) )
    testthat::expect_equal( get_vec_val( rasta_stash$get_vote_raster() ), base::rep( 18, 16 ) )
    testthat::expect_equal( get_vec_val( rasta_stash$get_wgt_mrule_raster() ), base::rep( 1, 16 ) ) 
    testthat::expect_equal( get_vec_val( rasta_stash$get_prob_raster() ), base::rep( 0.6, 16 ) )
    testthat::expect_equal( get_vec_val( rasta_stash$get_prob_raster() ),
                            get_vec_val( rasta_stash$get_wgt_prob_raster() ) )

    cons_rasta_state <- rasta_stash$test_priv_fun( 'init_unw_rasta_state' )
    testthat::expect_that( cons_rasta_state, testthat::is_a( 'RastaState' ) )
    #testthat::expect_equal( cons_rasta_state$get_weights(), base::rep( 1/5, 5 ) )
    testthat::expect_that( rasta_stash$get_mrule_raster(), testthat::is_a( 'RasterLayer' ) )
    testthat::expect_equal(get_vec_val( rasta_stash$get_mrule_raster() ), base::rep( 1, 16 ) ) 
    testthat::expect_equal( rasta_stash$get_mrule_raster(), rasta_stash$get_wgt_mrule_raster() )

    rasta_stash <- RastaStash$new( stash = get_votes_stable() )
    testthat::expect_that( rasta_stash$get_efficiency_raster(), testthat::is_a( 'RasterLayer' ) )
    testthat::expect_equal( get_vec_val( rasta_stash$get_efficiency_raster() ) %>% base::round( ., 4 ), base::rep( 1, 16 ) )
    testthat::expect_equal( rasta_stash$get_efficiency() %>% base::round( ., 4 ), 1 )
    
    rasta_stash <- RastaStash$new( stash = get_rf_svm_unstable_6() )
    h_stack <- rasta_stash$test_priv_fun( 'get_h_stack' )

    for( i in 1:3 )
    {
        testthat::expect_equal( get_vec_val( h_stack[[ i ]] ) %>% base::sum(.), 0 )
    }
    for( i in 4:5 )
    {
        testthat::expect_equal( get_vec_val( h_stack[[ i ]] ) %>% base::sum(.), 16 )
    }

    d_raster <- rasta_stash$test_priv_fun( 'get_d_raster', base::list( h_stack ) )
    testthat::expect_equal( get_vec_val( d_raster ), base::rep( 3, 16 ) )

    weight_stack <- rasta_stash$get_weight_stack()

    for( i in 1:3 )
    {
        testthat::expect_equal( get_vec_val( weight_stack[[ i ]] ), base::rep( 1/3, 16 ) )
    }
    for( i in 4:5 )
    {
        testthat::expect_equal( get_vec_val( weight_stack[[ i ]] ), base::rep( 0, 16 ) )
    }

    state_mx <- base::matrix( c( base::rep( 5, 16 ), base::rep( 6, 16 ), base::rep( 5, 16 ), base::rep( 6, 16 ),
                           base::rep( 5, 16 ), base::rep( 6, 16 ) ), nrow = 6, byrow = TRUE )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_mx' ), state_mx )

    weight_mx <- base::matrix( c( base::rep( 1/3, 48 ), base::rep( 0, 32 ) ), nrow = 5, byrow = TRUE )
    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_weight_mx' ), weight_mx )
    testthat::expect_equal( get_vec_val( rasta_stash$get_wgt_mrule_raster() ), base::rep( 0, 16 ) )
    testthat::expect_equal( get_vec_val( rasta_stash$get_mrule_raster() ), base::rep( 0, 16 ) )
    testthat::expect_equal( get_vec_val( rasta_stash$get_wgt_prob_raster() ), base::rep( 1/3, 16 ) )
    testthat::expect_equal( get_vec_val( rasta_stash$get_prob_raster() ), base::rep( 0.4, 16 ) )

    weight_mx <- weight_mx + 1e-07
    efficiency_values <- base::apply( weight_mx * base::log2( weight_mx ) * ( -1 ), 2, sum ) / base::log2( 5 )
    efficiency_raster <- rasta_stash$get_efficiency_raster()
    testthat::expect_that( efficiency_raster, testthat::is_a( "RasterLayer" ) )
    testthat::expect_equal(get_vec_val( efficiency_raster ), efficiency_values )
    testthat::expect_true( rasta_stash$get_efficiency() < 16 )
})

test_that( "Raster weighted votes - 7 reps",
{
    rasta_stash <- RastaStash$new( stash = get_rf_svm_unstable_7() )
    state_mx <- base::matrix( c( base::rep( 5, 16 ), base::rep( 6, 16 ), base::rep( 5, 16 ), base::rep( 6, 16 ),
                           base::rep( 5, 16 ), base::rep( 6, 16 ), base::rep( 7, 16) ), nrow = 7, byrow = TRUE )

    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_mx' ), state_mx )
    testthat::expect_equal( get_vec_val( rasta_stash$get_wgt_mrule_raster() ), base::rep( 0, 16 ) )
    testthat::expect_equal( get_vec_val( rasta_stash$get_mrule_raster() ), base::rep( 1/7, 16 ) )

    diff_raster <- rasta_stash$get_mrule_raster() - rasta_stash$get_wgt_mrule_raster()
    testthat::expect_true( get_vec_val( diff_raster ) %>% base::sum(.) > 0 )

    rasta_stash <- RastaStash$new( stash = get_rf_svm_unstable_7_NA() )
    state_mx <- base::matrix( c( base::rep( 5, 15 ), base::rep( 6, 15 ), base::rep( 5, 15 ), base::rep( 6, 15 ),
                           base::rep( 5, 15 ), base::rep( 6, 15 ), base::rep( 7, 15) ), nrow = 7, byrow = TRUE )

    testthat::expect_equal( rasta_stash$test_priv_fun( 'get_state_mx' ), state_mx )
    testthat::expect_equal( get_vec_val( rasta_stash$get_wgt_mrule_raster() ), c( base::rep( 0, 15 ), NA ) )
    testthat::expect_equal( get_vec_val( rasta_stash$get_mrule_raster() ), c( base::rep( 1/7, 15 ), NA ) )

    rasta_stash <- RastaStash$new( stash = get_rf_svm_unstable_7_large() )
    diff_raster <- rasta_stash$get_mrule_raster() - rasta_stash$get_wgt_mrule_raster()
    testthat::expect_true( get_vec_val( diff_raster ) %>% stats::na.omit(.) %>% base::sum(.) > 0 )

    rasta_stash <- RastaStash$new( stash = get_random_7_large() )
    testthat::expect_true( get_vec_val( rasta_stash$get_mrule_raster() ) %>% stats::na.omit(.) %>% base::sum(.) >
                get_vec_val( rasta_stash$get_wgt_mrule_raster() ) %>% stats::na.omit(.) %>% base::sum(.) )
})

test_that( "BS uncertainty - 7 reps",
{
    rasta_stash <- RastaStash$new()
    testthat::expect_error( rasta_stash$bootstrap_uncertainty(), "no 'RastaLand'" )
    
    rasta_stash <- RastaStash$new( stash = get_rf_svm_unstable_7() )
    testthat::expect_error( rasta_stash$bootstrap_uncertainty( bs_reps = 0.5 ), "bs_reps.+double" )
    testthat::expect_error( rasta_stash$bootstrap_uncertainty( bs_reps = -1 ), "smaller than 1" )
    testthat::expect_error( rasta_stash$bootstrap_uncertainty( bs_reps = "a" ), "bs_reps.+integer" )
    testthat::expect_error( rasta_stash$bootstrap_uncertainty( bs_matrix = base::list() ), "bs_matrix.+matrix" )

    uncertainty <- rasta_stash$get_uncertainty()
    bs_list <- rasta_stash$bootstrap_uncertainty()

    bs_uncertainty_1 <- bs_list$bs_uncertainty

    testthat::expect_true( base::is.numeric( bs_uncertainty_1 ) )
    testthat::expect_equal( base::length( bs_uncertainty_1 ), 7 )
    testthat::expect_false( base::any( bs_uncertainty_1 == 0 ) )
    testthat::expect_true( base::mean( bs_uncertainty_1 ) > 0 )
    testthat::expect_true( base::mean( bs_uncertainty_1 ) != uncertainty )

    bs_matrix <- bs_list$bs_matrix

    testthat::expect_true( base::is.matrix( bs_matrix ) )
    testthat::expect_equal( base::nrow( bs_matrix ), 7 )
    testthat::expect_equal( base::ncol( bs_matrix ), 7 )

    bs_list <- rasta_stash$bootstrap_uncertainty()
    bs_uncertainty_2 <- bs_list$bs_uncertainty

    testthat::expect_true( base::mean( bs_uncertainty_1 ) != base::mean( bs_uncertainty_2 ) )
    testthat::expect_false( base::any( bs_uncertainty_2 == 0 ) )

    bs_list <- rasta_stash$bootstrap_uncertainty( bs_matrix = bs_matrix )
    bs_uncertainty_3 <- bs_list$bs_uncertainty

    testthat::expect_true( base::mean( bs_uncertainty_1 ) == base::mean( bs_uncertainty_3 ) )

    bs_list <- rasta_stash$bootstrap_uncertainty( bs_reps = 10 )
    bs_uncertainty_4 <- bs_list$bs_uncertainty

    testthat::expect_equal( base::length( bs_uncertainty_4 ), 10 )
    testthat::expect_true( base::mean( bs_uncertainty_4 ) != base::mean( bs_uncertainty_1 ) )
})
