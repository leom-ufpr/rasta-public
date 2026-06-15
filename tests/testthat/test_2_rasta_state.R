test_aux_subs <- function( f, states_vec, names_vec, value )
{
    base::stopifnot( base::typeof( f ) == 'closure', base::is.vector( value ) )

    rasta_state <- get_rasta_state( states_vec, names_vec )
    testthat::expect_identical( f( rasta_state ), value )

    return( TRUE )
}

compare_list <- function( ltest, lpack )
{
    base::stopifnot( base::is.list( ltest ), base::is.list( lpack ) )

    for( key in ltest %>% base::names(.) )
    {
        if( ! base::identical( ltest[[ key ]], lpack[[ key ]] ) )
            { warning( key ) }

        testthat::expect_true( base::identical( ltest[[ key ]], lpack[[ key ]] ) )
    }

    return( TRUE )
}

test_method_power <- function( f, rasta_state )
{
    base::stopifnot( base::typeof( f ) == 'closure' )

    testthat::expect_that( rasta_state, testthat::is_a( "RastaState" ) )

    method_power <- base::length( f() ) - 1

    for( method in base::sort( f() ) )
    { 
        testthat::expect_equal( rasta_state$get_method_power( method ), method_power )
        method_power <- method_power - 1
    }

    return( TRUE )
}

testthat::test_that( "Auxiliary subs",
{
    source( "helper_rasta_state_lib.R" )
    rasta_state_000 <- get_rasta_state( get_rasta_state_000(), get_methods_1() )
    testthat::expect_identical( rasta_state_000, initialize_state_vec( get_methods_1() ) )

    test_aux_subs( get_dec_state, get_rasta_state_000(), get_methods_1(), 0 )
    test_aux_subs( get_dec_state, get_rasta_state_111(), get_methods_1(), 7 )
    test_aux_subs( get_dec_state, get_rasta_state_010(), get_methods_1(), 2 )
    test_aux_subs( get_dec_state, get_rasta_state_101(), get_methods_1(), 5 )

    test_aux_subs( get_consensus, get_rasta_state_000(), get_methods_1(), 0 )
    test_aux_subs( get_consensus, get_rasta_state_111(), get_methods_1(), 1 )
    test_aux_subs( get_consensus, get_rasta_state_010(), get_methods_1(), 0 )
    test_aux_subs( get_consensus, get_rasta_state_101(), get_methods_1(), 1 )

    test_aux_subs( get_consensus_type, get_rasta_state_000(), get_methods_1(), '3x0' )
    test_aux_subs( get_consensus_type, get_rasta_state_111(), get_methods_1(), '3x0' )
    test_aux_subs( get_consensus_type, get_rasta_state_010(), get_methods_1(), '2x1' )
    test_aux_subs( get_consensus_type, get_rasta_state_101(), get_methods_1(), '2x1' )
    test_aux_subs( get_consensus_type, get_rasta_state_110(), get_methods_1(), '2x1' )
    test_aux_subs( get_consensus_type, get_rasta_state_011(), get_methods_1(), '2x1' )
    
    test_aux_subs( get_agreement, get_rasta_state_000(), get_methods_1(),
                   get_agreement_111() )
    test_aux_subs( get_agreement, get_rasta_state_111(), get_methods_1(),
                   get_agreement_111() )
    test_aux_subs( get_agreement, get_rasta_state_010(), get_methods_1(),
                   get_agreement_101() )
    test_aux_subs( get_agreement, get_rasta_state_101(), get_methods_1(),
                   get_agreement_101() )
   
    testthat::expect_false( test_bin_state( raster::raster() ) )
    testthat::expect_false( test_bin_state( 0 ) )
    testthat::expect_false( test_bin_state( c( 0, 'a' ) ) )
    testthat::expect_false( test_bin_state( c() ) )
    testthat::expect_false( test_bin_state( NA ) )
    testthat::expect_false( test_bin_state( NULL ) )
    testthat::expect_false( test_bin_state( base::list() ) )
    testthat::expect_false( test_bin_state( base::list( a = 1, b = 2 ) ) )
    testthat::expect_false( test_bin_state( base::list( a = 'c', b = 'd' ) ) )
    testthat::expect_false( test_bin_state( base::character() ) )
    testthat::expect_false( test_bin_state( base::numeric() ) )
    testthat::expect_false( test_bin_state( 'bino001' ) )

    testthat::expect_true( test_bin_state( c( 'bin001') ) )
    testthat::expect_true( test_bin_state( 'bin001' ) )
                 
    testthat::expect_identical( get_rasta_state_000() %>%
                                get_attrbs( ., get_methods_1() ), get_attrbs_000() )
    testthat::expect_identical( get_rasta_state_111() %>%
                                get_attrbs( ., get_methods_1() ), get_attrbs_111() )
    testthat::expect_identical( get_rasta_state_101() %>%
                                get_attrbs( ., get_methods_1() ), get_attrbs_101() )
    testthat::expect_identical( get_rasta_state_010() %>%
                                get_attrbs( ., get_methods_1() ), get_attrbs_010() )

    compare_list( get_test_state_list(), get_state_list( get_methods_1() ) )
} )

testthat::test_that( "Class interface",
{
    testthat::expect_error( RastaState$new( methods = raster::raster() ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = 0 ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = c( 0, 'a' ) ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = c() ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = NA ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = NULL ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = base::list() ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = base::list( a = 1, b = 2 ) ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = base::list( a = 'c', b = 'd' ) ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = base::character() ), "Constructor" )
    testthat::expect_error( RastaState$new( methods = base::numeric() ), "Constructor" )
    testthat::expect_that( RastaState$new( methods = 'a' ), is_a( "RastaState" ) )    
    testthat::expect_that( RastaState$new( methods = c( 'ann', 'rf' ) ), is_a( "RastaState" ) )    
})

testthat::test_that( "Class methods",
{
    rasta_state <- get_methods_1() %>%
        RastaState$new( methods = . )
    testthat::expect_error( sanitize_dec_index( c( 0, 'a' ) ), "dec_index" )
    testthat::expect_error( sanitize_dec_index( c() ), "dec_index" )
    testthat::expect_error( sanitize_dec_index( NA ), "dec_index" )
    testthat::expect_error( sanitize_dec_index( NULL ), "dec_index" )
    testthat::expect_error( sanitize_dec_index( base::list() ), "dec_index" )
    testthat::expect_error( sanitize_dec_index( base::list( a = 1, b = 2 ) ), "dec_index" )
    testthat::expect_error( sanitize_dec_index( base::list( a = 'c', b = 'd' ) ), "dec_index" )
    testthat::expect_error( sanitize_dec_index( base::character() ), "dec_index" )
    testthat::expect_error( sanitize_dec_index( base::numeric() ), "dec_index" )
    
    test_method_power( get_methods_1, rasta_state )
    testthat::expect_error( rasta_state$get_method_agreement( 5, 'cnn' ), "Method 'cnn'" )
    testthat::expect_error( rasta_state$get_dec_state( 'bin10101' ), "No 'dec_state'" )
    testthat::expect_identical( rasta_state$get_method_agreement( 5, 'ann' ), 1 )
    testthat::expect_identical( rasta_state$get_method_agreement( 5, 'rf' ), 0 )
    testthat::expect_identical( rasta_state$get_method_agreement( 5, 'svm' ), 1 )
    testthat::expect_identical( rasta_state$get_consensus( 6 ), 1 )
    testthat::expect_identical( rasta_state$get_consensus_type( 3 ), '2x1' )
    testthat::expect_identical( rasta_state$get_votes( 3 ), 2 )
    testthat::expect_identical( rasta_state$get_dec_state( 'bin101' ), 5 )
    testthat::expect_identical( rasta_state$get_bin_vec( 5 ), get_rasta_state_101() )
    testthat::expect_identical( rasta_state$get_dec_id_method_list( 5 ), c( "ann", "svm" ) )
    testthat::expect_identical( rasta_state$get_dec_id_method_count( 5 ), 2L )

    rasta_state <- get_methods_2() %>% RastaState$new( methods = . )
    test_method_power( get_methods_2, rasta_state )
    testthat::expect_identical( rasta_state$get_consensus( 21 ), 1 )
    testthat::expect_identical( rasta_state$get_consensus_type( 25 ), '3x2' )
    testthat::expect_identical( rasta_state$get_consensus_type( 27 ), '4x1' )
    testthat::expect_identical( rasta_state$get_votes( 10 ), 2 )
    testthat::expect_identical( rasta_state$get_dec_state( 'bin00111' ), 7 )
    testthat::expect_identical( rasta_state$get_method_agreement( 12, 'rf' ), 1 )
    testthat::expect_identical( rasta_state$get_method_agreement( 11, 'maxent' ), 0 )
})

testthat::test_that( "Weights",
{
    rasta_state <- get_methods_2() %>% RastaState$new( methods = . )
    testthat::expect_that( rasta_state, is_a( 'RastaState' ) )
    testthat::expect_error( rasta_state$get_method_weights(), 'Weights' )
    testthat::expect_error( rasta_state$add_weights( base::list( a = 1, b = 2 ) ), 'Expected' )
    testthat::expect_error( rasta_state$add_weights( 'a' ), 'Expected' )
    testthat::expect_error( rasta_state$add_weights( c( 1, 2, 3, 4, 'a' ) ), 'Expected' )
    testthat::expect_error( rasta_state$add_weights( c( 1, 2, 3 ) ), 'lengths' )
    testthat::expect_true( rasta_state$add_weights( c( 1, 2, 3, 4, 5 ) ) )
    testthat::expect_true( rasta_state$add_weights( c( 1 ) ) )
    testthat::expect_true( rasta_state$add_weights( 1 ) )

    testthat::expect_true( rasta_state$add_weights( 1/5 ) )
    testthat::expect_identical( rasta_state$get_method_weights(), 1/5 )
    testthat::expect_identical( rasta_state$get_wgt_prod( 31 ) %>% base::as.numeric(), base::rep( 0.2, 5 ) )
    testthat::expect_identical( rasta_state$get_wgt_votes( 31, "mrule" ), 1 )
    testthat::expect_identical( rasta_state$get_wgt_prod( 0 ) %>% base::as.numeric(), base::rep( 0, 5 ) )
    testthat::expect_identical( rasta_state$get_wgt_votes( 0, "mrule" ), 0 )
    testthat::expect_identical( rasta_state$get_wgt_prod( 10 ) %>% base::as.numeric(), c( 0, 0.2, 0, 0.2, 0 ) )
    testthat::expect_identical( rasta_state$get_wgt_votes( 10, "mrule" ), 0 )
    testthat::expect_identical( rasta_state$get_wgt_prod( 11 ) %>% base::as.numeric(), c( 0, 0.2, 0, 0.2, 0.2 ) )
    testthat::expect_identical( rasta_state$get_wgt_votes( 11, "mrule" ), 1 )

    testthat::expect_true( rasta_state$add_weights( c( 0.25, 0.25, 0.25, 0.125, 0.125 ) ) )
    testthat::expect_identical( rasta_state$get_method_weights(), c( 0.25, 0.25, 0.25, 0.125, 0.125 ) )

    testthat::expect_identical( rasta_state$get_wgt_prod( 3 ) %>% base::as.numeric(), c( 0, 0, 0, 0.125, 0.125 ) )
    testthat::expect_identical( rasta_state$get_wgt_votes( 3, "sum" ), 0.25 )

    testthat::expect_true( rasta_state$add_weights( c( 0.25, 0.25, 0.25, 0.125, 0.125 ) ) )
    testthat::expect_identical( rasta_state$get_wgt_votes( 3, "mrule" ), 0 )

    testthat::expect_identical( rasta_state$get_wgt_votes( 28, "mrule" ), 1 )
    testthat::expect_identical( rasta_state$get_wgt_votes( 17, "mrule" ), 0 )
    testthat::expect_identical( rasta_state$get_wgt_votes( 14, "mrule" ), 1 )

    testthat::expect_true( rasta_state$add_weights( c( 0.125, 0.125, 0.25, 0.25, 0.25 ) ) )
    testthat::expect_identical( rasta_state$get_method_weights(), c( 0.125, 0.125, 0.25, 0.25, 0.25 ) )
    testthat::expect_identical( rasta_state$get_wgt_votes( 3, "mrule" ), 0 )
    testthat::expect_identical( rasta_state$get_wgt_votes( 28, "mrule" ), 0 )
    testthat::expect_identical( rasta_state$get_wgt_votes( 17, "mrule" ), 0 )
    testthat::expect_identical( rasta_state$get_wgt_votes( 14, "mrule" ), 1 )
})
