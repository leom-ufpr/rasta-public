testthat::test_that( "Class interface",
{
    source( "helper_rasta_land_lib.R" )
    testthat::expect_true( isnot_alnum( raster::raster() ) )
    testthat::expect_true( isnot_alnum( c( 'a', 'b' ) ) )
    testthat::expect_true( isnot_alnum( base::list( a = 1, b = 2 ) ) )
    testthat::expect_true( isnot_alnum( NA ) )
    testthat::expect_true( isnot_alnum( NULL ) )
    testthat::expect_true( isnot_alnum( '' ) )
    testthat::expect_true( isnot_alnum( "" ) )
    testthat::expect_true( isnot_alnum( base::character() ) )
    
    testthat::expect_false( isnot_alnum( c( 0,1 ) ) )
    testthat::expect_false( isnot_alnum( base::numeric() ) )
    testthat::expect_false( isnot_alnum( 1 ) )
    testthat::expect_false( is_integer( 'a' ) )
    testthat::expect_false( is_integer( '1' ) )
    
    testthat::expect_match( get_usage_message(), "RastaLand" )
    testthat::expect_match( get_attrb_type_message( 'method', 'character', 'numeric' ),
                 "Wrong 'method'.*character expected numeric instead.*RastaLand" )
    
    testthat::expect_error( sanitize_arg( raster::raster(), 'b', 'character' ), "is.string" )
    testthat::expect_error( sanitize_arg( 0, 'b', 'character' ), "is.string" )
    testthat::expect_error( sanitize_arg( 1, 2, 'character' ), "is.string" )
    testthat::expect_error( sanitize_arg( 'a', 'b', NA ), "is.string" )
    testthat::expect_error( sanitize_arg( 'a', 'b', '' ), "type" )
    testthat::expect_error( sanitize_arg( 'a', 'b' ), "no default" )

    testthat::expect_error( sanitize_arg( 'a', raster::raster(), 'character' ), "Wrong 'a'" )
    testthat::expect_error( sanitize_arg( 'a', NA, 'character' ), "Wrong 'a'" )
    testthat::expect_error( sanitize_arg( 'a', NULL, 'character' ), "Wrong 'a'" )

    testthat::expect_error( sanitize_arg( 'a', raster::raster(), 'numeric' ), "Wrong 'a'" )
    testthat::expect_error( sanitize_arg( 'a', NA , 'numeric' ), "Wrong 'a'" )
    testthat::expect_error( sanitize_arg( 'a', NULL, 'numeric' ), "Wrong 'a'" )
    testthat::expect_error( sanitize_arg( 'a', 'b', 'numeric' ), "Wrong 'a'" )
    
    testthat::expect_error( sanitize_arg( 'a', '', 'character' ), "no argument" )
    testthat::expect_error( sanitize_arg( 'a', '', 'numeric' ), "no argument" )
    testthat::expect_error( sanitize_arg( 'a', base::character(), 'character' ), "Wrong 'a'" )
    testthat::expect_error( sanitize_arg( 'a', base::numeric(), 'numeric' ), "no argument" )

    testthat::expect_true( sanitize_arg( 'a', 'b', 'character' ) )
    testthat::expect_true( sanitize_arg( 'a', 1, 'numeric' ) )
} )

testthat::test_that( "Object creation and basic attributes",
{
    testthat::expect_error( RastaLand$new( method = 'ann',
                            rep = 1,
                            rep_count = 100,
                            raster_obj = get_raster_layer_16_max() ),
                            "Wrong 'class'.*no argument" )

    testthat::expect_error( RastaLand$new( class = 'S. siderea',
                            rep = 1,
                            rep_count = 100,
                            raster_obj = get_raster_layer_16_max() ),
                            "Wrong 'method'.*no argument" )

    testthat::expect_error( RastaLand$new( class = 'S. siderea',
                            method = 'ann',
                            rep_count = 100,
                            raster_obj = get_raster_layer_16_max() ),
                            "Wrong 'rep'.*no argument" )

    testthat::expect_error( RastaLand$new( class = 'S. siderea',
                            method = 'ann',
                            rep = 1,
                            raster_obj = get_raster_layer_16_max() ),
                            "Wrong 'rep_count'.*no argument" )

    testthat::expect_error( RastaLand$new( class = 'S. siderea',
                            method = 'ann',
                            rep = 1,
                            rep_count = 100 ),
                            "Wrong 'raster_obj'.*no argument" )

    testthat::expect_error( RastaLand$new( class = 'S. siderea',
                            method = 'ann',
                            rep = 'a',
                            rep_count = 100,
                            raster_obj = get_raster_layer_16_max() ),
                            "Wrong 'rep'.*character" )

    testthat::expect_error( RastaLand$new( class = 'S. siderea',
                            method = 'ann',
                            rep = 1,
                            rep_count = 'a',
                            raster_obj = get_raster_layer_16_max() ),
                            "Wrong 'rep_count'.*character" )

    testthat::expect_error( RastaLand$new( class = 'S. siderea',
                            method = 'ann',
                            rep = 1,
                            rep_count = 100,
                            raster_obj = raster::brick() ),
                            "Wrong 'raster_obj'.+RasterBrick" )

    rasta_land <- RastaLand$new( class = 'S. siderea',
                                 method = 'ann',
                                 rep = 1,
                                 rep_count = 100,
                                 raster_obj = get_raster_layer_16_max() )
    
    testthat::expect_that( rasta_land, testthat::is_a( 'RastaLand' ) )
    testthat::expect_identical( rasta_land$get_class(), 'S. siderea' )
    testthat::expect_identical( rasta_land$get_method(), 'ann' )
    testthat::expect_identical( rasta_land$get_rep(), 1 )
    testthat::expect_identical( rasta_land$get_rep_count(), 100 )
    testthat::expect_equal( rasta_land$get_raster_obj(), get_raster_layer_16_max() )

    f <- get_raster_layer_16_max() %>% raster::freq(.)
    comp_vec <- f[ , 2 ]
    names( comp_vec ) <- f[ , 1 ]
    testthat::expect_equal( rasta_land$get_state_freqs(), comp_vec )
    testthat::expect_equal( rasta_land$get_states(), c( 1:16 ) )

    layer_keys <- base::list()
    for ( i in c( 1:16 ) )
    {
        layer_keys[[ base::paste( 'layer', i, sep = '.' ) ]] = i
    }

    rasta_land <- get_rasta_land_1_50( 1, 'ann', 1 )
    testthat::expect_that( rasta_land, testthat::is_a( 'RastaLand' ) )

    comp_vec <- c( 8, 8 )
    base::names( comp_vec ) <- c( 0, 1 )
    testthat::expect_equal( rasta_land$get_state_freqs(), comp_vec )
    testthat::expect_equal( rasta_land$get_states(), c( 0, 1 ) )
} )

testthat::test_that( "Alpha entropy",
{
    rasta_land <- get_rasta_land( get_raster_layer_16_max )
    testthat::expect_equal( rasta_land$get_entropy(), base::log2( 16 ) )

    rasta_land <- get_rasta_land( get_raster_layer_1_100 )
    testthat::expect_equal( rasta_land$get_entropy(), 0 )

    rasta_land <- get_rasta_land( get_raster_layer_0_100 )
    testthat::expect_equal( rasta_land$get_entropy(), 0 )

    rasta_land <- get_rasta_land( get_raster_layer_0_50 )
    testthat::expect_equal( rasta_land$get_entropy(), 1 )

    rasta_land <- get_rasta_land( get_raster_layer_1_50_inter )
    testthat::expect_equal( rasta_land$get_entropy(), 1 )

    rasta_land <- get_rasta_land( get_raster_layer_8_max )
    testthat::expect_equal( rasta_land$get_entropy(), base::log2( 8 ) )

    rasta_land <- get_rasta_land( get_raster_layer_1_100_NA )
    testthat::expect_lt( rasta_land$get_entropy(), base::log2( 16 ) )

    rasta_land <- get_rasta_land( get_raster_layer_0_100_NA )
    testthat::expect_lt( rasta_land$get_entropy(), base::log2( 16 ) )

    rasta_land <- get_rasta_land( get_raster_layer_mid_entropy )
    testthat::expect_lt( rasta_land$get_entropy(), base::log2( 16 ) )
} )
