get_vec_val <- function( raster_layer )
{
    base::stopifnot( test_class( raster_layer, 'RasterLayer' ) )

    return( raster_layer %>% raster::getValues( . ) %>% base::as.vector( . ) ) 
}

get_method_list <- function()
{
    base::list( ann = 1, rf = 1, svm = 1, glm = 1 ) %>% return(.) 
}

get_power_list <- function()
{
    base::list( ann = 3, glm = 2, rf = 1, svm = 0 ) %>% return(.) 
}

get_raster_sum <- function( raster_layer )
{
    base::stopifnot( test_class( raster_layer, 'RasterLayer' ) )

    get_vec_val( raster_layer ) %>% base::sum(.) %>% return(.)
}

get_binary_vector <- function( raster_layer )
{
    raster_layer %>%
        raster::getValues(.) %>%
        stats::na.omit(.) %>%
        base::as.numeric(.) %>%
        return(.)
}

get_ensemble_rasta_land_1_100 <- function( rep, method, method_power )
{
    base::stopifnot( is_integer( rep ),
        base::is.character( method ) )

    get_rasta_land( method_power %>%
        get_power_raster_layer_1_100(.),
        my_rep = rep,
        my_method = method,
        my_rep_count = 2 ) %>% return(.)
}

get_max_h_1_sum <- function()
{
    get_raster_layer_0_50()*2**3 +
    get_raster_layer_1_50()*2**2 +
    get_raster_layer_1_50()*2**1 +
    get_raster_layer_1_50()*2**0 %>% return(.)
}

get_max_h_2_sum <- function()
{
    get_raster_layer_1_50()*2**3 +
    get_raster_layer_0_50()*2**2 +
    get_raster_layer_0_50()*2**1 +
    get_raster_layer_1_50()*2**0 %>% return(.)
}

get_rep_stash <- function( f, rep, method_list )
{
    base::stopifnot( typeof( f ) == 'closure',
        is_integer( rep ),
        base::is.list( method_list ) )
    
    rep_stash <- base::list()
    
    for ( method in base::names( method_list ) )
    {
        rep_stash <- c( rep_stash, f( rep, method ) )
    }

    return( rep_stash )
}

get_by_rep_stash <- function( rep, method_list )
{
    base::stopifnot( is_integer( rep ),
        base::is.list( method_list ) )

    base::lapply( base::names( method_list ), function( method )
        { get_rasta_land_1_100( rep, method ) } ) %>%
        return(.)
}

get_na_votes <- function()
{
    c( base::rep( 2, 15 ), NA ) %>%
        get_raster_layer(.) %>%
        return(.)
}

get_na_votes_rasta_land <- function( f, na_rep )
{
    base::stopifnot( typeof( f ) == 'closure',
        is_integer( na_rep ) )

    get_rasta_land( f, my_rep = na_rep,
        my_method = 'na_votes',
        my_rep_count = 2 ) %>%
        return(.)
}

get_no_warning_stash_1 <- function()
{
    c( get_method_list() %>%
       get_rep_stash( get_rasta_land_1_100, 1, . ),
       get_method_list() %>%
       get_rep_stash( get_rasta_land_1_100, 2, . ) ) %>%
       return(.)
}

get_no_warning_stash_0 <- function()
{
    c( get_method_list() %>%
       get_rep_stash( get_rasta_land_0_100, 1, . ),
       get_method_list() %>%
       get_rep_stash( get_rasta_land_0_100, 2, . ) ) %>%
       return(.)
}

get_warning_stash <- function()
{
    c( base::list( ann = 1, rf = 1, svm = 1, glm = 1 ) %>%
           get_rep_stash( get_rasta_land_1_100, 1, . ),
       base::list( glm = 1, glm = 1, svm = 1, glm = 1 ) %>%
           get_rep_stash( get_rasta_land_1_100, 2, . ) ) %>%
       return(.)
}

get_unbalanced_stash <- function()
{
    base::list( get_rasta_land_0_50_NA( 1, 'ann', 2 ),
         get_rasta_land_1_50_NA( 1, 'rf', 2 ),
         get_rasta_land_1_50_NA( 1, 'glm', 2 ),
         get_rasta_land_1_50_NA( 2, 'ann', 2 ),
         get_rasta_land_1_50_NA( 2, 'rf', 2 ),
         get_rasta_land_0_50_NA( 2, 'svm', 2 ),
         get_rasta_land_1_50_NA( 2, 'glm', 2 ) ) %>% return(.)
}

get_single_rep_stash <- function()
{
    base::list( get_rasta_land_0_50_NA( 1, 'ann', 2 ),
          get_rasta_land_1_50_NA( 1, 'rf', 2 ),
          get_rasta_land_0_50_NA( 1, 'svm', 2 ),
          get_rasta_land_1_50_NA( 1, 'glm', 2 ),
          get_rasta_land_1_50_NA( 1, 'ann', 2 ),
          get_rasta_land_1_50_NA( 1, 'rf', 2 ),
          get_rasta_land_0_50_NA( 1, 'svm', 2 ),
          get_rasta_land_1_50_NA( 1, 'glm', 2 ) ) %>% return(.)
}

get_no_warning_votes <- function()
{
    get_raster_layer( c( rep( 8, 16 ) ) ) %>% return(.)
}

get_no_warning_perc_votes <- function()
{
    get_raster_layer( c( rep( 100, 16 ) ) ) %>% return(.)
}

get_max_h_50_stash <- function( f1, f2 )
{
    base::stopifnot( base::typeof( f1 ) == 'closure',
        base::typeof( f2 ) == 'closure' )
    
    max_h_stash <- base::list()
    group_count <- 0
    
    for ( method in base::names( get_method_list() ) )
    {
        if( group_count %% 2 == 0 )
        {            
            max_h_stash <- c( max_h_stash, f1( 1, method ) )
            max_h_stash <- c( max_h_stash, f2( 2, method ) )
        }
        else
        {
            max_h_stash <- c( max_h_stash, f2( 1, method ) )
            max_h_stash <- c( max_h_stash, f1( 2, method ) )
        }
        group_count <- group_count + 1
    }

    return( max_h_stash )
}

get_1_100_gamma_freqs <- function()
{
    gamma_freqs <- c( 32 )
    base::names( gamma_freqs ) <- c( 15 )

    return( gamma_freqs )
}
get_state_mx_stash_1 <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 2 ),
          get_rasta_land_1_100( 1, 'rf', 2 ),
          get_rasta_land_0_100( 1, 'svm', 2 ),
          get_rasta_land_1_100( 1, 'glm', 2 ),
          get_rasta_land_1_100( 2, 'ann', 2 ),
          get_rasta_land_1_100( 2, 'rf', 2 ),
          get_rasta_land_0_100( 2, 'svm', 2 ),
          get_rasta_land_1_100( 2, 'glm', 2 ) ) %>% return(.)
}

get_state_mx_stash_NA <- function()
{
    base::list( get_rasta_land_0_50_NA( 1, 'ann', 2 ),
          get_rasta_land_1_50_NA( 1, 'rf', 2 ),
          get_rasta_land_0_50_NA( 1, 'svm', 2 ),
          get_rasta_land_1_50_NA( 1, 'glm', 2 ),
          get_rasta_land_1_50_NA( 2, 'ann', 2 ),
          get_rasta_land_1_50_NA( 2, 'rf', 2 ),
          get_rasta_land_0_50_NA( 2, 'svm', 2 ),
          get_rasta_land_1_50_NA( 2, 'glm', 2 ) ) %>% return(.)
}

get_max_h_gamma_freqs <- function()
{
    gamma_freqs <- rep( 8, 4 )
    base::names( gamma_freqs ) <- c( 6, 7, 8, 9 )

    return( gamma_freqs )
}

get_max_h_1_stash <- function()
{
    base::list( get_rasta_land_0_50( 1, 'ann' ),
          get_rasta_land_1_50( 1, 'glm' ),
          get_rasta_land_1_50( 1, 'rf' ),
          get_rasta_land_1_50( 1, 'svm' ) ) %>% return(.)
}

get_max_h_2_1_stash <- function()
{
    base::list( get_rasta_land_1_50( 1, 'ann' ),
          get_rasta_land_0_50( 1, 'glm' ),
          get_rasta_land_0_50( 1, 'rf' ),
          get_rasta_land_1_50( 1, 'svm' ) ) %>% return(.)
}

get_max_h_2_stash <- function()
{
    base::list( get_rasta_land_1_50( 2, 'ann' ),
          get_rasta_land_0_50( 2, 'glm' ),
          get_rasta_land_0_50( 2, 'rf' ),
          get_rasta_land_1_50( 2, 'svm' ) ) %>% return(.)
}

get_max_h_1_stash_NA <- function()
{
    base::list( get_rasta_land_0_50_NA( 1, 'ann' ),
          get_rasta_land_1_50_NA( 1, 'glm' ),
          get_rasta_land_1_50_NA( 1, 'rf' ),
          get_rasta_land_1_50_NA( 1, 'svm' ) ) %>% return(.)
}

get_max_h_2_stash_NA <- function()
{
    base::list( get_rasta_land_1_50_NA( 2, 'ann' ),
          get_rasta_land_0_50_NA( 2, 'glm' ),
          get_rasta_land_0_50_NA( 2, 'rf' ),
          get_rasta_land_1_50_NA( 2, 'svm' ) ) %>% return(.)
}

get_merge_1_stash <- function()
{
    base::list( get_rasta_land_0_50( 1, 'ann', 5 ),
          get_rasta_land_1_50( 1, 'glm', 5 ),
          get_rasta_land_1_50( 1, 'rf', 5 ),
          get_rasta_land_1_50( 1, 'svm', 5 ) ) %>% return(.)
}

get_merge_2_stash <- function()
{
    base::list( get_rasta_land_1_50( 2, 'ann', 5 ),
          get_rasta_land_0_50( 2, 'glm', 5 ),
          get_rasta_land_0_50( 2, 'rf', 5 ),
          get_rasta_land_1_50( 2, 'svm', 5 ) ) %>% return(.)
}

get_merge_3_stash <- function()
{
    base::list( get_rasta_land_1_50( 3, 'ann', 5 ),
          get_rasta_land_0_50( 3, 'glm', 5 ),
          get_rasta_land_0_50( 3, 'rf', 5 ),
          get_rasta_land_1_50( 3, 'svm', 5 ) ) %>% return(.)
}

get_merge_4_stash <- function()
{
    base::list( get_rasta_land_0_50( 4, 'ann', 5 ),
          get_rasta_land_1_50( 4, 'glm', 5 ),
          get_rasta_land_1_50( 4, 'rf', 5 ),
          get_rasta_land_1_50( 4, 'svm', 5 ) ) %>% return(.)
}

get_merge_5_stash <- function()
{
    base::list( get_rasta_land_1_50( 5, 'ann', 5 ),
          get_rasta_land_0_50( 5, 'glm', 5 ),
          get_rasta_land_0_50( 5, 'rf', 5 ),
          get_rasta_land_1_50( 5, 'svm', 5 ) ) %>% return(.)
}

get_merge_NA <- function()
{
    base::list( get_rasta_land_0_50_NA( 5, 'ann', 5 ),
          get_rasta_land_1_50_NA( 5, 'glm', 5 ),
          get_rasta_land_1_50_NA( 5, 'rf', 5 ),
          get_rasta_land_1_50_NA( 5, 'svm', 5 ) ) %>% return(.)
}

get_merge_unequal <- function()
{
    base::list( get_rasta_land_0_50_NA( 5, 'ann', 5 ),
          get_rasta_land_1_50_NA( 5, 'glm', 5 ),
          get_rasta_land_1_50_NA( 5, 'bioclim', 5 ),
          get_rasta_land_1_50_NA( 5, 'svm', 5 ) ) %>% return(.)
}

get_weird_rl <- function( method, my_raster_layer )
{
    base::stopifnot( base::is.character( method ),
                     test_class( my_raster_layer, "RasterLayer" ) )

    rasta_land <- RastaLand$new( class = 'weird_class',
                                 method = method,
                                 rep = 5,
                                 rep_count = 5,
                                 raster_obj = my_raster_layer )
}

get_weird_stash <- function()
{
    base::list( get_weird_rl( "ann", get_raster_layer_1_100() ),
                get_weird_rl( "glm", get_raster_layer_0_100() ),
                get_weird_rl( "rf", get_raster_layer_0_100() ),
                get_weird_rl( "svm", get_raster_layer_1_100() ) ) %>% return(.)
}

get_max_h_stash_2rep <- function()
{
    c( get_max_h_1_stash(),
       get_max_h_2_stash() ) %>% return(.)
}

get_max_h_stash_2rep_NA <- function()
{
    c( get_max_h_1_stash_NA(),
       get_max_h_2_stash_NA() ) %>% return(.)
}

get_max_h_stash_2rep_votes <- function( f )
{
    base::stopifnot( typeof( f ) == 'closure' )

    votes_sum <- NULL
    
    for( rasta_land in f() )
    {
        if( base::is.null( votes_sum ) )
        {
            votes_sum <- rasta_land$get_raster_obj()
        }
        else
        {
            votes_sum <- votes_sum + rasta_land$get_raster_obj()
        }
    }

    return( ( votes_sum / 8 ) %>% get_vec_val(.) )
}

get_max_h_stash_2rep_shuff <- function()
{
    base::list( get_rasta_land_1_50( 2, 'rf' ),
          get_rasta_land_1_50( 1, 'glm' ),
          get_rasta_land_1_50( 2, 'ann' ),
          get_rasta_land_1_50( 1, 'rf' ),
          get_rasta_land_1_50( 1, 'svm' ),
          get_rasta_land_0_50( 2, 'glm' ),
          get_rasta_land_0_50( 2, 'svm' ),
          get_rasta_land_0_50( 1, 'ann' ) ) %>% return(.)
}

get_inter_h_stash_5rep <- function()
{
    base::list( get_rasta_land_0_50( 1, 'ann', 5 ),
          get_rasta_land_1_100( 1, 'glm', 5 ),
          get_rasta_land_1_50( 1, 'rf', 5 ),
          get_rasta_land_1_50( 1, 'svm', 5 ),

          get_rasta_land_1_100( 2, 'rf', 5 ),
          get_rasta_land_0_50( 2, 'glm', 5 ),
          get_rasta_land_0_50( 2, 'svm', 5 ),
          get_rasta_land_1_50( 2, 'ann', 5 ),

          get_rasta_land_1_50( 3, 'rf', 5 ),
          get_rasta_land_0_100( 3, 'glm', 5 ),
          get_rasta_land_1_50( 3, 'svm', 5 ),
          get_rasta_land_1_50( 3, 'ann', 5 ),

          get_rasta_land_1_50( 4, 'rf', 5 ),
          get_rasta_land_0_50( 4, 'glm', 5 ),
          get_rasta_land_1_50( 4, 'svm', 5 ),
          get_rasta_land_1_50( 4, 'ann', 5 ),

          get_rasta_land_1_50( 5, 'rf', 5 ),
          get_rasta_land_0_100( 5, 'glm', 5 ),
          get_rasta_land_1_50( 5, 'svm', 5 ),
          get_rasta_land_1_50( 5, 'ann', 5 ) ) %>% return(.)
}

get_max_h_stash_2rep_shuff <- function()
{
    base::list( get_rasta_land_0_50( 1, 'ann' ),
          get_rasta_land_1_50( 1, 'glm' ),
          get_rasta_land_1_50( 1, 'rf' ),
          get_rasta_land_1_50( 1, 'svm' ),

          get_rasta_land_1_50( 2, 'rf' ),
          get_rasta_land_0_50( 2, 'glm' ),
          get_rasta_land_0_50( 2, 'svm' ),
          get_rasta_land_1_50( 2, 'ann' ) ) %>% return(.)
}

get_0_0_h_stash_single_method <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 4 ),
          get_rasta_land_0_100( 2, 'ann', 4 ),
          get_rasta_land_0_100( 3, 'ann', 4 ),
          get_rasta_land_0_100( 4, 'ann', 4 ) ) %>% return(.)
}

get_1_0_h_stash_single_method <- function()
{
    base::list( get_rasta_land_1_100( 1, 'ann', 4 ),
          get_rasta_land_1_100( 2, 'ann', 4 ),
          get_rasta_land_1_100( 3, 'ann', 4 ),
          get_rasta_land_1_100( 4, 'ann', 4 ) ) %>% return(.)
}

get_max_h_stash_single_method <- function()
{
    base::list( get_rasta_land_0_50( 1, 'ann', 4 ),
          get_rasta_land_1_50( 2, 'ann', 4 ),
          get_rasta_land_0_50( 3, 'ann', 4 ),
          get_rasta_land_1_50( 4, 'ann', 4 ) ) %>% return(.)
}

get_20_80_stash <- function()
{
    stash = base::list()
    
    for( i in 1:8 )
    {
        stash <- c( stash, get_rasta_land_0_100( i, 'ann', 10 ) )
    }

    stash <- c( stash,
        get_rasta_land_1_100( 9, 'ann', 10 ),
        get_rasta_land_1_100( 10, 'ann', 10 ) )

    return( stash )
}

get_votes_6rep <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 6 ),
          get_rasta_land_1_100( 2, 'ann', 6 ),
          get_rasta_land_0_100( 3, 'ann', 6 ),
          get_rasta_land_1_100( 4, 'ann', 6 ),
          get_rasta_land_0_100( 5, 'ann', 6 ),
          get_rasta_land_1_100( 6, 'ann', 6 ),

          get_rasta_land_1_50( 1, 'glm', 6 ),
          get_rasta_land_1_50( 2, 'glm', 6 ),
          get_rasta_land_1_50( 3, 'glm', 6 ),
          get_rasta_land_1_50( 4, 'glm', 6 ),
          get_rasta_land_1_50( 5, 'glm', 6 ),
          get_rasta_land_1_50( 6, 'glm', 6 ),

          get_rasta_land_1_50( 1, 'rf', 6 ),
          get_rasta_land_0_50( 2, 'rf', 6 ),
          get_rasta_land_1_50( 3, 'rf', 6 ),
          get_rasta_land_0_50( 4, 'rf', 6 ),
          get_rasta_land_1_50( 5, 'rf', 6 ),
          get_rasta_land_0_50( 6, 'rf', 6 ),
         
          get_rasta_land_0_50( 1, 'svm', 6 ),
          get_rasta_land_0_50( 2, 'svm', 6 ),
          get_rasta_land_0_50( 3, 'svm', 6 ),
          get_rasta_land_0_50( 4, 'svm', 6 ),
          get_rasta_land_0_50( 5, 'svm', 6 ),
          get_rasta_land_0_50( 6, 'svm', 6 ) ) %>% return(.)
}

get_votes_stable <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 6 ),
          get_rasta_land_0_100( 2, 'ann', 6 ),
          get_rasta_land_0_100( 3, 'ann', 6 ),
          get_rasta_land_0_100( 4, 'ann', 6 ),
          get_rasta_land_0_100( 5, 'ann', 6 ),
          get_rasta_land_0_100( 6, 'ann', 6 ),
         
          get_rasta_land_1_100( 1, 'glm', 6 ),
          get_rasta_land_1_100( 2, 'glm', 6 ),
          get_rasta_land_1_100( 3, 'glm', 6 ),
          get_rasta_land_1_100( 4, 'glm', 6 ),
          get_rasta_land_1_100( 5, 'glm', 6 ),
          get_rasta_land_1_100( 6, 'glm', 6 ),
         
          get_rasta_land_0_100( 1, 'rf', 6 ),
          get_rasta_land_0_100( 2, 'rf', 6 ),
          get_rasta_land_0_100( 3, 'rf', 6 ),
          get_rasta_land_0_100( 4, 'rf', 6 ),
          get_rasta_land_0_100( 5, 'rf', 6 ),
          get_rasta_land_0_100( 6, 'rf', 6 ),
         
          get_rasta_land_1_100( 1, 'svm', 6 ),
          get_rasta_land_1_100( 2, 'svm', 6 ),
          get_rasta_land_1_100( 3, 'svm', 6 ),
          get_rasta_land_1_100( 4, 'svm', 6 ),
          get_rasta_land_1_100( 5, 'svm', 6 ),
          get_rasta_land_1_100( 6, 'svm', 6 ),
         
          get_rasta_land_1_100( 1, 'gam', 6 ),
          get_rasta_land_1_100( 2, 'gam', 6 ),
          get_rasta_land_1_100( 3, 'gam', 6 ),
          get_rasta_land_1_100( 4, 'gam', 6 ),
          get_rasta_land_1_100( 5, 'gam', 6 ),
          get_rasta_land_1_100( 6, 'gam', 6 ) ) %>% return(.)
}

get_rf_svm_unstable_6 <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 6 ),
          get_rasta_land_0_100( 2, 'ann', 6 ),
          get_rasta_land_0_100( 3, 'ann', 6 ),
          get_rasta_land_0_100( 4, 'ann', 6 ),
          get_rasta_land_0_100( 5, 'ann', 6 ),
          get_rasta_land_0_100( 6, 'ann', 6 ),

          get_rasta_land_0_100( 1, 'gam', 6 ),
          get_rasta_land_0_100( 2, 'gam', 6 ),
          get_rasta_land_0_100( 3, 'gam', 6 ),
          get_rasta_land_0_100( 4, 'gam', 6 ),
          get_rasta_land_0_100( 5, 'gam', 6 ),
          get_rasta_land_0_100( 6, 'gam', 6 ),

          get_rasta_land_1_100( 1, 'glm', 6 ),
          get_rasta_land_1_100( 2, 'glm', 6 ),
          get_rasta_land_1_100( 3, 'glm', 6 ),
          get_rasta_land_1_100( 4, 'glm', 6 ),
          get_rasta_land_1_100( 5, 'glm', 6 ),
          get_rasta_land_1_100( 6, 'glm', 6 ),

          get_rasta_land_0_100( 1, 'rf', 6 ),
          get_rasta_land_1_100( 2, 'rf', 6 ),
          get_rasta_land_0_100( 3, 'rf', 6 ),
          get_rasta_land_1_100( 4, 'rf', 6 ),
          get_rasta_land_0_100( 5, 'rf', 6 ),
          get_rasta_land_1_100( 6, 'rf', 6 ),

          get_rasta_land_1_100( 1, 'svm', 6 ),
          get_rasta_land_0_100( 2, 'svm', 6 ),
          get_rasta_land_1_100( 3, 'svm', 6 ),
          get_rasta_land_0_100( 4, 'svm', 6 ),
          get_rasta_land_1_100( 5, 'svm', 6 ),
          get_rasta_land_0_100( 6, 'svm', 6 ) )%>% return(.)
}

get_rf_svm_unstable_7 <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 6 ),
          get_rasta_land_0_100( 2, 'ann', 6 ),
          get_rasta_land_0_100( 3, 'ann', 6 ),
          get_rasta_land_0_100( 4, 'ann', 6 ),
          get_rasta_land_0_100( 5, 'ann', 6 ),
          get_rasta_land_0_100( 6, 'ann', 6 ),
          get_rasta_land_0_100( 7, 'ann', 6 ),

          get_rasta_land_0_100( 1, 'gam', 6 ),
          get_rasta_land_0_100( 2, 'gam', 6 ),
          get_rasta_land_0_100( 3, 'gam', 6 ),
          get_rasta_land_0_100( 4, 'gam', 6 ),
          get_rasta_land_0_100( 5, 'gam', 6 ),
          get_rasta_land_0_100( 6, 'gam', 6 ),
          get_rasta_land_0_100( 7, 'gam', 6 ),

          get_rasta_land_1_100( 1, 'glm', 6 ),
          get_rasta_land_1_100( 2, 'glm', 6 ),
          get_rasta_land_1_100( 3, 'glm', 6 ),
          get_rasta_land_1_100( 4, 'glm', 6 ),
          get_rasta_land_1_100( 5, 'glm', 6 ),
          get_rasta_land_1_100( 6, 'glm', 6 ),
          get_rasta_land_1_100( 7, 'glm', 6 ),

          get_rasta_land_0_100( 1, 'rf', 6 ),
          get_rasta_land_1_100( 2, 'rf', 6 ),
          get_rasta_land_0_100( 3, 'rf', 6 ),
          get_rasta_land_1_100( 4, 'rf', 6 ),
          get_rasta_land_0_100( 5, 'rf', 6 ),
          get_rasta_land_1_100( 6, 'rf', 6 ),
          get_rasta_land_1_100( 7, 'rf', 6 ),

          get_rasta_land_1_100( 1, 'svm', 6 ),
          get_rasta_land_0_100( 2, 'svm', 6 ),
          get_rasta_land_1_100( 3, 'svm', 6 ),
          get_rasta_land_0_100( 4, 'svm', 6 ),
          get_rasta_land_1_100( 5, 'svm', 6 ),
          get_rasta_land_0_100( 6, 'svm', 6 ),
          get_rasta_land_1_100( 7, 'svm', 6 ) ) %>% return(.)
}

get_rf_svm_unstable_7_NA <- function()
{
    base::list( get_rasta_land_0_100_NA( 1, 'ann', 6 ),
          get_rasta_land_0_100_NA( 2, 'ann', 6 ),
          get_rasta_land_0_100_NA( 3, 'ann', 6 ),
          get_rasta_land_0_100_NA( 4, 'ann', 6 ),
          get_rasta_land_0_100_NA( 5, 'ann', 6 ),
          get_rasta_land_0_100_NA( 6, 'ann', 6 ),
          get_rasta_land_0_100_NA( 7, 'ann', 6 ),

          get_rasta_land_0_100_NA( 1, 'gam', 6 ),
          get_rasta_land_0_100_NA( 2, 'gam', 6 ),
          get_rasta_land_0_100_NA( 3, 'gam', 6 ),
          get_rasta_land_0_100_NA( 4, 'gam', 6 ),
          get_rasta_land_0_100_NA( 5, 'gam', 6 ),
          get_rasta_land_0_100_NA( 6, 'gam', 6 ),
          get_rasta_land_0_100_NA( 7, 'gam', 6 ),

          get_rasta_land_1_100_NA( 1, 'glm', 6 ),
          get_rasta_land_1_100_NA( 2, 'glm', 6 ),
          get_rasta_land_1_100_NA( 3, 'glm', 6 ),
          get_rasta_land_1_100_NA( 4, 'glm', 6 ),
          get_rasta_land_1_100_NA( 5, 'glm', 6 ),
          get_rasta_land_1_100_NA( 6, 'glm', 6 ),
          get_rasta_land_1_100_NA( 7, 'glm', 6 ),

          get_rasta_land_0_100_NA( 1, 'rf', 6 ),
          get_rasta_land_1_100_NA( 2, 'rf', 6 ),
          get_rasta_land_0_100_NA( 3, 'rf', 6 ),
          get_rasta_land_1_100_NA( 4, 'rf', 6 ),
          get_rasta_land_0_100_NA( 5, 'rf', 6 ),
          get_rasta_land_1_100_NA( 6, 'rf', 6 ),
          get_rasta_land_1_100_NA( 7, 'rf', 6 ),

          get_rasta_land_1_100_NA( 1, 'svm', 6 ),
          get_rasta_land_0_100_NA( 2, 'svm', 6 ),
          get_rasta_land_1_100_NA( 3, 'svm', 6 ),
          get_rasta_land_0_100_NA( 4, 'svm', 6 ),
          get_rasta_land_1_100_NA( 5, 'svm', 6 ),
          get_rasta_land_0_100_NA( 6, 'svm', 6 ),
          get_rasta_land_1_100_NA( 7, 'svm', 6 ) ) %>% return(.)
}

get_rf_svm_unstable_7_large <- function()
{
    base::list( get_large_rl_fill_0( 1, 'ann', 6 ),
          get_large_rl_fill_0( 2, 'ann', 6 ),
          get_large_rl_fill_0( 3, 'ann', 6 ),
          get_large_rl_fill_0( 4, 'ann', 6 ),
          get_large_rl_fill_0( 5, 'ann', 6 ),
          get_large_rl_fill_0( 6, 'ann', 6 ),
          get_large_rl_fill_0( 7, 'ann', 6 ),

          get_large_rl_fill_0( 1, 'gam', 6 ),
          get_large_rl_fill_0( 2, 'gam', 6 ),
          get_large_rl_fill_0( 3, 'gam', 6 ),
          get_large_rl_fill_0( 4, 'gam', 6 ),
          get_large_rl_fill_0( 5, 'gam', 6 ),
          get_large_rl_fill_0( 6, 'gam', 6 ),
          get_large_rl_fill_0( 7, 'gam', 6 ),

          get_large_rl_fill_1( 1, 'glm', 6 ),
          get_large_rl_fill_1( 2, 'glm', 6 ),
          get_large_rl_fill_1( 3, 'glm', 6 ),
          get_large_rl_fill_1( 4, 'glm', 6 ),
          get_large_rl_fill_1( 5, 'glm', 6 ),
          get_large_rl_fill_1( 6, 'glm', 6 ),
          get_large_rl_fill_1( 7, 'glm', 6 ),

          get_large_rl_fill_0( 1, 'rf', 6 ),
          get_large_rl_fill_1( 2, 'rf', 6 ),
          get_large_rl_fill_0( 3, 'rf', 6 ),
          get_large_rl_fill_1( 4, 'rf', 6 ),
          get_large_rl_fill_0( 5, 'rf', 6 ),
          get_large_rl_fill_1( 6, 'rf', 6 ),
          get_large_rl_fill_1( 7, 'rf', 6 ),

          get_large_rl_fill_1( 1, 'svm', 6 ),
          get_large_rl_fill_0( 2, 'svm', 6 ),
          get_large_rl_fill_1( 3, 'svm', 6 ),
          get_large_rl_fill_0( 4, 'svm', 6 ),
          get_large_rl_fill_1( 5, 'svm', 6 ),
          get_large_rl_fill_0( 6, 'svm', 6 ),
          get_large_rl_fill_1( 7, 'svm', 6 ) ) %>% return(.)
}

get_random_7_large <- function()
{
    base::list( get_large_rl_fill_0( 1, 'ann', 6 ),
          get_large_rl_fill_0( 2, 'ann', 6 ),
          get_large_rl_fill_0( 3, 'ann', 6 ),
          get_large_rl_fill_0( 4, 'ann', 6 ),
          get_large_rl_fill_0( 5, 'ann', 6 ),
          get_large_rl_fill_0( 6, 'ann', 6 ),
          get_large_rl_fill_0( 7, 'ann', 6 ),

          get_large_rl_fill_0( 1, 'gam', 6 ),
          get_large_rl_fill_0( 2, 'gam', 6 ),
          get_large_rl_fill_0( 3, 'gam', 6 ),
          get_large_rl_fill_0( 4, 'gam', 6 ),
          get_large_rl_fill_0( 5, 'gam', 6 ),
          get_large_rl_fill_0( 6, 'gam', 6 ),
          get_large_rl_fill_0( 7, 'gam', 6 ),

          get_large_rl_fill_1( 1, 'glm', 6 ),
          get_large_rl_fill_1( 2, 'glm', 6 ),
          get_large_rl_fill_1( 3, 'glm', 6 ),
          get_large_rl_fill_1( 4, 'glm', 6 ),
          get_large_rl_fill_1( 5, 'glm', 6 ),
          get_large_rl_fill_1( 6, 'glm', 6 ),
          get_large_rl_fill_1( 7, 'glm', 6 ),

          get_large_rl_random( 1, 'rf', 6 ),
          get_large_rl_random( 2, 'rf', 6 ),
          get_large_rl_random( 3, 'rf', 6 ),
          get_large_rl_random( 4, 'rf', 6 ),
          get_large_rl_random( 5, 'rf', 6 ),
          get_large_rl_random( 6, 'rf', 6 ),
          get_large_rl_random( 7, 'rf', 6 ),

          get_large_rl_random( 1, 'svm', 6 ),
          get_large_rl_random( 2, 'svm', 6 ),
          get_large_rl_random( 3, 'svm', 6 ),
          get_large_rl_random( 4, 'svm', 6 ),
          get_large_rl_random( 5, 'svm', 6 ),
          get_large_rl_random( 6, 'svm', 6 ),
          get_large_rl_random( 7, 'svm', 6 ) ) %>% return(.)
}

get_pareto_6_reps <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 6 ),
          get_rasta_land_1_100( 2, 'ann', 6 ),
          get_rasta_land_0_100( 3, 'ann', 6 ),
          get_rasta_land_1_100( 4, 'ann', 6 ),
          get_rasta_land_0_100( 5, 'ann', 6 ),
          get_rasta_land_1_100( 6, 'ann', 6 ),

          get_rasta_land_1_100( 1, 'gam', 6 ),
          get_rasta_land_1_100( 2, 'gam', 6 ),
          get_rasta_land_1_100( 3, 'gam', 6 ),
          get_rasta_land_1_100( 4, 'gam', 6 ),
          get_rasta_land_1_100( 5, 'gam', 6 ),
          get_rasta_land_1_100( 6, 'gam', 6 ),

          get_rasta_land_1_100( 1, 'glm', 6 ),
          get_rasta_land_1_100( 2, 'glm', 6 ),
          get_rasta_land_1_100( 3, 'glm', 6 ),
          get_rasta_land_1_100( 4, 'glm', 6 ),
          get_rasta_land_1_100( 5, 'glm', 6 ),
          get_rasta_land_1_100( 6, 'glm', 6 ),

          get_rasta_land_0_100( 1, 'rf', 6 ),
          get_rasta_land_1_100( 2, 'rf', 6 ),
          get_rasta_land_0_100( 3, 'rf', 6 ),
          get_rasta_land_1_100( 4, 'rf', 6 ),
          get_rasta_land_0_100( 5, 'rf', 6 ),
          get_rasta_land_1_100( 6, 'rf', 6 ),

          get_rasta_land_0_100( 1, 'svm', 6 ),
          get_rasta_land_1_100( 2, 'svm', 6 ),
          get_rasta_land_0_100( 3, 'svm', 6 ),
          get_rasta_land_1_100( 4, 'svm', 6 ),
          get_rasta_land_1_100( 5, 'svm', 6 ),
          get_rasta_land_1_100( 6, 'svm', 6 ) ) %>% return(.)
}

get_pareto_mx_2_methods <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 2 ),
          get_rasta_land_1_100( 1, 'gam', 2 ),

          get_rasta_land_1_100( 2, 'ann', 2 ),
          get_rasta_land_0_100( 2, 'gam', 2 ) ) %>% return(.)
}

get_pareto_mx_3_methods <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 6 ),
          get_rasta_land_1_100( 1, 'gam', 6 ),
          get_rasta_land_1_100( 1, 'glm', 6 ),

          get_rasta_land_1_100( 2, 'ann', 6 ),
          get_rasta_land_0_100( 2, 'gam', 6 ),
          get_rasta_land_1_100( 2, 'glm', 6 ),

          get_rasta_land_1_100( 3, 'ann', 6 ),
          get_rasta_land_1_100( 3, 'gam', 6 ),
          get_rasta_land_1_100( 3, 'glm', 6 ),

          get_rasta_land_0_100( 4, 'ann', 6 ),
          get_rasta_land_0_100( 4, 'gam', 6 ),
          get_rasta_land_1_100( 4, 'glm', 6 ),

          get_rasta_land_0_100( 5, 'ann', 6 ),
          get_rasta_land_1_100( 5, 'gam', 6 ),
          get_rasta_land_0_100( 5, 'glm', 6 ),

          get_rasta_land_0_100( 6, 'ann', 6 ),
          get_rasta_land_0_100( 6, 'gam', 6 ),
          get_rasta_land_0_100( 6, 'glm', 6 ) ) %>% return(.)
}

get_pareto_mx_5_methods <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 6 ),
          get_rasta_land_1_100( 1, 'gam', 6 ),
          get_rasta_land_1_100( 1, 'glm', 6 ),
          get_rasta_land_0_100( 1, 'rf', 6 ),
          get_rasta_land_0_100( 1, 'svm', 6 ),         

          get_rasta_land_1_100( 2, 'ann', 6 ),
          get_rasta_land_0_100( 2, 'gam', 6 ),
          get_rasta_land_1_100( 2, 'glm', 6 ),
          get_rasta_land_1_100( 2, 'rf', 6 ),
          get_rasta_land_0_100( 2, 'svm', 6 ),

          get_rasta_land_1_100( 3, 'ann', 6 ),
          get_rasta_land_1_100( 3, 'gam', 6 ),
          get_rasta_land_1_100( 3, 'glm', 6 ),
          get_rasta_land_0_100( 3, 'rf', 6 ),
          get_rasta_land_0_100( 3, 'svm', 6 ),

          get_rasta_land_0_100( 4, 'ann', 6 ),
          get_rasta_land_0_100( 4, 'gam', 6 ),
          get_rasta_land_1_100( 4, 'glm', 6 ),
          get_rasta_land_1_100( 4, 'rf', 6 ),
          get_rasta_land_1_100( 4, 'svm', 6 ),

          get_rasta_land_0_100( 5, 'ann', 6 ),
          get_rasta_land_1_100( 5, 'gam', 6 ),
          get_rasta_land_0_100( 5, 'glm', 6 ),
          get_rasta_land_1_100( 5, 'rf', 6 ),
          get_rasta_land_0_100( 5, 'svm', 6 ),         

          get_rasta_land_0_100( 6, 'ann', 6 ),
          get_rasta_land_0_100( 6, 'gam', 6 ),
          get_rasta_land_0_100( 6, 'glm', 6 ),
          get_rasta_land_0_100( 6, 'rf', 6 ),
          get_rasta_land_1_100( 6, 'svm', 6 ) ) %>% return(.)
}

get_pareto_mx_7_methods <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 6 ),
          get_rasta_land_1_100( 1, 'biomod', 6 ),
          get_rasta_land_1_100( 1, 'gam', 6 ),
          get_rasta_land_1_100( 1, 'glm', 6 ),
          get_rasta_land_0_100( 1, 'mahal', 6 ),
          get_rasta_land_0_100( 1, 'rf', 6 ),
          get_rasta_land_0_100( 1, 'svm', 6 ),         

          get_rasta_land_1_100( 2, 'ann', 6 ),
          get_rasta_land_0_100( 2, 'biomod', 6 ),
          get_rasta_land_0_100( 2, 'gam', 6 ),
          get_rasta_land_1_100( 2, 'glm', 6 ),
          get_rasta_land_1_100( 2, 'mahal', 6 ),
          get_rasta_land_1_100( 2, 'rf', 6 ),
          get_rasta_land_0_100( 2, 'svm', 6 ),

          get_rasta_land_1_100( 3, 'ann', 6 ),
          get_rasta_land_0_100( 3, 'biomod', 6 ),
          get_rasta_land_1_100( 3, 'gam', 6 ),
          get_rasta_land_1_100( 3, 'glm', 6 ),
          get_rasta_land_1_100( 3, 'mahal', 6 ),
          get_rasta_land_0_100( 3, 'rf', 6 ),
          get_rasta_land_0_100( 3, 'svm', 6 ),

          get_rasta_land_0_100( 4, 'ann', 6 ),
          get_rasta_land_1_100( 4, 'biomod', 6 ),
          get_rasta_land_0_100( 4, 'gam', 6 ),
          get_rasta_land_1_100( 4, 'glm', 6 ),
          get_rasta_land_0_100( 4, 'mahal', 6 ),
          get_rasta_land_1_100( 4, 'rf', 6 ),
          get_rasta_land_1_100( 4, 'svm', 6 ),

          get_rasta_land_0_100( 5, 'ann', 6 ),
          get_rasta_land_0_100( 5, 'biomod', 6 ),
          get_rasta_land_1_100( 5, 'gam', 6 ),
          get_rasta_land_0_100( 5, 'glm', 6 ),
          get_rasta_land_1_100( 5, 'mahal', 6 ),
          get_rasta_land_1_100( 5, 'rf', 6 ),
          get_rasta_land_0_100( 5, 'svm', 6 ),         

          get_rasta_land_0_100( 6, 'ann', 6 ),
          get_rasta_land_1_100( 6, 'biomod', 6 ),
          get_rasta_land_0_100( 6, 'gam', 6 ),
          get_rasta_land_0_100( 6, 'glm', 6 ),
          get_rasta_land_1_100( 6, 'mahal', 6 ),
          get_rasta_land_0_100( 6, 'rf', 6 ),
          get_rasta_land_1_100( 6, 'svm', 6 ) ) %>% return(.)
}

get_pareto_mx_6_100_50 <- function()
{
    base::list( get_rasta_land_0_100( 1, 'ann', 6 ),
          get_rasta_land_1_100( 1, 'biomod', 6 ),
          get_rasta_land_0_50( 1, 'gam', 6 ),
          get_rasta_land_0_100( 1, 'glm', 6 ),
          get_rasta_land_1_50( 1, 'mahal', 6 ),
          get_rasta_land_1_100( 1, 'rf', 6 ),
         
          get_rasta_land_0_100( 2, 'ann', 6 ),
          get_rasta_land_1_100( 2, 'biomod', 6 ),
          get_rasta_land_0_50( 2, 'gam', 6 ),
          get_rasta_land_0_100( 2, 'glm', 6 ),
          get_rasta_land_1_50( 2, 'mahal', 6 ),
          get_rasta_land_1_100( 2, 'rf', 6 ),

          get_rasta_land_0_100( 3, 'ann', 6 ),
          get_rasta_land_1_100( 3, 'biomod', 6 ),
          get_rasta_land_0_50( 3, 'gam', 6 ),
          get_rasta_land_0_100( 3, 'glm', 6 ),
          get_rasta_land_1_50( 3, 'mahal', 6 ),
          get_rasta_land_1_100( 3, 'rf', 6 ),

          get_rasta_land_0_100( 4, 'ann', 6 ),
          get_rasta_land_1_100( 4, 'biomod', 6 ),
          get_rasta_land_0_50( 4, 'gam', 6 ),
          get_rasta_land_0_100( 4, 'glm', 6 ),
          get_rasta_land_1_50( 4, 'mahal', 6 ),
          get_rasta_land_1_100( 4, 'rf', 6 ),

          get_rasta_land_0_100( 5, 'ann', 6 ),
          get_rasta_land_1_100( 5, 'biomod', 6 ),
          get_rasta_land_0_50( 5, 'gam', 6 ),
          get_rasta_land_0_100( 5, 'glm', 6 ),
          get_rasta_land_1_50( 5, 'mahal', 6 ),
          get_rasta_land_1_100( 5, 'rf', 6 ),

          get_rasta_land_0_100( 6, 'ann', 6 ),
          get_rasta_land_1_100( 6, 'biomod', 6 ),
          get_rasta_land_0_50( 6, 'gam', 6 ),
          get_rasta_land_0_100( 6, 'glm', 6 ),
          get_rasta_land_1_50( 6, 'mahal', 6 ),
          get_rasta_land_1_100( 6, 'rf', 6 ) ) %>% return(.)
}

get_pareto_shift <- function()
{
    base::list( get_rasta_land_shift_1( 1, 'ann', 3 ),
          get_rasta_land_third( 1, 'biomod', 3 ),
          get_rasta_land_third( 1, 'gam', 3 ),
          get_rasta_land_shift_2( 1, 'glm', 3 ),

          get_rasta_land_shift_1( 2, 'ann', 3 ),
          get_rasta_land_third( 2, 'biomod', 3 ),
          get_rasta_land_third( 2, 'gam', 3 ),
          get_rasta_land_shift_2( 2, 'glm', 3 ),

          get_rasta_land_shift_1( 3, 'ann', 3 ),
          get_rasta_land_third( 3, 'biomod', 3 ),
          get_rasta_land_third( 3, 'gam', 3 ),
          get_rasta_land_shift_2( 3, 'glm', 3 ) )
}

get_pareto_shift_9 <- function()
{
    base::list( get_rasta_land_shift_1( 1, 'ann', 3 ),
          get_rasta_land_shift_2( 1, 'glm', 3 ),

          get_rasta_land_shift_1( 2, 'ann', 3 ),
          get_rasta_land_shift_2( 2, 'glm', 3 ),

          get_rasta_land_shift_1( 3, 'ann', 3 ),
          get_rasta_land_shift_2( 3, 'glm', 3 ) )
}

get_pareto_shift_14 <- function()
{
    base::list( get_rasta_land_shift_1( 1, 'ann', 3 ),
          get_rasta_land_third( 1, 'biomod', 3 ),
          get_rasta_land_third( 1, 'gam', 3 ),

          get_rasta_land_shift_1( 2, 'ann', 3 ),
          get_rasta_land_third( 2, 'biomod', 3 ),
          get_rasta_land_third( 2, 'gam', 3 ),

          get_rasta_land_shift_1( 3, 'ann', 3 ),
          get_rasta_land_third( 3, 'biomod', 3 ),
          get_rasta_land_third( 3, 'gam', 3 ) )
}

get_pareto_mx_4_100_50 <- function()
{
    base::list( get_rasta_land_1_100( 1, 'biomod', 6 ),
          get_rasta_land_0_50( 1, 'gam', 6 ),
          get_rasta_land_0_100( 1, 'glm', 6 ),
          get_rasta_land_1_50( 1, 'mahal', 6 ),

          get_rasta_land_1_100( 2, 'biomod', 6 ),
          get_rasta_land_0_50( 2, 'gam', 6 ),
          get_rasta_land_0_100( 2, 'glm', 6 ),
          get_rasta_land_1_50( 2, 'mahal', 6 ),

          get_rasta_land_1_100( 3, 'biomod', 6 ),
          get_rasta_land_0_50( 3, 'gam', 6 ),
          get_rasta_land_0_100( 3, 'glm', 6 ),
          get_rasta_land_1_50( 3, 'mahal', 6 ),

          get_rasta_land_1_100( 4, 'biomod', 6 ),
          get_rasta_land_0_50( 4, 'gam', 6 ),
          get_rasta_land_0_100( 4, 'glm', 6 ),
          get_rasta_land_1_50( 4, 'mahal', 6 ),

          get_rasta_land_1_100( 5, 'biomod', 6 ),
          get_rasta_land_0_50( 5, 'gam', 6 ),
          get_rasta_land_0_100( 5, 'glm', 6 ),
          get_rasta_land_1_50( 5, 'mahal', 6 ),

          get_rasta_land_1_100( 6, 'biomod', 6 ),
          get_rasta_land_0_50( 6, 'gam', 6 ),
          get_rasta_land_0_100( 6, 'glm', 6 ),
          get_rasta_land_1_50( 6, 'mahal', 6 ) ) %>% return(.)
}

get_pareto_zero_cells <- function()
{
    base::list( get_rasta_land_1_100( 1, 'biomod', 6 ),
          get_rasta_land_0_50( 1, 'gam', 6 ),
          get_rasta_land_0_100( 1, 'glm', 6 ),
          get_rasta_land_1_50( 1, 'mahal', 6 ),

          get_rasta_land_0_100( 2, 'biomod', 6 ),
          get_rasta_land_0_100( 2, 'gam', 6 ),
          get_rasta_land_0_100( 2, 'glm', 6 ),
          get_rasta_land_0_100( 2, 'mahal', 6 ),

          get_rasta_land_1_100( 3, 'biomod', 6 ),
          get_rasta_land_0_50( 3, 'gam', 6 ),
          get_rasta_land_0_100( 3, 'glm', 6 ),
          get_rasta_land_1_50( 3, 'mahal', 6 ),

          get_rasta_land_0_100( 4, 'biomod', 6 ),
          get_rasta_land_0_100( 4, 'gam', 6 ),
          get_rasta_land_0_100( 4, 'glm', 6 ),
          get_rasta_land_0_100( 4, 'mahal', 6 ),

          get_rasta_land_1_100( 5, 'biomod', 6 ),
          get_rasta_land_0_50( 5, 'gam', 6 ),
          get_rasta_land_0_100( 5, 'glm', 6 ),
          get_rasta_land_1_50( 5, 'mahal', 6 ),

          get_rasta_land_1_100( 6, 'biomod', 6 ),
          get_rasta_land_0_50( 6, 'gam', 6 ),
          get_rasta_land_0_100( 6, 'glm', 6 ),
          get_rasta_land_1_50( 6, 'mahal', 6 ) ) %>% return(.)
}
