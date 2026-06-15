get_rasta_land <- function( f, my_method = 'ann', my_rep = 1,
    my_rep_count = 100, my_class = 'S. siderea' )
{
    base::stopifnot( base::typeof( f ) == 'closure' )

    RastaLand$new( class = my_class,
                   method = my_method,
                   rep = my_rep,
                   rep_count = my_rep_count,
                   raster_obj = f() ) %>% return(.)
}

get_rasta_land_f <- function( f, rep, method, rep_count = 2 )
{
    base::stopifnot( base::typeof( f ) == 'closure',
        is_integer( rep ),
        base::is.character( method ) )

    get_rasta_land( f, my_method = method, my_rep = rep,
            my_rep_count = rep_count ) %>%
        return(.)
}

get_raster_layer <- function( x )
{
    base::stopifnot( base::is.numeric( x ) )

    base::matrix( x, nrow = 4, ncol = 4 ) %>%
        raster::raster(.) %>%
        return(.)
}

get_raster_layer_16_max <- function()
{
    c( 1:16 ) %>% get_raster_layer(.)
}

get_raster_layer_8_max <- function()
{
    c( 1:8 ) %>% get_raster_layer(.)
}

get_raster_layer_1_b <- function()
{
    c( 1, base::rep( 0, 15 ) ) %>% get_raster_layer(.)
}

get_raster_layer_8_b <- function()
{
    c( base::rep( 0, 7 ), 1, base::rep( 0, 8 ) ) %>% get_raster_layer(.)
}

get_raster_layer_16_b <- function()
{
    c( base::rep( 0, 15 ), 1 ) %>% get_raster_layer(.)
}

get_raster_layer_0_100 <- function()
{
    c( base::rep( 0, 16 ) ) %>% get_raster_layer(.)
}

get_raster_layer_1_100 <- function()
{
    c( base::rep( 1, 16 ) ) %>% get_raster_layer(.)
}

get_raster_layer_0_100_NA <- function()
{
    c( base::rep( 0, 15 ), NA ) %>% get_raster_layer(.)
}

get_raster_layer_1_100_NA <- function()
{
    c( base::rep( 1, 15 ), NA ) %>% get_raster_layer(.)
}

get_raster_layer_15_100 <- function()
{
    c( base::rep( 15, 16 ) ) %>% get_raster_layer(.)
}

get_raster_layer_0_50 <- function()
{
    c( base::rep( 0, 8 ), base::rep( 1, 8 ) ) %>% get_raster_layer(.)
}

get_raster_layer_1_50 <- function()
{
    c( base::rep( 1, 8 ), base::rep( 0, 8 ) ) %>% get_raster_layer(.)
}

get_raster_layer_0_50_NA <- function()
{
    c( base::rep( 0, 8 ), base::rep( 1, 7 ), NA ) %>% get_raster_layer(.)
}

get_raster_layer_1_50_NA <- function()
{
    c( base::rep( 1, 8 ), base::rep( 0, 7 ), NA ) %>% get_raster_layer(.)
}

get_raster_layer_0_50_inter <- function()
{
    c( base::rep( 0, 4 ), base::rep( 1, 4 ) ) %>% base::rep( ., 2 ) %>% get_raster_layer(.)
}

get_raster_layer_1_50_inter <- function()
{
    c( base::rep( 1, 4 ), base::rep( 0, 4 ) ) %>% base::rep( ., 2 ) %>% get_raster_layer(.)
}

get_raster_layer_8 <- function()
{
    c( 1:8 ) %>% get_raster_layer(.)
}

get_raster_layer_mid_entropy <- function()
{
    c( c( 1:8 ), base::rep( 9, 4 ), base::rep( 10, 4 ) ) %>% get_raster_layer(.)
}

get_raster_layer_0_100_NA <- function()
{
    c( base::rep( 0, 15 ), NA ) %>% get_raster_layer(.)
}

get_raster_layer_1_100_NA <- function()
{
    c( base::rep( 1, 15 ), NA ) %>% get_raster_layer(.)
}

get_raster_layer_third <- function()
{
    c( base::rep( 0, 5 ), base::rep( 1, 6 ), base::rep( 0, 5 ) ) %>% get_raster_layer(.)
}

get_raster_layer_shift_1 <- function()
{
    c( base::rep( 0, 4 ), base::rep( 1, 6 ), base::rep( 0, 2 ), base::rep( 1, 4 ) ) %>% get_raster_layer(.)
}

get_raster_layer_shift_2 <- function()
{
    c( base::rep( 0, 6 ), base::rep( 1, 4 ), base::rep( 0, 4 ), base::rep( 1, 2 ) ) %>% get_raster_layer(.)
}

get_rasta_land_0_50 <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_raster_layer_0_50, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_1_50 <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_raster_layer_1_50, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_0_50_NA <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_raster_layer_0_50_NA, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_1_50_NA <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_raster_layer_1_50_NA, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_1_100 <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_raster_layer_1_100, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_0_100 <- function( rep, method, rep_count = 2 )
{    
    get_rasta_land_f( get_raster_layer_0_100, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_0_100_NA <- function( rep, method, rep_count = 2 )
{    
    get_rasta_land_f( get_raster_layer_0_100_NA, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_1_100_NA <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_raster_layer_1_100_NA, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_third <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_raster_layer_third, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_shift_1  <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_raster_layer_shift_1, rep, method, rep_count ) %>%
        return(.)
}

get_rasta_land_shift_2  <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_raster_layer_shift_2, rep, method, rep_count ) %>%
        return(.)
}

get_large_rl_fill_0 <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_masked_filled_raster_0, rep, method, rep_count ) %>%
        return(.)
}

get_large_rl_fill_1 <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_masked_filled_raster_1, rep, method, rep_count ) %>%
        return(.)
}

get_large_rl_random <- function( rep, method, rep_count = 2 )
{
    get_rasta_land_f( get_masked_random_raster, rep, method, rep_count ) %>%
        return(.)
}

get_masked_filled_raster_0 <- function()
{
    get_masked_filled_raster( 0 ) %>%
        return( . )
}

get_masked_filled_raster_1 <- function()
{
    get_masked_filled_raster( 1 ) %>%
        return( . )
}

get_masked_random_raster <- function()
{
    get_large_random_mx() %>%
        get_masked_raster(.) %>%
        return( . )
}

get_masked_filled_raster <- function( filler )
{
    base::stopifnot( filler == 0 || filler == 1 )

    get_large_fill_mx( filler ) %>%
        get_masked_raster( . ) %>%
        return( . )
}

get_large_fill_mx <- function( filler )
{
    base::stopifnot( filler == 0 || filler == 1 )
    
    base::matrix( base::rep( filler, 100**2 ), nrow = 100, ncol = 100 ) %>%
        return(.)
}

get_large_random_mx <- function()
{
    base::matrix( stats::rbinom( n = 100**2, size = 1, prob = 0.5 ),
        nrow = 100,
        ncol = 100 ) %>%
        return(.)
}

get_masked_raster <- function( mx )
{
    base::stopifnot( base::is.matrix( mx ) )

    mask_list <- get_mask_list()

    mx[ 1:10, mask_list$full ] <- NA
    mx[ 11:20, mask_list$margin ] <- NA
    mx[ 21:80, mask_list$hole ] <- NA
    mx[ 81:90, mask_list$margin ] <- NA
    mx[ 91:100, mask_list$full ] <- NA

    return( raster( mx ) )    
}

get_mask_list <- function()
{
    base::list( full = c( 1:100 ),
                margin = c( 1:20, 81:100 ),
                hole = c( 1:20, 41:60, 81:100 ) ) %>% return(.)
}
