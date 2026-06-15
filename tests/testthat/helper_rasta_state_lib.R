get_methods_1 <- function()
{
    return( c( 'ann', 'rf', 'svm' ) )
}

get_methods_2 <- function()
{
    return( c( 'ann', 'rf', 'svm', 'bioclim', 'maxent' ) )
}

get_rasta_state_000 <- function() #111
{
    return( c( 0, 0, 0 ) %>% get_rasta_state( ., get_methods_1() ) )
}

get_rasta_state_001 <- function() #110
{
    return( c( 0, 0, 1 ) %>% get_rasta_state( ., get_methods_1() ) )
}

get_rasta_state_010 <- function() #101
{
    return( c( 0, 1, 0 ) %>% get_rasta_state( ., get_methods_1() ) )
}

get_rasta_state_100 <- function() #011
{
    return( c( 1, 0, 0 ) %>% get_rasta_state( ., get_methods_1() ) )
}

get_rasta_state_011 <- function() #011
{
    return( c( 0, 1, 1 ) %>% get_rasta_state( ., get_methods_1() ) )
}

get_rasta_state_101 <- function() #101
{
    return( c( 1, 0, 1 ) %>% get_rasta_state( ., get_methods_1() ) )
}

get_rasta_state_110 <- function() #110
{
    return( c( 1, 1, 0 ) %>% get_rasta_state( ., get_methods_1() ) )
}

get_rasta_state_111 <- function() #111
{
    return( c( 1, 1, 1 ) %>% get_rasta_state( ., get_methods_1() ) )
}

get_bin_vec <- function( bin_string )
{
    base::stopifnot( base::is.character( bin_string ) )

    f <- base::paste( 'get_rasta_state', bin_string, sep = '_' )
    bin_vec <- base::do.call( f, list() )
    base::names( bin_vec ) <- get_methods_1()

    return( bin_vec )
}

get_wt_test_cons_states <- function( bin_string )
{
    base::stopifnot( base::is.character( bin_string ) )

    bin_vec <- get_bin_vec( bin_string )
    bin_vec[ base::which( bin_vec == 0 ) ] <- -1

    return( bin_vec )
}

get_agreement_111 <- function()
{
    get_rasta_state( get_rasta_state_111(),
        get_methods_1() ) %>%
        return(.)
}

get_agreement_110 <- function()
{
    get_rasta_state( get_rasta_state_110(),
        get_methods_1() ) %>%
        return(.)
}

get_agreement_101 <- function()
{
    get_rasta_state( get_rasta_state_101(),
        get_methods_1() ) %>%
        return(.)
}

get_agreement_011 <- function()
{
    get_rasta_state( get_rasta_state_011(),
        get_methods_1() ) %>%
        return(.)
}

get_attrbs_000 <- function()
{
    return( base::list( bin_vec = get_bin_vec( '000' ),
                        consensus = 0,
                        consensus_type = '3x0',
                        ensemble_agreement = get_agreement_111(),
                        votes = 0 ) )
}

get_attrbs_001 <- function()
{
    return( base::list( bin_vec = get_bin_vec( '001' ),
                        consensus = 0,
                        consensus_type = '2x1',
                        ensemble_agreement = get_agreement_111(),        
                        votes = 0 ) )
}

get_attrbs_010 <- function()
{
    return( base::list( bin_vec = get_bin_vec( '010' ),
                        consensus = 0,
                        consensus_type = '2x1',
                        ensemble_agreement = get_agreement_101(),                 
                        votes = 1 ) )
}

get_attrbs_101 <- function()
{
    return( base::list( bin_vec = get_bin_vec( '101' ),
                        consensus = 1,
                        consensus_type = '2x1',
                        ensemble_agreement = get_agreement_101(),
                        votes = 2 ) )
}

get_attrbs_011 <- function()
{
    return( base::list( bin_vec = get_bin_vec( '011' ),
                        consensus = 1,
                        consensus_type = '2x1',
                        ensemble_agreement = get_agreement_101(),
                        votes = 2 ) )
}

get_attrbs_110 <- function()
{
    return( base::list( bin_vec = get_bin_vec( '110' ),
                        consensus = 1,
                        consensus_type = '2x1',
                        ensemble_agreement = get_agreement_101(),
                        votes = 2 ) )
}

get_attrbs_111 <- function()
{
    return( base::list( bin_vec = get_bin_vec( '111' ),
                        consensus = 1,
                        consensus_type = '3x0',
                        ensemble_agreement = get_agreement_111(),
                        votes = 3 ) )
}

get_list_000 <- function()
{
    return( base::list( 'state0' = get_attrbs_000(),
                        'state0' = base::list( state = 0 ),
                        'bin000' = 0 ) )
}

get_list_111 <- function()
{
    return( base::list( 'state7' = get_attrbs_111(),
                        'state7' = base::list( state = 7 ),
                        'bin111' = 7 ) )
}

get_list_010 <- function()
{
    return( base::list( 'state2' = get_attrbs_010(),
                        'state2'= base::list( state = 2 ),
                        'bin010' = 2 ) )
}

get_list_101 <- function()
{
    return( base::list( 'state5' = get_attrbs_101(),
                        'state5' = base::list( state = 5 ),
                        'bin101' = 5 ) )
}

get_test_state_list <- function()
{
    return( base::list( 'bin000' = 0,
                       'state0' = base::list( bin_vec = get_bin_vec( '000' ),
                                              consensus = 0,
                                              consensus_type = '3x0',
                                              ensemble_agreement = get_agreement_111(),
                                              votes = 0,
                                              state = 0 ),
                       'bin100' = 4,
                       'state4' = base::list( bin_vec = get_bin_vec( '100' ),
                                              consensus = 0,
                                              consensus_type = '2x1',
                                              ensemble_agreement = get_agreement_011(),
                                              votes = 1,
                                              state = 4 ),
                       'bin010' = 2,
                       'state2' = base::list( bin_vec = get_bin_vec( '010' ),
                                              consensus = 0,
                                              consensus_type = '2x1',
                                              ensemble_agreement = get_agreement_101(),
                                              votes = 1,
                                              state = 2 ),
                       'bin001' = 1,
                       'state1' = base::list( bin_vec = get_bin_vec( '001' ),
                                              consensus = 0,
                                              consensus_type = '2x1',
                                              ensemble_agreement = get_agreement_110(),
                                              votes = 1,
                                              state = 1 ),
                       'bin110' = 6,
                       'state6' = base::list( bin_vec = get_bin_vec( '110' ),
                                              consensus = 1,
                                              consensus_type = '2x1',
                                              ensemble_agreement = get_agreement_110(),
                                              votes = 2,
                                              state = 6 ),
                       'bin101' = 5,
                       'state5' = base::list( bin_vec = get_bin_vec( '101' ),
                                              consensus = 1,
                                              consensus_type = '2x1',
                                              ensemble_agreement = get_agreement_101(),
                                              votes = 2,
                                              state = 5 ),
                       'bin011' = 3,
                       'state3' = base::list( bin_vec = get_bin_vec( '011' ),
                                              consensus = 1,
                                              consensus_type = '2x1',
                                              ensemble_agreement = get_agreement_011(),
                                              votes = 2,
                                              state = 3 ),
                       'bin111' = 7,
                       'state6' = base::list( bin_vec = get_bin_vec( '011' ),
                                              consensus = 1,
                                              consensus_type = '3x0',
                                              ensemble_agreement = get_agreement_111(),
                                              votes = 3,
                                              state = 7 ) ) )
}

get_rasta_state <- function( states_vec, names_vec )
{
    base::stopifnot( base::is.vector( states_vec ),
        base::is.vector( names_vec ) )

    rasta_states <- states_vec
    base::names( rasta_states ) <- names_vec

    return( rasta_states )
}
