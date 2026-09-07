# Model tuning functions
tune_ann <- function()
{
   # Initialize h2o and evaluation grid, supressing progress bar
    h2o::h2o.init( port = H2O_PORT,
                   nthreads = get_num_cores( N_CORES ),
                   strict_version_check = TRUE )

    h2o::h2o.no_progress()

    # Build grid
    eval_grid <- tidyr::expand_grid( size = ANN_SIZES,
                                     decay = ANN_DECAY )

    tuning_results <- tune_all( eval_grid, "ann" )

    H2O_WAS_RUN <- TRUE

    return( tuning_results )
}

tune_bioclim <- function()
{
    return( empty_tuning_results() )
}

tune_enfa <- function()
{
    sanitize_enfa()
    
    eval_grid <- tidyr::expand_grid( nf = ENFA_NF,
                                     ridge = ENFA_RIDGE )

    tune_all( eval_grid, "enfa" ) %>%
        return(.)
}

tune_gam <- function()
{
    tune_turbine( "gam" ) %>%
        return(.)
}

tune_glm <- function()
{
    tune_turbine( "glm" ) %>%
        return(.)
}

tune_gower <- function()
{
    return( empty_tuning_results() )
}

tune_mars <- function()
{
    tune_turbine( "mars" ) %>%
        return(.)
}

tune_maxent <- function()
{
    eval_grid <- tidyr::expand_grid( rm = MAXENT_RM,
                                     fc = MAXENT_FC )

    tune_all( eval_grid, "maxent" ) %>%
        return(.)
}

tune_svm <- function()
{
    eval_grid <- tidyr::expand_grid( cost = SVM_COST,
                                     gamma = SVM_GAMMA )

    tune_all( eval_grid, "svm" ) %>%
        return(.)
}

# Turbine model tuner
tune_turbine <- function( model_name )
{
    # The turbine model tuning grid has best formulas
    formula_grid <- get_formula_grid( model = model_name )

    tune_all( formula_grid, model_name ) %>%
        return(.)
}

tune_all <- function( par_sets, model_name )
{
    tuning_results <- evaluate_via_tss( evaluation_tb = FOR_CHOICE_TB,
                                        par_sets = par_sets,
                                        train_fun = get_model_fun( model_name, "trainer" ),
                                        test_fun = get_model_fun( model_name, "tester" ),
                                        model_name = model_name )

    # Return list with results of tuning steps
    get_tuning_list( tuning_results, par_sets ) %>%
        return(.)
}

# Assemble grid of best formula per replicate according to backward, BIC based selection
get_formula_grid <- function( model_name )
{
    selection_closure <- get_selection_closure( model_name )

    reps <- get_unique_vals( FRM_CHOICE_TB$rep_id )
    message <- base::paste0( "Selecting '", model_name, "' formula" )
    
    cli::cli_progress_along( reps, message ) %>%
        base::lapply( ., selection_closure ) %>%
        base::unlist(.) %>%
        base::unique(.) %>%
        tidyr::expand_grid( formula = . ) %>%
        return(.)
}

# Polymorphically call formula selection functions
get_selection_closure <- function( model_name )
{
    selection_fun <- get_model_fun( model_name, "selector" )
    formula_sel_fun <- base::paste0( "select_", model_name, "_formula" ) %>%
        base::get( ., envir = .GlobalEnv )

    return( function( sel_rep_id )
    {
        rep_tb <- FRM_CHOICE_TB %>%
            dplyr::filter( rep_id == sel_rep_id )

        object_list <- selection_fun( rep_tb )

        base::lapply( object_list, formula_sel_fun ) %>%
            return(.)
    } )
}

# Formula selection closures
get_gam_selector <- function()
{
    return( function( rep_tb )
    {
        gam_list <- base::lapply( GAM_GAMMA,
                                  function(x) { backward_gam_selector( x, rep_tb ) } )

        return( gam_list )
    } )
}

backward_gam_selector <- function( gamma, rep_tb )
{
    # select = TRUE - penalization to shrink unneeded smooths to ~0
    # gamma - >1 for stronger penalization; try 1.4–2
    mgcv::gam( formula = GAM_FORMULA,
               data = rep_tb,
               family = stats::binomial( link = "logit" ),
               method = "REML",
               select = TRUE, 
               gamma = gamma ) %>%
        return(.)
}

get_glm_selector <- function()
{
    return( function( rep_tb )
    {
        # Fit model with full formula
        full_glm <- stats::glm( formula = FULL_FORMULA,
                                data = rep_tb,
                                family = stats::binomial( link = "logit" ),
                                control = base::list( trace = FALSE ) )

        # Backward selection of significant terms via BIC
        bic <- base::nrow( rep_tb ) %>%
            base::log(.)
        
        MASS::stepAIC( full_glm,
                       direction = "backward",
                       trace = FALSE,
                       k = bic ) %>%
            base::list(.) %>%
            return(.)
    } )
}

get_mars_selector <- function()
{
    return( function( rep_tb )
    {
        earth::earth( formula = BASE_FORMULA,
                      data = rep_tb,
                      degree = 2,
                      glm = base::list( family = binomial ), # logistic response
                      thresh = 0.01 ) %>% 
            base::list(.) %>%
            return(.)
    } )
}

select_gam_formula <- function( best_gam )
{
    # Get full formula from the fitted model
    full_formula <- stats::formula( best_gam )
    
    # Get term labels on the RHS (right hand side )
    rhs_terms <- stats::terms( best_gam ) %>%
        base::attr( ., "term.labels" )
    
    # Smooth table (rows correspond to smooth terms)
    s_tab <- summary(best_gam)$s.table
    
    if ( ! base::is.null( s_tab ) && base::nrow( s_tab ) > 0 )
    {
        smooth_names <- base::rownames( s_tab )
        keep_smooth  <- s_tab[, "edf"] > GAM_EDF
        keep_smooth_names <- smooth_names[ keep_smooth ]
        
        # Keep parametric terms (non-smooth) as is
        parametric_terms <- base::setdiff( rhs_terms, smooth_names )
        
        # Build RHS with kept smooths + parametric terms
        rhs_keep <- c( parametric_terms, keep_smooth_names )
    }
    else
    {
        # No smooths; keep all parametric terms
        rhs_keep <- rhs_terms
    }

    # If RHS is empty, return nothing; else return formula
    if ( base::length( rhs_keep ) == 0L )
    {
        stop( "No variables were selected for GAM fitting." )
    }
    else
    {
        assemble_formula( rhs_keep ) %>%
            return(.) 
    }
}

select_glm_formula <- function( best_glm )
{
    best_glm$coefficients[ -1 ] %>%
        base::names(.) %>%
        assemble_formula(.) %>%
        return(.)
}

select_mars_formula <- function( best_mars )
{
    # Fit best MARS with backward selection
    mars_evimp <- earth::evimp( best_mars )
    
    # Return best formula selected via ANOVA
    base::attr( mars_evimp, "dimnames" )[[ 1 ]] %>%
        assemble_formula(.) %>%
        return(.)
}

get_tuning_list <- function( tuning_results, par_sets )
{
    # Eliminate row with NAs
    tuning_valid <- tuning_results %>%
        stats::na.omit(.)

    # If no rows left, return empty list
    if( base::nrow( tuning_valid ) < 1 )
    {
        warning( "Search grid did not include suitable parameters, please redefine" )
        return( base::list() )
    }

    # Count unique parameter indices
    par_idx_count <- get_unique_vals( tuning_valid$par_idx ) %>%
        base::length(.)

    if( par_idx_count > 1 )
    {
        # If more than one parameter combination remains, select worst case replicate,
        # i.e., the one with the highest false omission rate (pa.for)
        tuning_sel <- tuning_valid %>%
            dplyr::select( par_idx, pa.for ) %>%
            dplyr::group_by( par_idx ) %>%
            dplyr::summarise( pa.for = base::max( pa.for ) ) %>%
            dplyr::ungroup(.)
    }
    else
    {
        # If only one set of parameters were selected, assign valid tibble to selection
        # tibble
        tuning_sel <- tuning_valid
    }

    # Extract lowest pa.for from selection tibble
    min_pa.for <- base::min( tuning_sel$pa.for )

    if( min_pa.for > MA_FOR )
    {
        # Warns if lowest e,for is above maximum acceptable threshold
        warning( "Lowest False Omission Rate during tuning(",
                 min_pa.for,
                 ") was greater than minimum acceptable value(",
                 MA_FOR,
                 ")" )
    }

    # Select parameter set index that yields the lowest pa.for
    best_par_idx <- base::which.min( tuning_sel$pa.for ) %>%
        tuning_sel$par_idx[.]

    # Create logical column "best_par" in original tibble flagging best parameters
    # Return list with results tibble and grid element with best parameters
    best_par_vec <- c( tuning_results$par_idx == best_par_idx &
        tuning_results$pa.for == min_pa.for )

    # Set saving flag to true
    SAVE_TUNING_RESULTS <<- TRUE

    # Attach column flagging best parameter set and return results
    tuning_results %>%
        dplyr::mutate( best_par = best_par_vec ) %>%
            base::list( results = .,
                        par_sets = par_sets,
                        best_pars = par_sets[ best_par_idx, ] ) %>%
        return(.)
}

sanitize_enfa <- function()
{
    if( ! base::exists( "ENFA_NF" ) )
    {
        stop_on_error( "ENFA",
                       "ENFA_NF",
                       "vector to be set in configuration script" )
    }

    if( ! base::exists( "ENFA_RIDGE" ) )
    {
        stop_on_error( "ENFA",
                       "ENFA_RIDGE",
                       "vector to be set in configuration script" )
    }
    
    if( ! is_positive_integer( ENFA_NF ) )
    {
        stop_on_error( "ENFA",
                       "ENFA_NF",
                       "vector with positive integers" )
    }

    if( base::min( ENFA_NF ) < 2 )
    {
        stop_on_error( "ENFA",
                       "ENFA_NF",
                       "minimum number of factors for ENFA_NF is 2" )
    }

    if( base::max( ENFA_NF ) > base::length( LAYER_CODES ) )
    {
        stop_on_error( "ENFA",
                       "ENFA_NF",
                       "maximum NF must not exceed number of environmental layers" )
    }

    if( ! is_positive_numeric( ENFA_RIDGE ) )
    {
        stop_on_error( "ENFA",
                       "ENFA_RIDGE",
                       "vector with positive real numbers" )
    }

    invisible( NULL )
}

# Return list with empty results for aquarium models
empty_tuning_results <- function()
{
    base::list( results = tibble::tibble(),
                best_pars = tidyr::expand_grid() ) %>%
    return(.) 
}

# Assemble GAM, GLM and MARS formula
assemble_formula <- function( terms )
{
    base::sort( terms ) %>%
        base::paste( ., collapse = "+" ) %>%
        base::paste0( "resp_var ~ ", . ) %>%
        return(.)
}

# Kills program with error messages if tunable parameters are misspecified
stop_on_error <- function( model, parameter, expected )
{
    stop( "Wrong parameter type ", parameter,
          "passed to ", model,
          " - expects ", expected )
}

