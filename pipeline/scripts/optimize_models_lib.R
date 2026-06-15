main <- function()
{
    # Prepare parallelization
    future::plan( multisession, workers = get_num_cores( N_CORES ) )

    for( code in CUTOUT_CODES )
    {
        # Open log file to which warnings will be silently directed
        log_con <- base::paste0( "optimization_warnings_", code, ".log" ) %>% 
            get_log_con(.)

        # Store closure that handles warnings
        handle_warnings <- get_warning_handler( log_con, code )
        
        message( "Processing '", code, "' data" )

        withCallingHandlers(
        {
            process_cutout( code )
        },
        warning = handle_warnings )

        message( "DONE!" )
        message()
        
        base::close( log_con )
    }

    # Shutdpwn
    message( "Cleaning up" )
    
    if( H2O_WAS_RUN ) h2o::h2o.shutdown( prompt = FALSE )
    
    future::plan( sequential )
    base::gc()
}

process_cutout <- function( code )
{
    # Load and prepare globals, including tibbles
    set_tibbles( code )

    # Get tuning results
    tuning_results <- base::lapply( EVAL_MODELS, tune_models )
    base::names( tuning_results ) <- EVAL_MODELS
    
     Save tuning results if any of the models was tuned
    if( SAVE_TUNING_RESULTS )
    {
        save_rds( tuning_results,
                 TUNING_FNAME,
                 code,
                 OUTPUT_ENV )
    }

    # Get evaluation tibble, capturing warnings
    tss_eval_tb <- get_tss_eval_tb( tuning_results )

    # Save TSS evaluation tibble
    save_rds( tss_eval_tb,
              TSS_EVAL_FNAME,
              code,
              OUTPUT_ENV )
}
        
set_tibbles <- function( code )
{
    # Load and prepare folds tibble corresponding to the cutout code
    folds_tibble <- base::paste0( "folds_tb_", code, ".rds" ) %>%
        read_rds( ., OUTPUT_ENV ) %>%
        dplyr::select( c( rep_id,
                          resp_var,
                          fold_id,
                          lon,
                          lat,
                          dplyr::all_of( LAYER_CODES ) ) )

    # Set number of first replicates to be used in model optimization
    optm_reps <- FRM_COUNT + FOR_COUNT

    # Select distinct rep numbers
    folds_tb_reps <- folds_tibble %>%
        dplyr::distinct( rep_id ) %>%
        base::nrow(.)

    # Check if enough non-optimization reps exist 
    if( folds_tb_reps <= optm_reps )
    {
        stop( "Number of non-optimization reps in folds tibble (",
              folds_tb_reps,
              ") is no greater than the number of optimization reps (",
              optm_reps,
              "); please reset SEL_COUNT and TUN_COUNT" ) 
    }

    # Set global tibbles
    FRM_CHOICE_TB <<- folds_tibble %>%
        dplyr::filter( rep_id <= FRM_COUNT )

    FOR_CHOICE_TB <<- folds_tibble %>%
        dplyr::filter( rep_id > FRM_COUNT & rep_id <= optm_reps )

    # Select all non-optimization reps
    if( EVAL_COUNT < 0L ) 
    {
        eval_idx <- folds_tibble$rep_id > optm_reps
    }
    # Select one non-optimization rep
    else if( EVAL_COUNT < 1L )
    {
        eval_idx <- folds_tibble$rep_id == optm_reps + 1L
    }
    # Use selected number of non-optimization reps
    else
    {
        eval_idx <- c( folds_tibble$rep_id > optm_reps &
            folds_tibble$rep_id <= optm_reps + EVAL_COUNT )
    }

    # Set TSS_EVAL tibble
    TSS_EVAL_TB <<- folds_tibble[ eval_idx, ]

    return( TRUE )
}

# Polymorphically call model tuning functions
tune_models <- function( model_name )
{
    tuning_results <- base::paste0( "tune_", model_name ) %>% 
        base::do.call( ., base::list() ) %>%
        return(.)
}

# Get tibble with confusion matrix stats after binarization via TSS
get_tss_eval_tb <- function( tuning_results )
{
    get_results_tibble <- get_eval_tb_closure( tuning_results )
    
    # Save tibble with evaluation results for non-optimizing pseudo-replicates
    results_tibble <- base::lapply( EVAL_MODELS, get_results_tibble ) %>%
        dplyr::bind_rows(.) %>%
        dplyr::select( -c( par_idx ) ) %>%
        dplyr::mutate_at( c( "rep_id",  "model" ), as.factor ) %>%
        return(.)
}

get_eval_tb_closure <- function( tuning_results )
{
    return( function( model_name )
    {
        # Return evaluation tibble
        evaluate_via_tss( evaluation_tb = TSS_EVAL_TB,
                          par_sets = tuning_results[[model_name]]$best_pars,
                          train_fun = get_model_fun( model_name, "trainer" ),
                          test_fun = get_model_fun( model_name, "tester" ),
                          model_name = model_name,
                          tag = "Evaluating" ) %>%
            dplyr::mutate( ., model = model_name ) %>%
            return(.)
    } )
}

# Load libraries
library( "dismo" )
library( "future" )
library( "mgcv" )

custom_libraries <- c( "aux_lib.R",
                       "evaluation_lib.R",
                       "optimize_aux_lib.R",
                       "tuning_lib.R",
                       "train_test_lib.R" )

for( library in custom_libraries  )
{
    base::file.path( SCRIPT_ENV, library  ) %>%
        source(.)
}
