evaluate_via_tss <- function( evaluation_tb, par_sets, train_fun, test_fun,
                              model_name, tag = "Optimizing" )
{
    sanitize_args( evaluation_tb, par_sets, train_fun, test_fun, model_name )

    rep_eval_grid <- get_eval_grid( evaluation_tb, par_sets, model_name )

    # Set up evaluation closure
    eval_closure <- get_eval_closure( evaluation_tb = evaluation_tb,
                                      par_sets = par_sets,
                                      rep_eval_grid = rep_eval_grid,
                                      train_fun = train_fun,
                                      test_fun = test_fun,
                                      model_name = model_name )

    # Maxent and SVM  predictions over large grids are parallelized
    parallel_run <- ( model_name == "maxent" | model_name == "svm" ) &
        base::nrow( rep_eval_grid ) > 99L

    # Message to display
    message <- base::paste0( tag, " '", model_name, "' " )                

    # Indices of parameter/replicate to tune
    tune_idx <- base::nrow( rep_eval_grid ) %>%
        base::seq_len(.)
    
    # Get tuning results
    run_eval_closure( eval_closure, parallel_run, message, tune_idx ) %>%
        return(.)
}

get_eval_grid <- function( evaluation_tb, par_sets, model_name )
{
    base::stopifnot( base::is.character( model_name ) )
    sanitize_tb( evaluation_tb, "evaluation_tb", model_name, "`get_eval_grid`" )
    sanitize_tb( par_sets, "par_sets", model_name, "`get_eval_grid`" )

    # Set tune grid: rep x pars if pars, or simply reps if no pars
    rep_ids <- get_unique_vals( evaluation_tb$rep_id )

    if( base::nrow( par_sets ) > 0 )
    {    
        par_idxs <- base::nrow( par_sets ) %>%
            base::seq_len(.)

        rep_eval_grid <- tidyr::expand_grid( par_idx = par_idxs,
                                             rep_id = rep_ids )
    }
    else
    {
        rep_eval_grid <- tidyr::expand_grid( rep_id = rep_ids )
    }

    return( rep_eval_grid )
}

get_eval_closure <- function( evaluation_tb, par_sets, rep_eval_grid, train_fun,
                              test_fun, model_name )
{
    sanitize_args( evaluation_tb, par_sets, train_fun, test_fun, model_name )

    # Assume fold IDs (e.g. spatial CV) in 'fold_id'
    folds <- get_unique_vals( evaluation_tb$fold_id )

    return( function( i )
    {
        # Select replicate
        eval_rep_id <- rep_eval_grid$rep_id[ i ]

        # Subset tibble according to replicate
        rep_tb <- evaluation_tb %>%
            dplyr::filter( rep_id == eval_rep_id )

        # Select parameter index
        par_idx <- rep_eval_grid$par_idx[ i ]


        # Set observations and empty suitability vectors
        observations <- rep_tb$resp_var

        suitability <- get_suitability( rep_tb,
                                        par_sets,
                                        par_idx,
                                        folds,
                                        eval_rep_id,
                                        train_fun,
                                        test_fun,
                                        model_name )

        valid_idx <- ! base::is.na( suitability )

        try_tibble <- base::try( build_results_tibble( observations[ valid_idx ],
                                                       suitability[ valid_idx ],
                                                       eval_rep_id,
                                                       par_idx ), silent = TRUE )

        sanitize_results( model_name, eval_rep_id, par_idx, suitability, try_tibble ) %>% 
            return(.)
    })
}

run_eval_closure <- function( eval_closure, parallel_run, message, tune_idx )
{
    sanitize_fn( eval_closure, "eval_closure", "", "message" )
    base::stopifnot( base::is.logical( parallel_run ) )
    base::stopifnot( base::is.character( message ) )
    base::stopifnot( is_positive_integer( tune_idx ) )
    
    if( parallel_run )
    {
        progressr::handler_cli() %>%
            progressr::handlers(.)

        tuning_results <- progressr::with_progress({
            p <- progressr::progressor( along = tune_idx, message = message )

            future.apply::future_lapply( X = tune_idx,
                                        FUN = function(i)
                                        {
                                            base::source( "scripts/train_test_lib.R" )
                                            base::source( "scripts/aux_lib.R" )
                                            p()
                                            eval_closure( i )
                                        },
                                        future.packages = c( "magrittr",
                                                             "pROC",
                                                             "PresenceAbsence",
                                                             "e1071",
                                                             "maxnet",
                                                             "predicts" ) ) })

        dplyr::bind_rows( tuning_results ) %>%
            return(.)
    }

    # Other models are serialized
    else
    {
        tuning_results <- cli::cli_progress_along( tune_idx, message ) %>%
            base::lapply( ., eval_closure ) %>%
            dplyr::bind_rows(.) %>%
            return(.)
    }
}

get_suitability <- function( rep_tb, par_sets, par_idx, folds, eval_rep_id, train_fun,
                             test_fun, model_name )
{
    base::stopifnot( is_positive_integer( par_idx ) |
                     is_positive_integer( folds ) |
                     is_positive_integer( eval_rep_id ) )

    base::stopifnot( base::is.character( model_name ) )

    sanitize_tb( rep_tb, "rep_tb", model_name, "-`get_suitability`" )
    sanitize_tb( par_sets, "par_sets", model_name, "-`get_suitability`" )
    
    sanitize_fn( train_fun, "train_fun", model_name, "-`get_suitability`" )
    sanitize_fn( test_fun, "test_fun", model_name, "-get_suitability`" )
    
    # Set empty suitability vector
    suitability <- base::length( rep_tb$resp_var ) %>%
        base::rep( NA, . )

    if( base::nrow( par_sets ) > 0 )
    {
        par_set <- par_sets[ par_idx, ]
    }
    else
    {
        par_set <- par_sets
    }

    for ( f in folds )
    {
        # Select training set
        train_idx <- rep_tb$fold_id != f

        # Select testing set
        test_idx <- ! train_idx
        
        # Train model
        try_fit <- base::try( train_fun( rep_tb = rep_tb,
                                         train_idx = train_idx,
                                         my_pars = par_set ), silent = TRUE )
        
        if( base::inherits( try_fit, "try-error" ) )
        {
            handle_eval_errors( par_set, model_name, eval_rep_id, f, try_fit )
            next
        }

        try_predict <- base::try( test_fun( rep_tb = rep_tb,
                                            test_idx = test_idx,
                                            fit = try_fit ), silent = TRUE )

        if( base::inherits( try_predict, "try-error" ) )
        {
            handle_eval_errors( par_set, model_name, eval_rep_id, f, try_predict )
        }
        else
        {
            suitability[ test_idx ] <- try_predict
        }
    }

    return( suitability )
}

handle_eval_errors <- function( par_set, model_name, eval_rep_id, f, try_obj )
{
    sanitize_tb( par_set, "par_set", model_name, "-`handle_fit_errors`" )
    base::stopifnot( base::is.character( model_name ) )
    base::stopifnot( is_positive_integer( eval_rep_id ) )
    base::stopifnot( is_positive_integer( f ) )
    base::stopifnot( base::class( try_obj ) == "try-error" )
    sanitize_tb( par_set, "par_set", model_name, "-`handle_fit_errors`" )

    par_names <- base::names( par_set )

    par_str <- base::ncol( par_set ) %>%
        base::seq_len(.) %>%
        base::sapply( ., function( j )
        { base::paste( par_names[ j ], par_set[ 1, j ], sep = "=" ) } ) %>%
        base::paste( ., collapse = " " )

    message( "Model '", model_name,
             "' failed in rep=", eval_rep_id,
             ", fold=", f,
             " for parameter set:", par_str, "\n",
             base::as.character( try_obj ) )

    base::invisible( NULL )
}


build_results_tibble <- function( observations, suitability, rep_id, par_idx )
{
    # Sanitize parameters
    if ( ! base::is.numeric( rep_id ) )
    {
        stop( "``rep_id` must be numeric vectors; got ", base::typeof( rep_id ) )
    }

    if ( ! base::is.numeric( par_idx ) )
    {
        stop( "``par_idx` must be a tibble; got ", base::typeof( par_idx ) )
    }
    
    # Validate vector structure
    if ( ! base::is.numeric( observations ) || ! base::is.numeric( suitability ) )
    {
        stop( "`observations` and `suitability` must be numeric vectors." )
    }

    if ( base::length( observations ) != base::length( suitability ) )
    {
        stop( "`observations` and `suitability` must have the same length." )
    }

    # Use exactly the same complete cases in all calculations
    keep <- ! base::is.na( observations ) & ! base::is.na( suitability )
    observations <- observations[ keep ]
    suitability  <- suitability[ keep ]

    if ( base::length( observations ) == 0L )
    {
        stop( "No complete observation–suitability pairs remain." )
    }

    # Validate binary observations after removing NAs
    if (!base::all(observations %in% c(0, 1))) {
        stop("`observations` must contain only 0 and 1.")
    }

    assemble_results_tibble( observations, suitability, rep_id, par_idx ) %>%
        return(.)
}


# =============================================================================
# Internal validation helper for ENM evaluation metrics
# @keywords internal
validate_enm_inputs <- function(observations, suitability, rep_id, par_idx) {
    
    # Validate that observations is a numeric vector with nonzero length
    if ( ! base::is.numeric( observations ) || ! base::is.vector( observations ) )
    {
        stop( "`observations` must be a numeric vector.", call. = FALSE )
    }

    # Validate that suitability is a numeric vector with nonzero length
    if ( ! base::is.numeric( suitability ) || ! base::is.vector( suitability ) )
    {
        stop( "`suitability` must be a numeric vector.", call. = FALSE )
    }

    # Check both vectors have the same nonzero length
    if (base::length(observations) == 0L ||
        base::length(observations) != base::length(suitability)) {

        stop( "`observations` and `suitability` must have the same",
               "nonzero length.", call. = FALSE )
    }

    # Validate rep_id is a positive integer
    if ( ! is_positive_integer( rep_id ) )
    {
        stop( "`rep_id` must be one finite numeric value.", call. = FALSE )
    }

    # Validate par_idx is a positive integer
    if ( ! is_positive_integer( par_idx ) )
    {
        stop( "`par_idx` must be one finite numeric value.", call. = FALSE )
    }

    # Remove incomplete cases from both vectors
    keep <- stats::complete.cases(observations, suitability)
    observations <- observations[keep]
    suitability <- suitability[keep]

    if (base::length(observations) == 0L) {
        stop( "No complete observation-suitability pairs remain.", call. = FALSE )
    }

    # Validate binary observations after removing NAs
    if (! base::all( observations %in% c( 0, 1 ) ) )
    {
        stop( "`observations` must contain only 0 and 1.", call. = FALSE )
    }

    # Check suitability values are finite (not NA or Inf)
    if ( ! base::all( base::is.finite( suitability ) ) )
    {
        stop( "`suitability` must contain only finite values in",
               "non-missing pairs.", call. = FALSE )
    }

    # Require at least one presence and one absence for threshold-dependent metrics
    if ( ! base::any( observations == 1 ) || ! base::any( observations == 0 ) )
    {
        stop( "At least one presence and one absence are required.", call. = FALSE )
    }

    # Check suitability values for BSS (must be in [0, 1])
    if (! base::all( suitability >= 0 & suitability <= 1 ) )
    {
        stop( "`suitability` must contain probabilities in [0, 1] when",
               "computing Brier Skill Score.", call. = FALSE )
    }

    # Check for constant predictions (degenerate case)
    if (base::length(base::unique(suitability)) < 2L) {
        warning( "Suitability values are constant; some metrics may be undefined." )
    }

    presence_suitability <- suitability[ observations == 1 ]
    uniq_pres_suit <- base::unique( presence_suitability )
    
    # Check for degenerate presence-only or absence-only cases
    if (base::length(base::unique(presence_suitability)) < 2L) {
        warning( "Insufficient unique values in presence predictions; CBI may be undefined." )
    }

    # Check for degenerate reference Brier score case
    ref_prob <- base::mean( observations, na.rm = TRUE )
    
    if (ref_prob == 0 || ref_prob == 1) {
        warning( "Reference probability is at boundary; BSS may be undefined." )
    }

    # Check for degenerate ROC AUC case
    unique_suit <- base::unique( suitability )
    
    if (base::length(unique_suit) < 2L) {
        warning( "Unique suitability values are insufficient; PR AUC and",
                  "ROC AUC may be undefined." )
    }

    # Check for degenerate CBI case
    if (base::length(base::unique(suitability)) < 2L ||
        base::length(base::unique(presence_suitability)) < 2L) {
        warning( "CBI cannot be estimated from constant or insufficient",
                  "presence predictions." )
    }

    # Check for degenerate BSS case
    if (ref_prob == mean(suitability, na.rm = TRUE)) {
        warning( "Model and reference probabilities are identical; BSS is undefined." )
    }

    base::list( observations = observations,
                suitability = suitability,
                ref_prob = ref_prob ) %>%
        return(.)
}


get_tss_statistics <- function( observations, suitability )
{
    thresh_data <- tibble::tibble(
        ID = base::seq_along(observations),
        obs = observations,
        pred = suitability
    )

    # Sort unique suitability values for thresh search
    thresh_intervals <- base::unique( suitability ) %>%
        base::sort( ., decreasing = FALSE )

    tryCatch(
    {
        thresh_stats <- PresenceAbsence::optimal.thresholds( DATA = thresh_data,
                                                                opt.methods = "MaxSens+Spec",
                                                                thresh = thresh_intervals,
                                                                which.model = 1,
                                                                na.rm = FALSE )
        
        # Extract the MaxSens+Spec thresh (first occurrence)
        thresh <- thresh_stats$pred[ thresh_stats$Method == "MaxSens+Spec" ][ 1L ]

        if ( base::length( thresh ) == 0L || ! is_positive_numeric( thresh ) )
        {
            # Fallback: use midpoint of range or first unique value
            thresh <- base::range( suitability, na.rm = TRUE ) %>%
                base::mean( ., na.rm = TRUE)
        }

        confusion <- PresenceAbsence::cmx( DATA = thresh_data,
                                           thresh = thresh,
                                           which.model = 1,
                                           na.rm = FALSE )
        
        tn <- base::unname( confusion["0", "0"] )
        fp <- base::unname( confusion["1", "0"] )
        fn <- base::unname( confusion["0", "1"] )
        tp <- base::unname( confusion["1", "1"] )

        # Handle edge cases for sensitivity and specificity denominators
        sensitivity_denominator <- tp + fn
        
        if (sensitivity_denominator > 0) {
            sensitivity <- tp / sensitivity_denominator
        } else {
            sensitivity <- NA_real_
        }

        specificity_denominator <- tn + fp
        
        if (specificity_denominator > 0) {
            specificity <- tn / specificity_denominator
        } else {
            specificity <- NA_real_
        }

        omission_denominator <- tn + fn
        
        if (omission_denominator > 0) { 
            false_omission_rate <- fn / omission_denominator
        } else { 
            false_omission_rate <- NA_real_
        }

        tss <- sensitivity + specificity - 1

        base::list( thresh = thresh,
                    tn = tn,
                    fp = fp,
                    fn = fn,
                    tp = tp,
                    sens = sensitivity,
                    spec = specificity,
                    for = false_omission_rate,
                    tss = tss ) %>%
            return(.)
    }, error = function(e) {
        # Return NA values if PresenceAbsence fails for any reason
        base::list( thresh = NA_real_,
                    tn = NA_integer_,
                    fp = NA_integer_,
                    fn = NA_integer_,
                    tp = NA_integer_,
                    sens = NA_real_,
                    spec = NA_real_,
                    for = NA_real_,
                    tss = NA_real_ ) %>%
            return(.)
    } )
}

get_pr_auc <- function( observations, suitability )
{
    # Validate inputs before computation
    
    base::stopifnot( 
        is_positive_integer( length(observations) ),
        is_positive_integer( length(suitability) )
    )
    
    base::stopifnot( 
        all( c( "numeric" %in% typeof( observations ),
                 "numeric" %in% typeof( suitability ) ) )
    )

    # Extract positive and negative class scores
    
    positive_scores <- suitability[ observations == 1 ]
    negative_scores <- suitability[ observations == 0 ]

    # Check for constant predictions (degenerate case)
    
    if( base::length(base::unique(suitability)) < 2L )
    {
        warning( "PR AUC is undefined for constant suitability values; returning NA." ) %>%
            return(.)
    }

    # Handle degenerate ROC curve cases
    
    if (base::length(negative_scores) == 0L | base::length(positive_scores) == 0L )
    {
        warning( "One or both classes have zero observations; PR AUC is undefined." ) %>%
            return(.)
    }

    # Compute partial ROC curve
    
    pr <- PRROC::pr.curve( scores.class0 = positive_scores,
                           scores.class1 = negative_scores,
                           curve = FALSE )

    base::as.numeric( pr$auc.integral ) %>%
        return(.)
}

get_continuous_boyce <- function( observations, suitability )
{
    # Validate inputs before computation
    
    base::stopifnot( 
        is_positive_integer( length(observations) ),
        is_positive_integer( length(suitability) )
    )
    
    base::stopifnot( 
        all( c( "numeric" %in% typeof( observations ),
                 "numeric" %in% typeof( suitability ) ) )
    )

    # Extract presence-only predictions
    
    presence_suitability <- suitability[ observations == 1 ]
    uniq_pres_suit <- base::unique( presence_suitability )
    
    if (base::length( uniq_pres_suit ) < 2L) {
        return( NA_real_ )
    }

    # Compute Continuous Boyce Index
    
    boyce <- base::suppressWarnings( ecospat::ecospat.boyce( fit = suitability, obs = presence_suitability, nclass = 0, window.w = "default", res = 100, PEplot = FALSE, rm.duplicate = TRUE, method = "spearman" ) )

    cbi <- base::as.numeric( boyce$Spearman.cor )[ 1L ]

    if (base::length( cbi ) == 0L || ! base::is.finite( cbi )) {
        warning( "The Continuous Boyce Index could not be estimated; returning NA." ) %>%
            return(.)
    }

    return( cbi )
}

get_brier_skill_score <- function( observations, suitability, ref_probability = NULL )
{
    # observations: binary vector where 1=presence and 0=absence
    
    if ( base::any( suitability < 0 | suitability > 1 ) )
    {
        stop( "Brier Skill Score requires `suitability` to contain probabilities in [0, 1].", call. = FALSE )
    }

    if ( base::is.null( ref_probability ) )
    {
        ref_probability <- base::mean( observations )
    }

    prob_is_frac <- ( ref_probability < 0 || ref_probability > 1 )
    
    if ( ! is_positive_numeric( ref_probability ) || ! prob_is_frac )
    {
        stop( "`ref_probability` must be one finite value in [0, 1].", call. = FALSE )
    }

    model_bs <- base::mean( ( suitability - observations ) ^2 )
    
    # Handle case where ref Brier score is zero (perfect random forecast)
    if ( ref_probability == base::mean( observations ) )
    {
        ref_obs_mean <- base::mean( observations, na.rm = TRUE )

        if ( ! base::is.na( ref_obs_mean ) && ! ref_obs_mean %in% c( 0, 1 ) )
        {
            ref_bs <- base::mean( ( ref_probability - observations ) ^ 2 )

            # If model predictions match the prevalence exactly, BSS is undefined
            if ( model_bs == ref_bs )
            {
                warning( "Model and ref probabilities are identical; Brier Skill Score is undefined." )                

                return( NA_real_ )
            }
        } else {
            # Ref probability equals observation mean, so BS_ref would be 0
            warning( "Ref Brier Skill Score is zero (perfect random forecast); BSS is undefined.", call. = FALSE )

            return( NA_real_ )
        }
    }

    ref_bs <- base::mean( ( ref_probability - observations ) ^ 2 )

    if ( base::is.na( ref_bs ) )
    {
        warning( "Ref Brier Skill Score is NA; returning NA for BSS.", call. = FALSE )

        return( NA_real_ )
    }

    # Handle division by zero case
    if ( ref_bs == 0 )
    {
        warning( "Ref Brier Skill Score is zero; Brier Skill Score is undefined." )

        return( NA_real_ )
    }

    bss <- 1 - model_bs / ref_bs
    
    # Handle degenerate case where both scores are equal (BSS = 0)
    if ( ! base::is.finite( bss ) )
    {
        warning( "Brier Skill Score is undefined due to division by zero; returning NA.", call. = FALSE )

        return( NA_real_ )
    }

    return( bss )
}

get_roc_auc <- function( observations, suitability )
{
    # Validate inputs before computation
    
    base::stopifnot( 
        is_positive_integer( length(observations) ),
        is_positive_integer( length(suitability) )
    )
    
    base::stopifnot( 
        all( c( "numeric" %in% typeof( observations ),
                 "numeric" %in% typeof( suitability ) ) )
    )

    # Check for constant predictions (degenerate case)
    
    if (base::length(base::unique(suitability)) < 2L) {
        warning( "ROC AUC is undefined for constant suitability values; returning NA." ) %>%
            return(.)
    }

    presences <- suitability[ observations == 1 ]
    absences <- suitability[ observations == 0 ]
    
    # Handle case where one class has no predictions (degenerate ROC curve)
    if (base::length(presences) < 2L || base::length(absences) < 2L) {
        warning( "One or both classes have insufficient observations; ROC AUC is undefined." ) %>%
            return(.)
    }

    # Compute ROC object
    
    roc_obj <- base::suppressMessages( pROC::roc( response = observations, predictor = suitability, levels = c(0, 1), direction = "<", auc = TRUE, plot = FALSE ) )

    is_trycatch <- inherits( tryCatch({ roc_obj$auc }, error = function(e) NA_real_ ), "error" )
    
    if (is_trycatch) { 
        warning( "ROC AUC could not be computed; returning NA." ) %>%
            return(.)
    }

    base::as.numeric( roc_obj$auc ) %>%
        return(.)
}


assemble_results_tibble <- function( observations, suitability, rep_id, par_idx, reference_probability = NULL ) {

    # Validate inputs first (returns list with validated data and warnings already issued)
    validated <- validate_enm_inputs( observations = observations, suitability = suitability, rep_id = rep_id, par_idx = par_idx )

    observations <- validated$observations
    suitability <- validated$suitability
    
    # Compute TSS statistics (threshold-dependent metrics)
    tss_statistics <- get_tss_statistics( observations = observations, suitability = suitability )

    # Compute PR AUC (threshold-independent metric for positive class)
    pr_auc <- get_pr_auc( observations = observations, suitability = suitability )

    # Compute Continuous Boyce Index (presence-only comparison with all sites)
    continuous_boyce <- get_continuous_boyce( observations = observations, suitability = suitability )

    # Compute Brier Skill Score (relative to reference forecast)
    brier_skill_score <- get_brier_skill_score( observations = observations, suitability = suitability, reference_probability = reference_probability )

    # Compute ROC AUC for backward compatibility with existing code expecting this column
    roc_auc <- get_roc_auc( observations = observations, suitability = suitability )

    tibble::tibble( par_idx = par_idx, rep_id = rep_id, pa.cutoff = tss_statistics$thresh, pa.tn = tss_statistics$tn, pa.fp = tss_statistics$fp, pa.fn = tss_statistics$fn, pa.tp = tss_statistics$tp, pa.sens = tss_statistics$sens, pa.spec = tss_statistics$spec, pa.for = tss_statistics$for, pa.tss = tss_statistics$tss, pr.auc = pr_auc, roc.auc = roc_auc, cbi = continuous_boyce, bss = brier_skill_score )
}    

sanitize_results <- function( model_name, eval_rep_id, par_idx, suitability, try_tibble )
{
    base::stopifnot( base::is.character( model_name ) )
    base::stopifnot( is_positive_integer( eval_rep_id ) )
    base::stopifnot( is_positive_integer( par_idx ) )

    if ( ! base::inherits( try_tibble, "try-error" ) )
    {
        return( try_tibble )
    }
    
    suitability_summary <- base::summary( suitability ) %>% utils::capture.output(.) %>% base::paste0(. , collapse = "\n")
    
    message( "Failed to get valid confusion matrix stats for model '", model_name, "' replicate ID=", eval_rep_id, "\n", "Suitability summary:\n", suitability_summary, "Error message:\n", base::as.character( try_tibble ))
    
    get_na_tibble( eval_rep_id, par_idx ) %>% return(.)
}

get_na_tibble <- function( rep_id, par_idx )
{
    stopifnot( base::is.numeric( rep_id ) | base::is.numeric( par_idx ) )

    tibble::tibble( par_idx = par_idx, rep_id = rep_id, pa.cutoff = NA, pa.tn = NA_integer_, pa.fp = NA_integer_, pa.fn = NA_integer_, pa.tp = NA_integer_, pa.sens = NA_real_, pa.spec = NA_real_, pa.for = NA_real_, pa.tss = NA_real_, pr.auc = NA_real_, roc.auc = NA_real_, cbi = NA_real_, bss = NA_real_ ) %>% return(.)
}    

sanitize_args <- function( evaluation_tb, par_sets, train_fun, test_fun, model_name )
{
    base::stopifnot( base::is.character( model_name ) )

    sys_call <- base::sys.call( sys.parent( 1 ) )
    
    message <- base::paste0( "- called from ", sys_call[[ 1 ]], ", client of ", sys_call[[ 2 ]] )
    
    sanitize_tb( evaluation_tb, "evaluation_tb", model_name, message )
    sanitize_tb( par_sets, "par_sets", model_name, message )
    sanitize_fn( train_fun, "train_fun", model_name, message )
    sanitize_fn( test_fun, "test_fun", model_name, message )
    
    base::invisible( NULL )
}
