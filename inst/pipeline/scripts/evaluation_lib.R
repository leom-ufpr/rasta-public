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
            return( . )
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

  if (!base::is.numeric(observations) || !base::is.vector(observations)) {

    base::stop("`observations` must be a numeric vector.", call. = FALSE)

  }

  if (!base::is.numeric(suitability) || !base::is.vector(suitability)) {

    base::stop("`suitability` must be a numeric vector.", call. = FALSE)

  }

  if (base::length(observations) == 0L ||
      base::length(observations) != base::length(suitability)) {

    base::stop(

      "`observations` and `suitability` must have the same nonzero length.",

      call. = FALSE

    )

  }

  if (!base::is.numeric(rep_id) || base::length(rep_id) != 1L ||
      base::is.na(rep_id) || !base::is.finite(rep_id)) {

    base::stop(

      "`rep_id` must be one finite numeric value.",

      call. = FALSE

    )

  }

  if (!base::is.numeric(par_idx) || base::length(par_idx) != 1L ||
      base::is.na(par_idx) || !base::is.finite(par_idx)) {

    base::stop(

      "`par_idx` must be one finite numeric value.",

      call. = FALSE

    )

  }

  keep <- stats::complete.cases(observations, suitability)

  observations <- observations[keep]

  suitability <- suitability[keep]

  if (base::length(observations) == 0L) {

    base::stop(

      "No complete observation-suitability pairs remain.",

      call. = FALSE

    )

  }

  # Validate binary observations after removing NAs
  if (!base::all(observations %in% c(0, 1))) {

    base::stop(

      "`observations` must contain only 0 and 1.",

      call. = FALSE

    )

  }

  # Check suitability values are finite (not NA or Inf)
  if (!base::all(base::is.finite(suitability))) {

    base::stop(

      "`suitability` must contain only finite values after removing missing pairs.",

      call. = FALSE

    )

  }

  # Require at least one presence and one absence for threshold-dependent metrics
  if (!base::any(observations == 1) ||
      !base::any(observations == 0)) {

    base::stop(

      "At least one presence and one absence are required.",

      call. = FALSE

    )

  }

  # Check suitability values for BSS (must be in [0, 1])
  if (!all(suitability >= 0 & suitability <= 1)) {

    base::stop(

      "`suitability` must contain probabilities in [0, 1] when computing Brier Skill Score.",

      call. = FALSE

    )

  }

  # Check for constant predictions (degenerate case)
  if (base::length(base::unique(suitability)) < 2L) {

    base::warning(

      "Suitability values are constant; some metrics may be undefined.",

      call. = FALSE

    )

  }

  # Check for degenerate presence-only or absence-only cases
  if (base::length(base::unique(presence_suitability <- suitability[observations == 1])) < 2L) {

    base::warning(

      "Insufficient unique values in presence predictions; CBI may be undefined.",

      call. = FALSE

    )

  }

  # Check for degenerate reference Brier score case
  ref_prob <- mean(observations, na.rm = TRUE)
  
  if (ref_prob == 0 || ref_prob == 1) {

    base::warning(

      "Reference probability is at boundary; BSS may be undefined.",

      call. = FALSE

    )

  }

  # Check for degenerate ROC AUC case
  unique_suit <- base::unique(suitability)
  
  if (base::length(unique_suit) < 2L) {

    base::warning(

      "Unique suitability values are insufficient; PR AUC and ROC AUC may be undefined.",

      call. = FALSE

    )

  }

  # Check for degenerate CBI case
  if (base::length(base::unique(suitability)) < 2L ||
      base::length(base::unique(presence_suitability)) < 2L) {

    base::warning(

      "The Continuous Boyce Index cannot be estimated from constant or insufficient presence predictions.",

      call. = FALSE

    )

  }

  # Check for degenerate BSS case
  if (ref_prob == mean(suitability, na.rm = TRUE)) {

    base::warning(

      "Model and reference probabilities are identical; BSS is undefined.",

      call. = FALSE

    )

  }

  base::list(

    observations = observations,

    suitability = suitability,

    ref_prob = ref_prob

  )
}


# =============================================================================
# Internal helper to compute Precision-Recall AUC using PRROC package
# @keywords internal
compute_pr_auc <- function(observations, suitability) {
  
  # observations: binary vector where 1=presence and 0=absence
  
  # Extract scores for each class (note: in PRROC terminology, 
  # 'scores.class0' contains the positive class predictions - this is counterintuitive!)
  positive_scores <- suitability[observations == 1]
  
  negative_scores <- suitability[observations == 0]

  if (base::length(base::unique(suitability)) < 2L) {
    base::warning(
      "PR AUC is undefined for constant suitability values; returning NA.",
      call. = FALSE
    )
    return(NA_real_)
  }

  # Handle case where one class has no predictions (degenerate ROC curve)
  if (base::length(negative_scores) == 0L || base::length(positive_scores) == 0L) {
    base::warning(
      "One or both classes have zero observations; PR AUC is undefined.",
      call. = FALSE
    )
    return(NA_real_)
  }

  pr <- PRROC::pr.curve(
    scores.class0 = positive_scores, # Note: counterintuitive naming in PRROC!
                                     # 'scores.class0' actually contains the POSITIVE class (1)
    scores.class1 = negative_scores,
    curve = FALSE
  )

  base::as.numeric(pr$auc.integral)
}


# =============================================================================
# Internal helper to compute Continuous Boyce Index using ecospat package
# @keywords internal
compute_continuous_boyce <- function(observations, suitability) {
  
  # observations: binary vector where 1=presence and 0=absence
  
  presence_suitability <- suitability[observations == 1]

  if (base::length(base::unique(suitability)) < 2L ||
      base::length(base::unique(presence_suitability)) < 2L) {
    base::warning(
      paste0(
        "The Continuous Boyce Index cannot be estimated from ",
        "constant or insufficient presence predictions; returning NA."
      ),
      call. = FALSE
    )
    return(NA_real_)
  }

  boyce <- base::suppressWarnings(
    ecospat::ecospat.boyce(
      fit = suitability,
      obs = presence_suitability,
      nclass = 0,
      window.w = "default",
      res = 100,
      PEplot = FALSE,
      rm.duplicate = TRUE,
      method = "spearman"
    )
  )

  value <- base::as.numeric(boyce$Spearman.cor)[1L]

  if (base::length(value) == 0L || !base::is.finite(value)) {
    base::warning(
      "The Continuous Boyce Index could not be estimated; returning NA.",
      call. = FALSE
    )
    return(NA_real_)
  }

  value
}


# =============================================================================
# Internal helper to compute Brier Skill Score directly from definition
# @keywords internal
compute_brier_skill_score <- function(
  observations,
  suitability,
  reference_probability = NULL
) {
  
  # observations: binary vector where 1=presence and 0=absence
  
  if (base::any(suitability < 0 | suitability > 1)) {
    base::stop(
      paste0(
        "Brier Skill Score requires `suitability` to contain ",
        "probabilities in [0, 1]."
      ),
      call. = FALSE
    )
  }

  if (base::is.null(reference_probability)) {
    reference_probability <- base::mean(observations)
  }

  if (!base::is.numeric(reference_probability) ||
      base::length(reference_probability) != 1L ||
      base::is.na(reference_probability) ||
      !base::is.finite(reference_probability) ||
      reference_probability < 0 ||
      reference_probability > 1) {
    base::stop(
      "`reference_probability` must be one finite value in [0, 1].",
      call. = FALSE
    )
  }

  model_bs <- base::mean((suitability - observations)^2)
  
  # Handle case where reference Brier score is zero (perfect random forecast)
  if (reference_probability == mean(observations)) {
    ref_obs_mean <- mean(observations, na.rm = TRUE)
    
    if (!is.na(ref_obs_mean) && !ref_obs_mean %in% c(0, 1)) {
      reference_bs <- base::mean((reference_probability - observations)^2)
      
      # If model predictions match the prevalence exactly, BSS is undefined
      if (model_bs == reference_bs) {
        base::warning(
          "Model and reference probabilities are identical; Brier Skill Score is undefined.",
          call. = FALSE
        )
        return(NA_real_)
      }
    } else {
      # Reference probability equals observation mean, so BS_reference would be 0
      base::warning(
        "Reference Brier score is zero (perfect random forecast); BSS is undefined.",
        call. = FALSE
      )
      return(NA_real_)
    }
  }

  reference_bs <- base::mean((reference_probability - observations)^2)

  if (base::is.na(reference_bs)) {
    base::warning(
      "Reference Brier score is NA; returning NA for BSS.",
      call. = FALSE
    )
    return(NA_real_)
  }

  # Handle division by zero case
  if (reference_bs == 0) {
    base::warning(
      "Reference Brier score is zero; Brier Skill Score is undefined.",
      call. = FALSE
    )
    return(NA_real_)
  }

  bss <- 1 - model_bs / reference_bs
  
  # Handle degenerate case where both scores are equal (BSS = 0)
  if (!is.finite(bss)) {
    base::warning(
      "Brier Skill Score is undefined due to division by zero; returning NA.",
      call. = FALSE
    )
    return(NA_real_)
  }

  bss
}


# =============================================================================
# Internal helper to compute TSS statistics using PresenceAbsence package
# @keywords internal
compute_tss_statistics <- function(observations, suitability) {
  
  threshold_data <- base::data.frame(
    ID = base::seq_along(observations),
    obs = observations,
    pred = suitability
  )

  # Sort unique suitability values for threshold search
  threshold_intervals <- base::sort(
    base::unique(suitability),
    decreasing = FALSE
  )

  tryCatch({
    threshold_stats <- PresenceAbsence::optimal.thresholds(
      DATA = threshold_data,
      opt.methods = "MaxSens+Spec",
      threshold = threshold_intervals,
      which.model = 1,
      na.rm = FALSE
    )
    
    # Extract the MaxSens+Spec threshold (first occurrence)
    threshold <- threshold_stats$pred[
      threshold_stats$Method == "MaxSens+Spec"
    ][1L]

    if (base::length(threshold) == 0L ||
        base::is.na(threshold) ||
        !base::is.finite(threshold)) {
      
      # Fallback: use midpoint of range or first unique value
      threshold <- mean(range(suitability, na.rm = TRUE), na.rm = TRUE)
    }

    confusion <- PresenceAbsence::cmx(
      DATA = threshold_data,
      threshold = threshold,
      which.model = 1,
      na.rm = FALSE
    )
    
    tn <- base::unname(confusion["0", "0"])
    fp <- base::unname(confusion["1", "0"])
    fn <- base::unname(confusion["0", "1"])
    tp <- base::unname(confusion["1", "1"])

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

    base::list(
      threshold = threshold,
      tn = tn,
      fp = fp,
      fn = fn,
      tp = tp,
      sensitivity = sensitivity,
      specificity = specificity,
      false_omission_rate = false_omission_rate,
      tss = tss
    )

  }, error = function(e) {
    # Return NA values if PresenceAbsence fails for any reason
    base::list(
      threshold = NA_real_,
      tn = NA_integer_,
      fp = NA_integer_,
      fn = NA_integer_,
      tp = NA_integer_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      false_omission_rate = NA_real_,
      tss = NA_real_
    )
  })
}


# =============================================================================
# Internal helper to compute ROC AUC using pROC package (for backward compatibility)
# @keywords internal
compute_roc_auc <- function(observations, suitability) {
  
  # observations: binary vector where 1=presence and 0=absence
  
  if (base::length(base::unique(suitability)) < 2L) {
    base::warning(
      "ROC AUC is undefined for constant suitability values; returning NA.",
      call. = FALSE
    )
    return(NA_real_)
  }

  # Handle case where one class has no predictions (degenerate ROC curve)
  if (base::length(suitability[observations == 1]) < 2L || 
      base::length(suitability[observations == 0]) < 2L) {
    base::warning(
      "One or both classes have insufficient observations; ROC AUC is undefined.",
      call. = FALSE
    )
    return(NA_real_)
  }

  roc_obj <- base::suppressMessages(pROC::roc(
    response = observations,
    predictor = suitability,
    levels = c(0, 1),
    direction = "<",
    auc = TRUE,
    plot = FALSE
  ))

  if (inherits(tryCatch({
    roc_obj$auc
  }, error = function(e) NA_real_), "error")) {
    
    base::warning(
      "ROC AUC could not be computed; returning NA.",
      call. = FALSE
    )
    return(NA_real_)
  }

  base::as.numeric(roc_obj$auc)
}


# =============================================================================
# Internal helper to assemble results tibble from all metrics
# @keywords internal
assemble_results_tibble <- function(
  observations,
  suitability,
  rep_id,
  par_idx,
  reference_probability = NULL
) {

  # Validate inputs first (returns list with validated data and warnings already issued)
  validated <- validate_enm_inputs(
    observations = observations,
    suitability = suitability,
    rep_id = rep_id,
    par_idx = par_idx
  )

  observations <- validated$observations
  suitability <- validated$suitability
  
  # Compute TSS statistics (threshold-dependent metrics)
  tss_statistics <- compute_tss_statistics(
    observations = observations,
    suitability = suitability
  )

  # Compute PR AUC (threshold-independent metric for positive class)
  pr_auc <- compute_pr_auc(
    observations = observations,
    suitability = suitability
  )

  # Compute Continuous Boyce Index (presence-only comparison with all sites)
  continuous_boyce <- compute_continuous_boyce(
    observations = observations,
    suitability = suitability
  )

  # Compute Brier Skill Score (relative to reference forecast)
  brier_skill_score <- compute_brier_skill_score(
    observations = observations,
    suitability = suitability,
    reference_probability = reference_probability
  )

  # Compute ROC AUC for backward compatibility with existing code expecting this column
  roc_auc <- compute_roc_auc(
    observations = observations,
    suitability = suitability
  )

  tibble::tibble(
    par_idx = par_idx,
    rep_id = rep_id,
    pa.cutoff = tss_statistics$threshold,
    pa.tn = tss_statistics$tn,
    pa.fp = tss_statistics$fp,
    pa.fn = tss_statistics$fn,
    pa.tp = tss_statistics$tp,
    pa.sens = tss_statistics$sensitivity,
    pa.spec = tss_statistics$specificity,
    pa.for = tss_statistics$false_omission_rate,
    pa.tss = tss_statistics$tss,
    pr.auc = pr_auc,  # Precision-Recall AUC (not ROC AUC!)
    roc.auc = roc_auc, # Added for backward compatibility - contains actual ROC AUC
    cbi = continuous_boyce,
    bss = brier_skill_score
  )

}    

sanitize_results <- function( model_name, eval_rep_id, par_idx, suitability,
                              try_tibble )
{
    base::stopifnot( base::is.character( model_name ) )
    base::stopifnot( is_positive_integer( eval_rep_id ) )
    base::stopifnot( is_positive_integer( par_idx ) )

    if( ! base::inherits( try_tibble, "try-error" ) )
    {
        return( try_tibble )
    }
    
    suitability_summary <- base::summary( suitability ) %>%
        utils::capture.output(.) %>%
        base::paste0( ., collapse = "\n" )
    
    message( "Failed to get valid confusion matrix stats for model '",
            model_name,
            "' replicate ID=", eval_rep_id, "\n",
            "Suitability summary:\n",
            suitability_summary,
            "Error message:\n",
            base::as.character( try_tibble ))
    
    get_na_tibble( eval_rep_id, par_idx ) %>%
        return(.)
}

get_na_tibble <- function( rep_id, par_idx )
{
    stopifnot( base::is.numeric( rep_id ) | base::is.numeric( par_idx ) )

    return( tibble::tibble( par_idx = par_idx,
                            rep_id = rep_id,
                            pa.cutoff = NA, 
                            pa.tn = NA_integer_,
                            pa.fp = NA_integer_,
                            pa.fn = NA_integer_,
                            pa.tp = NA_integer_,
                            pa.sens = NA_real_,
                            pa.spec = NA_real_,
                            pa.for = NA_real_,
                            pa.tss = NA_real_,
                            pr.auc = NA_real_,  # Precision-Recall AUC (not ROC!)
                            roc.auc = NA_real_, # Added for backward compatibility
                            cbi = NA_real_,     # Continuous Boyce Index
                            bss = NA_real_ )    # Brier Skill Score

}    

sanitize_args <- function( evaluation_tb, par_sets, train_fun, test_fun, model_name )
{
    base::stopifnot( base::is.character( model_name ) )

    sys_call <- base::sys.call( sys.parent( 1 ) )
    
    message <- base::paste0( "- called from ",
                            sys_call[[ 1 ]],
                            ", client of ",
                            sys_call[[ 2 ]] )
    
    sanitize_tb( evaluation_tb, "evaluation_tb", model_name, message )
    sanitize_tb( par_sets, "par_sets", model_name, message )
    sanitize_fn( train_fun, "train_fun", model_name, message )
    sanitize_fn( test_fun, "test_fun", model_name, message )
    
    base::invisible( NULL )
}
