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

    if ( ! base::all( observations %in% c( 0, 1 ) ) )
    {
        stop( "`observations` must contain only 0, 1, or NA." )
    }

    if ( base::any( suitability < 0 | suitability > 1 ) )
    {
        stop( "`suitability` must contain only values in [0, 1] or NA." )
    }

    if ( ! base::all( c( 0, 1 ) %in% base::unique( observations ) ) )
    {
        stop( "Both observed absences (0) and presences (1) are required." )
    }

    assemble_results_tibble( observations, suitability, rep_id, par_idx ) %>%
        return(.)
}

assemble_results_tibble <- function( observations, suitability, rep_id, par_idx ) 
{
    base::stopifnot( base::is.numeric( observations ) |
                     base::is.numeric( suitability ) |
                     base::is.numeric( rep_id ) |
                     base::is.numeric( par_idx ) )
    
    # Compute AUC
    roc_obj <- base::suppressMessages( pROC::roc( response = observations,
                                                  predictor = suitability,
                                                  levels = c( 0, 1 ),
                                                  direction = "<",
                                                  auc = TRUE,
                                                  plot = FALSE ) )

    # Compute TSS and associated threshold (Youden's coefficient )
    thresh_dat <- base::data.frame( ID = base::seq_along( observations ),
                                    obs = observations,
                                    pred = suitability )

    thresh_range <- base::range( suitability, na.rm = TRUE )

    thresh_intervals <- base::unique( suitability ) %>%
        base::sort( ., decreasing = FALSE )
    
    thresh_stats <- PresenceAbsence::optimal.thresholds( DATA = thresh_dat,
                                                         opt.methods = c( "MaxSens+Spec" ),
                                                         threshold = thresh_intervals,
                                                         which.model = 1,
                                                         na.rm = FALSE )

    tss_thresh <- thresh_stats$pred[ thresh_stats$Method == "MaxSens+Spec" ][1]
    
    # Compute confusion matrix
    cm <- PresenceAbsence::cmx( DATA = thresh_dat,
                                threshold = tss_thresh,
                                which.model = 1,
                                na.rm = FALSE )

    tn <- base::unname( cm["0", "0"] )     # True negatives
    fp <- base::unname( cm["1", "0"] )     # False positives
    fn <- base::unname( cm["0", "1"] )     # False negatives
    tp <- base::unname( cm["1", "1"] )     # True positives

    sens <- tp / ( tp + fn )               # Sensitivity
    spec <- tn / ( tn + fp )               # Specificity
    for_rate <- fn / ( tn + fn )           # False Omission Rate
    tss <- sens + spec - 1                 # True Skill Statistic
    auc <- base::as.numeric( roc_obj$auc ) # Area Under the ROC Curve

    tibble::tibble( par_idx = par_idx,
                    rep_id = rep_id,
                    pa.cutoff = tss_thresh,
                    pa.tn = tn,
                    pa.fp = fp,
                    pa.fn = fn,
                    pa.tp = tp,
                    pa.sens = sens,
                    pa.spec = spec,
                    pa.for = for_rate,
                    pa.tss = tss,
                    pr.auc = auc ) %>%
        return(.)
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
                            pa.tn = NA,
                            pa.fp = NA,
                            pa.fn = NA,
                            pa.tp = NA,
                            pa.sens = NA,
                            pa.spec = NA,
                            pa.for = NA,
                            pa.tss = NA,
                            pr.auc = NA ) )
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
