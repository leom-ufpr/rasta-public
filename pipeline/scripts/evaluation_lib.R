evaluate_via_tss <- function( evaluation_tb, par_sets, train_fun, test_fun, model_name, tag = "Optimizing" )
{
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
        rep_eval_grid <- tidyr::expand_grid( rep_is = reps )
    }

    # Set up evaluation closure
    eval_closure <- get_eval_closure( evaluation_tb = evaluation_tb,
                                      par_sets = par_sets,
                                      rep_eval_grid = rep_eval_grid,
                                      train_fun = train_fun,
                                      test_fun = test_fun,
                                      model_name = model_name )

    # Return tibble with tuning results
    tune_idx <- base::nrow( rep_eval_grid ) %>%
        base::seq_len(.)

    message <- base::paste0( tag, " '", model_name, "' " )

    # Maxent and SVM  predictions over large grids are parallelized
    if( ( model_name == "maxent" | model_name == "svm" ) & base::nrow( rep_eval_grid ) > 99L )
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

get_eval_closure <- function( evaluation_tb, par_sets, rep_eval_grid, train_fun, test_fun, model_name )
{
    # Assume fold IDs (e.g. spatial CV) in 'fold_id'
    folds <- get_unique_vals( evaluation_tb$fold_id )

    return( function( i )
    {
        # Select replicate
        eval_rep_id <- rep_eval_grid$rep_id[ i ]

        # Subset tibble according to replicate
        rep_tb <- evaluation_tb %>%
            dplyr::filter( rep_id == eval_rep_id )

        # Set observations and empty suitability vectors
        observations <- rep_tb$resp_var
        suitability <- base::length( observations ) %>%
            base::rep( NA, . )

        # Select parameter index
        par_idx <- rep_eval_grid$par_idx[ i ]

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
                                                my_pars = par_set ),
                                 silent = TRUE )
            
            if( ! base::inherits( try_fit, "try-error" ) )
            {
                # If valid model, fill suitability vector
                suitability[ test_idx ] <- test_fun( rep_tb = rep_tb,
                                                        test_idx = test_idx,
                                                        fit = try_fit )
            }
            else
            {
                # Warn error and assign NA to suitability vector
                par_names <- base::names( par_set )

                par_str <- base::ncol( par_set ) %>%
                    base::seq_len(.) %>%
                    base::sapply( ., function( j )
                        { base::paste( par_names[ j ], par_set[ 1, j ], sep = "=" ) } ) %>%
                    base::paste( ., collapse = " " )

                warning( "Model '", model_name,
                         "' failed in rep=", eval_rep_id,
                         ", fold=", f,
                         " for parameter set:", par_str, "\n",
                         base::as.character( try_fit ) )

                suitability[ test_idx ] <- NA
            }
        }

        valid_idx <- ! base::is.na( suitability )

        try_tibble <- base::try( assemble_results_tibble( observations[ valid_idx ],
                                                          suitability[ valid_idx ],
                                                          eval_rep_id,
                                                          par_idx,
                                                          model_name ), silent = TRUE )

        # Warn error and assign NA line to evaluation tibble
        if( ! base::inherits( try_tibble, "try-error" ) )
        {
            return( try_tibble )
        }
        else
        {
            suitability_summary <- base::summary( suitability ) %>%
                utils::capture.output(.) %>%
                base::paste0( ., collapse = "\n" )
            
            warning( "Failed to get valid confusion matrix stats for model '", model_name,
                     "' replicate ID=", eval_rep_id, "\n",
                     "Suitability summary:\n",
                     suitability_summary )
            
            get_na_tibble( eval_rep_id, par_idx ) %>%
                return(.)
        }
    })
}

assemble_results_tibble <- function( observations, suitability, rep_id, par_idx, model_name )
{
    # Compute AUC
    roc <- base::suppressMessages( pROC::roc( observations,
                                              suitability,
                                              auc = TRUE,
                                              plot = FALSE ) )

    # Compute TSS and associated threshold (Youden's coefficient )
    thresh_dat <- base::data.frame( ID = base::seq_along( observations ),
                                    obs = observations,
                                    pred = suitability )

    thresh_range <- base::range( suitability, na.rm = TRUE )

    thresh_intervals <- base::seq( thresh_range[1],
                                   thresh_range[2],
                                   length.out = 1e3 )
    
    thresh_stats <- PresenceAbsence::optimal.thresholds( DATA = thresh_dat,
                                                         opt.methods = c( "MaxSens+Spec" ),
                                                         threshold = thresh_intervals )
    
    tss_thresh <- thresh_stats$pred[1]


    # Start results tibble
    results_tibble <- tibble::tibble( par_idx = par_idx,
                                      rep_id = rep_id,
                                      pa.cutoff = tss_thresh, 
                                      pr.auc = base::as.numeric( roc$auc ) )
    
    # Compute confusion matrix
    cm <- PresenceAbsence::cmx( DATA = thresh_dat,
                                threshold = tss_thresh,
                                which.model = 1 )

    # Get binomial metrics
    results_tibble %<>%
        dplyr::mutate( pa.tn = cm["0", "0"],
                       pa.fp = cm["1", "0"],
                       pa.fn = cm["0", "1"],
                       pa.tp = cm["1", "1"],
                       pa.sens = pa.tp / ( pa.tp + pa.fn ), # Sensitivity (TPR, recall)
                       pa.spec = pa.tn / ( pa.tn + pa.fp ), # Specificity (TNR)
                       pa.for = pa.fn / ( pa.tn + pa.fn ),  # False Omission Rate 
                       pa.tss = pa.sens + pa.spec - 1 )     # True Skill Statistic

    return( results_tibble )
}    

get_na_tibble <- function( rep_id, par_idx )
{
    return( tibble::tibble( par_idx = par_idx,
                            rep_id = rep_id,
                            pa.cutoff = NA, 
                            pr.auc = NA,
                            pa.tn = NA,
                            pa.fp = NA,
                            pa.fn = NA,
                            pa.tp = NA,
                            pa.sens = NA,
                            pa.spec = NA,
                            pa.for = NA,
                            pa.tss = NA ) )
}
                             
