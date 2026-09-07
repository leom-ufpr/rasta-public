# Trainer closures
get_ann_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
        sanitize_trainer_pars( rep_tb, train_idx, "ann" )
        sanitize_tb( my_pars, "my_pars", "ann", "trainer" )
   
        training_h2o <- rep_tb[ train_idx, ] %>%
            dplyr::mutate_at( "resp_var", as.factor ) %>%
            h2o::as.h2o(.)

        h2o::h2o.deeplearning( x = LAYER_CODES,
                               y = "resp_var",
                               training_frame = training_h2o,
                               distribution = "bernoulli",
                               activation = "Rectifier",
                               hidden = my_pars$size[1],
                               l2 = my_pars$decay[1],
                               epochs = ANN_EPOCHS,
                               seed = ANN_SEED ) %>%
            return(.)
    } )
}

get_bioclim_trainer <- function()
{
    return( function( rep_tb, train_idx, ... )
    {
        sanitize_trainer_pars( rep_tb, train_idx, "bioclim" )

        po_rep_tb <- get_pres_only_data( rep_tb, train_idx )

        predicts::envelope( po_rep_tb ) %>% 
            return(.)
    } )
}

get_enfa_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
        sanitize_trainer_pars( rep_tb, train_idx, "enfa" )
        sanitize_tb( my_pars, "my_pars", "enfa", "trainer" )

        nf <- my_pars$nf[1]
        ridge <- my_pars$ridge[1]

        # Fit PCA
        pca_fit <- ade4::dudi.pca( df = rep_tb[ train_idx, LAYER_CODES ],
                                   center = TRUE,
                                   scale = TRUE,
                                   scannf = FALSE,
                                   nf = nf )
        
        train_y <- rep_tb[ train_idx, "resp_var", drop = TRUE ]

        # Fit ENFA on the training PCA object and its 0/1 presence vector
        enfa_fit <- adehabitatHS::enfa( dudi = pca_fit,
                                        pr = train_y,
                                        scannf = FALSE,
                                        nf = nf - 1 )

        train_enfa_scores <- enfa_fit$li %>% base::as.matrix(.)

        # Define the occupied niche using training presences only
        occ_enfa_scores <- train_enfa_scores[ train_y == 1, , drop = FALSE ]
        niche_center <- base::colMeans( occ_enfa_scores )

        # Estimate covariance of presences in retained ENFA space; regularize
        # covariance
        niche_cov <- stats::cov( occ_enfa_scores )
        niche_cov_reg <- niche_cov + base::diag( ridge, nrow = nf, ncol = nf )

        # Protect against singular / non-finite covariance matrices.
        infinity_in_matrix <- ! base::is.finite( niche_cov_reg ) %>%
            base::any(.)
        
        if ( infinity_in_matrix )
        {
            stop( "Regularized ENFA covariance matrix contains non-finite values." )
        }

        # Invert the matrix
        sigma_inv <- tryCatch( base::solve( niche_cov_reg ),
                               error = function( e )
                               {
                                  stop( "Unable to invert the regularized ENFA",
                                        "covariance matrix. Try a larger `ridge`",
                                        "value than", ridge, "or a smaller `nf`",
                                        "value than", nf, ". Original error: ",
                                        base::conditionMessage(e) )
                               } )

        # Stores everything in a structure that reperesents the fitted model
        base::list( nf = nf,
                    pca_fit = pca_fit,
                    enfa_fit = enfa_fit,
                    niche_center = niche_center,
                    sigma_inv = sigma_inv ) %>%
            base::structure( ., class = "enfa_trainer" ) %>%
            return(.)
    })
}

get_gam_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
        sanitize_trainer_pars( rep_tb, train_idx, "gam" )
        sanitize_tb( my_pars, "my_pars", "gam", "trainer" )

        mgcv::gam( formula = get_training_formula( my_pars$formula[1] ),
                   data = rep_tb[ train_idx, ],
                   family = stats::binomial( link = "logit" ),
                   method = "REML",
                   select = FALSE ) %>%
            return(.)
    } )
}

get_glm_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
        sanitize_trainer_pars( rep_tb, train_idx, "glm" )
        sanitize_tb( my_pars, "my_pars", "glm", "trainer" )

        stats::glm( formula = get_training_formula( my_pars$formula[1] ),
                    data = rep_tb[ train_idx, ],
                    family = stats::binomial( link = "logit" ),
                    control = base::list( trace = FALSE ) ) %>%
            return(.)
    } )
}

get_gower_trainer <- function()
{
    return( function( rep_tb, train_idx, ... )
    {
        sanitize_trainer_pars( rep_tb, train_idx, "gower" )

        po_rep_mx <- get_pres_only_data( rep_tb, train_idx )
 
        dismo::domain( po_rep_mx ) %>% 
            return(.)
    } )
}

get_mars_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
        sanitize_trainer_pars( rep_tb, train_idx, "mars" )
        sanitize_tb( my_pars, "my_pars", "mars", "trainer" )

        earth::earth( formula = get_training_formula( my_pars$formula[1] ),
                      data = rep_tb[ train_idx, ],
                      glm = base::list( family = binomial ) ) %>%
            return(.)
    } )
}

get_maxent_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
        sanitize_trainer_pars( rep_tb, train_idx, "maxent" )
        sanitize_tb( my_pars, "my_pars", "maxent", "trainer" )

        pa_vector <- rep_tb$resp_var[ train_idx ] %>%
            base::as.numeric(.)

        maxnet::maxnet( p = pa_vector,
                        data = rep_tb[ train_idx, LAYER_CODES ],
                        regmult = my_pars$rm[1],
                        classes = my_pars$fc[1] ) %>%
            return(.)
    } )
}

get_svm_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
        sanitize_trainer_pars( rep_tb, train_idx, "svm" )
        sanitize_tb( my_pars, "my_pars", "svm", "trainer" )

        # Factorize response variable
        resp_var <- rep_tb$resp_var[ train_idx ] %>%
            base::factor(., levels = c( 0, 1 ) )

        fit <- e1071::svm( x = get_var_mx( rep_tb, train_idx ),
                           y = resp_var,
                           kernel = "radial",
                           cost = my_pars$cost[1],
                           gamma = my_pars$gamma[1],
                           probability = TRUE,
                           class_weights = "inverse" )

        base::attr( fit, "presence_level" ) <- "1"

        return( fit )
    } )
}

# Tester closures (model selection and evaluation)
get_ann_tester <- function()
{
    get_tester_for( "ann" ) %>%
        return(.)
}

get_bioclim_tester <- function()
{
    get_tester_for( "bioclim" ) %>%
        return(.)
}

get_enfa_tester <- function()
{
    get_tester_for( "enfa" ) %>%
        return(.)
}

get_gam_tester <- function()
{
    get_tester_for( "gam" ) %>%
        return(.)
}

get_glm_tester <- function()
{
    get_stats_tester() %>%
        return(.)
}

get_gower_tester <- function()
{
    get_tester_for( "gower" ) %>%
        return(.)
}

get_mars_tester <- function()
{
    get_stats_tester() %>%
        return(.)
}

get_maxent_tester <- function()
{
    get_tester_for( "maxent" ) %>%
        return(.)
}

get_svm_tester <- function()
{
    get_tester_for( "svm" ) %>%
        return(.)
}

# Generic tester
get_stats_tester <- function()
{
    get_tester_for( "stats" ) %>%
        return(.)
}

get_tester_for <- function( model_name )
{
    model_predictor <- get_model_fun( model_name, "predictor" )
    
    return( function( rep_tb, test_idx, fit )
    {
        model_predictor( model = fit,
                         data = rep_tb[ test_idx, LAYER_CODES ] ) %>%
            return(.)
    } )
}

# Predictors (used in training, testing, evaluations and generalizations)
# Model specific predictors
get_ann_predictor <- function()
{
    return( function( model, data, ...)
    {
        out_data <- base::nrow( data ) %>%
            base::rep( NA_real_, . )

        comp_rows <- stats::complete.cases( data )
        data_ok <- data[ comp_rows, , drop = FALSE ]
                
        h2o_pred <- h2o::as.h2o( data_ok ) %>%
            h2o::h2o.predict( object = model,
                              newdata = . )

        out_data[ comp_rows ] <- h2o_pred[ , "p1" ] %>%
            base::as.vector(.)
        
        return( out_data )
    })
}

get_bioclim_predictor <- function()
{
    function( model, data, ...)
    {
        predicts::predict( object = model, x = data ) %>%
            base::as.numeric(.) %>%
            return(.)
    }
}

get_enfa_predictor <- function()
{
    return( function( model, data, ...)
    {
        # Standardize test data using parameters from the training set
        pca_fit <- model$pca_fit

        test_scaled <- base::as.matrix( data ) %>%
            base::sweep( . , 2, pca_fit$cent, FUN = "-" ) %>%
            base::sweep( . , 2, pca_fit$norm, FUN = "/" )

        # Extract ENFA axis coefficients
        enfa_fit <- model$enfa_fit
        nf_idx <- base::seq_len( model$nf )
        axis_coeff <- enfa_fit$co[ , nf_idx, drop = FALSE ] %>%
            base::as.matrix(.)
        
        # Project PCA scores onto the retained ENFA space
        test_enfa_scores <- test_scaled %*% axis_coeff

        # Compute squared Mahalanobis distance to the fitted presence niche
        d2 <- mahalanobis_d2( x = test_enfa_scores,
                              center = model$niche_center,
                              sigma_inv = model$sigma_inv )
        
        # Convert distance to continuous suitability in (0, 1] using the
        # Chi-square upper tail probability.
        stats::pchisq( d2, df = model$nf, lower.tail = FALSE ) %>%
            base::as.numeric(.) %>%
            return(.)
    })
}

get_gam_predictor <- function()
{
    return( function( model, data, ...)
    {
        mgcv::predict.gam( object = model,
                           newdata = data,
                           type = "response" ) %>%
            base::as.numeric(.) %>%
            return(.)
    })
}

get_glm_predictor <- function()
{
    get_stats_predictor() %>%
        return(.)
}

get_gower_predictor <- function()
{
    function( model, data, ...)
    {
        dismo::predict( object = model, x = data ) %>%
            base::as.numeric(.) %>%
            return(.)
    }
}

get_mars_predictor <- function()
{
    get_stats_predictor() %>%
        return(.)
}

get_maxent_predictor <- function()
{
    function( model, data, ... )
    {
        predicts::predict( object  = model,
                           newdata = data,
                           type = "cloglog",
                           clamp = FALSE ) %>%
            base::as.numeric(.) %>%
            return(.)
  }
}

get_svm_predictor <- function()
{
    function( model, data, ...)
    {
        # Ensure that NAs are added back to the prediction results
        out_data <- base::nrow( data ) %>%
            base::rep( NA_real_, . )

        comp_rows <- stats::complete.cases( data )
        data_ok <- data[ comp_rows, , drop = FALSE ]
        
        pred <- stats::predict( object = model,
                               newdata = data_ok, 
                               probability = TRUE )
        
        prob_mat <- base::attr( pred, "probabilities" )

        # Ensure that probabilities were correctly assigned internally in the object
        if ( base::is.null( prob_mat ) )
        {
            stop( "Null probability matrix. Was SVM fitted with probability=TRUE?" )
        }

        pres_level <- base::attr( model, "presence_level" )

        if ( base::is.null( pres_level ) )
        {
            pres_level <- base::tail( model$levels, 1 )
        }

        col_idx <- base::match( pres_level, base::colnames( prob_mat ) )

        if ( base::is.na( col_idx ) )
        {
            col_names <- base::colnames( prob_mat ) %>%
                base::paste( ., collapse = ", " )

            stop( "Presence level '", pres_level,
                 "' not found in probability columns: ", col_names )
        }
        
        out_data[ comp_rows ] <- prob_mat[ , col_idx ] %>%
            base::as.numeric(.)

        return( out_data )
    }
}

# Generic predictor closures
get_stats_predictor <- function()
{
    return( function( model, data, ...)
    {
        stats::predict( object = model,
                        newdata = data,
                        type = "response",
                        control = list( maxit = 100 ) ) %>%
            base::as.numeric(.) %>%
            return(.)
    })
}

# Get training formula for GAM, GLM and MARS
get_training_formula <- function( formula_string )
{
    base::as.character( formula_string ) %>%
        stats::as.formula(.) %>%
        return(.)
}

# Get presence only data for BIOCLIM and Gower
get_pres_only_data <- function( rep_tb, sel_idx, cols = LAYER_CODES )
{
        pres_train <- rep_tb$resp_var[ sel_idx ] == 1
        
        get_var_mx( rep_tb, pres_train, cols ) %>%
            return(.)
}

# Convert data frame columns into a matrix
get_var_mx <- function( my_data, idx, cols = LAYER_CODES )
{
    X <- base::as.matrix( my_data[ , cols ] )

    return( X[ idx, , drop = TRUE ] )
}

# Calculate squared Mahalanobis distances from rows of `x` to an ENFA niche
# centroid, using a precomputed inverse covariance matrix.
mahalanobis_d2 <- function( x, center, sigma_inv )
{
    centered_x <- base::as.matrix( x ) %>%
        base::sweep( ., 2, center, FUN = "-" )

    mahal_mx <- ( centered_x %*% sigma_inv ) * centered_x

    base::rowSums( mahal_mx ) %>%
        return(.)
}

sanitize_trainer_pars <- function( rep_tb, train_idx, model_name )
{
    base::stopifnot( base::is.character( model_name ) )

    sanitize_tb( rep_tb, "rep_tb", model_name, " trainer" )

    if ( ! base::is.logical( train_idx ) )
    {
        stop( "`rep_tb` must be a tibble; got ",
              base::typeof( rep_tb ), " instead from `",
              model_name, "` trainer" )
    }

    if ( ! base::nrow( rep_tb ) == base::length( train_idx ) )
    {
        stop( "`rep_tb` and `train_idx` have differente lengths - `",
              model_name, "` trainer" )
    }

    base::invisible( NULL )
}
