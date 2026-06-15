# Trainer closures
get_ann_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
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

get_b_bioclim_trainer <- function()
{
    # biomod2::SRE does not return a trained object, just the predictions
    return( function( rep_tb, train_idx, my_pars )
    {
        # Reverse to testing set
        test_idx <- ! train_idx
        
        biomod2::bm_SRE( resp.var = rep_tb$resp_var[ test_idx ],
                         expl.var = get_var_mx( rep_tb, test_idx ),
                         new.env = NULL,
                         quant = my_pars$quant[1],
                         do.extrem = FALSE ) %>%
            return(.)
    } )
}

get_d_bioclim_trainer <- function()
{
    get_aquarium_trainer( "bioclim" ) %>%
        return(.)
}

get_gam_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
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
        stats::glm( formula = get_training_formula( my_pars$formula[1] ),
                    data = rep_tb[ train_idx, ],
                    family = stats::binomial( link = "logit" ),
                    control = base::list( trace = FALSE ) ) %>%
            return(.)
    } )
}

get_gower_trainer <- function()
{
    get_aquarium_trainer( "domain" ) %>%
        return(.)
}

get_mahal_trainer <- function()
{
    get_aquarium_trainer( "mahal" ) %>%
        return(.)
}

get_mars_trainer <- function()
{
    return( function( rep_tb, train_idx, my_pars )
    {
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

get_aquarium_trainer <- function( fun_name )
{
    training_fun <- base::asNamespace( "dismo" ) %>%
        base::get( fun_name, envir = . )

   return( function( rep_tb, train_idx, ... )
   {
       expl_var <- get_pres_only_data( rep_tb, train_idx )

       training_fun( expl_var ) %>% 
           return(.)
   } )
}

# Tester closures (model selection and evaluation)
get_ann_tester <- function()
{
    get_tester_for( "ann" ) %>%
        return(.)
}

get_b_bioclim_tester <- function()
{
    # Simply return predictions for test set
    return( function( rep_tb, test_idx, fit )
    {
        return( fit )
    } )
}

get_d_bioclim_tester <- function()
{
    get_aquarium_tester() %>%
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
    get_aquarium_tester() %>%
        return(.)
}

get_mahal_tester <- function()
{
    get_tester_for( "mahal" ) %>%
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

# Generic testers
get_aquarium_tester <- function()
{
    get_tester_for( "aquarium" ) %>%
        return(.)
}

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
    get_aquarium_predictor() %>%
        return(.)
}

get_mahal_predictor <- function()
{
    aquarium_predictor <- get_aquarium_predictor()
    
    return( function( model, data, ...)
    {
        d2 <- 1 - aquarium_predictor( model = model,
                                      data = data)
        
        stats::pchisq( q = d2,
                       df = base::ncol( data ),
                       lower.tail = FALSE ) %>%
            base::as.numeric(.) %>%
            return(.)
    })
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
get_aquarium_predictor <- function()
{
    function( model, data, ...)
    {
        dismo::predict( object = model,
                        x = data ) %>%
            base::as.numeric(.) %>%
            return(.)
        
    }
}

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

# Auxiliary functions
# Get environmental variables for presence only data
get_pres_only_data <- function( rep_tb, idx, cols = LAYER_CODES )
{
    pres_train <- rep_tb$resp_var[ idx ] == 1

    get_var_mx( rep_tb, pres_train, cols ) %>%
        return(.)
}

# Get training formula for GAM, GLM and MARS
get_training_formula <- function( formula_string )
{
    base::as.character( formula_string ) %>%
        stats::as.formula(.) %>%
        return(.)
}

# Convert data frame columns into a matrix
get_var_mx <- function( my_data, idx, cols = LAYER_CODES )
{
    X <- base::as.matrix( my_data[ , cols ] )

    return( X[ idx, , drop = TRUE ] )
}
