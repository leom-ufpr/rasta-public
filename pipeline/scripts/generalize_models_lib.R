main <- function( stack_file_name )
{
    # Load globals in environment
    my_globals$n_cores <- get_num_cores( N_CORES )
    my_globals$env_stack <- get_env_stack( stack_file_name )

    # Use first raster in stack as template
    my_globals$raster_temp <- my_globals$env_stack[[ 1 ]]

    my_globals$env_data <- base::as.matrix( my_globals$env_stack ) %>%
        tibble::as_tibble(.)

    my_globals$cell_count <- terra::values( my_globals$raster_temp ) %>%
        base::length(.)

    # Silently process codes storing warnings if file
    for( code in CUTOUT_CODES )
    {
        log_con <- base::paste0( "generalization_warnings_", code, ".log" ) %>%
            get_log_con( . )

        handle_warnings <- get_warning_handler( log_con, code )
        
        message( "Processing '", code, "' data" )
        
        withCallingHandlers(
        {
            process_cutout( code = code )
        },
        warning = handle_warnings )

        message( "DONE!" )

        base::close( log_con )
        base::gc()
    }
}

process_cutout <- function( code )
{
    # Load tibbles and tuning list
    eval_tb <- get_eval_tibble( code )
    folds_tb <- get_folds_tibble( code )
    tuning_results <- get_tuning_results( code )

    # Select replicate IDs from the model with the smallest number of non-NA IDs 
    rep_ids <- get_rep_ids( eval_tb )
    
    for( model_name in USE_MODELS )
    {
        # Store arguments in list to simplify function calling
        gen_args <- base::list( model_name = model_name,
                                rep_ids = rep_ids, 
                                cli_message = base::paste0( "Generalizing '", model_name, "' " ),
                                train_fun = get_model_fun( model_name, "trainer" ),
                                folds_tb = folds_tb,
                                best_pars = tuning_results[[ model_name ]][[ "best_pars" ]] )

        # Polymorphically invokes generalizing functions
        generalize_model_fun <- base::paste0( "generalize_", model_name ) %>%
            base::get( ., envir = .GlobalEnv ) 

        # Save multilayer rasters
        save_rasters( gens = generalize_model_fun( gen_args ),
                      code = code,
                      model_name = model_name,
                      rep_ids = rep_ids,
                      eval_tb = eval_tb ) %>%
            message(.)
            message()
    }
}    

get_env_stack <- function( stack_file_name  )
{
    base::file.path( OUTPUT_ENV, stack_file_name ) %>%
        terra::rast(.) %>%
        return(.)
}

get_eval_tibble <- function( code )
{
    get_tibble( "evaluation_results_", code ) %>%
        stats::na.omit() %>%
        return(.)
}

get_folds_tibble <- function( code )
{
    get_tibble( "folds_tb_", code ) %>%
        return(.)
}

get_tuning_results <- function( code )
{
    get_rds_file( "tuning_results_", code ) %>%
        return(.)
}

get_rep_ids <- function( eval_tb )
{
    reps_by_model <- eval_tb %>%
        tidyr::drop_na() %>%
        dplyr::select( model, rep_id ) %>%
        dplyr::summarise( count_rep = dplyr::n(), .by = c( model ) ) %>%
        dplyr::arrange( count_rep )

    min_rep_model <- reps_by_model$model[ 1 ] %>%
        base::as.character(.)

    eval_tb %>%
        dplyr::mutate_at( "rep_id", as.character ) %>%
        dplyr::filter( model == min_rep_model ) %>%
        dplyr::pull( rep_id ) %>%
        base::as.numeric() %>%
        return(.)
}

# Model specific generalizers
generalize_ann <- function( gen_args )
{
    h2o::h2o.init( nthreads = my_globals$n_cores )
    h2o::h2o.no_progress()

    gen_args$gen_fun_name <- "generalize_others"
    gen_args$pred_fun <- get_model_fun( gen_args$model_name, "predictor" )
    gens <- process_gens( gen_args )

    h2o::h2o.shutdown( prompt = FALSE )

    return( gens )
}

generalize_b_bioclim <- function( gen_args )
{
    gen_args$gen_fun_name <- "generalize_bioclim" 

    process_gens( gen_args ) %>%
        return(.)
}    

generalize_gower <- function( gen_args )
{
    parallel_generalize( gen_args ) %>%
        return(.)
}

generalize_glm <- function( gen_args )
{
    generalize_all( gen_args ) %>%
        return(.)
}

generalize_gam <- function( gen_args )
{
    generalize_all( gen_args ) %>%
        return(.)
}

generalize_mahal <- function( gen_args )
{
    parallel_generalize( gen_args ) %>%
        return(.)
}

generalize_mars <- function( gen_args )
{
    generalize_all( gen_args ) %>%
        return(.)
}

generalize_maxent <- function( gen_args )
{
    generalize_all( gen_args ) %>%
        return(.)
}

generalize_svm <- function( gen_args )
{
    generalize_all( gen_args ) %>%
        return(.)
}

save_rasters <- function( gens, code, model_name, rep_ids, eval_tb )
{
    # Sanity check
    base::length( gens ) %>%
        check_lengths( ., rep_ids )

    # Make a copy of template raster
    #raster_temp <- terra::deepcopy( my_globals$raster_temp )
    
    # All vectors must match template cell count
    n_layers <- base::length( gens )

    ok_len <- base::vapply( gens,
                            base::length,
                            base::integer( 1 ) ) == my_globals$cell_count

    if ( ! base::all( ok_len ) )
    {
        stop( "All elements of 'gens' must have length == ncell(raster_temp)" )
    }

    # Assemble layers and file prefix
    prefix <- base::paste( code, model_name, sep = "_" )

    # Create multi-layer SpatRaster based on template
    spat_stack <- terra::rast( my_globals$raster_temp, nlyrs = n_layers )
    
    # Name layers: prefix_001, prefix_002, ...
    base::names( spat_stack ) <- sprintf( "%s_%03d",
                                         prefix,
                                         base::as.numeric( rep_ids ) )

    # Convert list of values into matrix, store in multilayer SpatRaster and save
    gen_matrix <- base::do.call( base::cbind, gens )
    terra::values( spat_stack ) <- gen_matrix

    outcome <- write_rasters( file_prefix = prefix,
                              spat_stack = spat_stack )
 
    # Skips binarization if model is bioclim
    if( model_name == "b_bioclim" )
    {
        return( outcome )
    }
    else
    {
        message( outcome )
    }

    # Binarize according 
    cutoffs <- eval_tb %>%
        dplyr::filter( model == model_name & rep_id == rep_ids ) %>%
        dplyr::pull( pa.cutoff ) %>%
        base::as.numeric(.)

    # Sanity check
    base::ncol( gen_matrix ) %>%
        check_lengths( ., cutoffs )

    bin_gen_matrix <- base::t( base::t( gen_matrix ) >= cutoffs )
    terra::values( spat_stack ) <- +bin_gen_matrix

    base::paste0( prefix, "_bin" ) %>%
        write_rasters( file_prefix = ., spat_stack = spat_stack ) %>%
        return(.)
}

get_binarized_gens <- function( gens, eval_tb, model_name )
{
    # Filter rows corresponding to focal model
    tss_tb <- eval_tb %>%
        dplyr::filter( model == model_name ) %>%
        dplyr::select( pa.cutoff )

    assertthat::assert_that( base::nrow( tss_tb ) == base::length( gens ) )

    # Binarize values vectors using the corresponding TSS value
    base::seq_along( gens ) %>%
        base::lapply( ., function( i )
        {
            bin_values <- ( gens[[ i ]] - tss_tb$pa.cutoff[i] ) >= 0
            return( +bin_values )

        } ) %>%return(.)
}

generalize_all <- function( gen_args )
{
    gen_args$gen_fun_name <- "generalize_others" 
    gen_args$pred_fun <- get_model_fun( gen_args$model_name, "predictor" )

    process_gens( gen_args ) %>%
        return(.)
}

parallel_generalize <- function( gen_args )
{
    message( "Progress bar may take a while to show up, please be patient..." )
    message()
    
    gen_args$gen_fun_name <- "generalize_dismo" 
    gen_args$pred_fun <- get_model_fun( gen_args$model_name, "predictor" )

    process_gens( gen_args ) %>%
        return(.)
}

process_gens <- function( gen_args )
{
    # Prepare "cli" bar
    num_rep_ids <- base::as.numeric( gen_args$rep_ids ) %>%
        cli::cli_progress_along( ., name = gen_args$cli_message )

    # Get function corresponding to focal model
    generalize_fun <- base::get( gen_args$gen_fun_name, envir = .GlobalEnv )

    # Polymorphically call generalizing functions
    gens <- base::lapply( X = num_rep_ids,
                          FUN = function( my_rep_id )
                          {
                              gen_args$my_rep_id <- my_rep_id 
                              base::do.call( generalize_fun, gen_args )
                          })
    return( gens )
}

# Wildcard function used in most models
generalize_others <- function( train_fun, pred_fun, folds_tb, my_rep_id, best_pars, ... )
{
    fit <- get_fit( folds_tb, my_rep_id, train_fun, best_pars )

    pred_fun( model = fit, data = my_globals$env_data ) %>%
        return(.)
}

generalize_bioclim <- function( train_fun, folds_tb, my_rep_id, best_pars, ... )
{
    # Assemble presence/absence matrix: presences from folds tibble, absences as
    # the environmental data extracted from the multilayer raster
    presence_tb <- folds_tb %>%
        dplyr::filter( resp_var == 1 & rep_id == my_rep_id ) %>%
        dplyr::select( resp_var, dplyr::all_of( LAYER_CODES ) )

    absence_tb <- my_globals$env_data %>%
        dplyr::mutate( resp_var = 0 )

    biomod_tb <- dplyr::bind_rows( presence_tb, absence_tb )

    biomod_train_idx <- base::nrow( biomod_tb ) %>%
        base::rep( FALSE, . )

    biomod_pred_idx <- base::nrow( absence_tb )

    biomod_pred <- train_fun( rep_tb = biomod_tb,
                              train_idx = biomod_train_idx,
                              my_pars = best_pars )

    # Return predictions for the new data only
    utils::tail( biomod_pred, biomod_pred_idx ) %>%
        return(.)
}

generalize_dismo <- function( train_fun, folds_tb, my_rep_id, best_pars, ... )
{
    # Use "terra::predict" to generalize "dismo" models based on Gower (domain)
    # and Mahalanobis distances
    terra::terraOptions( progress = 0 )

    # More than 5 cores will not speed the analysis up
    n_cores <- my_globals$n_cores
    if( n_cores > 5 ) n_cores <- 5
    
    fit <- get_fit( folds_tb, my_rep_id, train_fun, best_pars )

    spat_raster <- terra::predict( object = my_globals$env_stack,
                                   model = fit,
                                   cores = 5,
                                   wopt = list( steps = 10 ) )

    # Return values for compatibility with other functions
    terra::values( spat_raster ) %>%
        return(.)
}

# Polymorphically return fitted object 
get_fit <- function( folds_tb, my_rep_id, train_fun, best_pars )
{
    rep_tb <- folds_tb %>%
        dplyr::filter( rep_id == my_rep_id )
    
    train_fun( rep_tb = rep_tb,
               train_idx = base::nrow( rep_tb ) %>% base::rep( TRUE, . ),
               my_pars = best_pars ) %>%
        return(.)
}

get_tibble <- function( prefix, code )
{
    get_rds_file( prefix, code ) %>%
        dplyr::mutate( cutout = code ) %>%
        return(.)
}

get_rds_file <- function( prefix, code )
{
    base::paste0( prefix, code, ".rds" ) %>%
    read_rds( ., OUTPUT_ENV ) %>%
        return(.)
}

check_lengths <- function( gen_count, rep_ids )
{
    # Sanity check
    rep_count <- base::length( rep_ids )

    if ( gen_count != rep_count )
    {
        stop( "Number of generalizations ", gen_count,
              " does not match number of replicate IDs or cutoffs  ", rep_count )
    }
    else
    {
        return( TRUE )
    }
}

write_rasters <- function( file_prefix, spat_stack )
{
    file_path <- base::file.path( OUTPUT_ENV,
                                  base::paste0( file_prefix, ".tif" ) )
    
    terra::writeRaster( spat_stack,
                        filename = file_path,
                        overwrite = TRUE )
    
    base::paste0( "File '", file_path, "' was successfully saved" ) %>%
        return(.)
}



# Set options
base::options( na.action = "na.pass" )

# Load libraries    
custom_libraries <- c( "aux_lib.R",
                       "train_test_lib.R" )

for( library in custom_libraries  )
{
    base::file.path( SCRIPT_ENV, library  ) %>%
        source(.)
}

# Set global environment
my_globals <- base::new.env()
