## ========================================================================================================
## 1. USER INPUTS (EDIT HERE)
## ========================================================================================================
# Number of cores for paralelization - only applicable to some algorithms - increase for faster processing
N_CORES <- 15L

# Number of replicates to use in generalizations
N_REPS <- 100L

# Number of epochs to be used in ANN
ANN_EPOCHS <- 50L

# Seed to be used in ANN
ANN_SEED <- 1234L

# Models used to generate SpatStacks
USE_MODELS <- c( "glm", "mars", "b_bioclim", "ann", "svm", "maxent", "gam", "gower", "mahal" )

## ========================================================================================================
## 2. SAVE STACKS WITH GENERALIZATION RESULTS AS GEOTIFFS
## ========================================================================================================
source( "config/globals.R" )
    
base::file.path( SCRIPT_ENV, "generalize_models_lib.R" ) %>%
    source(.)

main( "env_stack_atl.grd" ) 



