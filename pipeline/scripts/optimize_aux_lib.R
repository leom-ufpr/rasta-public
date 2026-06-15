# Get unique, sorted values from vector
get_unique_vals <- function( my_vector )
{
    base::unique( my_vector ) %>%
        base::sort(.) %>%
        return(.)
}

# Set options
options( future.debug = FALSE,
         future.globals.maxSize = 8 * 1024^3,
         na.action = "na.fail" )

# Internal globals
# Determine if there are parameters to be saved
SAVE_TUNING_RESULTS <- FALSE

# Replicates employed in BIC selection of linear
# combination parameters for engine models
BIC_CHOICE_TB <- tibble::tibble()

# Replicates employed in FOR selection of
# parameters/linear combinations for engine and vault models
FOR_CHOICE_TB <- tibble::tibble()

# Replicates employed in model evaluation via TSS
TSS_EVAL_TB <- tibble::tibble()

# h2o shutdown flag
H2O_WAS_RUN <- FALSE

# Testing variable
PEEK_VARIABLE_1 <- NULL
