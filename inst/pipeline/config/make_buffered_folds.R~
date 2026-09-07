## =======================================================================================
## 1. USER INPUTS (EDIT HERE)
## =======================================================================================
# Edit to your needs
N_REPS <- 120L         # Number of folds/replicates
THIN_DIST_KM <- 10L    # spThin distance (km)
N_PA_POINTS <- 10000L  # Number of pseudo-absence/background points
BLOCK_CV_ITER <- 50L   # Number of blockCV iterations
SPATIAL_FOLDS <- 5L    # Number of cross validation folds
BASE_SEED <- 1234      # Base seed for reproducibility
N_CORES <- 15L         # Number of cores - increase for parallel (= faster) processing

## =======================================================================================
## 2. SAVE BUFFERED FOLDS FOR ALL CUTOUTS
## =======================================================================================
source( "config/globals.R" )

# Occurrence data as CSV (one row per point, separate by cutout)
# Must have columns: species, lon, lat, cutout (atl/brz/crb);
# functions expect semi-colon instead of comma delimited fields
OCC_FILE <- base::file.path( DATA_ENV, "siderastrea_data.csv" )

base::file.path( SCRIPT_ENV, "make_buffered_folds_lib.R" ) %>%
    source(.)

# Save tibble with all information for downstream analyses (model tuning and evaluation)
main()
