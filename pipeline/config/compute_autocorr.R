## ===============================================================================================================================
## 1. USER INPUTS (EDIT HERE)
## ===============================================================================================================================
# How to derive an "effective" range from the correlogram:
# options here: "first_non_sig" = first distance where Moran's I nonsignificant,
# "max_sig" = max distance with significant Moran's I
RANGE_RULE <- "first_non_sig"

# Depth threshold for photic corals (m)
MAX_DEPTH <- 60L

# Number of random raster cells to use in cv_spatial_autocor
N_SAMP <- 2500L

## ===============================================================================================================================
## 2. SHOW BUFFERING DISTANCE AND SAVE OBJECTS
## ===============================================================================================================================
source( "config/globals.R" )

base::file.path( SCRIPT_ENV, "compute_autocorr_lib.R" ) %>%
    source(.)

# Show range in meters for different cutouts and save all objects needed for next step
print( main() )
