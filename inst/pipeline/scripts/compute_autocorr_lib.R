# Master function
main <- function()
{
    # Initialize range list
    range <- base::list()

    for( code in CUTOUT_CODES )
    {
        message( "Processing data for '", code, "', please wait..." )

        # Load original stack
        env_stack <- load_cutout_stack( code )
        save_stack( env_stack, 'env_stack', code )

        # Mask shallow areas
        shallow_stack <- mask_shallow_areas( env_stack, MAX_DEPTH )
        save_stack( shallow_stack, 'shallow_stack', code )

        # Compute spatial autocorrelation of environmental variables
        # in shallow areas
        blockcv_sac <- blockCV::cv_spatial_autocor( r = shallow_stack,
                                                    num_sample = N_SAMP,
                                                    plot = FALSE )
        message()

        # Save environmental stacks
        save_rds( blockcv_sac, 'blockcv_sac', code, OUTPUT_ENV )

        range[[ code ]] <- blockcv_sac$range
        message()
    }

    # Assemble tibble
    range_tibble <- tibble::tibble( cutout = base::names( range ),
                                    range = base::unlist( range ) %>%
                                    base::as.vector(.) )

    # Save tibble
    save_rds( range_tibble, "autocorr", "range", OUTPUT_ENV )

    return( range_tibble )
}


# Function to build a multi-layer SpatRaster stack for one cutout
load_cutout_stack <- function( code )
{
    env_stack <- c()

    for( layer_code in LAYER_CODES )
    {
        # Load ESRI layer as SpatRaster
        file_path <- base::paste0( layer_code, "_", code, ".asc" ) %>%
            base::file.path( DATA_ENV, . )
        
        layer_rast <- terra::rast( file_path )

        # Correct projection
        original_crs <- terra::crs( layer_rast )
        
        if( base::length( original_crs ) > 0 )
            { terra::crs( layer_rast ) <- PREFERRED_CRS }
        
        layer_rast <- terra::project( layer_rast, PREFERRED_CRS )
        base::names( layer_rast ) <- layer_code
                
        # Enforce raster alignment
        if( base::length( env_stack ) > 0 )
        {
            layer_rast <- terra::resample( layer_rast,
                                           env_stack[[ 1 ]],
                                           method = "bilinear" )
        }

        # Stack layer
        env_stack <- c( env_stack, layer_rast )
    }

    terra::rast( env_stack ) %>%
        return(.) 
}

# Function to mask deep cells: TRUE for shallow (deafault is <=60 m), NA for deep
# Assuming bathymetry is negative downwards; adapt if your rasters use positive depths
mask_shallow_areas <- function( env_stack, depth_thr )
{
  btm <- env_stack[[ "btm" ]]

  # Here, we consider "depth" = abs(btm), so > 60 m becomes NA
  depth <- base::abs( btm )
  shallow_mask <- depth <= depth_thr

  terra::mask( env_stack,
               shallow_mask,
               maskvalues = 0,
               updatevalue = NA ) %>%
      return(.)
}

save_stack <- function( stack, obj_name, code )
{
    filename <- get_filename( obj_name, code, OUTPUT_ENV, 'grd' )
    terra::writeRaster( stack, file = filename, overwrite = TRUE )
    message( "Saved ", filename )

    return( TRUE )
}

base::file.path( SCRIPT_ENV, "aux_lib.R" ) %>%
    source(.)
