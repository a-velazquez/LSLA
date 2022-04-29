library(sf)           # Classes a methods for spatial data - simple features
library(terra)          # Classes and methods for raster data
library(RColorBrewer) # Creates nice color schemes
library(ggplot2)      # Functions for graphing and mapping

# sets wd to folder where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in 2020 population data
pop <- rast("raw/landscan-global-2020-colorized.tif")

# look at attributes of the object
class(pop)
summary(pop, size = 1e6)
ncell(pop)


# read in peruvian states shapefile
pr_states <- st_read("raw/per_adm_ign_20200714_shp/per_admbnda_adm1_ign_20200714.shp")

# this crops the raster down to a lat/long rectangle that zooms in on Peru
r2 <- crop(pop, raster::extent(pr_states))
# this SUPPOSEDLY makes everything that isn't contained within Peru's borders NA, but it does not work
r2 <- mask(r2, pr_states)

# this (ripped off from internet) converts to df
rasterdf <- function(x, aggregate = 5) {
  resampleFactor <- aggregate        
  inputRaster <- x    
  inCols <- ncol(inputRaster)
  inRows <- nrow(inputRaster)
  # Compute numbers of columns and rows in the new raster for mapping
  resampledRaster <- rast(ncol=(inCols / resampleFactor), 
                          nrow=(inRows / resampleFactor),
                          crs = crs(inputRaster))
  # Match to the extent of the original raster
  ext(resampledRaster) <- ext(inputRaster)
  # Resample data on the new raster
  y <- resample(inputRaster,resampledRaster,method='near')
  # Extract cell coordinates into a data frame
  coords <- xyFromCell(y, seq_len(ncell(y)))
  # Extract layer names
  dat <- stack(values(y, dataframe = TRUE))
  # Add names - 'value' for data, 'variable' to indicate different raster layers
  # in a stack
  names(dat) <- c('value', 'variable')
  dat <- cbind(coords, dat)
  return(dat)
}

# need to specify which raster grid to put into the df
r2_df <- rasterdf(r2$`landscan-global-2020-colorized_3`)

summary(r2_df)

# ggplot(pr_states) +
#   geom_sf()+
#   geom_raster(data = r2_df, aes(x = x, y = y, fill = value))+
#   scale_fill_gradient(name = "Population", low = "yellow", high = "red")


# no longer useless
ggplot() +  
  geom_raster(data = r2_df, aes(x = x, y = y, fill = value)) +
  geom_sf(data=pr_states, color = "grey50", fill = NA, size = 1) +
  scale_fill_gradient(name = "Population", low = "yellow", high = "red") +
  coord_sf(expand = F) 

# plotting the raster directly lets you see that it's not actually useless, just need to change the df conversion
plot(r2)
