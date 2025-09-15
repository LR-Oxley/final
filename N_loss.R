## including N loss in the N thawed calculation: 
## thawed_taiga = thawed_taiga - f_n_loss  * thawed_taiga
## f_n_loss = f_denitrification + f_runoff
## als Test z.B. : 
## f_denitrification = 0.01
## f_runoff = 0.01

#considering rooting depth of grasses: limit to 2m depth
library(terra)
library(here)
# Load NetCDF files
ALD <- rast("final_data/mean_ssp126_corr.nc")
N_data <- rast("final_data/TN_30deg_corr.nc", lyr = 1)
LC <- rast("final_data/LC_remapnn_corr.nc")

cell_area<-cellSize(N_data, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_area, fun = "sum", na.rm = TRUE)$sum
print(total_area)
plot(N_data)
# Print the total area (in m²)
print(N_data)
plot(ALD[[250]])

# Get latitude and longitude from the grid
lat <- unique(yFromCell(ALD, 1:ncell(ALD)))
lon <- unique(xFromCell(ALD, 1:ncell(ALD)))

# Set common extent
common_extent <- ext(-179.95, 179.95, 30, 90)
ext(ALD) <- ext(N_data) <- ext(LC) <- common_extent

# Ensure masks are numeric (not logical)
# Masks (no as.numeric!)
taiga_mask    <- LC %in% c(1,2,3,4,5,8,9)
tundra_mask   <- LC %in% c(6,7,10)
wetlands_mask <- LC == 11
barren_mask   <- LC %in% c(15,16)


taiga_mask <- as.numeric(LC %in% c(1,2,3,4,5,8,9))
tundra_mask <- as.numeric(LC %in% c(6,7,10))
wetlands_mask <- as.numeric(LC == 11)
other<- as.numeric(LC %in% c(12,13,14))
barren_mask <- as.numeric(LC %in% c(15,16))
plot(other)
total_mask<-barren_mask+wetlands_mask+tundra_mask+taiga_mask

# Land cover parameters
params <- list(
  taiga = c(a = 0.007, b = 0.097, k = 0.027),
  tundra = c(a = 0.01, b = 0.017, k = 0.019),
  barren = c(a = 0, b = 0.0161, k = 0.016)
)
### calculate wetlands like homogeneous 

# Normalize Nitrogen (replace ifel with terra::ifel)
normalize_N <- function(N, a, b, k) {
  A_3m <- 3 * a + (b / k) * (1 - exp(-3 * k))
  N / A_3m
}

taiga_N <- normalize_N(N_data * taiga_mask, params$taiga['a'], params$taiga['b'], params$taiga['k'])
tundra_N <- normalize_N(N_data * tundra_mask, params$tundra['a'], params$tundra['b'], params$tundra['k'])
barren_N <- normalize_N(N_data * barren_mask, params$barren['a'], params$barren['b'], params$barren['k'])
wetlands_N <- (N_data * wetlands_mask) / 3
plot(taiga_N)


# Compute thawed nitrogen
compute_thawed_N <- function(ALD, N, a, b, k) {
  A_ALD <- a * ALD + (b / k) * (1 - exp(-k * ALD))
  ifel(N == 0, NA, N * A_ALD)  # Avoid multiplying by zero
}


# parameters
k_N_loss <- 0.02  # 2% loss per year

# initialize thawed N
thawed_taiga <- ALD * 0  # empty raster with same structure
thawed_taiga[[1]] <- taiga_N  # starting pool (or whichever N you want)
plot(thawed_taiga[[1]])
# loop over time
for (t in 2:nlyr(ALD)) {
  # compute thawed N for current time step
  thawed_current <- compute_thawed_N(ALD[[t]], taiga_N, 
                                     params$taiga['a'], 
                                     params$taiga['b'], 
                                     params$taiga['k'])
  
  # apply cumulative loss to the *previous pool*
  thawed_taiga[[t]] <- thawed_taiga[[t-1]] * (1 - k_N_loss)
}

plot(thawed_taiga[[250]])


#######

library(terra)
library(here)

# Load NetCDF files
ALD <- rast("final_data/mean_ssp126_corr.nc")
N_data <- rast("final_data/TN_30deg_corr.nc", lyr = 1)
LC <- rast("final_data/LC_remapnn_corr.nc")

# Set common extent
common_extent <- ext(-179.95, 179.95, 30, 90)
ext(ALD) <- ext(N_data) <- ext(LC) <- common_extent
LC <- resample(LC, N_data, method = "near")
crs(N_data)
crs(LC)
# Create masks
# Masks (no as.numeric!)
taiga_mask    <- LC %in% c(1,2,3,4,5,8,9)
tundra_mask   <- LC %in% c(6,7,10)
wetlands_mask <- LC == 11
barren_mask   <- LC %in% c(15,16)
plot(wetlands_mask)
# Land cover parameters
params <- list(
  taiga = c(a = 0.007, b = 0.097, k = 0.027),
  tundra = c(a = 0.01, b = 0.017, k = 0.019),
  barren = c(a = 0, b = 0.0161, k = 0.016)
)

# Nitrogen loss parameters
f_denitrification <- 0.01
f_runoff <- 0.01
f_n_loss <- f_denitrification + f_runoff

# Apply ALD cap for tundra, barren, and wetlands: set to 2 when larger than 2
capped_ALD <- ifel(ALD > 2, 2, ALD)
plot(capped_ALD[[250]])
# Normalize Nitrogen
normalize_N <- function(N, a, b, k) {
  A_3m <- 3 * a + (b / k) * (1 - exp(-3 * k))
  N / A_3m
}

taiga_N <- normalize_N(N_data * taiga_mask, params$taiga['a'], params$taiga['b'], params$taiga['k'])
plot(taiga_N)
tundra_N <- normalize_N(N_data * tundra_mask, params$tundra['a'], params$tundra['b'], params$tundra['k'])
barren_N <- normalize_N(N_data * barren_mask, params$barren['a'], params$barren['b'], params$barren['k'])
wetlands_N <- (N_data * wetlands_mask) / 3

# Compute thawed nitrogen with N loss
compute_thawed_N <- function(ALD, N, a, b, k) {
  # Calculate thawed nitrogen
  A_ALD <- a * ALD + (b / k) * (1 - exp(-k * ALD))
  thawed_N <- ifel(N == 0, NA, N * A_ALD)
  
  # Apply nitrogen loss
  thawed_N <- thawed_N - (f_n_loss * thawed_N)
  
  return(thawed_N)
}

# Compute thawed nitrogen with N loss for tundra, wetlands, barren
compute_thawed_N_2m_lim <- function(capped_ALD, N, a, b, k) {
  # Calculate thawed nitrogen
  A_ALD <- a * capped_ALD + (b / k) * (1 - exp(-k * capped_ALD))
  thawed_N <- ifel(N == 0, NA, N * A_ALD)
  
  # Apply nitrogen loss
  thawed_N <- thawed_N - (f_n_loss * thawed_N)
  
  return(thawed_N)
}


# Compute thawed nitrogen
thawed_taiga   <- compute_thawed_N(ALD, taiga_N, params$taiga['a'], params$taiga['b'], params$taiga['k'])
thawed_tundra  <- compute_thawed_N_2m_lim(capped_ALD, tundra_N, params$tundra['a'], params$tundra['b'], params$tundra['k'])
thawed_barren  <- compute_thawed_N_2m_lim(capped_ALD, barren_N, params$barren['a'], params$barren['b'], params$barren['k'])
thawed_wetlands <- ifel(wetlands_N == 0, NA, wetlands_N * capped_ALD)
thawed_wetlands <- thawed_wetlands - (f_n_loss * thawed_wetlands)


plot(thawed_wetlands[[2]])

#install.packages("ncdf4")
### save rasters to netcdf files
writeCDF(
  thawed_taiga,
  filename = "/Users/laraoxley/Desktop/data/CMIP6/final/thawed_taiga_ssp126_nloss.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)
writeCDF(
  thawed_tundra,
  filename = "/Users/laraoxley/Desktop/data/CMIP6/final/thawed_tundra_ssp126_nloss.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)
writeCDF(
  thawed_barren,
  filename = "/Users/laraoxley/Desktop/data/CMIP6/final/thawed_barren_ssp126_nloss.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)
writeCDF(
  thawed_wetlands,
  filename = "/Users/laraoxley/Desktop/data/CMIP6/final/thawed_wetlands_ssp126_nloss.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)
# Replace NA values with 0 to be able to combine the rasters
thawed_taiga[is.na(thawed_taiga)] <- 0
thawed_tundra[is.na(thawed_tundra)] <- 0
thawed_barren[is.na(thawed_barren)] <- 0
thawed_wetlands[is.na(thawed_wetlands)] <- 0
plot(thawed_wetlands[[2]])
# Combine the rasters for each year
combined_thawed <- thawed_tundra + thawed_barren + thawed_wetlands+thawed_taiga
graminoid_thawed<- thawed_tundra + thawed_barren + thawed_wetlands
#taiga<-ifel(thawed_taiga == 0, NA, thawed_taiga)
combined_thawed <- ifel(combined_thawed == 0, NA, combined_thawed)
plot(combined_thawed[[2]])
# Save combined raster to a NetCDF file
writeCDF(
  combined_thawed,
  filename = "/Users/laraoxley/Desktop/data/CMIP6/final/thawed_total_ssp126_2m_limit.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)

graminoid_thawed <- ifel(graminoid_thawed == 0, NA, graminoid_thawed)
plot(graminoid_thawed[[2]])
# Save combined raster to a NetCDF file
writeCDF(
  graminoid_thawed,
  filename = "/Users/laraoxley/Desktop/data/CMIP6/final/thawed_graminoids_ssp126_2m_limit.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)


library("terra")
combined_thawed <- rast("thawed_total_ssp126_2m_limit.nc")

# Compute cell areas for each year
cell_areas_all_years <- rast(lapply(1:nlyr(combined_thawed), function(i) {
  cellSize(combined_thawed[[i]], mask = TRUE, unit = "m")
}))
plot(cell_areas_all_years[[250]])
# Compute weighted thawed nitrogen for each year
weighted_thawed_all_years <- combined_thawed * cell_areas_all_years
plot(weighted_thawed_all_years[[250]])
# Compute total weighted thawed nitrogen (sum over all cells)
total_weighted_thawed <- global(weighted_thawed_all_years, "sum", na.rm = TRUE)
total_thawed_Pg <- total_weighted_thawed / 1e12
write.csv(total_thawed_Pg, "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/thawed_total_mean_2m_lim_Pg_ssp126.csv")

# Compute total area for each year
total_area_all_years <- global(cell_areas_all_years, "sum", na.rm = TRUE)

# Compute the final weighted average thawed nitrogen (kg N / m²)
weighted_mean_thawed <- total_weighted_thawed / total_area_all_years
write.csv(weighted_mean_thawed, "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/weighted_mean_thawed_total_mean_2m_lim_ssp126.csv")


#############################################################################
thawed_taiga <- rast("thawed_taiga_ssp126_nloss.nc")
plot(thawed_taiga[[2]])

# Compute cell areas for each year
cell_areas_all_years <- rast(lapply(1:nlyr(thawed_taiga), function(i) {
  cellSize(thawed_taiga[[i]], mask = TRUE, unit = "m")
}))

# Compute weighted thawed nitrogen for each year
weighted_thawed_all_years <- thawed_taiga * cell_areas_all_years

# Compute total weighted thawed nitrogen (sum over all cells)
total_weighted_thawed_taiga <- global(weighted_thawed_all_years, "sum", na.rm = TRUE)

# Compute total area for each year
total_area_all_years_taiga <- global(cell_areas_all_years, "sum", na.rm = TRUE)

# Compute the final weighted average thawed nitrogen (kg N / m²)
weighted_mean_thawed_taiga <- total_weighted_thawed_taiga / total_area_all_years_taiga
write.csv(weighted_mean_thawed_taiga, "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/weighted_mean_thawed_taiga_mean_2m_lim_nloss_ssp126.csv")

plot(1:251, weighted_mean_thawed_taiga$sum, type = "l", main = "Total Arctic Area Over Time",
     xlab = "Year", ylab = "Total Area (m²)")



thawed_grasslands <- rast("thawed_graminoids_ssp126_2m_limit.nc")


plot(thawed_grasslands[[1]])
# Compute cell areas for each year
cell_areas_all_years <- rast(lapply(1:nlyr(thawed_grasslands), function(i) {
  cellSize(thawed_grasslands[[i]], mask = TRUE, unit = "m")
}))

# Compute weighted thawed nitrogen for each year
weighted_thawed_all_years <- thawed_grasslands * cell_areas_all_years

# Compute total weighted thawed nitrogen (sum over all cells)
total_weighted_thawed_grasslands <- global(weighted_thawed_all_years, "sum", na.rm = TRUE)

# Compute total area for each year
total_area_all_years_grasslands <- global(cell_areas_all_years, "sum", na.rm = TRUE)

# Compute the final weighted average thawed nitrogen (kg N / m²)
weighted_mean_thawed_grasslands <- total_weighted_thawed_grasslands / total_area_all_years_grasslands
write.csv(weighted_mean_thawed_grasslands, "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/weighted_mean_thawed_grasslands_2m_lim_nloss_ssp126.csv")
