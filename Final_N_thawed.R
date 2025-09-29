# calculating N(t) = N(t-1) + N(permafrost_release) without N loss

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

# amount of N in Pg over panArctic region: 
N_kg<-N_data * cell_area
total_N <- global(N_kg, fun = "sum", na.rm = TRUE)$sum
total_N_Tg<-total_N/10^9


plot(ALD[[250]])

# Get latitude and longitude from the grid
lat <- unique(yFromCell(ALD, 1:ncell(ALD)))
lon <- unique(xFromCell(ALD, 1:ncell(ALD)))

# Set common extent
common_extent <- ext(-179.95, 179.95, 30, 90)
ext(ALD) <- ext(N_data) <- ext(LC) <- common_extent
LC <- resample(LC, N_data, method = "near")
# Ensure masks are numeric (not logical)
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
graminoids_N<-barren_N+wetlands_N+tundra_N
graminoids_N[graminoids_N == 0] <- NA
plot(graminoids_N)
taiga_N[taiga_N == 0] <- NA
tundra_N[tundra_N == 0] <- NA
wetlands_N[wetlands_N == 0] <- NA
barren_N[barren_N == 0] <- NA

cell_area<-cellSize(graminoids_N, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_area, fun = "sum", na.rm = TRUE)$sum
cell_area<-cellSize(taiga_N, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_area, fun = "sum", na.rm = TRUE)$sum

total_N<-barren_N+wetlands_N+tundra_N+taiga_N

# Compute thawed nitrogen
thawed_taiga <- compute_thawed_N(ALD, taiga_N, params$taiga['a'], params$taiga['b'], params$taiga['k'])
plot(thawed_taiga[[251]])
# For tundra, barren, and wetlands: set to NA where ALD < 2
capped_ALD <- ifel(ALD > 2, 2, ALD)

thawed_tundra <- compute_thawed_N(capped_ALD, tundra_N, params$tundra['a'], params$tundra['b'], params$tundra['k'])
thawed_barren <- compute_thawed_N(capped_ALD, barren_N, params$barren['a'], params$barren['b'], params$barren['k'])
thawed_wetlands <- wetlands_N * capped_ALD

plot(thawed_tundra[[1]])
plot(thawed_wetlands[[250]])
plot(thawed_barren[[250]])
plot(thawed_tundra[[1]])

# Replace NA values with 0 to be able to combine the rasters
thawed_taiga[is.na(thawed_taiga)] <- 0
thawed_tundra[is.na(thawed_tundra)] <- 0
thawed_barren[is.na(thawed_barren)] <- 0
thawed_wetlands[is.na(thawed_wetlands)] <- 0

# Combine the rasters for each year
combined_thawed <- thawed_tundra + thawed_barren + thawed_wetlands+thawed_taiga
#taiga<-ifel(thawed_taiga == 0, NA, thawed_taiga)
combined_thawed <- ifel(combined_thawed == 0, NA, combined_thawed)
plot(combined_thawed[[2]])
# Save combined raster to a NetCDF file
writeCDF(
  combined_thawed,
  filename = "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/thawed_total_ssp126_2m_limit_nonloss.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)

plot(thawed_wetlands[[250]])
plot(combined_thawed[[250]])
library(terra)
combined_thawed<- rast("final_results/thawed_total_ssp126_2m_limit_nonloss.nc")

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
write.csv(total_thawed_Pg, "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/thawed_total_ssp126_mean_2m_lim_nonloss.csv")

# Compute total area for each year
total_area_all_years <- global(cell_areas_all_years, "sum", na.rm = TRUE)

# Compute the final weighted average thawed nitrogen (kg N / m²)
weighted_mean_thawed <- total_weighted_thawed / total_area_all_years
write.csv(weighted_mean_thawed, "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/weighted_mean_thawed_total_ssp126_mean_2m_lim_nonloss.csv")

plot(1:250, total_area_all_years$sum, type = "l", main = "Total Arctic Area Over Time",
     xlab = "Year", ylab = "Total Area (m²)")
#############################################################################
thawed_taiga<-ifel(thawed_taiga == 0, NA, thawed_taiga)
plot(thawed_taiga[[2]])

#for taiga

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
write.csv(weighted_mean_thawed_taiga, "weighted_mean_thawed_taiga.csv")

plot(1:251, total_area_all_years$sum, type = "l", main = "Total Arctic Area Over Time",
     xlab = "Year", ylab = "Total Area (m²)")


thawed_taiga<-ifel(thawed_taiga == 0, NA, thawed_taiga)


#for grasslands
thawed_tundra<-ifel(thawed_taiga == 0, NA, thawed_tundra)
thawed_barren<-ifel(thawed_taiga == 0, NA, thawed_barren)
thawed_wetlands<-ifel(thawed_taiga == 0, NA, thawed_wetlands)
plot(thawed_tundra[[1]])
thawed_grasslands<-thawed_tundra + thawed_barren + thawed_wetlands
thawed_grasslands <- ifel(thawed_grasslands == 0, NA, thawed_grasslands)
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
write.csv(weighted_mean_thawed_grasslands, "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/weighted_mean_thawed_grasslands.csv")

plot(1:251, total_area_all_years$sum, type = "l", main = "Total Arctic Area Over Time",
     xlab = "Year", ylab = "Total Area (m²)")


#############################################################################
## to look at 2m limit
library(terra)
library(here)
# Load NetCDF files
arctic_N <- rast("thawed_N_individual_biomes/thawed_total_ssp585_2m_limit.nc")
plot(arctic_N[[250]])
# Compute cell areas for each year
cell_areas_all_years <- rast(lapply(1:nlyr(arctic_N), function(i) {
  cellSize(arctic_N[[i]], mask = TRUE, unit = "m")
}))

# Compute weighted thawed nitrogen for each year
weighted_thawed_all_years <- arctic_N * cell_areas_all_years

# Compute total weighted thawed nitrogen (sum over all cells)
total_weighted_thawed_arctic <- global(weighted_thawed_all_years, "sum", na.rm = TRUE)

# Compute total area for each year
total_area_all_years_arctic <- global(cell_areas_all_years, "sum", na.rm = TRUE)

# Compute the final weighted average thawed nitrogen (kg N / m²)
weighted_mean_thawed_arctic <- total_weighted_thawed_arctic / total_area_all_years_arctic
write.csv(weighted_mean_thawed_taiga, "final_data/weighted_mean_thawed_arctic_585.csv")





#############################################################################

# plot to show asymptotic distribution of N in soil with depth
# Define the equation function
eq <- function(depth, a, b, k) {
  a + b * exp(-k * abs(depth))  # Use absolute depth to ensure exponent works correctly
}

# Define parameters for each biome
params <- list(
  Taiga  = list(a = 0.007, b = 0.097, k = 0.027),
  Tundra = list(a = 0.01,  b = 0.017, k = 0.019),
  Barren = list(a = 0,     b = 0.0161, k = 0.016)
)

# Set up the plot with depth from 0 to -300 cm
plot(NA, xlim = c(0, 0.12), ylim = c(-300, 0), xlab = "TN (kg/m²)", ylab = "Depth (cm)", main = "TN vs Depth")
grid()

# Add curves for each biome
colors <- c("darkgreen", "lightblue", "grey")
i <- 1

for (biome in names(params)) {
  depth_seq <- seq(0, 300, length.out = 100)  # Depth in cm (positive)
  TN_values <- eq(depth_seq, params[[biome]]$a, params[[biome]]$b, params[[biome]]$k)  # Compute TN
  
  lines(TN_values, -depth_seq, col = colors[i], lwd = 2)  # Flip depth to negative
  i <- i + 1
}
# Add Wetlands as a vertical line
wetlands_TN <- 0.022 # Constant TN value for wetlands
lines(rep(wetlands_TN, 100), seq(-300, 0, length.out = 100), col = "green", lwd = 2, lty = 1)

# Add legend
legend("topright", legend = c(names(params), "Wetlands"), col = c(colors, "green"), lwd = 2, lty = c(1, 1, 1, 1))


## plot the spatial distribution of the land cover types

library(terra)
library(here)

LC <- rast("LC_remapnn_corr.nc")

# Ensure masks are numeric (not logical)
taiga_mask <- as.numeric(LC %in% c(1,2,3,4,5,8,9))
tundra_mask <- as.numeric(LC %in% c(6,7,10))
wetlands_mask <- as.numeric(LC == 11)
other<- as.numeric(LC %in% c(12,13,14))
barren_mask <- as.numeric(LC %in% c(15,16))
plot(barren_mask)

####plotting the biomes together in one map
# Create a new raster where each biome gets a unique value
biome_mask <- LC
values(biome_mask) <- NA

# Assign biome codes
biome_mask[LC %in% c(1,2,3,4,5,8,9)] <- 1  # Taiga
biome_mask[LC %in% c(6,7,10)]       <- 2  # Tundra
biome_mask[LC == 11]                <- 3  # Wetlands
biome_mask[LC %in% c(12,13,14)]     <- 4  # Other
biome_mask[LC %in% c(15,16)]        <- 5  # Barren

# Define colors
biome_colors <- c("darkgreen",  # Taiga
"lightblue",  # Tundra
"lightgreen",       # Wetlands
"grey",       # Other
"tan"         # Barren
)

biome_labels <- c("Taiga", "Tundra", "Wetlands", "Other", "Barren")

# Convert raster to data frame with coordinates
biome_df <- as.data.frame(biome_mask, xy = TRUE)

# Suppose the raster has one layer with integer biome IDs
colnames(biome_df)[3] <- "biome_id"
# Plot
biome_labels <- c(
  "1" = "Taiga",
  "2" = "Tundra",
  "3" = "Wetlands",
  "4" = "Other",
  "5" = "Barren"
)

biome_colors <- c("darkgreen",  # Taiga
                  "lightblue",  # Tundra
                  "lightgreen",       # Wetlands
                  "grey",       # Other
                  "tan"         # Barren
)
# Convert biome_id to factor with labels
biome_df$biome <- factor(
  biome_df$biome_id,
  levels = names(biome_labels),
  labels = biome_labels
)

ggplot(biome_df) +
  geom_tile(aes(x = x, y = y, fill = biome)) +
  scale_fill_manual(values = biome_colors, name = "Biome") +
  coord_equal() +
  theme_minimal() +
  theme(
    legend.position = "right",   # put legend outside (right)
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10)
  ) +
  labs(title = "Arctic Biomes", x="Longitude", y = "Latitude")



##############################################################################################################################
# no 2m depth limit for graminoid/ grass types
library(terra)
library(here)
getwd()
# Load NetCDF files
ALD <- rast("final_data/mean_ssp585_corr_new.nc")
N_data <- rast("final_data/TN_30deg_corr.nc", lyr = 1)
LC <- rast("final_data/LC_remapnn_corr.nc")

plot(ALD[[2]])
cell_area<-cellSize(LC, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_area, fun = "sum", na.rm = TRUE)$sum

cell_area<-cellSize(N_data, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area_N <- global(cell_area, fun = "sum", na.rm = TRUE)$sum

cell_area<-cellSize(ALD, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area_ALD <- global(cell_area, fun = "sum", na.rm = TRUE)$sum

# Print the total area (in m²)
print(N_data)
plot(ALD[[1]])

# Get latitude and longitude from the grid
lat <- unique(yFromCell(ALD, 1:ncell(ALD)))
lon <- unique(xFromCell(ALD, 1:ncell(ALD)))

# Set common extent
common_extent <- ext(-179.95, 179.95, 30, 90)
ext(ALD) <- ext(N_data) <- ext(LC) <- common_extent
LC <- resample(LC, N_data, method = "near")  # nearest neighbor for masks
# Ensure masks are numeric (not logical)
taiga_mask <- as.numeric(LC %in% c(1,2,3,4,5,8,9))
tundra_mask <- as.numeric(LC %in% c(6,7,10))
wetlands_mask <- as.numeric(LC == 11)
other<- as.numeric(LC %in% c(12,13,14))
barren_mask <- as.numeric(LC %in% c(15,16))
plot(barren_mask)

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

# Compute thawed nitrogen
compute_thawed_N <- function(ALD, N, a, b, k) {
  A_ALD <- a * ALD + (b / k) * (1 - exp(-k * ALD))
  ifel(N == 0, NA, N * A_ALD)  # Avoid multiplying by zero
}

taiga_N[taiga_N == 0] <- NA
tundra_N[tundra_N == 0] <- NA
wetlands_N[wetlands_N == 0] <- NA
barren_N[barren_N == 0] <- NA
plot(taiga_N)

# Compute thawed nitrogen
thawed_taiga <- compute_thawed_N(ALD, taiga_N, params$taiga['a'], params$taiga['b'], params$taiga['k'])
thawed_tundra <- compute_thawed_N(ALD, tundra_N, params$tundra['a'], params$tundra['b'], params$tundra['k'])
thawed_barren <- compute_thawed_N(ALD, barren_N, params$barren['a'], params$barren['b'], params$barren['k'])
thawed_wetlands <- (wetlands_N * ALD)

plot(thawed_taiga[[250]], main = "Thawed Nitrogen: Taiga")

# Replace NA values with 0
thawed_taiga[is.na(thawed_taiga)] <- 0
thawed_tundra[is.na(thawed_tundra)] <- 0
thawed_barren[is.na(thawed_barren)] <- 0
thawed_wetlands[is.na(thawed_wetlands)] <- 0

# Combine the rasters for each year
combined_thawed <- thawed_taiga + thawed_tundra + thawed_barren + thawed_wetlands

combined_thawed <- ifel(combined_thawed == 0, NA, combined_thawed)
plot(combined_thawed[[1]])
getwd()
# Save combined raster to a NetCDF file
writeCDF(
  combined_thawed,
  "thawed_total_ssp585_new.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)

# Compute cell areas for each year (different for each layer)
cell_areas_all_years <- rast(lapply(1:nlyr(combined_thawed), function(i) {
  cellSize(combined_thawed[[i]], mask = TRUE, unit = "m")
}))
plot(cell_areas_all_years[[250]])
# Compute weighted thawed nitrogen for each year
weighted_thawed_all_years <- combined_thawed * cell_areas_all_years
plot(weighted_thawed_all_years[[250]])
# Compute total weighted thawed nitrogen (sum over all cells) in kg 
total_weighted_thawed <- global(weighted_thawed_all_years, "sum", na.rm = TRUE)

total_thawed_Pg <- total_weighted_thawed / 1e12
write.csv(total_thawed_Pg, "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/thawed_total_ssp585_std_nonloss.csv")

# Compute total area for each year
total_area_all_years <- global(cell_areas_all_years, "sum", na.rm = TRUE)

# Compute the final weighted average thawed nitrogen (kg N / m²)
weighted_mean_thawed <- total_weighted_thawed / total_area_all_years
write.csv(weighted_mean_thawed, "/Users/laraoxley/Desktop/data/CMIP6/final/final_results/weighted_mean_thawed_total_ssp585_std_nonloss.csv")



gc()               # clean up R memory
terra::tmpFiles(remove = TRUE)  # clear terra temp files
terra::tmpFiles(remove = TRUE, current = TRUE, orphan = TRUE, old = TRUE)

################################################################################




#### assuming permafrost thaws to 5m depth, using ESA map
library(terra)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")
# Load NetCDF files
N_data <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/TN_ESA_regrid.nc")
LC <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/LC_ESA_regrid.nc")
dim(LC)
dim(N_data)
plot(LC)
plot(N_data)
cell_area<-cellSize(LC, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_area, fun = "sum", na.rm = TRUE)$sum

# Set common extent
common_extent <- ext(-179.995, 179.995, 25.005, 84.99499)
ext(N_data) <- ext(LC) <- common_extent

# Ensure masks are numeric (not logical)
taiga_mask <- as.numeric(LC %in% c(1,2,3,4,5,8,9))
tundra_mask <- as.numeric(LC %in% c(6,7,10))
wetlands_mask <- as.numeric(LC == 11)
other<- as.numeric(LC %in% c(12,13,14))
barren_mask <- as.numeric(LC %in% c(15,16))
plot(taiga_mask)
total_mask<-barren_mask+wetlands_mask+tundra_mask+taiga_mask
plot(total_mask)


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

# Set depth to 5 meters

ALD<-5

# Compute thawed nitrogen
compute_thawed_N <- function(ALD, N, a, b, k) {
  A_ALD <- a * ALD + (b / k) * (1 - exp(-k * ALD))
  ifel(N == 0, NA, N * A_ALD)  # Avoid multiplying by zero
}

taiga_N[taiga_N == 0] <- NA
tundra_N[tundra_N == 0] <- NA
wetlands_N[wetlands_N == 0] <- NA
barren_N[barren_N == 0] <- NA
plot(taiga_N)
total_N<-barren_N+wetlands_N+tundra_N+taiga_N

# Compute thawed nitrogen
thawed_taiga <- compute_thawed_N(ALD, taiga_N, params$taiga['a'], params$taiga['b'], params$taiga['k'])
thawed_tundra <- compute_thawed_N(ALD, tundra_N, params$tundra['a'], params$tundra['b'], params$tundra['k'])
thawed_barren <- compute_thawed_N(ALD, barren_N, params$barren['a'], params$barren['b'], params$barren['k'])
thawed_wetlands <- (wetlands_N * ALD)

plot(thawed_tundra)
plot(thawed_taiga)
plot(thawed_barren)
plot(thawed_wetlands)
# Replace NA values with 0
thawed_taiga[is.na(thawed_taiga)] <- 0
thawed_tundra[is.na(thawed_tundra)] <- 0
thawed_barren[is.na(thawed_barren)] <- 0
thawed_wetlands[is.na(thawed_wetlands)] <- 0

# Combine the rasters for each year
combined_thawed <- thawed_taiga + thawed_tundra + thawed_barren + thawed_wetlands

combined_thawed <- ifel(combined_thawed == 0, NA, combined_thawed)
plot(combined_thawed)

writeCDF(
  combined_thawed,
  filename = "/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/total_thawed_N_5m_ESA.nc",
  varname = "thawed_nitrogen",
  unit = "kg/m2",
  overwrite = TRUE
)

old_data <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/total_thawed_N_5m.nc")
cell_areas_old<-cellSize(old_data, mask = TRUE, unit = "m")

cell_areas<-cellSize(combined_thawed, mask = TRUE, unit = "m")
plot(cell_areas)

# Compute weighted thawed nitrogen for each year
weighted_thawed <- old_data * cell_areas_old

# Compute weighted thawed nitrogen for each year
weighted_thawed <- combined_thawed * cell_areas
plot(weighted_thawed)
# Compute total weighted thawed nitrogen (sum over all cells)
total_weighted_thawed <- global(weighted_thawed, "sum", na.rm = TRUE)
#in Pg
total_thawed_Pg <- total_weighted_thawed / 1e12
plot(weighted_thawed)

# Compute total area for each year
total_area <- global(cell_areas, "sum", na.rm = TRUE)

# Compute the final weighted average thawed nitrogen (kg N / m²)
weighted_mean_thawed <- total_weighted_thawed / total_area
write.csv(weighted_mean_thawed_taiga, "weighted_mean_thawed_taiga.csv")
