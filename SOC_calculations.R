# carbon calculations 
# Define the equation function
eq <- function(depth, a, b, k) {
  a + b * exp(-k * abs(depth))  # Use absolute depth to ensure exponent works correctly
}

# Define parameters for each biome
params <- list(
  Taiga  = list(a = 0.05, b = 0.5, k = 0.04),
  Tundra = list(a = 0.2,  b = 0.30, k = 0.02),
  Barren = list(a = 0,     b = 0.2, k = 0.04)
)

# Set up the plot with depth from 0 to -300 cm
plot(NA, xlim = c(0, 0.8), ylim = c(-300, 0), xlab = "SOC (kg/m²)", ylab = "Depth (cm)", main = "SOC vs Depth")
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
wetlands_TN <- 0.4 # Constant TN value for wetlands
lines(rep(wetlands_TN, 100), seq(-300, 0, length.out = 100), col = "green", lwd = 2, lty = 1)

# Add legend
legend("topright", legend = c(names(params), "Wetlands"), col = c(colors, "green"), lwd = 2, lty = c(1, 1, 1, 1))


############################

library(terra)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/SOC")
# Load NetCDF files
ALD <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/SOC/std_ssp585_corr.nc")
SOC_data <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/SOC/TN_30deg_corr.nc",lyr=2)
LC <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/SOC/LC_remapnn_corr.nc")

cell_area<-cellSize(LC, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_area, fun = "sum", na.rm = TRUE)$sum

# Print the total area (in m²)
print(SOC_data)
plot(SOC_data)

# Get latitude and longitude from the grid
lat <- unique(yFromCell(ALD, 1:ncell(ALD)))
lon <- unique(xFromCell(ALD, 1:ncell(ALD)))

# Set common extent
common_extent <- ext(-179.95, 179.95, 30, 90)
ext(ALD) <- ext(SOC_data) <- ext(LC) <- common_extent

# Ensure masks are numeric (not logical)
taiga_mask <- as.numeric(LC %in% c(1,2,3,4,5,8,9))
tundra_mask <- as.numeric(LC %in% c(6,7,10))
wetlands_mask <- as.numeric(LC == 11)
other<- as.numeric(LC %in% c(12,13,14))
barren_mask <- as.numeric(LC %in% c(15,16))
plot(taiga_mask)
total_mask<-barren_mask+wetlands_mask+tundra_mask+taiga_mask

# Create a categorical raster with unique values for each land cover type
landcover_map <- taiga_mask * 1 + 
  tundra_mask * 2 + 
  wetlands_mask * 3 + 
  other * 4 + 
  barren_mask * 5

# Convert raster to data frame for ggplot
df <- as.data.frame(landcover_map, xy = TRUE)
colnames(df) <- c("lon", "lat", "landcover")

# Define land cover categories
df$landcover <- factor(df$landcover, 
                       levels = c(1, 2, 3, 4, 5),
                       labels = c("Taiga", "Tundra", "Wetlands", "Other", "Barren"))

# Define color mapping
landcover_colors <- c("Taiga" = "darkgreen", 
                      "Tundra" = "lightblue", 
                      "Wetlands" = "lightgreen", 
                      "Other" = "grey", 
                      "Barren" = "brown")

# Get bounding box for 40–90° latitude in LAEA
bbox <- st_bbox(df_sf)  # Get data extent
bbox[2] <- min(df_sf$geometry[[1]][, 2])  # Set lower latitude limit (~40°)


# Create the plot
#ggplot(df, aes(x = lon, y = lat, fill = landcover)) +
  geom_tile() +
  scale_fill_manual(values = landcover_colors, name = "Land Cover Type") +
  labs(title = "Land Cover Masks") +
  theme_minimal() +
  theme(legend.position = "right") # Move legend outside of plot

params <- list(
  taiga  = list(a = 0.05, b = 0.5, k = 0.04),
  tundra = list(a = 0.2,  b = 0.30, k = 0.02),
  barren = list(a = 0,     b = 0.2, k = 0.04))

str(params$taiga)
### calculate wetlands like homogeneous 

# Normalize Nitrogen (replace ifel with terra::ifel)
normalize_SOC <- function(SOC, a, b, k) {
  A_3m <- 3 * a + (b / k) * (1 - exp(-3 * k))
  SOC / A_3m
}
print(class(SOC_data))
print(class(taiga_mask))
print(SOC_data * taiga_mask)
class(SOC_data * taiga_mask)

taiga_SOC <- normalize_SOC(SOC_data * taiga_mask, params$taiga$a, params$taiga$b, params$taiga$k)
tundra_SOC<- normalize_SOC(SOC_data * tundra_mask, params$tundra$a, params$tundra$b, params$tundra$k)
barren_SOC <- normalize_SOC(SOC_data * barren_mask, params$barren$a, params$barren$b, params$barren$k)
wetlands_SOC <- (SOC_data * wetlands_mask) / 3


# Compute thawed nitrogen
compute_thawed_SOC <- function(ALD, SOC, a, b, k) {
  A_ALD <- a * ALD + (b / k) * (1 - exp(-k * ALD))
  ifel(SOC == 0, NA, SOC * A_ALD)  # Avoid multiplying by zero
}

taiga_SOC[taiga_SOC == 0] <- NA
tundra_SOC[tundra_SOC == 0] <- NA
wetlands_SOC[wetlands_SOC == 0] <- NA
barren_SOC[barren_SOC == 0] <- NA
plot(taiga_SOC)
total_SOC<-barren_SOC+wetlands_SOC+tundra_SOC+taiga_SOC
plot(total_SOC)
# Compute thawed nitrogen
thawed_taiga <- compute_thawed_SOC(ALD ,taiga_SOC, params$taiga$a, params$taiga$b, params$taiga$k)
thawed_tundra <- compute_thawed_SOC(ALD, tundra_SOC, params$tundra$a, params$tundra$b, params$tundra$k)
thawed_barren <- compute_thawed_SOC(ALD, barren_SOC, params$barren$a, params$barren$b, params$barren$k)
thawed_wetlands <- (wetlands_SOC * ALD)


plot(thawed_taiga[[1]], main = "Thawed Nitrogen: Taiga")
plot(thawed_tundra[[1]], main = "Thawed Nitrogen: Tundra")
plot(thawed_barren[[1]], main = "Thawed Nitrogen: Barren")
plot(thawed_wetlands[[1]], main = "Thawed Nitrogen: Wetlands")
combined<-thawed_taiga+thawed_tundra+thawed_barren+thawed_wetlands
# Replace NA values with 0
thawed_taiga[is.na(thawed_taiga)] <- 0
thawed_tundra[is.na(thawed_tundra)] <- 0
thawed_barren[is.na(thawed_barren)] <- 0
thawed_wetlands[is.na(thawed_wetlands)] <- 0

# Combine the rasters for each year
combined_thawed <- thawed_taiga + thawed_tundra + thawed_barren + thawed_wetlands

combined_thawed <- ifel(combined_thawed == 0, NA, combined_thawed)
plot(combined_thawed[[1]])
plot()
# Calculate mean for each layer (year) in the SpatRaster object
taiga_thawed_N<- global(thawed_taiga, fun = "mean", na.rm = TRUE)
tundra_thawed_N<- global(thawed_tundra, fun = "mean", na.rm = TRUE)
barren_thawed_N<- global(thawed_barren, fun = "mean", na.rm = TRUE)
wetlands_thawed_N<- global(thawed_wetlands, fun = "mean", na.rm = TRUE)

setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/SOC/std_ssp370")

# Save combined raster to a NetCDF file
writeCDF(
  thawed_taiga,
  "thawed_taiga.nc",
  varname = "Thawed_SOC",
  overwrite = TRUE
)
# Save combined raster to a NetCDF file
writeCDF(
  thawed_tundra,
  "thawed_tundra.nc",
  varname = "Thawed_SOC",
  overwrite = TRUE
)
# Save combined raster to a NetCDF file
writeCDF(
  thawed_wetlands,
  "thawed_wetlands.nc",
  varname = "Thawed_SOC",
  overwrite = TRUE
)
# Save combined raster to a NetCDF file
writeCDF(
  thawed_barren,
  "thawed_barren.nc",
  varname = "Thawed_SOC",
  overwrite = TRUE
)


library(terra)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/SOC/std_ssp585")

# Load NetCDF thawed nitrogen rasters
thawed_taiga <- rast("thawed_taiga.nc")
thawed_tundra <- rast("thawed_tundra.nc")
thawed_barren <- rast("thawed_barren.nc")
thawed_wetlands <- rast("thawed_wetlands.nc")
plot(thawed_taiga)
# Replace NA values with 0 to combine the individual rasters
thawed_taiga[is.na(thawed_taiga)] <- 0
thawed_tundra[is.na(thawed_tundra)] <- 0
thawed_barren[is.na(thawed_barren)] <- 0
thawed_wetlands[is.na(thawed_wetlands)] <- 0

plot(thawed_tundra)
# Combine the rasters for each year
combined_thawed <- thawed_taiga + thawed_tundra + thawed_barren + thawed_wetlands

combined_thawed <- ifel(combined_thawed == 0, NA, combined_thawed)
plot(combined_thawed)


writeCDF(
  combined_thawed,
  "thawed_mean_585.nc",
  varname = "Thawed_N_std",
  overwrite = TRUE
)
# Calculate the area of each grid cell in m^2
??cellSize
cell_areas <- cellSize(combined_thawed, mask=TRUE, unit = "m")
plot(cell_areas)
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_areas, fun = "sum", na.rm = TRUE)$sum

# Print the total area (in m²)
print(total_area)
# Weight the thawed nitrogen values by grid cell area
weighted_thawed <- combined_thawed * cell_areas
#plot(weighted_thawed)

# Sum the weighted values for each year
total_thawed <- global(weighted_thawed, "sum", na.rm = TRUE)
total_thawed_Pg <- total_thawed / 1e12
# Convert to a data frame
total_thawed_df <- data.frame(
  Year = 1:nlyr(combined_thawed),  # Assuming 251 years
  Total_Thawed_C_Pg = total_thawed_Pg$sum
)
write.csv(total_thawed_df, "total_thawed_C_Pg_std_ssp585.csv")

# Print the first few rows
head(total_thawed_df)
tail(total_thawed_df)


# Compute weighted average thawed nitrogen (kg N / m²)
weighted_avg_thawed_N <- total_thawed / total_area  # kg N/m²

df<-data.frame(weighted_avg_thawed_N)
df$Year<-rep(1851:2100)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/SOC/std_ssp585")
write.csv(df, "SOC_weighted_kg_per_m2_std_ssp585.csv")
library(ggplot2)
ggplot(df, aes(x = Year, y = sum)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "Total Thawed C Over Time",
    x = "Year",
    y = "Total Thawed C (kg)"
  ) +
  theme_minimal()

