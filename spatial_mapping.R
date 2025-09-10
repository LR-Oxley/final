## plot thawed nitrogen maps
library(terra)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")
# Load the NetCDF file
thawedN <- rast("thawed_total_ssp370_2m_limit.nc")  # replace with your actual file name

# Check layer time (optional, in case time is part of the layer names)
names(thawedN)  # You might see names like "thawed_N_1850", "thawed_N_1851", etc.

# Extract years from names if necessary
years <- 1850:2100  # adjust if needed
names(thawedN) <- paste0("Y", years)

# Subset for each time slice
thawedN_1880_1900 <- thawedN[[which(years %in% 1880:1900)]]
thawedN_1990_2010 <- thawedN[[which(years %in% 1990:2010)]]
thawedN_2080_2100 <- thawedN[[which(years %in% 2080:2100)]]



# difference preindustrial - present day - future
delta_present_day<-thawedN_1990_2010-thawedN_1880_1900
delta_future<-thawedN_2080_2100-thawedN_1880_1900
plot(thawedN_1880_1900[[21]])
# Compute mean across years
mean_1880_1900 <- mean(thawedN_1880_1900, na.rm = TRUE)
mean_1990_2010 <- mean(delta_present_day, na.rm = TRUE)
mean_2080_2100 <- mean(delta_future, na.rm = TRUE)

# Convert units from kg/m² to kg/ha
mean_1880_1900 <- mean_1880_1900 * 10000
mean_1990_2010 <- mean_1990_2010 * 10000
mean_2080_2100 <- mean_2080_2100 * 10000
mean_1990_2010_5per <- mean_1990_2010 * 0.05
mean_2080_2100_5per <- mean_2080_2100 * 0.05
#for annualized mean
mean_1990_2010_5per <- mean_1990_2010 / 20
mean_2080_2100_5per <- mean_2080_2100 / 20


# Stack them for plotting
all_means <- c(mean_1880_1900, mean_1990_2010, mean_2080_2100)
names(all_means) <- c("1880-1900", "1990-2010", "2080-2100")

# Plot using terra
plot(all_means, main = names(all_means), col = rev(terrain.colors(20)))

df_plot <- function(r, period) {
  df <- as.data.frame(r, xy = TRUE)
  names(df)[3] <- "thawed_N"
  df$Period <- period
  return(df)
}

df_1880 <- df_plot(mean_1880_1900, "1880–1900")
df_1990 <- df_plot(mean_1990_2010, "1990–2010")
df_2080 <- df_plot(mean_2080_2100, "2080–2100")
df_1990_5per <- df_plot(mean_1990_2010_5per, "1990–2010")
df_2080_5per <- df_plot(mean_2080_2100_5per, "2080–2100")

# Calculate increase in thawed nitrogen from 1990–2010 to 2080–2100
delta_thawedN <- mean_2080_2100_5per - mean_1990_2010_5per

# Convert raster to data frame for ggplot
delta_df <- as.data.frame(delta_thawedN, xy = TRUE, na.rm = TRUE)
colnames(delta_df) <- c("x", "y", "thawed_N")

ggplot(delta_df, aes(x = x, y = y, fill = change)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Δ kg N/ha") +
  labs(
    title = "Increase in Thawed Nitrogen (1990–2010 to 2080–2100)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

ggplot(delta_df, aes(x = x, y = y, fill = thawed_N)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Thawed N") +
  theme_minimal() +
  labs(title = "Mean Thawed Nitrogen in Arctic Soil")


## for a flat circular projection
# Step 1: Filter the data to include only the Arctic region
arctic_df <- subset(df_2080_5per)

# Step 2: Convert the filtered data to an sf object
arctic_sf <- st_as_sf(arctic_df, coords = c("x", "y"), crs = 4326)  # WGS84

# Step 3: Define the LAEA projection centered on the North Pole
laea_crs <- "+proj=laea +lat_0=90 +lon_0=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Step 4: Reproject the data into the LAEA projection
arctic_sf_laea <- st_transform(arctic_sf, crs = laea_crs)
bbox <- st_bbox(arctic_sf_laea)
print(bbox)
# Define custom limits for the plot
xlim <- c(-4500000, 4500000)  # Adjust these values based on your data
ylim <- c(-4500000, 4500000)  # Adjust these values based on your data
coastlines <- ne_coastline(scale = "medium", returnclass = "sf")

# Step 5: Reproject the coastlines to match the LAEA projection
coastlines_laea <- st_transform(coastlines, crs = laea_crs)
arctic_sf_laea <- arctic_sf_laea[!is.na(arctic_sf_laea$thawed_N), ]
# Step 6: Create the plot with the LAEA projection
ggplot() +
  geom_sf(data = arctic_sf_laea, aes(color = thawed_N), size = 0.5) +
  scale_color_viridis_c(
    name = "Soil N (kg N / ha)",
    limits = c(0, 5000)  # Show only min and max
  )+
  geom_sf(data = coastlines_laea, color = "black", size = 0.2) +  # Add reprojected coastlines
  coord_sf(crs = laea_crs, xlim = xlim, ylim = ylim) +  # LAEA projection with custom limits
  theme_minimal() +
  theme(legend.position = "right") +  # Remove legend
  labs(
    x = "Longitude", y = "Latitude"
  )


# Calculate increase in thawed nitrogen from 1990–2010 to 2080–2100
delta_thawedN <- mean_2080_2100 - mean_1990_2010

# Convert raster to data frame for ggplot
delta_df <- as.data.frame(delta_thawedN, xy = TRUE, na.rm = TRUE)
colnames(delta_df) <- c("x", "y", "change")

ggplot(delta_df, aes(x = x, y = y, fill = change)) +
  geom_tile() +
  scale_fill_viridis(name = "Δ kg N/ha") +
  labs(
    title = "Increase in Thawed Nitrogen (1990–2010 to 2080–2100)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

library(terra)
library(ggplot2)
library(sf)
library(viridis)
library(rnaturalearth)
library(ggplot2)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(tidyr)
library(viridis)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/")

thawed_N <- rast("total_thawed_N_5m_ESA.nc")


# Get world coastlines
coastlines <- ne_coastline(scale = "medium", returnclass = "sf")
# Convert raster to data frame


# Step 2: Calculate the mean thawed N for each grid cell over 2080-2100
thawed_n <- thawed_N * 10000

# Step 3: Convert the mean SpatRaster to a dataframe for ggplot2
mean_thawed_n_df <- as.data.frame(thawed_N, xy = TRUE, na.rm = TRUE)
colnames(mean_thawed_n_df) <- c("x", "y", "Thawed_N")

ggplot() +
  geom_tile(data = mean_thawed_n_df, aes(x = x, y = y, fill = Thawed_N)) +
  scale_fill_viridis(
    name = "Soil N (kg N / m2)",
    limits = c(0, 12)  # Show only min and max
  )+  # Adjust colors as needed
  geom_sf(data = coastlines, color = "black", size = 0.5) +  # Add coastlines
  coord_sf(xlim = c(-180, 180), ylim = c(50, 90)) +  # Keep spatial consistency
  theme_minimal() +
  labs(title = "Soil Nitrogen down to 5m thaw", x = "Longitude", y = "Latitude")


## for a flat circular projection
# Step 1: Filter the data to include only the Arctic region
arctic_df <- subset(mean_thawed_n_df, y >= 30)

# Step 2: Convert the filtered data to an sf object
arctic_sf <- st_as_sf(arctic_df, coords = c("x", "y"), crs = 4326)  # WGS84

# Step 3: Define the LAEA projection centered on the North Pole
laea_crs <- "+proj=laea +lat_0=90 +lon_0=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Step 4: Reproject the data into the LAEA projection
arctic_sf_laea <- st_transform(arctic_sf, crs = laea_crs)
bbox <- st_bbox(arctic_sf_laea)
print(bbox)
# Define custom limits for the plot
xlim <- c(-4500000, 4500000)  # Adjust these values based on your data
ylim <- c(-4500000, 4500000)  # Adjust these values based on your data

# Step 5: Reproject the coastlines to match the LAEA projection
coastlines_laea <- st_transform(coastlines, crs = laea_crs)
arctic_sf_laea <- arctic_sf_laea[!is.na(arctic_sf_laea$Thawed_N), ]
# Step 6: Create the plot with the LAEA projection

# Calculate the min and max values of Mean_Thawed_N
min_thawed_N <- min(arctic_sf_laea$Thawed_N, na.rm = TRUE)
max_thawed_N <- max(arctic_sf_laea$Thawed_N, na.rm = TRUE)
# Create a sequence of breaks for the legend
legend_breaks <- c(-0.2, 0, 2, 4, 6, 8, 10, 12, 14)# Create the plot with explicit legend limits
ggplot() +
  geom_sf(data = arctic_sf_laea, aes(color = Thawed_N), size = 0.5) +
  scale_color_viridis_c(
    name = "Soil N (kg N / m²)",
    limits = c(0, 12)  # Show only min and max
  )+
  geom_sf(data = coastlines_laea, color = "black", size = 0.2) +  # Add reprojected coastlines
  coord_sf(crs = laea_crs, xlim = xlim, ylim = ylim) +  # LAEA projection with custom limits
  theme_minimal() +
  theme(legend.position = "none") +  # Remove legend
  labs(
    x = "Longitude", y = "Latitude"
  )



################
### spatial mapping of land cover data
library(terra)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
landcover_map <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/LC_remapnn_corr.nc")

# Define Lambert Azimuthal Equal-Area (LAEA) projection centered on North Pole
laea_proj <- "+proj=laea +lat_0=90 +lon_0=0 +datum=WGS84"

# Convert raster to a data frame
df <- as.data.frame(landcover_map, xy = TRUE)
colnames(df) <- c("lon", "lat", "landcover")

# Remove NAs for efficiency
df <- df[!is.na(df$landcover), ]

# Convert to sf object and reproject
df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
df_sf <- st_transform(df_sf, crs = laea_proj)

# Convert back to data frame with coordinates
df_proj <- as.data.frame(st_coordinates(df_sf))
df_proj$landcover <- df$landcover  # Preserve land cover values

# Load simplified coastline and reproject
coastlines <- ne_countries(scale = "medium", returnclass = "sf")
coastlines <- st_transform(coastlines, crs = laea_proj)

# Define color mapping
landcover_colors <- c("1" = "darkgreen", 
                      "2" = "lightblue", 
                      "3" = "lightgreen", 
                      "4" = "grey", 
                      "5" = "brown")
xlim <- c(-4500000, 4500000)  # Adjust these values based on your data
ylim <- c(-4500000, 4500000)  # Adjust these values based on your data

# Create a fast plot
ggplot() +
  geom_tile(data = df_proj, aes(x = X, y = Y, fill = as.factor(landcover))) +  # Raster-like plot
  geom_sf(data = coastlines, color = "black", fill = NA, size = 0.2) +  # Coastlines
  scale_fill_manual(values = landcover_colors, name = "Land Cover Type") +
  labs(title = "Land Cover Masks in LAEA Projection (40-90° Lat)") +
  coord_sf() +  # Keep default limits
  theme_minimal() +
  ylim(30,90)+
  theme(legend.position = "right")



library(terra)
library(sf)
library(ggplot2)
library(viridis)

# Load the landcover raster
landcover_map <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/LC_remapnn_corr.nc")

# Define Lambert Azimuthal Equal-Area (LAEA) projection centered on North Pole
laea_proj <- "+proj=laea +lat_0=90 +lon_0=0 +datum=WGS84"

# Convert raster to a data frame
df <- as.data.frame(landcover_map, xy = TRUE)
colnames(df) <- c("x", "y", "landcover")

# Step 1: Create a new column for land cover categories
df$landcover_category <- NA

# Step 2: Map the original land cover values to the new categories
df$landcover_category[df$landcover %in% c(1, 2, 3, 4, 5, 8, 9)] <- "taiga"
df$landcover_category[df$landcover %in% c(6, 7, 10)] <- "tundra"
df$landcover_category[df$landcover == 11] <- "wetlands"
df$landcover_category[df$landcover %in% c(12, 13, 14)] <- "other"
df$landcover_category[df$landcover %in% c(15, 16)] <- "barren"

# Step 3: Filter the data to include only the Arctic region
arctic_df <- subset(df, y >= 30)

# Step 4: Convert the filtered data to an sf object
arctic_sf <- st_as_sf(arctic_df, coords = c("x", "y"), crs = 4326)  # WGS84

# Step 5: Define the LAEA projection centered on the North Pole
laea_crs <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Step 6: Reproject the data into the LAEA projection
arctic_sf_laea <- st_transform(arctic_sf, crs = laea_crs)
bbox <- st_bbox(arctic_sf_laea)
print(bbox)

# Define custom limits for the plot
xlim <- c(-4500000, 4500000)  # Adjust these values based on your data
ylim <- c(-4500000, 4500000)  # Adjust these values based on your data

# Step 7: Reproject the coastlines to match the LAEA projection
coastlines_laea <- st_transform(coastlines, crs = laea_crs)

# Step 8: Remove rows with NA landcover values
arctic_sf_laea <- arctic_sf_laea[!is.na(arctic_sf_laea$landcover_category), ]

# Step 9: Define the colors for each category
category_colors <- c(
  "tundra" = "lightblue",
  "taiga" = "darkgreen",
  "wetlands" = "green",
  "barren" = "brown",
  "other" = "grey"
)

# Step 10: Create the plot with the LAEA projection and custom colors
ggplot() +
  geom_sf(data = arctic_sf_laea, aes(color = landcover_category), size = 0.5) +
  scale_color_manual(
    name = "Land Cover Category",
    values = category_colors  # Use the defined colors
  ) +
  geom_sf(data = coastlines_laea, color = "black", size = 0.2) +  # Add reprojected coastlines
  coord_sf(crs = laea_crs, xlim = xlim, ylim = ylim) +  # LAEA projection with custom limits
  theme_minimal() +
  theme(
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_text(size = 14, face = "bold")  # Increase legend title size and make it bold
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5)))  # Increase the size of legend color keys

    ?theme
summary(arctic_sf_laea)
