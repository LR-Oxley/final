library(terra)
library(here)
library(ggplot2)
getwd()
# Load NetCDF thawed nitrogen rasters
rast_mean_585 <- rast("final_data/mean_ssp585_corr_new.nc")
rast_std_585<- rast("final_data/std_ssp585_corr_new.nc")
plot(rast_std_585[[200]])
# mean ALD all the cell areas (total area of the grid)
mean_ssp585 <- global(rast_mean_585, fun = "mean", na.rm = TRUE)
std_ssp585<- global(rast_std_585, fun = "sd", na.rm = TRUE)

plot(rast_mean_585[[1]])

# Load NetCDF thawed nitrogen rasters
rast_mean_370 <- rast("final_data/mean_ssp370_corr_new.nc")
rast_std_370<- rast("final_data/std_ssp370_corr_new.nc")
# mean ALD all the cell areas (total area of the grid)
mean_ssp370 <- global(rast_mean_370, fun = "mean", na.rm = TRUE)
std_ssp370<- global(rast_std_370, fun = "std", na.rm = TRUE)


# Load NetCDF thawed nitrogen rasters
rast_mean_245 <- rast("final_data/mean_ssp245_corr_new.nc")
rast_std_245 <- rast("final_data/std_ssp245_corr_new.nc")
# mean ALD all the cell areas (total area of the grid)
mean_ssp245 <- global(rast_mean_245, fun = "mean", na.rm = TRUE)
std_ssp245<- global(rast_std_245, fun = "std", na.rm = TRUE)
plot(mean_ssp245)

# Load NetCDF thawed nitrogen rasters
rast_mean_126 <- rast("final_data/mean_ssp126_corr_new.nc")
rast_std_126 <- rast("final_data/std_ssp126_corr_new.nc")
# mean ALD all the cell areas (total area of the grid)
mean_ssp126 <- global(rast_mean_126, fun = "mean", na.rm = TRUE)
std_ssp126<- global(rast_std_126, fun = "std", na.rm = TRUE)
plot(mean_ssp245)

df<-data.frame(mean_ssp585, std_ssp585)
df$Year<-rep(1850:2099)
names(df)<-c("mean_585", "std_585", "Year")
ggplot(df, aes(x = Year)) +
  # Add shaded area for variance (mean ± 1 standard deviation)
  geom_ribbon(aes(ymin = mean_585 - std_585, ymax = mean_585 + std_585), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_585), color = "green", linewidth = 0.8) +
  labs(x = "Year", y = "ALD [m]", title = "ALD all scenarios") +
  xlim(1850, 2100) +
  theme_bw() +
  theme(legend.position = "none")  # Remove the legend if not needed
  


df<-data.frame(mean_ssp585, mean_ssp370, mean_ssp245, mean_ssp126, std_ssp585, std_ssp370, std_ssp245, std_ssp126)
df$Year<-rep(1850:2099)
names(df)<-c("mean_585", "mean_370", "mean_245", "mean_126", "std_585", "std_370", "std_245","std_126", "Year")

write.csv(df, "final_results/ALD_final.csv")



df<-read.csv("final_results/ALD_final.csv")

# Function to compute anomaly for a given SSP scenario
compute_anomaly <- function(df, mean_col, std_col) {
  # Select relevant columns
  ssp_data <- df[, c("Year", mean_col, std_col)]
  
  # Compute reference period mean and standard deviation (1860-1900)
  ref_data <- ssp_data[ssp_data$Year >= 1860 & ssp_data$Year <= 1900, ]
  
  ref_mean <- mean(ref_data[[mean_col]], na.rm = TRUE)
  ref_std  <- mean(ref_data[[std_col]], na.rm = TRUE)
  
  # Compute anomalies
  ssp_data[[mean_col]] <- ssp_data[[mean_col]] - ref_mean
  ssp_data[[std_col]]  <- ssp_data[[std_col]] - ref_std  # Adjust standard deviation as well
  
  return(ssp_data)
}


# Apply the function to all SSPs
ssp585_a <- compute_anomaly(df, "mean_585", "std_585")
ssp370_a <- compute_anomaly(df, "mean_370", "std_370")
ssp245_a <- compute_anomaly(df, "mean_245", "std_245")
ssp126_a <- compute_anomaly(df, "mean_126", "std_126")

# Combine into a single data frame
anomaly_total <- Reduce(function(x, y) merge(x, y, by = "Year"), list(ssp585_a, ssp370_a, ssp245_a, ssp126_a))

# View the final dataset
head(anomaly_total)
anomaly_total<-anomaly_total[,c("Year", "mean_585", "std_585","mean_370","std_370","mean_245", "std_245", "mean_126","std_126")]


# Create a new column for color based on the Year
anomaly_total$color_period <- ifelse(anomaly_total$Year <= 2014, "black", "colored")

# Create separate dataframes for before and after 2015
before_2015 <- anomaly_total[anomaly_total$Year <= 2014, ]
after_2015 <- anomaly_total[anomaly_total$Year > 2014, ]

# Add period column
before_2015$Period <- "Before 2015"
after_2015$Period <- "After 2015"

# Combine datasets
combined_data <- bind_rows(before_2015, after_2015)

# Convert to long format
long_data <- combined_data %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "SSP", values_to = "Mean_ALD") %>%
  pivot_longer(cols = starts_with("std_"), names_to = "SSP_std", values_to = "STD_ALD") %>%
  filter(gsub("mean_", "", SSP) == gsub("std_", "", SSP_std)) %>%
  dplyr::select(-SSP_std) %>%
  mutate(SSP = gsub("mean_", "SSP", SSP))  # Rename scenarios

# Define SSP colors for after 2015
ssp_colors <- c("SSP585" = "blue", "SSP370" = "darkgreen", "SSP245" = "orange", "SSP126" = "red")


# Compute 20-year rolling mean and standard deviation for each SSP
long_data <- long_data %>%
  group_by(SSP) %>%
  mutate(
    Rolling_Mean_ALD = rollmean(Mean_ALD, k = 20, fill = NA, align = "center"),
    Rolling_STD_ALD  = rollapply(STD_ALD, width = 20, FUN = mean, fill = NA, align = "center")
  )

# Plot
ggplot(long_data, aes(x = Year, y = Mean_ALD, group = SSP)) +
  
  # Before 2015: Black lines & grey ribbons
  geom_line(data = filter(long_data, Period == "Before 2015"), color = "black", linewidth = 0.8) +
  geom_ribbon(data = filter(long_data, Period == "Before 2015"),
              aes(ymin = Mean_ALD - STD_ALD, ymax = Mean_ALD + STD_ALD),
              fill = "grey", alpha = 0.2) +
  
  # After 2015: Colored lines & ribbons
  geom_line(data = filter(long_data, Period == "After 2015"),
            aes(color = SSP), linewidth = 0.8) +
  geom_ribbon(data = filter(long_data, Period == "After 2015"),
              aes(ymin = Mean_ALD - STD_ALD, ymax = Mean_ALD + STD_ALD, fill = SSP),
              alpha = 0.2) +
  
  # Rolling Mean before 2015 (Black line)
  geom_line(data = filter(long_data, Year < 2015),
            aes(y = Rolling_Mean_ALD), color = "black", linewidth = 1.2, linetype = "solid") +
  
  # Rolling Mean after 2015 (Colored lines)
  geom_line(data = filter(long_data, Year >= 2015),
            aes(y = Rolling_Mean_ALD, color = SSP), linewidth = 1.2, linetype = "solid") +
  
  # Rolling STD before 2015 (Grey ribbon)
  geom_ribbon(data = filter(long_data, Year < 2015),
              aes(ymin = Rolling_Mean_ALD - Rolling_STD_ALD, 
                  ymax = Rolling_Mean_ALD + Rolling_STD_ALD),
              fill = "grey", alpha = 0.15) +
  
  # Rolling STD after 2015 (Colored ribbons)
  #geom_ribbon(data = filter(long_data, Year >= 2015),
  #aes(ymin = Rolling_Mean_ALD - Rolling_STD_ALD, 
  #ymax = Rolling_Mean_ALD + Rolling_STD_ALD, 
  #fill = SSP),
  #alpha = 0.15) +
  
  # Vertical line for 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  
  # Labels and title
  labs(x = "Year", y = "ALD [m]", title = "Pan-Arctic max active layer depth, relative to 1880-1900", 
       color = "SSP Scenario", fill = "SSP Scenario") +
  
  # Color scales
  scale_color_manual(values = ssp_colors) +
  scale_fill_manual(values = ssp_colors) +
  
  # Theme settings
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )




### calculate mean ALD
library(dplyr)

# Example dataframe (replace with your actual data)
df <- data.frame(df)

# Compute mean ALD for each SSP and overall mean for the two periods
mean_ALD_1880_1900 <- df %>%
  filter(Year >= 1890 & Year <= 1900) %>%
  summarise(across(starts_with("mean_"), mean)) %>%
  mutate(Mean_All_SSPs = rowMeans(.))

mean_ALD_1990_2010 <- df %>%
  filter(Year >= 2004 & Year <= 2014) %>%
  summarise(across(starts_with("mean_"), mean)) %>%
  mutate(Mean_All_SSPs = rowMeans(.))

mean_ALD_2080_2100 <- df %>%
  filter(Year >= 2080 & Year <= 2100) %>%
  summarise(across(starts_with("mean_"), mean)) %>%
  mutate(Mean_All_SSPs = rowMeans(.))

# Print results
print(mean_ALD_1880_1900)
print(mean_ALD_1990_2010)
print(mean_ALD_2080_2100)

# Compute percentage increase
percentage_increase <- ((mean_ALD_1990_2010 - mean_ALD_1880_1900) / mean_ALD_1880_1900) * 100
# Compute percentage increase
percentage_increase <- ((mean_ALD_2080_2100 - mean_ALD_1990_2010) / mean_ALD_1990_2010) * 100

# Print results
print(percentage_increase)

# Compute difference in cm 
mean_ALD_1880_1900_cm<-mean_ALD_1880_1900*100
mean_ALD_1990_2010_cm<-mean_ALD_1990_2010*100
mean_ALD_2080_2100_cm<-mean_ALD_2080_2100*100

diff_pre_industrial_present <- mean_ALD_1990_2010_cm-mean_ALD_1880_1900_cm
diff_present_future<-mean_ALD_2080_2100_cm-mean_ALD_1990_2010_cm

# past: 34.5 to 38.5 cm increase in 100 years: 0.3 cm / year
# future: 100 to 250cm increase in 100 years: 1 to 2.5 cm / year

