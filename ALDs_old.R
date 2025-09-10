library(terra)
library(ggplot2)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")

# Load NetCDF thawed nitrogen rasters
rast_mean_585 <- rast("mean_ssp585_corr.nc")
rast_std_585<- rast("std_ssp585_corr.nc")
# mean ALD all the cell areas (total area of the grid)
mean_ssp585 <- global(rast_mean_585, fun = "mean", na.rm = TRUE)
std_ssp585<- global(rast_std_585, fun = "std", na.rm = TRUE)
plot(rast_mean_585[[1]])
# Repeat the last row
mean_ssp585 <- rbind(mean_ssp585, mean_ssp585[nrow(mean_ssp585), ])
std_ssp585 <- rbind(std_ssp585, std_ssp585[nrow(std_ssp585), ])

# Load NetCDF thawed nitrogen rasters
rast_mean_370 <- rast("mean_ssp370_corr.nc")
rast_std_370<- rast("std_ssp370_corr.nc")
# mean ALD all the cell areas (total area of the grid)
mean_ssp370 <- global(rast_mean_370, fun = "mean", na.rm = TRUE)
std_ssp370<- global(rast_std_370, fun = "std", na.rm = TRUE)

# Load NetCDF thawed nitrogen rasters
rast_mean_585 <- rast("mean_ssp585_corr.nc")
rast_std_585 <- rast("std_ssp585_corr.nc")

# mean ALD all the cell areas (total area of the grid)
mean_ssp585 <- global(rast_mean_585, fun = "mean", na.rm = TRUE)
std_ssp585<- global(rast_std_585, fun = "std", na.rm = TRUE)

# Load NetCDF thawed nitrogen rasters
rast_mean_245 <- rast("mean_ssp245_corr.nc")
rast_std_245 <- rast("std_ssp245_corr.nc")

# mean ALD all the cell areas (total area of the grid)
mean_ssp245 <- global(rast_mean_245, fun = "mean", na.rm = TRUE)
std_ssp245<- global(rast_std_245, fun = "std", na.rm = TRUE)
plot(mean_ssp245)

df<-data.frame(mean_ssp585, std_ssp585)
df$Year<-rep(1850:2100)
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
df$Year<-rep(1850:2100)
names(df)<-c("mean_585", "mean_370", "mean_245", "mean_126", "std_585", "std_370", "std_245","std_126", "Year")

ggplot(df, aes(x = Year)) +
  # Add shaded area for variance (mean ± 1 standard deviation)
  geom_ribbon(aes(ymin = mean_370 - std_370, ymax = mean_370 + std_370), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_370), color = "green", linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_585 - std_585, ymax = mean_585 + std_585), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_585),  color = "blue", linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_585 - std_585, ymax = mean_585 + std_585), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_585), color = "red",  linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_245 - std_245, ymax = mean_245 + std_245), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_245), color = "yellow",  linewidth = 0.8) +
  labs(x = "Year", y = "ALD [m]", title = "ALD all scenarios") +
  xlim(1850, 2100) +
  theme_bw() +
  theme(legend.position = "none")  # Remove the legend if not needed

#anomaly plot
ssp585<-df[, c("Year", "mean_585", "std_585")]
# Filter the data for Year between 1860 and 1920
filtered_585 <- ssp585[ssp585$Year >= 1860 & ssp585$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_585<- mean(filtered_585$mean_585, na.rm = TRUE)

# Assuming 'Year' is a non-numeric column you want to exclude
ssp585_a <- ssp585
ssp585_a[ , -which(names(ssp585) %in% c("Year", "std_585"))] <- 
  ssp585_a[ , -which(names(ssp585) %in% c("Year", "std_585"))] - mean_585

#anomaly plot
ssp245<-df[, c("Year", "mean_245", "std_245")]
# Filter the data for Year between 1860 and 1920
filtered_245 <- ssp245[ssp245$Year >= 1860 & ssp245$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_245<- mean(filtered_245$mean_245, na.rm = TRUE)

# Assuming 'Year' is a non-numeric column you want to exclude
ssp245_a <- ssp245
ssp245_a[ , -which(names(ssp245) %in% c("Year", "std_245"))] <- 
  ssp245_a[ , -which(names(ssp245) %in% c("Year", "std_245"))] - mean_245

#anomaly plot
ssp126<-df[, c("Year", "mean_126", "std_126")]
# Filter the data for Year between 1860 and 1920
filtered_126 <- ssp126[ssp126$Year >= 1860 & ssp126$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_126<- mean(filtered_126$mean_126, na.rm = TRUE)

# Assuming 'Year' is a non-numeric column you want to exclude
ssp126_a <- ssp126
ssp126_a[ , -which(names(ssp126) %in% c("Year", "std_126"))] <- 
  ssp126_a[ , -which(names(ssp126) %in% c("Year", "std_126"))] - mean_126

#anomaly plot
ssp370<-df[, c("Year", "mean_370", "std_370")]
# Filter the data for Year between 1860 and 1920
filtered_370 <- ssp370[ssp370$Year >= 1860 & ssp370$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_370<- mean(filtered_370$mean_370, na.rm = TRUE)

# Assuming 'Year' is a non-numeric column you want to exclude
ssp370_a <- ssp370
ssp370_a[ , -which(names(ssp370) %in% c("Year", "std_370"))] <- 
  ssp370_a[ , -which(names(ssp370) %in% c("Year", "std_370"))] - mean_370

anomaly_total<-data.frame(ssp585_a, ssp370_a, ssp245_a,ssp126_a)
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
  
  # Vertical line for 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  
  # Labels and title
  labs(x = "Year", y = "ALD [m]", title = "Increase in Active Layer Depth Relative to 1880-1900", 
       color = "SSP Scenario", fill = "SSP Scenario") +
  
  # Color scales only for after 2015
  scale_color_manual(values = ssp_colors) +
  scale_fill_manual(values = ssp_colors) +
  
  # Theme settings
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10),      # Increase axis text size
    axis.title = element_text(size = 10)      # Increase axis title size
  )


## show rolling mean and rolling std
library(ggplot2)
library(dplyr)
library(zoo)

library(tidyr)

setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")
df<- read.csv("ALD_all_scenarios.csv")
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
  labs(x = "Year", y = "ALD [m]", title = "Increase in Active Layer Depth Relative to 1880-1900", 
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

