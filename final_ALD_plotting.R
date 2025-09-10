## Plotting Active layer depth increase, relative to preindustrial (1880-1900)
## plot active layer depth increase
library(ggplot2)
library(dplyr)
library(zoo)

library(tidyr)

setwd("/Users/laraoxley/Desktop/data/CMIP6/final")
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



### Calculating how much Nitrogen will be in the active layer following active layer depth increase


