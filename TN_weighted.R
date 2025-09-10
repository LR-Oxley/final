##ALD Year axis ssps
library(tidyverse)
library(zoo)
library(here)
maxN<-read.csv("final_data/TN_weighted_kg_per_m2_arctic.csv") ###without IPSL model, as this has a much lower ALD
str(maxN)
ggplot(maxN, aes(x = Year)) +
  # Add shaded area for variance (mean ± 1 standard deviation)
  geom_ribbon(aes(ymin = mean_370 - std_370, ymax = mean_370 + std_370), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_370), color = "darkgreen", linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_585 - std_585, ymax = mean_585 + std_585), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_585),  color = "blue", linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_126 - std_126, ymax = mean_126 + std_126), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_126), color = "red",  linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_245 - std_245, ymax = mean_245 + std_245), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_245), color = "yellow",  linewidth = 0.8) +
  labs(x = "Year", y = "N [kg N /m^2]", title = "Nitrogen released through permafrost thawing") +
  xlim(1850, 2100) +
  theme_bw() +
  theme(legend.position = "none")  # Remove the legend if not needed

# want it in kg / ha * yr
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
ssp585_a <- compute_anomaly(maxN, "mean_585", "std_585")
ssp370_a <- compute_anomaly(maxN, "mean_370", "std_370")
ssp245_a <- compute_anomaly(maxN, "mean_245", "std_245")
ssp126_a <- compute_anomaly(maxN, "mean_126", "std_126")

# Combine into a single data frame
anomaly_total <- Reduce(function(x, y) merge(x, y, by = "Year"), list(ssp585_a, ssp370_a, ssp245_a, ssp126_a))
anomaly_total<-data.frame(ssp585_a, ssp370_a, ssp126_a, ssp245_a)
anomaly_total<-anomaly_total[,c("Year", "mean_585", "std_585","mean_370","std_370","mean_245", "std_245","mean_126","std_126")]


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
  pivot_longer(cols = starts_with("mean_"), names_to = "SSP", values_to = "Mean_N") %>%
  pivot_longer(cols = starts_with("std_"), names_to = "SSP_std", values_to = "STD_N") %>%
  filter(gsub("mean_", "", SSP) == gsub("std_", "", SSP_std)) %>%
  dplyr::select(-SSP_std) %>%
  mutate(SSP = gsub("mean_", "SSP", SSP))  # Rename scenarios

# Define SSP colors for after 2015
ssp_colors <- c("SSP585" = "blue", "SSP370" = "darkgreen", "SSP245" = "orange", "SSP126" = "red")

# Compute 20-year rolling mean and standard deviation for each SSP
long_data <- long_data %>%
  group_by(SSP) %>%
  mutate(
    Rolling_Mean_N = rollmean(Mean_N, k = 20, fill = NA, align = "center"),
    Rolling_STD_N = rollapply(STD_N, width = 20, FUN = mean, fill = NA, align = "center")
  )

# Plot
ggplot(long_data, aes(x = Year, y = Mean_N, group = SSP)) +
  
  # Before 2015: Black lines & grey ribbons
  geom_line(data = filter(long_data, Period == "Before 2015"), color = "black", linewidth = 0.8) +
  geom_ribbon(data = filter(long_data, Period == "Before 2015"),
              aes(ymin = Mean_N - STD_N, ymax = Mean_N + STD_N),
              fill = "grey", alpha = 0.2) +
  
  # After 2015: Colored lines & ribbons
  geom_line(data = filter(long_data, Period == "After 2015"),
            aes(color = SSP), linewidth = 0.8) +
  geom_ribbon(data = filter(long_data, Period == "After 2015"),
              aes(ymin = Mean_N - STD_N, ymax = Mean_N + STD_N, fill = SSP),
              alpha = 0.2) +
  
  # Rolling Mean before 2015 (Black line)
  geom_line(data = filter(long_data, Year < 2015),
            aes(y = Rolling_Mean_N), color = "black", linewidth = 0.8, linetype = "solid") +
  
  # Rolling Mean after 2015 (Colored lines)
  geom_line(data = filter(long_data, Year >= 2015),
            aes(y = Rolling_Mean_N, color = SSP), linewidth = 0.8, linetype = "solid") +
  
  # Rolling STD before 2015 (Grey ribbon)
  geom_ribbon(data = filter(long_data, Year < 2015),
              aes(ymin = Rolling_Mean_N - Rolling_STD_N, 
                  ymax = Rolling_Mean_N + Rolling_STD_N),
              fill = "grey", alpha = 0.15) +
  # Vertical line for 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  
  # Labels and title
  labs(x = "Year", y = "N [kg / ha *yr]", title = "Increase in Soil N, Relative to 1880-1900", 
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

# Create a long-format dataframe for 1% and 10% anomalies
scaled_data <- long_data %>%
  mutate(N_1pct = Mean_N * 0.01,
         N_10pct = Mean_N * 0.10) %>%
  select(Year, SSP, Period, N_1pct, N_10pct) %>%
  pivot_longer(cols = starts_with("N_"), names_to = "Percent_Level", values_to = "Scaled_N") %>%
  mutate(Percent_Level = recode(Percent_Level, 
                                N_1pct = "1% of N anomaly", 
                                N_10pct = "10% of N anomaly"))


ggplot(scaled_data, aes(x = Year, y = Scaled_N, color = SSP, linetype = Percent_Level)) +
  geom_line(linewidth = 0.8) +
  
  # Highlight transition year
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  
  labs(
    title = "1% and 10% of Soil N Anomaly Over Time",
    x = "Year",
    y = "N [kg / ha * yr]",
    color = "SSP Scenario",
    linetype = "Percentage of N anomaly"
  ) +
  scale_color_manual(values = ssp_colors) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

###########

setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")
data <- read.csv("weighted_mean_thawed_grass_all_ssps.csv")

# Make sure the Year column is numeric
data$Year <- as.numeric(data$Year)

# Filter years
baseline <- data[data$Year >= 1990 & data$Year <= 2010, ]
future <- data[data$Year >= 2080 & data$Year <= 2100, ]



# Compute mean for each SSP during the two periods
baseline_means <- colMeans(baseline[, grep("mean_", names(baseline))], na.rm = TRUE)
future_means <- colMeans(future[, grep("mean_", names(future))], na.rm = TRUE)

# Compute anomalies (future - baseline)
anomalies <- future_means - baseline_means
names(anomalies)<-c("SSP126", "SSP245","SSP370","SSP585")
anomaly_df <- data.frame(
  Scenario = names(anomalies),
  Anomaly_kgNm2 = as.numeric(anomalies)
)

# Compute pooled SD for each SSP
pooled_sd <- data.frame(
  Scenario = c("SSP126", "SSP245", "SSP370", "SSP585"),
  pooled_sd = c(
    sqrt(mean(future$std_126^2, na.rm = TRUE)),
    sqrt(mean(future$std_245^2, na.rm = TRUE)),
    sqrt(mean(future$std_370^2, na.rm = TRUE)),
    sqrt(mean(future$std_585^2, na.rm = TRUE))
  )
)

# Merge with pooled_sd
anomaly_df <- left_join(anomaly_df, pooled_sd, by = "Scenario")

library(ggplot2)
# Define custom colors
ssp_colors <- c("SSP585" = "blue", "SSP370" = "darkgreen", "SSP245" = "orange", "SSP126" = "red")

# Plot with custom colors
ggplot(anomaly_df, aes(x = Scenario, y = Anomaly_kgNm2, fill = Scenario)) +
  geom_bar(stat = "identity") +
  #geom_errorbar(aes(ymin = Anomaly_kgNm2 - pooled_sd, ymax = Anomaly_kgNm2 + pooled_sd), width = 0.2) +
  scale_fill_manual(values = ssp_colors) +
  theme_minimal() +
  ylim(0,1.1)+
  labs(
    title = "Thawed Nitrogen, relative to 1990–2010",
    x = "Scenario",
    y = "N [kg / m2]"
  )

