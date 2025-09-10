##### total thawed N in Pg

library(ggplot2)
library(dplyr)
library(zoo)
library(tidyr)
df<-read.csv("total_thawed_N.csv") ###without IPSL model, as this has a much lower ALD
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
ssp585_a <- compute_anomaly(df, "Total_585", "STD_585")
ssp370_a <- compute_anomaly(df, "Total_370", "STD_370")
ssp245_a <- compute_anomaly(df, "Total_245", "STD_245")
ssp126_a <- compute_anomaly(df, "Total_126", "STD_126")

# Combine into a single data frame
anomaly_total <- Reduce(function(x, y) merge(x, y, by = "Year"), list(ssp585_a, ssp370_a, ssp245_a, ssp126_a))

anomaly_total<-data.frame(ssp585_a, ssp370_a, ssp245_a,ssp126_a)
anomaly_total<-anomaly_total[,c("Year", "Total_585", "STD_585","Total_370","STD_370","Total_245", "STD_245", "Total_126","STD_126")]


# Create a new column for color based on the year
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
  pivot_longer(cols = starts_with("Total_"), names_to = "SSP", values_to = "Mean_N") %>%
  pivot_longer(cols = starts_with("STD_"), names_to = "SSP_std", values_to = "STD_N") %>%
  filter(gsub("Total_", "", SSP) == gsub("STD_", "", SSP_std)) %>%
  dplyr::select(-SSP_std) %>%
  mutate(SSP = gsub("Total_", "SSP", SSP))  # Rename scenarios

# Define SSP colors for after 2015
ssp_colors <- c("SSP585" = "blue", "SSP370" = "darkgreen", "SSP245" = "orange", "SSP126" = "red")

# Compute 20-year rolling mean and standard deviation for each SSP
long_data <- long_data %>%
  group_by(SSP) %>%
  mutate(
    Rolling_Mean_N = rollmean(Mean_N, k = 20, fill = NA, align = "center"),
    Rolling_STD_N  = rollapply(STD_N, width = 20, FUN = mean, fill = NA, align = "center")
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
  
  # Rolling STD after 2015 (Colored ribbons)
  #geom_ribbon(data = filter(long_data, Year >= 2015),
  #aes(ymin = Rolling_Mean_N - Rolling_STD_N, 
  #ymax = Rolling_Mean_N + Rolling_STD_N, 
  #fill = SSP),
  #alpha = 0.15) +
  
  # Vertical line for 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  
  # Labels and title
  labs(x = "Year", y = "N [Pg]", title = "Increase in Soil Nitrogen Relative to 1880-1900", 
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



##### bar plotting

data<-read.csv("total_thawed_N.csv")
# Filter the data for the relevant time frames: 1880-1900, 1990-2010, 2080-2100
time_frames <- c('1880-1900', '1990-2010', '2080-2100')

filtered_data <- data[data$TF %in% time_frames, ]

# Aggregate the data by the mean nitrogen values for each time frame
agg_data <- filtered_data %>%
  group_by(TF) %>%
  summarize(
    mean_126 = mean(Total_126, na.rm = TRUE),
    mean_245 = mean(Total_245, na.rm = TRUE),
    mean_370 = mean(Total_370, na.rm = TRUE),
    mean_585 = mean(Total_585, na.rm = TRUE)
  )

# Reshape data for plotting
agg_data_melted <- agg_data %>%
  gather(key = "Scenario", value = "Nitrogen_Storage", mean_126,mean_245, mean_370, mean_585)

# Plotting
ggplot(agg_data_melted, aes(x = TF, y = Nitrogen_Storage, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("mean_126" = "red", "mean_370" = "green", "mean_585" = "blue")) +
  labs(
    title = "Mean Nitrogen Storage for Different Scenarios and Time Frames",
    x = "Time Frame",
    y = "Mean Nitrogen Storage (kg N/m²)",
    fill = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 12), legend.position = "top")


###as anomaly plot

# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Filter the data for the relevant time frames: 1880-1900, 1990-2010, 2080-2100
time_frames <- c('1880-1900', '1990-2010', '2080-2100')
filtered_data <- data[data$TF %in% time_frames, ]

# Aggregate the data by the mean nitrogen values and standard deviation for each time frame
agg_data <- filtered_data %>%
  group_by(TF) %>%
  summarize(
    mean_126 = mean(Total_126, na.rm = TRUE),
    mean_370 = mean(Total_370, na.rm = TRUE),
    mean_585 = mean(Total_585, na.rm = TRUE),
    std_126 = mean(STD_126, na.rm = TRUE),
    std_370 = mean(STD_370, na.rm = TRUE),
    std_585 = mean(STD_585, na.rm = TRUE)
  )

# Reshape the data for plotting
agg_data_melted <- agg_data %>%
  gather(key = "Scenario", value = "Nitrogen_Storage", mean_585, mean_370, mean_126) %>%
  mutate(
    Std_Dev = case_when(
      Scenario == "mean_126" ~ std_126,
      Scenario == "mean_370" ~ std_370,
      Scenario == "mean_585" ~ std_585
    ),
    Scenario = factor(Scenario, levels = c("mean_126", "mean_370", "mean_585"))
  )

# Plotting with error bars
ggplot(agg_data_melted, aes(x = TF, y = Nitrogen_Storage, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(
    aes(ymin = Nitrogen_Storage - Std_Dev, ymax = Nitrogen_Storage + Std_Dev),
    position = position_dodge(width = 0.7), width = 0.25
  ) +
  scale_fill_manual(values = c("mean_126" = "red", "mean_370" = "green", "mean_585" = "blue")) +
  labs(
    title = "Nitrogen Storage Anomalies with Standard Deviation Relative to 1880-1900 Reference Period",
    x = "Time Frame",
    y = "Anomaly in Nitrogen Storage (kg N/m²)",
    fill = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Set the 1880-1900 values as the reference (i.e., 0 for anomalies)
reference_values <- agg_data[agg_data$TF == '1880-1900', c("mean_126", "mean_370", "mean_585")]

# Calculate anomalies for the timeframes 1990-2010 and 2080-2100
agg_data_anomalies <- agg_data %>%
  filter(TF != '1880-1900') %>%
  mutate(
    anomaly_n_126 = mean_126 - reference_values$mean_126,
    anomaly_n_370 = mean_370 - reference_values$mean_370,
    anomaly_n_585 = mean_585 - reference_values$mean_585,
    std_n_126 = std_126,
    std_n_370 = std_370,
    std_n_585 = std_585
  )

# Reshape the data for plotting
agg_data_anomalies_melted <- agg_data_anomalies %>%
  gather(key = "Scenario", value = "Nitrogen_Storage", anomaly_n_126, anomaly_n_370, anomaly_n_585) %>%
  mutate(
    Std_Dev = case_when(
      Scenario == "anomaly_n_126" ~ std_n_126,
      Scenario == "anomaly_n_370" ~ std_n_370,
      Scenario == "anomaly_n_585" ~ std_n_585
    ),
    Scenario = factor(Scenario, levels = c("anomaly_n_126", "anomaly_n_370", "anomaly_n_585"))
  )


# Plotting with error bars
ggplot(agg_data_anomalies_melted, aes(x = TF, y = Nitrogen_Storage, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Nitrogen_Storage - Std_Dev, ymax = Nitrogen_Storage + Std_Dev),
                position = position_dodge(width = 0.7), width = 0.25) +
  scale_fill_manual(values = c("anomaly_n_126" = "red", "anomaly_n_370" = "darkgreen", "anomaly_n_585" = "blue")) +
  labs(
    title = "Soil nitrogen content, relative to preindustrial",
    x = "Time Frame",
    y = "N (kg N/m²)",
    fill = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

