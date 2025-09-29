##ALD Year axis ssps
library(tidyverse)
library(here)
library(dplyr)
library(zoo)
maxN<-read.csv("final_data/TN_weighted_kg_per_m2_arctic_no_2m_lim.csv") 


# ===== SSP585 =====
ssp585 <- maxN %>%
  select(Year, Mean = mean_585, STD = std_585)

ref_585 <- ssp585 %>% filter(Year >= 1860 & Year <= 1900)
ssp585 <- ssp585 %>%
  mutate(
    Mean = Mean - mean(ref_585$Mean, na.rm = TRUE),
    STD  = STD  - mean(ref_585$STD,  na.rm = TRUE),
    SSP = "SSP585",
    Period = ifelse(Year <= 2014, "Before 2015", "After 2015"),
    Rolling_Mean = rollmean(Mean, k = 20, fill = NA, align = "center"),
    Rolling_STD  = rollapply(STD, width = 20, FUN = mean, fill = NA, align = "center")
  )

# ===== SSP370 =====
ssp370 <- maxN %>%
  select(Year, Mean = mean_370, STD = std_370)

ref_370 <- ssp370 %>% filter(Year >= 1860 & Year <= 1900)
ssp370 <- ssp370 %>%
  mutate(
    Mean = Mean - mean(ref_370$Mean, na.rm = TRUE),
    STD  = STD  - mean(ref_370$STD,  na.rm = TRUE),
    SSP = "SSP370",
    Period = ifelse(Year <= 2014, "Before 2015", "After 2015"),
    Rolling_Mean = rollmean(Mean, k = 20, fill = NA, align = "center"),
    Rolling_STD  = rollapply(STD, width = 20, FUN = mean, fill = NA, align = "center")
  )

# ===== SSP245 =====
ssp245 <- maxN %>%
  select(Year, Mean = mean_245, STD = std_245)

ref_245 <- ssp245 %>% filter(Year >= 1860 & Year <= 1900)
ssp245 <- ssp245 %>%
  mutate(
    Mean = Mean - mean(ref_245$Mean, na.rm = TRUE),
    STD  = STD  - mean(ref_245$STD,  na.rm = TRUE),
    SSP = "SSP245",
    Period = ifelse(Year <= 2014, "Before 2015", "After 2015"),
    Rolling_Mean = rollmean(Mean, k = 20, fill = NA, align = "center"),
    Rolling_STD  = rollapply(STD, width = 20, FUN = mean, fill = NA, align = "center")
  )

# ===== SSP126 =====
ssp126 <- maxN %>%
  select(Year, Mean = mean_126, STD = std_126)

ref_126 <- ssp126 %>% filter(Year >= 1860 & Year <= 1900)
ssp126 <- ssp126 %>%
  mutate(
    Mean = Mean - mean(ref_126$Mean, na.rm = TRUE),
    STD  = STD  - mean(ref_126$STD,  na.rm = TRUE),
    SSP = "SSP126",
    Period = ifelse(Year <= 2014, "Before 2015", "After 2015"),
    Rolling_Mean = rollmean(Mean, k = 20, fill = NA, align = "center"),
    Rolling_STD  = rollapply(STD, width = 20, FUN = mean, fill = NA, align = "center")
  )

# ===== Combine all =====
long_data <- bind_rows(ssp585, ssp370, ssp245, ssp126)

# Define colors for plotting
ssp_colors <- c(
  "SSP585" = "blue",
  "SSP370" = "darkgreen",
  "SSP245" = "orange",
  "SSP126" = "red"
)


# Plot
ggplot(long_data, aes(x = Year, y = Mean, group = SSP)) +
  
  # Before 2015: Black lines & grey ribbons
  geom_line(data = filter(long_data, Period == "Before 2015"), color = "black", linewidth = 0.8) +
  geom_ribbon(data = filter(long_data, Period == "Before 2015"),
              aes(ymin = Mean - STD, ymax = Mean + STD),
              fill = "grey", alpha = 0.2) +
  
  # After 2015: Colored lines & ribbons
  geom_line(data = filter(long_data, Period == "After 2015"),
            aes(color = SSP), linewidth = 0.8) +
  geom_ribbon(data = filter(long_data, Period == "After 2015"),
              aes(ymin = Mean - STD, ymax = Mean + STD, fill = SSP),
              alpha = 0.2) +
  
  # Rolling Mean before 2015 (Black line)
  geom_line(data = filter(long_data, Year < 2015),
            aes(y = Rolling_Mean), color = "black", linewidth = 0.8, linetype = "solid") +
  
  # Rolling Mean after 2015 (Colored lines)
  geom_line(data = filter(long_data, Year >= 2015),
            aes(y = Rolling_Mean, color = SSP), linewidth = 0.8, linetype = "solid") +
  
  # Rolling STD before 2015 (Grey ribbon)
  geom_ribbon(data = filter(long_data, Year < 2015),
              aes(ymin = Rolling_Mean - Rolling_STD, 
                  ymax = Rolling_Mean + Rolling_STD),
              fill = "grey", alpha = 0.15) +
  # Vertical line for 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  
  # Labels and title
  labs(x = "Year", y = "N [kg / m2 *yr]", title = "Increase in soil Nitrogen / m2 , relative to 1880-1900", 
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







###########

# plot total pan-Arctic N in Pg

library(here)
maxN <- read.csv("final_data/total_thawed_N_Pg.csv")

# Make sure the Year column is numeric
maxN$Year <- as.numeric(maxN$Year)

# ===== SSP585 =====
ssp585 <- maxN %>%
  select(Year, Mean = Total_585, STD = STD_585)

ref_585 <- ssp585 %>% filter(Year >= 1860 & Year <= 1900)
ssp585 <- ssp585 %>%
  mutate(
    Mean = Mean - mean(ref_585$Mean, na.rm = TRUE),
    STD  = STD  - mean(ref_585$STD,  na.rm = TRUE),
    SSP = "SSP585",
    Period = ifelse(Year <= 2014, "Before 2015", "After 2015"),
    Rolling_Mean = rollmean(Mean, k = 20, fill = NA, align = "center"),
    Rolling_STD  = rollapply(STD, width = 20, FUN = mean, fill = NA, align = "center")
  )

# ===== SSP370 =====
ssp370 <- maxN %>%
  select(Year, Mean = Total_370, STD = STD_370)

ref_370 <- ssp370 %>% filter(Year >= 1860 & Year <= 1900)
ssp370 <- ssp370 %>%
  mutate(
    Mean = Mean - mean(ref_370$Mean, na.rm = TRUE),
    STD  = STD  - mean(ref_370$STD,  na.rm = TRUE),
    SSP = "SSP370",
    Period = ifelse(Year <= 2014, "Before 2015", "After 2015"),
    Rolling_Mean = rollmean(Mean, k = 20, fill = NA, align = "center"),
    Rolling_STD  = rollapply(STD, width = 20, FUN = mean, fill = NA, align = "center")
  )

# ===== SSP245 =====
ssp245 <- maxN %>%
  select(Year, Mean = Total_245, STD = STD_245)

ref_245 <- ssp245 %>% filter(Year >= 1860 & Year <= 1900)
ssp245 <- ssp245 %>%
  mutate(
    Mean = Mean - mean(ref_245$Mean, na.rm = TRUE),
    STD  = STD  - mean(ref_245$STD,  na.rm = TRUE),
    SSP = "SSP245",
    Period = ifelse(Year <= 2014, "Before 2015", "After 2015"),
    Rolling_Mean = rollmean(Mean, k = 20, fill = NA, align = "center"),
    Rolling_STD  = rollapply(STD, width = 20, FUN = mean, fill = NA, align = "center")
  )

# ===== SSP126 =====
ssp126 <- maxN %>%
  select(Year, Mean = Total_126, STD = STD_126)

ref_126 <- ssp126 %>% filter(Year >= 1860 & Year <= 1900)
ssp126 <- ssp126 %>%
  mutate(
    Mean = Mean - mean(ref_126$Mean, na.rm = TRUE),
    STD  = STD  - mean(ref_126$STD,  na.rm = TRUE),
    SSP = "SSP126",
    Period = ifelse(Year <= 2014, "Before 2015", "After 2015"),
    Rolling_Mean = rollmean(Mean, k = 20, fill = NA, align = "center"),
    Rolling_STD  = rollapply(STD, width = 20, FUN = mean, fill = NA, align = "center")
  )

# ===== Combine all =====
long_data <- bind_rows(ssp585, ssp370, ssp245, ssp126)

# Define colors for plotting
ssp_colors <- c(
  "SSP585" = "blue",
  "SSP370" = "darkgreen",
  "SSP245" = "orange",
  "SSP126" = "red"
)


# Plot
ggplot(long_data, aes(x = Year, y = Mean, group = SSP)) +
  
  # Before 2015: Black lines & grey ribbons
  geom_line(data = filter(long_data, Period == "Before 2015"), color = "black", linewidth = 0.8) +
  geom_ribbon(data = filter(long_data, Period == "Before 2015"),
              aes(ymin = Mean - STD, ymax = Mean + STD),
              fill = "grey", alpha = 0.2) +
  
  # After 2015: Colored lines & ribbons
  geom_line(data = filter(long_data, Period == "After 2015"),
            aes(color = SSP), linewidth = 0.8) +
  geom_ribbon(data = filter(long_data, Period == "After 2015"),
              aes(ymin = Mean - STD, ymax = Mean + STD, fill = SSP),
              alpha = 0.2) +
  
  # Rolling Mean before 2015 (Black line)
  geom_line(data = filter(long_data, Year < 2015),
            aes(y = Rolling_Mean), color = "black", linewidth = 0.8, linetype = "solid") +
  
  # Rolling Mean after 2015 (Colored lines)
  geom_line(data = filter(long_data, Year >= 2015),
            aes(y = Rolling_Mean, color = SSP), linewidth = 0.8, linetype = "solid") +
  
  # Rolling STD before 2015 (Grey ribbon)
  geom_ribbon(data = filter(long_data, Year < 2015),
              aes(ymin = Rolling_Mean - Rolling_STD, 
                  ymax = Rolling_Mean + Rolling_STD),
              fill = "grey", alpha = 0.15) +
  # Vertical line for 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  
  # Labels and title
  labs(x = "Year", y = "N [Pg]", title = "Increase in Soil N, Relative to 1880-1900", 
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




