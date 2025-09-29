library(here)

#for each SSP scenario

thawed_N_grasses_126<-read.csv("final_data/weighted_mean_thawed_grass_mean_ssp126.csv") 
thawed_N_grasses_245<-read.csv("final_data/weighted_mean_thawed_grass_mean_ssp245.csv") 
thawed_N_grasses_370<-read.csv("final_data/weighted_mean_thawed_grass_mean_ssp370.csv") 
thawed_N_grasses_585<-read.csv("final_data/weighted_mean_thawed_grass_mean_ssp585.csv") 
thawed_N_grasses_585 <- rbind(thawed_N_grasses_585, thawed_N_grasses_585[nrow(thawed_N_grasses_585), ])
thawed_N_grasses_std_126<-read.csv("final_data/weighted_mean_thawed_grass_std_ssp126.csv") 
thawed_N_grasses_std_245<-read.csv("final_data/weighted_mean_thawed_grass_std_ssp245.csv") 
thawed_N_grasses_std_370<-read.csv("final_data/weighted_mean_thawed_grass_std_ssp370.csv") 
thawed_N_grasses_std_585<-read.csv("final_data/weighted_mean_thawed_grass_std_ssp585.csv") 
thawed_N_grasses_std_585<- rbind(thawed_N_grasses_std_585, thawed_N_grasses_std_585[nrow(thawed_N_grasses_std_585), ])

thawed_n_grasses<-data.frame(thawed_N_grasses_126$sum, thawed_N_grasses_245$sum, thawed_N_grasses_370$sum, thawed_N_grasses_585$sum)
thawed_n_grasses$Year<-rep(1850:2100)

# First, let's name the columns properly (optional but recommended)
colnames(thawed_n_grasses) <- c("SSP126", "SSP245", "SSP370", "SSP585", "Year")

# Calculate means for each period
results <- list(
  period_1880_1900 = colMeans(thawed_n_grasses[thawed_n_grasses$Year %in% 1880:1900, 1:4]),
  period_1990_2010 = colMeans(thawed_n_grasses[thawed_n_grasses$Year %in% 1990:2010, 1:4]),
  period_2080_2100 = colMeans(thawed_n_grasses[thawed_n_grasses$Year %in% 2080:2100, 1:4])
)

# Print the results
results

# Calculate mean values for each period and SSP
mean_1990_2010 <- colMeans(thawed_n_grasses[thawed_n_grasses$Year %in% 1990:2010, 1:4])
mean_2080_2100 <- colMeans(thawed_n_grasses[thawed_n_grasses$Year %in% 2080:2100, 1:4])

# Calculate the difference (additional Nin soil due to permafrost thawing (difference 2080-2100 to 1990-2010)):
# this is in kg / m2
additional_N <- mean_2080_2100 - mean_1990_2010

# in g/ m2: 
add_N_gm2<-additional_N*1000

# considering 5 and 10 and 15% bioavailability: 
bioavailable_N_5<-add_N_gm2*0.05
bioavailable_N_10<-add_N_gm2*0.1
bioavailable_N_15<-add_N_gm2*0.15

n_bioavailable_graminoids<-data.frame(bioavailable_N_5, bioavailable_N_10, bioavailable_N_15)
n_bioavailable_graminoids<-n_bioavailable_graminoids[c(1:4),]
#for each SSP scenario
thawed_N_taiga_126<-read.csv("final_data/weighted_mean_thawed_taiga_mean_ssp126.csv") 
thawed_N_taiga_245<-read.csv("final_data/weighted_mean_thawed_taiga_mean_ssp245.csv") 
thawed_N_taiga_370<-read.csv("final_data/weighted_mean_thawed_taiga_mean_ssp370.csv") 
thawed_N_taiga_585<-read.csv("final_data/weighted_mean_thawed_taiga_mean_ssp585.csv") 
thawed_N_taiga_585 <- rbind(thawed_N_taiga_585, thawed_N_taiga_585[nrow(thawed_N_taiga_585), ])
thawed_N_taiga_std_126<-read.csv("final_data/weighted_mean_thawed_taiga_std_ssp126.csv") 
thawed_N_taiga_std_245<-read.csv("final_data/weighted_mean_thawed_taiga_std_ssp245.csv") 
thawed_N_taiga_std_370<-read.csv("final_data/weighted_mean_thawed_taiga_std_ssp370.csv") 
thawed_N_taiga_std_585<-read.csv("final_data/weighted_mean_thawed_taiga_std_ssp585.csv") 
thawed_N_taiga_std_585<- rbind(thawed_N_taiga_std_585, thawed_N_taiga_std_585[nrow(thawed_N_taiga_std_585), ])

thawed_n_taiga<-data.frame(thawed_N_taiga_126$sum, thawed_N_taiga_245$sum, thawed_N_taiga_370$sum, thawed_N_taiga_585$sum)
thawed_n_taiga$Year<-rep(1850:2100)

# First, let's name the columns properly (optional but recommended)
colnames(thawed_n_taiga) <- c("SSP126", "SSP245", "SSP370", "SSP585", "Year")

# Calculate means for each period
results <- list(
  period_1880_1900 = colMeans(thawed_n_taiga[thawed_n_taiga$Year %in% 1880:1900, 1:4]),
  period_1990_2010 = colMeans(thawed_n_taiga[thawed_n_taiga$Year %in% 1990:2010, 1:4]),
  period_2080_2100 = colMeans(thawed_n_taiga[thawed_n_taiga$Year %in% 2080:2100, 1:4])
)

# Print the results
results

# Calculate mean values for each period and SSP
mean_1990_2010 <- colMeans(thawed_n_taiga[thawed_n_taiga$Year %in% 1990:2010, 1:4])
mean_2080_2100 <- colMeans(thawed_n_taiga[thawed_n_taiga$Year %in% 2080:2100, 1:4])

# Calculate the difference (additional Nin soil due to permafrost thawing (difference 2080-2100 to 1990-2010)):
# this is in kg / m2
additional_N <- mean_2080_2100 - mean_1990_2010

# in g/ m2: 
add_N_gm2<-additional_N*1000

# considering 5 and 10 and 15% bioavailability: 
bioavailable_N_5<-add_N_gm2*0.05
bioavailable_N_10<-add_N_gm2*0.1
bioavailable_N_15<-add_N_gm2*0.15
n_bioavailable_boreal<-data.frame(bioavailable_N_5, bioavailable_N_10, bioavailable_N_15)

# need boreal fertilizer to be in kg / ha, therefore
# 1 g/m2 = 10 kg/ ha
n_bioavailable_boreal_ha<-n_bioavailable_boreal*10

#######

## results from MESI studies average N use efficiency

## graminoids ##

# aboveground mean biomass increase 2.483 g DW / g N (+/- 1.78)

agb_graminoids_mean<-n_bioavailable_graminoids*2.483
agb_graminoids_std<-n_bioavailable_graminoids*1.78

# belowground mean biomass increase: 2.17 g DW / g N +/- 3.65

bgb_graminoids_mean<-n_bioavailable_graminoids*2.17
bgb_graminoids_std<-n_bioavailable_graminoids*3.65

#mean biomass in g C / m2
biomass_graminoids_mean<-agb_graminoids_mean+bgb_graminoids_mean
biomass_graminoids_std<-sqrt((agb_graminoids_mean)^2+(bgb_graminoids_mean^2))

total_biomass_graminoids_mean<-biomass_graminoids_mean*0.5
total_biomass_graminoids_std<-biomass_graminoids_std*0.5
total_biomass_graminoids_mean_yearly<-total_biomass_graminoids_mean/100
total_biomass_graminoids_std_yearly<-total_biomass_graminoids_std/100

## boreal / taiga ##
#average N-use efficiency for NPK fertilizer: 
#14.1 kg C / ha per kg N / ha == 14.1 g C / m2 per g N / m2 for aboveground woody biomass --> already in C
#Belowground woody biomass = agb x 0.2
agb_boreal_mean<-n_bioavailable_boreal*14.1
agb_boreal_std<-n_bioavailable_boreal*3.5
bgb_boreal_mean<-agb_boreal_mean*0.2
bgb_boreal_std<-agb_boreal_std*0.2
total_biomass_boreal_mean<-(agb_boreal_mean+bgb_boreal_mean)
total_biomass_boreal_std<-sqrt((agb_boreal_mean)^2+(bgb_boreal_mean)^2)
total_biomass_boreal_mean_yearly<-total_biomass_boreal_mean/100
total_biomass_boreal_std_yearly<-total_biomass_boreal_std/100

## add grass + boreal biomass increase together
total_biomass_mean<-total_biomass_boreal_mean+total_biomass_graminoids_mean
total_biomass_se<-sqrt(total_biomass_boreal_std^2 +total_biomass_graminoids_std^2 )
total_biomass_mean_yearly<-total_biomass_boreal_mean_yearly+total_biomass_graminoids_mean_yearly
total_biomass_std_yearly<-sqrt(total_biomass_boreal_std_yearly^2+total_biomass_graminoids_std_yearly^2)


# Your data preparation 
boreal_data <- as.data.frame(total_biomass_boreal_mean_yearly) 
colnames(boreal_data) <- c("Lower", "Mean", "Upper")
boreal_data$Biome <- "Boreal"
boreal_data$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")

grasslands_data <- as.data.frame(total_biomass_graminoids_mean_yearly)
colnames(grasslands_data) <- c("Lower", "Mean", "Upper")
grasslands_data$Biome <- "Graminoids"
grasslands_data$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")
# Combine the data
combined_data <- rbind(boreal_data, grasslands_data)
library(dplyr)
# Sum the values across biomes for each SSP
combined_sum <- combined_data %>%
  group_by(SSP) %>%
  summarize(
    Lower = mean(Lower),
    Mean = mean(Mean),
    Upper = mean(Upper),
    Biome = "Mean"
  )

# Combine with the original data
final_data <- rbind(combined_data, combined_sum)

library(ggplot2)
final_data$Biome <- factor(final_data$Biome, levels = c("Graminoids", "Boreal", "Mean"))

ggplot(final_data, aes(x = SSP, y = Mean, fill = Biome)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    position = position_dodge(width = 0.9),
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    title = "",
    x = "SSP Scenario",
    y = "Biomass Increase [ g C / m-2 * yr-1 ]",
    fill = "Biome"
  ) +
  scale_fill_manual(
    values = c(
      "Boreal" = "#1f78b4", 
      "Graminoids" = "#33a02c",
      "Mean" = "#e31a1c"  # Red for the summed bar
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )





### multiply by the region to get Arctic NPP increase due to N from permafrost thaw:

total_biomass_mean
total_biomass_se

N_data <- rast("final_data/TN_30deg_corr.nc", lyr = 1)

cell_area<-cellSize(N_data, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_area, fun = "sum", na.rm = TRUE)$sum

arctic_biomass_mean<-total_biomass_mean*total_area
arctic_biomass_se<-total_biomass_boreal_std*total_area
arctic_biomass_mean_pg<-arctic_biomass_mean/(10^15)
arctic_biomass_se_pg<-arctic_biomass_se/(10^15)

arctic_data <- as.data.frame(arctic_biomass_mean_pg) 
colnames(arctic_data) <- c("Lower", "Mean", "Upper")
arctic_data$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")

ggplot(arctic_data, aes(x = SSP, y = Mean, , fill = SSP)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    position = position_dodge(width = 0.9),
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    title = "",
    x = "SSP Scenario",
    y = "Biomass Increase Pg",
  ) +
  scale_fill_manual(
    values = c(
      "SSP126" = "red", 
      "SSP245" = "orange",
      "SSP370" = "darkgreen",
      "SSP585" = "blue"  # Red for the summed bar
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )



# calculate overall arctic NPP by multiplying total_graminoid_mean * graminoid area + ...
graminoids_area_m2<-1.013084*(10^13)
taiga_area_m2<-8.700199*(10^12)
total_area_m2<-graminoids_area_m2+taiga_area_m2
taiga_mean_NPP<-total_biomass_boreal_mean*taiga_area_m2
taiga_mean_NPP_Pg<-taiga_mean_NPP/(10^15)
graminoid_mean_NPP<-total_biomass_graminoids_mean*graminoids_area_m2
graminoid_mean_NPP_Pg<-graminoid_mean_NPP/(10^15)

total_mean_NPP_Pg<-graminoid_mean_NPP_Pg+taiga_mean_NPP_Pg

taiga_mean_NPP_yr<-total_biomass_boreal_mean_yearly*taiga_area_m2
taiga_mean_NPP_Pg_yr<-taiga_mean_NPP_yr/(10^15)
graminoid_mean_NPP_yr<-total_biomass_graminoids_mean_yearly*graminoids_area_m2
graminoid_mean_NPP_Pg_yr<-graminoid_mean_NPP_yr/(10^15)

total_mean_NPP_Pg_yr<-graminoid_mean_NPP_Pg_yr+taiga_mean_NPP_Pg_yr

# Your data preparation 
boreal_data <- as.data.frame(taiga_mean_NPP_Pg_yr) 
colnames(boreal_data) <- c("Lower", "Mean", "Upper")
boreal_data$Biome <- "Boreal"
boreal_data$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")

grasslands_data <- as.data.frame(graminoid_mean_NPP_Pg_yr)
colnames(grasslands_data) <- c("Lower", "Mean", "Upper")
grasslands_data$Biome <- "Graminoids"
grasslands_data$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")
# Combine the data
combined_data <- rbind(boreal_data, grasslands_data)
library(dplyr)
# Sum the values across biomes for each SSP
combined_sum <- combined_data %>%
  group_by(SSP) %>%
  summarize(
    Lower = mean(Lower),
    Mean = mean(Mean),
    Upper = mean(Upper),
    Biome = "Mean"
  )

# Combine with the original data
final_data <- rbind(combined_data, combined_sum)
final_data_Teragram_C <- data.frame(
  Lower_Tg = final_data$Lower * 1000,
  Mean_Tg  = final_data$Mean  * 1000,
  Upper_Tg = final_data$Upper * 1000
)
library(ggplot2)
final_data_Teragram_C$Biome <- factor(final_data$Biome, levels = c("Graminoids", "Boreal", "Mean"))
final_data_Teragram_C$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")

ggplot(final_data_Teragram_C, aes(x = SSP, y = Mean_Tg, fill = Biome)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = Lower_Tg, ymax = Upper_Tg),
    position = position_dodge(width = 0.9),
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    title = "",
    x = "SSP Scenario",
    y = "Biomass Increase [ Teragram C * yr-1 ]",
    fill = "Biome"
  ) +
  scale_fill_manual(
    values = c(
      "Boreal" = "#1f78b4", 
      "Graminoids" = "#33a02c",
      "Mean" = "#e31a1c"  # Red for the summed bar
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

##################################################################################################################

## actual fertilizer calculation with Keuper et al results: 
#Root biomass
#Control: 0.8 +/- 0.3 mg / cm3
#Deep fertilised: 1.6 +/- 0.8 mg / cm 3
# --> additional biomass through fertilisation = 0.8 mg / cm3
#0.8 mg / cm3 * 10 cm depth = 8 mg / cm2
#8 mg / cm2 / 1000 = 0.008 g / cm2
#0.008 g / cm2 * 10'000 cm = 80 g / m2
#they added 8 g N / m2 fertiliser = 10 g biomass increase per g N fertilizer (N-use efficiency)

#Potential root Biomass increase due to permafrost thawing 
bgb_graminoids_mean_keuper<-n_bioavailable_graminoids*10

# Above-ground biomass: 
#Keuper et al Results: 
#Control: 8 g / m2
#Deep fertilised: 17 g / m2
# --> additional biomass through fertilisation = 9 g / m2
# they added 8 g N / m2 fertiliser = 1.125 g biomass increase per g N fertilizer
agb_graminoids_mean_keuper <- n_bioavailable_graminoids * 1.125 

# Calculate total biomass increase and convert to carbon (assuming 50% C content)
# Assuming the dataframes are already loaded
total_biomass_graminoids_mean_keuper <- (agb_graminoids_mean_keuper + bgb_graminoids_mean_keuper) *0.5
total_biomass_graminoids_mean_keuper_yearly<-total_biomass_graminoids_mean_keuper/100
total_biomass_graminoids_mean_yearly

#### for boreal fertilizer calculation: 
#average N-use efficiency for NPK fertilizer: 
#14.1 kg C / ha per kg N / ha == 1.41 g C / m2 per g N / m2 for aboveground woody biomass
#Belowground woody biomass = AGWB x 0.2
agb_boreal<-n_fertilizer_boreal*1.41
bgb_boreal<-agb_boreal*0.2
total_biomass_boreal<-(agb_boreal+bgb_boreal)*0.5
total_biomass_boreal_yearly<-total_biomass_boreal/100

## add grass + boreal biomass increase together

total_biomass_yearly<-total_biomass_boreal_yearly+total_biomass_increase_grasses_yearly
colnames(total_biomass_yearly)<-c("1%","5%","10%")

library(ggplot2)
library(tidyr)

# Convert to tidy format
tidy_data <- total_biomass_yearly %>%
  tibble::rownames_to_column("SSP") %>%
  pivot_longer(
    cols = -SSP,
    names_to = "Bioavailability",
    names_prefix = "Biomass_increase",
    values_to = "biomass_increase"
  )
tidy_data$Bioavailability <- factor(tidy_data$Bioavailability, levels = c("1%", "5%", "10%"))

# Plot
ggplot(tidy_data, aes(x = SSP, y = biomass_increase, fill = Bioavailability)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(
    values = c("#66c2a5", "#8da0cb", "#fc8d62"),
    labels = c("1%", "5%", "10%")
  ) +
  labs(
    title = "Bioavailable Nitrogen by SSP and Fertilization Level",
    x = "SSP Scenario",
    y = "Biomass increase [ g / m2 ]",
    fill = "Bioavailability"
  ) +
  theme_minimal() +
  theme(legend.position = "top")




##################################################################################################

library(tidyr)
library(dplyr)
library(ggplot2)

## to just plot the amount of N g / m2 as potentially bioavailable: 

n_bioavailable_boreal_yr<-n_bioavailable_boreal/100
n_bioavailable_graminoids_yr<-n_bioavailable_graminoids/100
n_bioavailable_boreal_kg_ha_yr<-n_bioavailable_boreal_yr*10
n_bioavailable_graminoids_kg_ha_yr<-n_bioavailable_graminoids_yr*10

colnames(n_bioavailable_boreal_kg_ha_yr) <- c("Lower", "Mean", "Upper")
n_bioavailable_boreal_kg_ha_yr$Biome <- "Boreal"
n_bioavailable_boreal_kg_ha_yr$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")

colnames(n_bioavailable_boreal_yr) <- c("Lower", "Mean", "Upper")
n_bioavailable_boreal_yr$Biome <- "Boreal"
n_bioavailable_boreal_yr$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")

colnames(n_bioavailable_graminoids_kg_ha_yr) <- c("Lower", "Mean", "Upper")
n_bioavailable_graminoids_kg_ha_yr$Biome <- "Graminoids"
n_bioavailable_graminoids_kg_ha_yr$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")

colnames(n_bioavailable_graminoids_yr) <- c("Lower", "Mean", "Upper")
n_bioavailable_graminoids_yr$Biome <- "Graminoids"
n_bioavailable_graminoids_yr$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")
# Combine the data
combined_data <- rbind(n_bioavailable_boreal_yr, n_bioavailable_graminoids_yr)
# Sum the values across biomes for each SSP
combined_mean <- combined_data %>%
  group_by(SSP) %>%
  summarise(
    Lower = mean(Lower),
    Mean = mean(Mean),
    Upper = mean(Upper),
    Biome = "Mean"
  )


ggplot(combined_mean, aes(x = SSP, y = Mean, fill = SSP)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    position = position_dodge(width = 0.9),
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  ylim(0,3)+
  labs(
    title = "Potentially Bioavailable Nitrogen",
    x = "SSP Scenario",
    y = "Bioavailable Nitrogen [g/m2*yr]",
    fill = "Biome"
  ) +
  scale_fill_manual(
    values = c(
      "SSP126" = "red", 
      "SSP245" = "orange",
      "SSP370" = "darkgreen",
      "SSP585" = "blue"  # Red for the summed bar
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

ggplot(combined_kg_ha, aes(x = SSP, y = Mean, fill = SSP)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    position = position_dodge(width = 0.9),
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  ylim(0,3500)+
  labs(
    title = "Potentially Bioavailable Nitrogen",
    x = "SSP Scenario",
    y = "Bioavailable Nitrogen [kg / ha]",
    fill = ""
  ) +
  scale_fill_manual(
    values = c(
      "SSP126" = "red", 
      "SSP245" = "orange",
      "SSP370" = "darkgreen",
      "SSP585" = "blue"  # Red for the summed bar
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


# Combine with the mean arctic data 
final_data <- rbind(combined_data, n_bioavailable_arctic_mean_yr)

library(ggplot2)
final_data$Biome <- factor(final_data$Biome, levels = c("Boreal", "Graminoids", "Mean"))

ggplot(final_data, aes(x = SSP, y = Mean, fill = Biome)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    position = position_dodge(width = 0.9),
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  ylim(0,3)+
  labs(
    title = "Potentially Bioavailable Nitrogen per Year",
    x = "SSP Scenario",
    y = "Bioavailable Nitrogen [kg / ha * yr]",
    fill = "Biome"
  ) +
  scale_fill_manual(
    values = c(
      "Boreal" = "#1f78b4", 
      "Graminoids" = "#33a02c",
      "Mean" = "#e31a1c"  # Red for the summed bar
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )




######## to just plot what is potentially bioavailable for whole arctic region ########

thawed_N_arctic<-read.csv("final_data/weighted_mean_thawed_arctic_2m.csv") 

# Calculate means for each period
results <- list(
  period_1880_1900 = colMeans(thawed_N_arctic[thawed_N_arctic$Year %in% 1880:1900, 2:5]),
  period_1990_2010 = colMeans(thawed_N_arctic[thawed_N_arctic$Year %in% 1990:2010, 2:5]),
  period_2080_2100 = colMeans(thawed_N_arctic[thawed_N_arctic$Year %in% 2080:2100, 2:5])
)

# Print the results
results

# Calculate mean values for each period and SSP
mean_1990_2010 <- colMeans(thawed_N_arctic[thawed_N_arctic$Year %in% 1990:2010, 2:5])
mean_2080_2100 <- colMeans(thawed_N_arctic[thawed_N_arctic$Year %in% 2080:2100, 2:5])

# Calculate the difference (additional Nin soil due to permafrost thawing (difference 2080-2100 to 1990-2010)):
# this is in kg / m2
additional_N <- mean_2080_2100 - mean_1990_2010

# in g/ m2: 
add_N_gm2<-additional_N*1000

# considering 1, 5 and 10% bioavailability: 
bioavailable_N_1<-add_N_gm2*0.01
bioavailable_N_5<-add_N_gm2*0.05
bioavailable_N_10<-add_N_gm2*0.1

n_bioavailable_arctic<-data.frame(bioavailable_N_1, bioavailable_N_5, bioavailable_N_10)

n_bioavailable_arctic_mean_yr<-n_bioavailable_arctic/100

n_bioavailable_arctic_std_yr<-n_bioavailable_arctic_std/100
n_bioavailable_arctic_mean_kg_ha_yr<-n_bioavailable_arctic_mean_yr*10
colnames(n_bioavailable_arctic_mean_yr) <- c("Lower", "Mean", "Upper")
n_bioavailable_arctic_mean_yr$Biome<-c("Mean", "Mean","Mean","Mean")
n_bioavailable_arctic_mean_yr$SSP <- c("SSP126", "SSP245", "SSP370", "SSP585")
rownames(n_bioavailable_arctic_mean_yr)<-c("SSP126", "SSP245", "SSP370", "SSP585")

ggplot(n_bioavailable_arctic_mean_yr, aes(x = SSP, y = Mean, fill = SSP)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    position = position_dodge(width = 0.9),
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  ylim(0,3)+
  labs(
    title = "Potentially Bioavailable Nitrogen over Arctic",
    x = "SSP Scenario",
    y = "Bioavailable Nitrogen [g/m2*yr]",
    fill = "Biome"
  ) +
  scale_fill_manual(
    values = c(
      "SSP126" = "red", 
      "SSP245" = "orange",
      "SSP370" = "darkgreen",
      "SSP585" = "blue"  # Red for the summed bar
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


##################################################

# Hypothetical current biomass in kg N/ha
current_biomass <- data.frame(
  Biome = c("Boreal", "Grasses"),
  CurrentBiomass = c(2000, 800)  # e.g., in kg N/ha
)

# Assuming 'final_data' is your previous combined dataset
library(dplyr)

# Join with current biomass
final_with_baseline <- final_data %>%
  left_join(current_biomass, by = "Biome") %>%
  mutate(
    Perc_Increase = (Mean / CurrentBiomass) * 100,
    Perc_Lower = (Lower / CurrentBiomass) * 100,
    Perc_Upper = (Upper / CurrentBiomass) * 100
  )

ggplot(final_with_baseline %>% filter(Biome != "Combined"),
       aes(x = SSP, y = Perc_Increase, fill = Biome)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = Perc_Lower, ymax = Perc_Upper),
                position = position_dodge(0.9), width = 0.2) +
  labs(
    y = "Biomass Increase (% of current)",
    title = "Projected Biomass Increase Relative to Current Arctic Biomass"
  ) +
  theme_minimal()




####################################################################################

# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Create the data frame
data <- data.frame(
  Fertilization = c("5%", "10%", "15%"),
  SSP126 = c(3.89, 7.78, 11.6),
  SSP245 = c(5.3, 10.7, 16.1),
  SSP370 = c(7.0, 14.0, 21.0),
  SSP585 = c(7.7, 15.5, 23.3)
)

# Reshape data to long format
data_long <- data %>%
  pivot_longer(cols = -Fertilization, names_to = "Scenario", values_to = "g_C_m2")

# Plot
ggplot(data_long, aes(x = Scenario, y = g_C_m2, fill = Fertilization)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Carbon Biomass Increase at Different Fertilization Levels",
       x = "Fertilization Level",
       y = "g C / m²",
       fill = "Scenario") +
  theme_minimal()


# Create the data frame with means (10% fertilization)
data <- data.frame(
  Scenario = c("SSP126", "SSP245", "SSP370", "SSP585"),
  Mean = c(7.78, 10.7, 14.0, 15.5),  # 10% fertilization values
  SD = c((11.6 - 7.78 + 7.78 - 3.89) / 2,  # (15% - 10%) + (10% - 5%) / 2
         (16.1 - 10.8 + 10.8 - 5.4) / 2,
         (21.0 - 14.0 + 14.0 - 7.0) / 2,
         (23.3 - 15.5 + 15.5 - 7.7) / 2)
)

# Plot
ggplot(data, aes(x = Scenario, y = Mean, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Potential yearly C Uptake due to Permafrost Thaw, until 2100",
       x = "Scenario",
       y = "g C / m²",
       fill = "Scenario") +
  # Add legend for after_2015 lines
  scale_fill_manual(values = c("SSP126" = "red", "SSP245" = "gold", "SSP370" = "darkgreen","SSP585" = "blue"),
                     name = "Scenario") +
  # Theme and appearance
  theme_minimal() +
  theme(legend.position = "bottom")


###########################
library(terra)
setwd("/Users/laraoxley/Desktop/data/CMIP6/final/thawed_N_individual_biomes")
N_total<-rast("thawed_total_ssp126_2m_limit.nc")

#SSP126 = "/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_total_ssp126_2m_limit.nc",
#SSP245 = "/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_total_ssp245_2m_limit.nc",
#SSP370 = "/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_total_ssp370_2m_limit.nc",
#SSP585 = "/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_total_ssp585_2m_limit.nc"


#first calculate total amount of N thawed, relative to preindustrial: 
cell_areas_all_years <- rast(lapply(1:nlyr(N_total), function(i) {
  cellSize(N_total[[i]], mask = TRUE, unit = "m")
}))

weighted_thawed_n_all_years<-N_total*cell_areas_all_years
N_thawed_kg<-global(weighted_thawed_n_all_years, fun = "sum", na.rm = TRUE) #in kg 
N_thawed_Pg <- N_thawed_kg / 1e12
years <- 1850:(1849 + nrow(N_thawed_Pg))
baseline_indices <- which(years >= 1880 & years <= 1900)
baseline_mean_Pg <- mean(N_thawed_Pg$sum[baseline_indices], na.rm = TRUE)
N_thawed_Pg$increase_vs_baseline <- N_thawed_Pg$sum - baseline_mean_Pg

plot(years, N_thawed_Pg$increase_vs_baseline, type = "l",
     ylab = "Thawed N increase (Pg)", xlab = "Year", col = "darkred", lwd = 2)
abline(h = 0, col = "grey", lty = 2)

# save all the SSPs
N_thawed_Pg_585<-N_thawed_Pg
N_thawed_Pg_585 <- rbind(N_thawed_Pg, N_thawed_Pg[nrow(N_thawed_Pg), ])
N_thawed_Pg_370<-N_thawed_Pg
N_thawed_Pg_245<-N_thawed_Pg
N_thawed_Pg_126<-N_thawed_Pg
N_thawed_total<-data.frame(N_thawed_Pg_585, N_thawed_Pg_370, N_thawed_Pg_245, N_thawed_Pg_126)

write.csv(N_thawed_total, "Total_N_thawed_relative_preindustrial.csv")
#SSP126: 26.5 Pg total sum
plot(N_thawed_total)

setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/")
total_thawed<-read.csv("Total_N_thawed_relative_preindustrial.csv")
library(tidyr)
library(dplyr)

df_long <- total_thawed %>%
  pivot_longer(cols = starts_with("SSP"),
               names_to = "Scenario",
               values_to = "Value")

library(ggplot2)

# Define SSP colors for after 2015
ssp_colors <- c("SSP585" = "blue", "SSP370" = "darkgreen", "SSP245" = "orange", "SSP126" = "red")

ggplot(df_long, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(size = 1) +
  scale_color_manual(values = ssp_colors) +
  theme_minimal() +
  labs(title = "Total N thawed",
       x = "Year",
       y = "N [Pg]",
       color = "Scenario")

# Compute weighted thawed nitrogen for each year bioavailability
weighted_thawed_all_years <- N_total * cell_areas_all_years
N_m2<-global(weighted_thawed_all_years, fun = "mean", na.rm = TRUE)






## plant available N thawed: 2m limit
library(terra)

N_total<-rast("/Users/laraoxley/Desktop/data/CMIP6/final/thawed_N_individual_biomes/thawed_total_ssp126_2m_limit.nc")

#SSP126 = "/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_total_ssp126_2m_limit.nc",
#SSP245 = "/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_total_ssp245_2m_limit.nc",
#SSP370 = "/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_total_ssp370_2m_limit.nc",
#SSP585 = "/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_total_ssp585_2m_limit.nc"

N_total_ha <- N_total * 10000

plot(N_total_ha[[250]])

cell_areas_all_years <- rast(lapply(1:nlyr(N_total_ha), function(i) {
  cellSize(N_total_ha[[i]], mask = TRUE, unit = "ha")
}))
plot(cell_areas_all_years[[250]])

weighted_N_thawed <- N_total_ha * cell_areas_all_years
# continue with kg / m2
one_per<-weighted_N_thawed*0.01
five_per<-weighted_N_thawed*0.05
ten_per<-weighted_N_thawed*0.1

one_per_mean<-global(one_per, fun = "sum", na.rm = TRUE)
five_per_mean<-global(five_per, fun = "sum", na.rm = TRUE)
ten_per_mean<-global(ten_per, fun = "sum", na.rm = TRUE)

combined<-cbind(one_per_mean, five_per_mean, ten_per_mean)

# Append last row
#combined <- rbind(combined, combined[nrow(combined), ])

# Now assign the year vector (1850 to 2100)
combined$Year <- 1850:2100

# Assign column names
names(combined) <- c("one", "five", "ten", "Year")
combined$Year<-rep(1850:2100)
names(combined)<-c("one", "five", "ten","Year")

#Step 1: Calculate the baseline means for 1990–2010
baseline <- combined[combined$Year >= 1880 & combined$Year <= 1900, ]

baseline_means <- colMeans(baseline[, c("one", "five", "ten")], na.rm = TRUE)

# Step 2: Subset data for 2010–2100
future <- combined[combined$Year >= 1950 & combined$Year <= 2100, ]

# Step 3: Calculate anomalies
future$anomaly_one <- future$one - baseline_means["one"]
future$anomaly_five <- future$five - baseline_means["five"]
future$anomaly_ten <- future$ten - baseline_means["ten"]

df <- data.frame(combined)
# First, make sure Year is numeric
df$Year <- as.numeric(df$Year)

# Define function to compute mean for a given year range
mean_in_range <- function(data, start_year, end_year) {
  subset <- data[data$Year >= start_year & data$Year <= end_year, ]
  colMeans(subset[, c("one", "five", "ten")], na.rm = TRUE)
}

# Calculate means for each period
mean_1880_1900 <- mean_in_range(df, 1880, 1900)
mean_1990_2010 <- mean_in_range(df, 1990, 2010)
mean_2080_2100 <- mean_in_range(df, 2080, 2100)

# Combine results into one data frame for easy viewing
means_df <- data.frame(
  Period = c("1880-1900", "1990-2010", "2080-2100"),
  rbind(mean_1880_1900, mean_1990_2010, mean_2080_2100)
)

print(means_df)

a<-373189722
a_Gg<-a / 10^9
