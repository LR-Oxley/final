### calculating N2O emissions
# calculate ratio N20 emission per total dissolved N from permafrost thaw (data from Voigt et al 2017)

#Total dissolved N: mg N / kg DW
# dry - bare: 307 +/- 111 
# dry - vegetated: 570 +/- 199

# wet - bare: 219 +/- 40
# wet - vegetated: 314 +/- 75

# mass = density * volume
# convert total dissolved N: mg N / kg DW into g / cm'3 
# need bulk density: bare = 0.11 g / cm3 = 0.00011kg/cm3
# vegetated = 0.14 g / cm3 = 0.00014 kg/cm3

# dry bare: 307 mg N / kg DW --> how much is that in mg N / cm'3
# 307 mg N / kg * 0.00011 kg/cm'3 = 0.03377 mg N / cm'3
# dry vegetated: 570 mg N / kg DW * 0.00014 kg/ cm3 = 0.0798 mg N / cm3

# wet bare: 219 mg N / kg DW * 0.00011 kg / cm'3 = 0.02409 mg N / cm3
# wet vegetated: 314 mg N / kg DW * 0.00014 kg / cm3 = 0.04396 mg N / cm3

# to integrate this concentration of total dissolved N over 3cm deep soil slice that's 1m2
# mg N/ cm3 --> mg N/m2

# 1 m2 * 3cm ==> 10'000cm2 * 3 cm = 30'000cm3

# total dissolved N in /m2 for 3 cm deep slice
# dry bare = 0.03377 mg N / cm3 * 30'000 cm3 = 1'013.1 mg N / m2
# dry vegetated = 0.0798 mg N / cm3 * 30'000cm3 = 2'394 mg N / m2
# wet bare = 0.02409 mg N / cm3 * 30'000 cm3 = 722.7 mg N / m2 
# wet vegetated = 0.04396 mg N / cm3 * 30'000 cm3 = 1'318.8 mg N / m2

# to compare with 15 cm deep permafrost core: multiply by 5
# dry bare = 1'013.1 mg N / m2 * 5 = 5065.5 mg N / m2 
# dry vegetated = 2'394 mg N / m2 * 5= 11970 mg N / m2
# wet bare = 722.7 mg N / m2 * 5= 3613.5 mg N / m2
# wet vegetated = 1'318.8 mg N / m2 * 5 = 6594 mg N / m2

# palmtag et al: 0.5 kg N / m2 == 500'000 mg N / m2 (but this includes particulate organic N)
# for 50 cm depth increment

# to compare this with Voigt et al 15 cm deep permafrost core: multiply by 3
# dry bare = 5065.5 mg N / m2 * 3 = 15'195 mg N / m2
# dry vegetated = 11970 mg N / m2 * 3 = 35'910 mg N / m2
# wet bare = 3613.5 mg N / m2 *3 = 10'840.5 mg N / m2
# wet vegetated = 6590 mg N / m2 *3 = 19'770 mg N / m2

# as Voigt et al only included Total dissolved N, which is roughly 10% of the total N (particulate and dissolved) in the soil, the rest would be: 
# dry bare = 15195 / 0.1 = 151'950 mg N / m2
# dry vegetated = 35'910 /0.1 = 359'100 mg N / m2
# wet bare = 10'840 / 0.1 = 108'400 mg N / m2
# wet vegetated = 19'770 = 197'700 mg N / m2


# mean N2O emissions: mg N2O / m2*day for a 15cm slice of permafrost
# dry - bare: 2.81 +/- 0.6
# dry - vegetated: 0.20 +/- 0.03

# wet - bare: 0.21 +/- 0.03
# wet - vegetated: 0.13 +/- 0.02

# n20 emission rate per thawed total dissolved N for 15 cm deep permafrost slice

# dry bare = 2.81 mg N20 / m2 * day / 5065.5 mg N / m2 = 0.0005547 mg N20 / m2 *day per mg N / m2 
# dry vegetated = 0.2 mg N20 / m2 * day / 11970 mg N / m2 = 0.0000167 mg N20 / m2 *day per mg N / m2
# wet bare = 0.21 mg N20 / m2 * day / 3'613.5 mg N / m2 = 0.00005811 mg N20 / m2 *day per mg N / m2
# wet vegetated = 0.13 mg N20 / m2 / 6'594 mg N / m2 = 0.00001971 mg N20 / m2 *day per mg N / m2

# scale n20 emission rate per thawed total dissolved N for 100 cm deep permafrost slice
# dry bare = 0.0005547 mg N20 / m2 *day per mg N / m2 * 6.6 =  0.00366124 mg N20 / m2 * day per mg N / m2
# dry vegetated = 0.0000167 mg N20 / m2 *day per mg N / m2 * 6.6 = 0.00011022 
# wet bare = 0.00005811 mg N20 / m2 *day per mg N / m2 * 6.6 = 0.00038353 mg N20 / m2 * day per mg N / m2
# wet vegetated = 0.00001971 mg N20 / m2 *day per mg N / m2 * 6.6 = 0.00013009 mg N20 / m2 * day per mg N / m2


########
### calculating N2O emissions
# calculate ratio N20 emission per total dissolved N from permafrost thaw (data from Voigt et al 2017)

#Total dissolved N: mg N / kg DW
# dry - bare: 307 +/- 111 
# dry - vegetated: 570 +/- 199

# wet - bare: 219 +/- 40
# wet - vegetated: 314 +/- 75

# mass = density * volume
# convert total dissolved N: mg N / kg DW into g / cm'3 
# need bulk density: bare = 0.11 g / cm3 = 0.00011kg/cm3
# vegetated = 0.14 g / cm3 = 0.00014 kg/cm3

# dry bare: 307 mg N / kg DW --> how much is that in mg N / cm'3
# volume = mass / bulk density
dry_bare<-307 *  0.00011 
dry_vegetated<- 570 * 0.00014 

wet_bare<-219 * 0.00011 
wet_vegetated<-314 * 0.00014 

print(dry_bare)
print(dry_vegetated)
print(wet_bare)
print(wet_vegetated)
# to integrate this concentration of total dissolved N over 3cm deep soil slice that's 1m2
# mg N/ cm3 --> mg N/m2

# 1 m2 * 3cm ==> 10'000cm2 * 3 cm = 30'000cm3

# total dissolved N in /m2 for 3 cm deep slice
dry_bare_m2 <- dry_bare * 30000
dry_vegetated_m2 <-dry_vegetated *30000
wet_bare_m2 <- wet_bare* 30000
wet_vegetated_m2 <- wet_vegetated * 30000 

# to compare with 15 cm deep permafrost core: multiply by 5
dry_bare_15cm<- dry_bare_m2 * 5 
dry_vegetated_15cm<- dry_vegetated_m2* 5
wet_bare_15cm<- wet_bare_m2 * 5
wet_vegetated_15cm <-wet_vegetated_m2 * 5 

# palmtag et al: 0.5 kg N / m2 == 500'000 mg N / m2 (but this includes particulate organic N)
# for 50 cm depth increment

# to compare this with Voigt et al 15 cm deep permafrost core: multiply by 3 to scale it to 45 cm which is comparable to the 50 cm of palmtag
dry_bare_voigt <- dry_bare_15cm * 3 
dry_vegetated_voigt <- dry_vegetated_15cm * 3 
wet_bare_voigt <- wet_bare_15cm*3 
wet_vegetated_voigt <- wet_vegetated_15cm *3 

# as Voigt et al only included Total dissolved N, which is roughly 10% of the total N (particulate and dissolved) in the soil, the rest would be: 
dry_bare_comparison<- dry_bare_voigt / 0.1 
dry_vegetated_comparison <- dry_vegetated_voigt/0.1 
wet_bare_comparison <- wet_bare_voigt / 0.1 
wet_vegetated <- wet_vegetated_voigt / 0.1 
comparison_with_palmtag<-dry_bare_comparison+dry_vegetated_comparison+wet_bare_comparison+wet_vegetated
print(comparison_with_palmtag)

# 817290 mg N /m2 which is a bit more than the palmtag estimate


#### mean N2O emissions: mg N2O / m2*day for a 15cm slice of permafrost
dry_bare_n2o<- 2.81 
dry_vegetated_n2o<-0.20 

wet_bare_n2o<- 0.21 
wet_vegetated_n2o<- 0.13 

# compare with Voigt, Carolina; van Delden, Lona; Marushchak, Maija E; Biasi, Christina; Abbott, Benjamin W; Elberling, Bo;
# Siciliano, Steven D; Sonnentag, Oliver; Stewart, Katherine J; Yang, Yuanhe; Martikainen, Pertti J (2020): Nitrous oxide fluxes from permafrost regions: worldwide synthesis dataset
# mean N2O emissions in mg N2O/m2*day
# dry bare: 3.88
# dry vegetated: 0.87
# wet vegetated: 0.602

# n20 emission rate per thawed total dissolved N for 15 cm deep permafrost slice == mg N20 / day per mg N 

dry_bare_rate_15 <-dry_bare_n2o / dry_bare_15cm 
dry_vegetated_rate_15 <-dry_vegetated_n2o / dry_vegetated_15cm
wet_bare_rate_15<-wet_bare_n2o/ wet_bare_15cm
wet_vegetated_rate_15<- wet_vegetated_n2o / wet_vegetated_15cm 

# n20 emission rate per thawed total dissolved N for 1 cm deep permafrost slice == mg N20 / day per mg N 

dry_bare_rate_1cm <-dry_bare_rate_15 / 15 
dry_vegetated_rate_1cm <-dry_vegetated_rate_15 / 15
wet_bare_rate_1cm<-wet_bare_rate_15/ 15
wet_vegetated_rate_1cm<- wet_vegetated_rate_15 / 15 

# scale n20 emission rate per thawed total dissolved N for 100 cm deep permafrost slice= *100 (as ALD is in m)
dry_bare_rate_1m<- dry_bare_rate_1cm* 100 
dry_vegetated_rate_1m<- dry_vegetated_rate_1cm* 100 
wet_bare_rate_1m<- wet_bare_rate_1cm * 100 
wet_vegetated_rate_1m<- wet_vegetated_rate_1cm * 100 


# calculate difference preindustrial - future release

# calculate into N : mol N to be able to convert N2o into N

## calculating n20 emissions
library(terra)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")

raster_tundra <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_tundra_ssp126.nc")
raster_taiga <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_taiga_ssp126.nc")
raster_wetlands <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_wetlands_ssp126.nc")
raster_barren <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/thawed_barren_ssp126.nc")
plot(raster_taiga[[250]])

#calculate difference thawed N - preindustrial baseline
baseline_tundra <- mean(raster_tundra[[31:51]])
baseline_taiga <- mean(raster_taiga[[31:51]])
baseline_wetlands <- mean(raster_wetlands[[31:51]])
baseline_barren <- mean(raster_barren[[31:51]])


thawed_tundra <- raster_tundra - baseline_tundra
thawed_taiga <- raster_taiga - baseline_taiga
thawed_wetlands <- raster_wetlands - baseline_wetlands
thawed_barren <- raster_barren - baseline_barren

#10% bioavailability
thawed_tundra_bioavailable<-thawed_tundra*0.1
thawed_taiga_bioavailable<-thawed_taiga*0.1
thawed_wetlands_bioavailable<-thawed_wetlands*0.1
thawed_barren_bioavailable<-thawed_barren*0.1

#1% bioavailability
thawed_tundra_bioavailable<-thawed_tundra*0.01
thawed_taiga_bioavailable<-thawed_taiga*0.01
thawed_wetlands_bioavailable<-thawed_wetlands*0.01
thawed_barren_bioavailable<-thawed_barren*0.01

#now convert thawed N kg --> mg 
thawed_tundra_bioavailable_mg<-thawed_tundra_bioavailable* 1e6
thawed_taiga_bioavailable_mg<-thawed_taiga_bioavailable* 1e6
thawed_wetlands_bioavailable_mg<-thawed_wetlands_bioavailable* 1e6
thawed_barren_bioavailable_mg<-thawed_barren_bioavailable* 1e6

# Compute potential N₂O emissions
# Voigt factor: mg N₂O per m depth thaw per m² fläche for dry vegetated
emission_factor_dry_vegetated <- 0.0001113896
emission_factor_wet_vegetated<-0.0001314326
emission_factor_dry_bare<-0.00369822

# Voigt factor: mg N₂O per m depth thaw per m² fläche for dry vegetated per year (112 season days)
emission_factor_year_dry_vegetated <- emission_factor_dry_vegetated*112
emission_factor_year_wet_vegetated<-emission_factor_wet_vegetated*112
emission_factor_year_dry_bare<-emission_factor_dry_bare*112

n2o_emissions_per_yr_dry_vegetated_tundra <- thawed_tundra_bioavailable_mg * emission_factor_year_dry_vegetated  # mg N₂O / m²
n2o_emissions_per_yr_dry_vegetated_taiga <- thawed_taiga_bioavailable_mg * emission_factor_year_dry_vegetated  # mg N₂O / m²
n2o_emissions_per_yr_wet_vegetated <- thawed_wetlands_bioavailable_mg * emission_factor_year_wet_vegetated  # mg N₂O / m²
n2o_emissions_per_yr_dry_bare <- thawed_barren_bioavailable_mg * emission_factor_year_dry_bare  # mg N₂O / m²
plot(n2o_emissions_per_yr_dry_vegetated_tundra[[250]])
plot(n2o_emissions_per_yr_dry_bare[[250]])

n2o_emissions_per_yr_dry_vegetated<-merge(n2o_emissions_per_yr_dry_vegetated_tundra,n2o_emissions_per_yr_dry_vegetated_taiga)

### weighted calculation
# Step 1: Multiply raster by cell area to get mg emissions per cell
cell_area_m2_dry_veg <- cellSize(n2o_emissions_per_yr_dry_vegetated,mask = TRUE, unit = "m")
cell_area_m2_wet_veg <- cellSize(n2o_emissions_per_yr_wet_vegetated,mask = TRUE, unit = "m")
cell_area_m2_dry_bare <- cellSize(n2o_emissions_per_yr_dry_bare,mask = TRUE, unit = "m")

plot(cell_area_m2_dry_veg)

n2o_emissions_per_yr_dry_vegetated <- n2o_emissions_per_yr_dry_vegetated * cell_area_m2_dry_veg
n2o_emissions_per_yr_wet_vegetated <- n2o_emissions_per_yr_wet_vegetated * cell_area_m2_wet_veg
n2o_emissions_per_yr_dry_bare <- n2o_emissions_per_yr_dry_bare * cell_area_m2_dry_bare

# Step 2: Sum all emissions (mg)
total_emissions_dry_vegetated <- global(n2o_emissions_per_yr_dry_vegetated, fun = "sum", na.rm = TRUE)
total_emissions_wet_vegetated <- global(n2o_emissions_per_yr_wet_vegetated, fun = "sum", na.rm = TRUE)
total_emissions_dry_bare <- global(n2o_emissions_per_yr_dry_bare, fun = "sum", na.rm = TRUE)

# Step 3: Sum and double for annual total (winter included)
total_area_emissions_mg_annual <- (total_emissions_dry_vegetated + total_emissions_wet_vegetated + total_emissions_dry_bare) * 2
total_area_emissions_mg<- (total_emissions_dry_vegetated + total_emissions_wet_vegetated + total_emissions_dry_bare)
# Step 4: Convert to Tg
total_area_emissions_Tg <- total_area_emissions_mg / 1e15

#convert to Tg N equivalent
# conversion factor: 
# M (N2O) = 44.013 g/mol
# M (N) = 14.007 g / mol
# Atomic weight of N (2 N atoms in N₂O) = 2 × 14.007 = 28.014 g/mol
# So, the fraction of N in N₂O is: 28.014 / 44.013 = 0.636

total_area_emissions_Tg_Neq <- total_area_emissions_mg * 0.636 / 1e15

setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")
write.csv(total_area_emissions_Tg_Neq, "n2o-n_emissions_Tg_126_1per.csv")


#for each biome: 
tundra_area_emissions_Tg_Neq <- total_emissions_dry_vegetated * 0.636 / 1e15
wetlands_area_emissions_Tg_Neq<-total_emissions_wet_vegetated * 0.636 / 1e15
barren_area_emissions_Tg_Neq<-total_emissions_dry_bare * 0.636 / 1e15

# estimates (Repo et al 2009): 0.1 Tg per year = 100'000'000 kg

df <- read.csv("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/N2O/n2o-n_emissions_Tg.csv")
# Reshape central values
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# First pivot longer to separate SSP from percentile
data_long <- df %>%
  pivot_longer(
    cols = -Year,
    names_to = c("SSP", "Percentile"),
    names_sep = "_",
    values_to = "Emissions"
  )

# Then pivot wider to get 1% and 10% as separate columns
data_ready <- data_long %>%
  pivot_wider(
    names_from = "Percentile",
    values_from = "Emissions",
    names_prefix = "emissions_"
  ) %>%
  rename(emissions_1pct = `emissions_1`,
         emissions_10pct = `emissions_10`)

# Now create the plot
ggplot(data_ready, aes(x = Year)) +
  geom_ribbon(aes(ymin = emissions_1pct, ymax = emissions_10pct, fill = SSP), alpha = 0.3) +
  geom_line(aes(y = emissions_1pct, color = SSP), linetype = "dashed", linewidth = 0.3) +
  geom_line(aes(y = emissions_10pct, color = SSP), linetype = "dashed", linewidth = 0.3) +
  labs(title = "N2O Emissions",
       x = "Year", y = "N2O emissions [Tg]") +
  scale_x_continuous(limits = c(1850, 2100), breaks = seq(1850, 2100, by = 25)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(data_ready)



## to show the yearly potential n2o emissions
df <- read.csv("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/N2O/n2o-n_emissions_Tg_10per.csv")

library(dplyr)

# Use dplyr to calculate year-on-year differences
library(dplyr)


# Filter for 1990 onward and calculate year-on-year differences
yearly_emissions <- df %>%
  filter(Year >= 1989) %>%  # include 1989 to allow for diff()
  arrange(Year) %>%
  mutate(
    SSP126_yr = c(NA, diff(SSP126)),
    SSP245_yr = c(NA, diff(SSP245)),
    SSP370_yr = c(NA, diff(SSP370)),
    SSP585_yr = c(NA, diff(SSP585))
  ) %>%
  filter(Year >= 1990)  # remove 1989 after computing diffs


yearly_long <- yearly_emissions %>%
  select(Year, ends_with("_yr")) %>%
  pivot_longer(-Year, names_to = "Scenario", values_to = "Yearly_Emissions") %>%
  mutate(Scenario = gsub("_yr", "", Scenario))
yearly_emissions <- yearly_emissions %>%
  mutate(
    mean_yr = rowMeans(select(., SSP126_yr, SSP245_yr, SSP370_yr, SSP585_yr), na.rm = TRUE)
  )
ggplot(yearly_long, aes(x = Year, y = Yearly_Emissions, color = Scenario)) +
  geom_line() +
  labs(title = "Yearly N₂O Emissions (1%) from 1990 Onward",
       y = "N₂O Emissions [Tg/year]",
       x = "Year") +
  theme_minimal()


# Convert to long format including mean
yearly_long <- yearly_emissions %>%
  select(Year, SSP126_yr, SSP245_yr, SSP370_yr, SSP585_yr, mean_yr) %>%
  pivot_longer(-Year, names_to = "Scenario", values_to = "Yearly_Emissions") %>%
  mutate(Scenario = gsub("_yr", "", Scenario))

# Plot
ggplot(yearly_long, aes(x = Year, y = Yearly_Emissions, color = Scenario)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Yearly N₂O Emissions (1%) from 1990 Onward",
       y = "N₂O Emissions [Tg N₂O / year]",
       x = "Year") +
  scale_color_manual(values = c(
    "SSP126" = "red",
    "SSP245" = "orange",
    "SSP370" = "forestgreen",
    "SSP585" = "blue",
    "mean" = "black"
  )) +
  theme_minimal()

yearly_emissions <- yearly_emissions %>%
  arrange(Year) %>%
  mutate(
    rollmean_126 = rollmean(SSP126_yr, k = 20, fill = NA, align = "center"),
    rollmean_245 = rollmean(SSP245_yr, k = 20, fill = NA, align = "center"),
    rollmean_370 = rollmean(SSP370_yr, k = 20, fill = NA, align = "center"),
    rollmean_585 = rollmean(SSP585_yr, k = 20, fill = NA, align = "center")
  )
# Prepare data for plotting rolling means
rolling_long <- yearly_emissions %>%
  select(Year, rollmean_126, rollmean_245, rollmean_370, rollmean_585) %>%
  pivot_longer(-Year, names_to = "Scenario", values_to = "Rolling_Mean") %>%
  mutate(Scenario = gsub("rollmean_", "SSP", Scenario))

ggplot(rolling_long, aes(x = Year, y = Rolling_Mean, color = Scenario)) +
  geom_line(linewidth = 1) +
  labs(title = "20-Year Rolling Mean of Yearly N2O Emissions",
       y = "Rolling Mean N2O Emissions [Tg / yr]",
       x = "Year") +
  scale_color_manual(values = c(
    "SSP126" = "red",
    "SSP245" = "orange",
    "SSP370" = "darkgreen",
    "SSP585" = "blue"
  ))+
  xlim(2000,2080)+
  theme_minimal()

# Ensure Year is numeric (if not already)
rolling_long$Year <- as.numeric(rolling_long$Year)

# Get min and max year from rolling_long
min_year <- min(rolling_long$Year, na.rm = TRUE)
max_year <- max(rolling_long$Year, na.rm = TRUE)

# Final plot
ggplot(rolling_long, aes(x = Year, y = Rolling_Mean, color = Scenario)) +
  geom_line(linewidth = 1) +
  
  # Horizontal line for reference (e.g., 0.67 Tg N2O/yr)
  geom_hline(yintercept = 0.67, linetype = "solid", color = "black") +
  
  # Add shaded reference zone
  annotate("rect", xmin = min_year, xmax = max_year,
           ymin = 0.07, ymax = 1.3, fill = "grey50", alpha = 0.2) +
  
  # Axis limits
  coord_cartesian(xlim = c(2000, 2020), ylim = c(0, 1.5)) +
  
  # Labels and style
  labs(title = "20-Year Rolling Mean of Yearly N₂O Emissions",
       y = "Rolling Mean N₂O Emissions [Tg / yr]",
       x = "Year") +
  
  scale_color_manual(values = c(
    "SSP126" = "red",
    "SSP245" = "orange",
    "SSP370" = "darkgreen",
    "SSP585" = "blue"
  )) +
  
  theme_minimal()





### to compare with Ramage, J., Kuhn, M., Virkkala, A.‐M., Voigt, C., Marushchak, M. E., Bastos, A.,et al. (2024). The net GHG balance and budget of the 
## permafrost region (2000–2020) from ecosystem flux upscaling. GlobalBiogeochemicalCycles, 38,e2023GB007953. https://doi.org/10.1029/2023GB007953

# they estimate: permafrost region (excluding Tibetan Plateau) 0.67 (0.07-1.13) Tg N2O-N emissions in period 2000-2020
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/N2O/N2O_Tg_2000-2020.csv")
# Long format, separate SSP and Version
df_long <- df %>%
  pivot_longer(-Year, names_to = "Scenario", values_to = "N2O") %>%
  separate(Scenario, into = c("SSP", "Version"), sep = "_") %>%
  mutate(Version = paste0("P", Version))  # P1 and P10

# Wide format per SSP/year
df_wide <- df_long %>%
  pivot_wider(names_from = Version, values_from = N2O) %>%
  mutate(mid = (P1 + P10) / 2)

# Custom SSP colors
ssp_colors <- c(
  "SSP126" = "red",
  "SSP245" = "goldenrod",
  "SSP370" = "forestgreen",
  "SSP585" = "blue"
)

# Plot
ggplot(df_wide, aes(x = Year)) +
  geom_ribbon(aes(ymin = P1, ymax = P10, fill = SSP), alpha = 0.25) +
  geom_line(aes(y = mid, color = SSP), size = 1.2) +
  geom_hline(yintercept = 0.67, linetype = "solid", color = "black") +
  annotate("rect", xmin = min(df$Year), xmax = max(df$Year),
           ymin = 0.07, ymax = 1.3, fill = "grey50", alpha = 0.2) +
  scale_color_manual(values = ssp_colors) +
  scale_fill_manual(values = ssp_colors) +
  labs(
    title = "Estimated Arctic N2O Emissions (2000–2020) by SSP Scenario",
    subtitle = "Shaded ribbon = 1%–10% bioavailability estimate; Black = literature (Ramage et al., 2024)",
    x = "Year",
    y = "N2O Emissions (Tg N2O/year)",
    fill = "SSP Scenario",
    color = "SSP Scenario"
  ) +
  theme_minimal()
