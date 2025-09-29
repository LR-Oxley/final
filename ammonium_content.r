library(here)
#install.packages("here")
#1.die Mean Denitrification Rate direkt aus Guo et al. nehmen:

denitrification_data<-read.csv(here("Denitrification", "Guo_mengje_denitrification.csv"))
mean_rate<-mean(denitrification_data$nmol_N_g_h)
sd_rate<-sd(denitrification_data$nmol_N_g_h)


#  die Bodenmasseneinheit umrechnen und wegbekommen = 3055 nmol N / kg * h
mean_rate<-mean_rate
mean_bulk_density_g_cm3<-0.750

mean_rate_nmol_n_cm3_h<-mean_rate*mean_bulk_density_g_cm3
mean_rate_nmol_n_m3_h<-mean_rate_nmol_n_cm3_h*(1*10^6)
#  Die N Einheit umrechnen und wegkürzen (nmol N in mg N):
mean_rate_mol_n_m3_h<-mean_rate_nmol_n_m3_h*(1*10^-9)
mean_rate_g_N_m3_h<-mean_rate_mol_n_m3_h*14
# in square m: multiply by average active layer depth
mean_rate_g_N_m2_h<-mean_rate_g_N_m3_h*1.4
#  h-1 in yr-1 umrechnen
mean_rate_g_N_m2_day<-mean_rate_g_N_m2_h*12
mean_rate_g_N_m2_yr<-mean_rate_g_N_m2_day*120
# Diese Rate multiplizieren mit dem bioverfügbaren N (eventuell noch NO3 abziehen)









######################### 
#. calculate denitrification rate with data from hansen-elberling review




denitrification_data<-read.csv("denitrification/Guo_denitrification.csv")

cor.test(denitrification_data$nmol_N_g_h, denitrification_data$NH4_N_mg_kg, method = "pearson")

denitrification_lm<-lm(nmol_N_g_h~NH4_N_mg_kg, data=denitrification_data)
summary(denitrification_lm)
# estimate = 0.921
# slope = 0.207
#denitrification_rate = 0.921 + 0.207 * ("Ammonium concentration in mg / kg")

ggplot(ammonium_data,
       aes(x = NH4_N_mg_kg,
           y = nmol_N_g_h)) +
  geom_point(aes(color = Site), size = 2, na.rm = TRUE) +
  geom_errorbar(aes(ymin = nmol_N_g_h - SD,
                    ymax = nmol_N_g_h + SD,
                    color = Site),
                width = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 0.7) +
  theme_minimal(base_size = 14) +
  labs(title = "Denitrification rate vs Ammonium concentration",
       x = "Ammonium concentration [NH4+ mg / kg]",
       y = "Denitrification rate [nmol_N_g_h]")



## Hansen Elberling 2023 Data 
library(ggplot2)
data<-read.csv("/Users/laraoxley/Desktop/Hansen_Elberling.csv")

permafrost<-data[data$Permafrost0_ActiveLayer_1==0,]

mean_NH4<-mean(permafrost$NH4_mg_l_Normalized, na.rm = TRUE)
std_NH4<-sd(permafrost$NH4_mg_l_Normalized, na.rm = TRUE)

model1<-lm(NH4_mg_l_Normalized~Study_description, data=ammonium)
summary(model1)

ggplot(permafrost,
       aes(x = Soil_type,
           y = NH4_mg_l_Normalized)) +
  geom_point(aes(color = Study), na.rm = TRUE) +
  theme_minimal(base_size = 14) +
  labs(title = "mg NH4+ per L",
       x = "Permafrost extent",
       y = "NH4+ [mg / L]")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(permafrost,
       aes(x = Study_description,
           y = NH4_mg_l_Normalized)) +
  geom_boxplot(na.rm = TRUE, fill = "skyblue") +
  theme_minimal(base_size = 14) +
  labs(title = "mg NH4+ per L",
       x = "Soil type",
       y = "NH4+ [mg / L]")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Hansen Elberling NH4 data is in mg NH4 / L. need to first convert NH4 to NH4_N
# From mg NH₄⁺/L to mg NH₄-N/kg: Molecular weight conversion:
# NH₄⁺ molecular weight = 18.04 g/mol (N=14, H₄=4.04)
# N atomic weight = 14 g/mol
# Conversion factor = 14/18.04 ≈ 0.776

## mg NH₄-N/kg soil = (mg NH₄⁺/L solution) × (water content % / 100) × (14/18.04)
# mg/L (solution) * water content (L water/kg soil) = mg/kg (soil)
NH4_mg_L<-permafrost$NH4_mg_l_Normalized

#if water content = 25% (dry mass basis) --> 25% = 0.25 g water per g dry soil
#Divide by 100 → gives g water per g dry soil
water_content<-(permafrost$Water_content/100)


NH4_N_mg_kg<-NH4_mg_L*water_content*(14/18.04)

permafrost$NH4_N_mg_kg <- NH4_N_mg_kg
#denitrification_rate = 0.921 + 0.207 * ("Ammonium concentration in mg / kg")

mean_NH4_N<-mean(permafrost$NH4_N_mg_kg, na.rm = TRUE)
median_NH4_N<-median(permafrost$NH4_N_mg_kg, na.rm = TRUE)
sd_NH4_N<-sd(permafrost$NH4_N_mg_kg, na.rm = TRUE)

denitrification_rate = 0.921 + 0.207 *NH4_N_mg_kg
permafrost$denitrification_rate_nmol_N_g_h <- denitrification_rate

mean_denitrification_rate<-mean(permafrost$denitrification_rate_nmol_N_g_h, na.rm = TRUE)
sd_denitrification_rate<-sd(permafrost$denitrification_rate_nmol_N_g_h, na.rm = TRUE)

ggplot(permafrost,
       aes(x = Study_description,
           y = denitrification_rate_nmol_N_g_h)) +
  geom_boxplot(na.rm = TRUE, fill = "skyblue") +
  theme_minimal(base_size = 14) +
  labs(title = "Denitrification rate",
       x = "Soil type",
       y = "nmol_N_g_h")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# scaling up for Arctic wide denitrification: 
# convert nmol N / g * h into kg N per km2 per day
# mean_denitrification_rate = 1.6679 nmol N / g * h
# 1. convert nanomole → mole
denitrification_rate<- mean_denitrification_rate* (1*10^-9) ## mol N / g * h
# 2. multiply by atomic weight of nitrogen (14 g/mol) to get grams of N
denitrification_rate<- denitrification_rate* 14 ## g N / g * h
# 3. multiplying by 1000 converts it to per kg soil
denitrification_rate<- denitrification_rate* 1000 ## g N / kg * h

# take palmtag N (bioavailable) * 90% == ammonium content in soil

# 4. multiply* (bulk_density in g / cm3 * 1000 = kg / m3) to get kg soil per m³ soil; density = mass / volume; volume = mass / density
denitrification_rate<- denitrification_rate * (1.5 * 1000) 
# 5. * active_layer_depth to get per m2
denitrification_rate<- denitrification_rate * 1.4 # mean ALD
# 6. * 24 to get rate in per day ??
denitrification_rate<- denitrification_rate*24
# 7. / 1000 to get g N / m2 * day rate in kg N / m2 * day
denitrification_rate<- denitrification_rate / 1000
# 8. per season: * 120 days == unrealistic?
denitrification_rate<- denitrification_rate*120
# 9. to get it per km2: multiply by 10^6
denitrification_rate<-denitrification_rate*10^6
# 10. Total annual Arctic denitrification = multiply by 1.7*10^7 km2
denitrification_rate <- denitrification_rate * 1.7*10^7
# Convert kg to Teragrams (Tg)
total_annual_Tg <- denitrification_rate / 1e9
##

## 100–200 Tg N/yr estimate for global terrestrial denitrification