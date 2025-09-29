library(terra)
library(ggplot2)

# Load NetCDF thawed nitrogen rasters
rast_mean_585 <- rast("mean_ssp126_corr.nc")
rast_std_585<- rast("std_ssp585_corr.nc")
# mean ALD all the cell areas (total area of the grid)
mean_ssp585 <- global(rast_mean_585, fun = "mean", na.rm = TRUE)
std_ssp585<- global(rast_std_585, fun = "std", na.rm = TRUE)
plot(rast_mean_585[[1]])

# Load NetCDF  rasters
rast_mean_585 <- rast("mean_ssp585_corr.nc")
rast_std_585 <- rast("std_ssp585_corr.nc")

# mean ALD all the cell areas (total area of the grid)
mean_ssp585 <- global(rast_mean_585, fun = "mean", na.rm = TRUE)
std_ssp585<- global(rast_std_585, fun = "std", na.rm = TRUE)

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