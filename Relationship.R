#Loading the data sets
library(readxl)
under_five_mortality_data <- read_excel("C:/Users/ADMIN/Downloads/under_five mortality rate.xlsx")
View(under_five_mortality_data)

library(readxl)
reference_data <- read_excel("C:/Users/ADMIN/Downloads/dataset_datascience.xlsx")
View(reference_data)

library(readxl)
neonatal_mortality_data <- read_excel("C:/Users/ADMIN/Downloads/neonatal_mortality_rate.xlsx")
View(neonatal_mortality_data)

# List of EAC Countries

eac_countries <- c("Kenya", "Uganda", "Rwanda", "Burundi", "United Republic of Tanzania",
                   "Somalia", "South Sudan", "Democratic Republic of the Congo")
print(eac_countries)

# Filtering from the reference file
reference_eac <- reference_data %>%
  filter(`Geographic area` %in% eac_countries)

View(reference_eac)

neonatal_eac <- neonatal_mortality_data %>%
  slice(rep(1:n(), each = length(eac_countries))) %>%
  mutate(`Geographic area` = rep(eac_countries, times = nrow(neonatal_mortality_data)))
View(neonatal_eac)

underfive_eac <- under_five_mortality_data %>%
  slice(rep(1:n(), each = length(eac_countries))) %>%
  mutate(`Geographic area` = rep(eac_countries, times = nrow(under_five_mortality_data)))
View(underfive_eac)

# Checking for duplicates in neonatal
neonatal_eac %>%
  group_by(`Geographic area`, TIME_PERIOD) %>%
  filter(n() > 1)

# Checking for duplicates in under five
underfive_eac %>%
  group_by(`Geographic area`, TIME_PERIOD) %>%
  filter(n() > 1)

#install.packages("sf")

# Load libraries
library(sf)
library(dplyr)
library(ggplot2)

# Read all countries
Kenya_shp <- st_read("C:\\Users\\ADMIN\\Downloads\\gadm41_KEN_shp\\gadm41_KEN_0.shp")
Uganda_shp <- st_read("C:\\Users\\ADMIN\\Downloads\\gadm41_UGA_shp\\gadm41_UGA_0.shp")
United_Republic_of_Tanzania_shp <- st_read("C:\\Users\\ADMIN\\Downloads\\gadm41_TZA_shp\\gadm41_TZA_0.shp")
Rwanda_shp <- st_read("C:\\Users\\ADMIN\\Downloads\\gadm41_RWA_shp\\gadm41_RWA_0.shp")
Burundi_shp <- st_read("C:\\Users\\ADMIN\\Downloads\\gadm41_BDI_shp\\gadm41_BDI_0.shp")
SouthSudan_shp <- st_read("C:\\Users\\ADMIN\\Downloads\\gadm41_SSD_shp\\gadm41_SSD_0.shp")
Somalia_shp <- st_read("C:\\Users\\ADMIN\\Downloads\\gadm41_SOM_shp\\gadm41_SOM_0.shp")
DRC_shp <- st_read("C:\\Users\\ADMIN\\Downloads\\gadm41_COD_shp\\gadm41_COD_0.shp")

# Binding all together
eac_shapefile <- bind_rows(Kenya_shp,Uganda_shp, United_Republic_of_Tanzania_shp, Rwanda_shp, Burundi_shp, SouthSudan_shp, Somalia_shp, DRC_shp)

# Plot
ggplot(eac_shapefile) +
  geom_sf(aes(fill = COUNTRY), color = "black") +
  theme_minimal() +
  labs(title = "EAC Countries Map", fill = "Country")

# Latest year available in neonatal data
latest_year <- max(neonatal_eac$TIME_PERIOD, na.rm = TRUE)

# Filtering neonatal data to only latest year
neonatal_latest <- neonatal_eac %>%
  filter(TIME_PERIOD == latest_year)

# Merging with shapefile
eac_neonatal_map <- eac_shapefile %>%
  left_join(neonatal_latest, by = c("COUNTRY" = "Geographic area"))

head(eac_neonatal_map)

# Neonatal mortality plot
ggplot(eac_neonatal_map) +
  geom_sf(aes(fill = OBS_VALUE), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = paste0("Neonatal Mortality Rates (", latest_year, ")"),
       fill = "Deaths per 1,000 live births")

# Latest under-five data
underfive_latest <- underfive_eac %>%
  filter(TIME_PERIOD == latest_year)

# Merge
eac_underfive_map <- eac_shapefile %>%
  left_join(underfive_latest, by = c("COUNTRY" = "Geographic area"))

# Plot
ggplot(eac_underfive_map) +
  geom_sf(aes(fill = OBS_VALUE), color = "white") +
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  theme_minimal() +
  labs(title = paste0("Under-Five Mortality Rates (", latest_year, ")"),
       fill = "Deaths per 1,000 live births")

# Neonatal
neonatal_avg <- neonatal_eac %>%
  group_by(TIME_PERIOD) %>%
  summarise(average_mortality = mean(OBS_VALUE, na.rm = TRUE))

# Under-five
underfive_avg <- underfive_eac %>%
  group_by(TIME_PERIOD) %>%
  summarise(average_mortality = mean(OBS_VALUE, na.rm = TRUE))

#Neonatal mortality over time

ggplot() +
  # Points for each country
  geom_point(data = neonatal_eac, aes(x = TIME_PERIOD, y = OBS_VALUE, color = `Geographic area`), alpha = 0.5) +
  
  # Average trend line
  geom_line(data = neonatal_avg, aes(x = TIME_PERIOD, y = average_mortality), color = "black", size = 1.5) +
  
  theme_minimal() +
  labs(
    title = "Neonatal Mortality Rates Over Time (EAC Countries)",
    x = "TIME_PERIOD",
    y = "Deaths per 1,000 live births",
    color = "Geographic Area"  
  ) +
  theme(legend.position = "right")  

# Under five mortality over time

ggplot() +
  # Points for each country
  geom_point(data = underfive_eac, aes(x = TIME_PERIOD, y = OBS_VALUE, color = `Geographic area`), alpha = 0.5) +
  
  # Average trend line
  geom_line(data = underfive_avg, aes(x = TIME_PERIOD, y = average_mortality), color = "black", size = 1.5) +
  
  theme_minimal() +
  labs(
    title = "Under-Five Mortality Rates Over Time (EAC Countries)",
    x = "TIME_PERIOD",
    y = "Deaths per 1,000 live births",
    color = "Country"
  )

# Find the most recent year
latest_year_neonatal <- max(neonatal_eac$TIME_PERIOD, na.rm = TRUE)

# Neonatal mortality - latest year
highest_neonatal <- neonatal_eac %>%
  filter(TIME_PERIOD == latest_year_neonatal) %>%
  arrange(desc(OBS_VALUE)) %>%
  slice(1)

highest_neonatal

# Bar plot for neonatal mortality
ggplot(neonatal_latest, aes(x = reorder(`Geographic area`, OBS_VALUE), y = OBS_VALUE, fill = `Geographic area`)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = paste0("Neonatal Mortality Rates (", latest_year_neonatal, ") - EAC Countries"),
    x = "Country",
    y = "Deaths per 1,000 live births"
  ) +
  geom_text(aes(label = round(OBS_VALUE, 1)), hjust = -0.2, size = 3) +
  ylim(0, max(neonatal_latest$OBS_VALUE) * 1.2)  # Small space on top


latest_year_underfive <- max(underfive_eac$TIME_PERIOD, na.rm = TRUE)

# Under-five mortality - latest year
highest_underfive <- underfive_eac %>%
  filter(TIME_PERIOD == latest_year_underfive) %>%
  arrange(desc(OBS_VALUE)) %>%
  slice(1)

highest_underfive

# Bar plot for under-five mortality
ggplot(underfive_latest, aes(x = reorder(`Geographic area`, OBS_VALUE), y = OBS_VALUE, fill = `Geographic area`)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = paste0("Under-Five Mortality Rates (", latest_year_underfive, ") - EAC Countries"),
    x = "Country",
    y = "Deaths per 1,000 live births"
  ) +
  geom_text(aes(label = round(OBS_VALUE, 1)), hjust = -0.2, size = 3) +
  ylim(0, max(underfive_latest$OBS_VALUE) * 1.2)


