
#install.packages("sf")

library(sf)
library(readxl)
library(dplyr)
library(ggplot2)

#We use st_read to read the shapefile

# Path to the shapefile
shapefile_path <- "C:/Users/paolo/Downloads/Prov1991_g_WGS84.shp"

map_data <- st_read(shapefile_path)

head(map_data)

#Very easy!
ggplot(data = map_data) +
  geom_sf(fill = "lightblue", color = "black") +
  ggtitle("Map from Shapefile in R") +
  theme_minimal()

ggplot(data = map_data) +
  geom_sf(fill = "lightblue", color = "black") +
  ggtitle("Map from Shapefile in R") +
  theme_void()


#Let's project some data on this maps

data <- read_excel("C:/Users/paolo/Downloads/data.xlsx")
data3 <- read_excel("C:/Users/paolo/Downloads/data3.xlsx")
data2 <- read_excel("C:/Users/paolo/Downloads/data2.xlsx")

#merge
df <- data %>%
  left_join(data3, by = c("province", "year")) %>%
  left_join(data2, by = c("province", "year"))

head(df)


#I merge the data with the map data

#check that the names correspond
head(map_data)

map_data <- map_data %>%
  rename(
    province = DEN_PROV
  )

#A few provinces have different names. We have to change them manually
map_data <- map_data %>%
  mutate(province = case_when(
    province == "Reggio di Calabria" ~ "Reggio Calabria",
    province == "Reggio nell'Emilia" ~ "Reggio Emilia",
    province == "Massa-Carrara" ~ "Massa Carrara",
    province == "Pesaro e Urbino" ~ "Pesaro Urbino",
    province == "Valle d'Aosta/Vallée d'Aoste" ~ "Aosta",
    province == "Bolzano/Bozen" ~ "Bolzano",
    
    TRUE ~ province
  ))

#Merge
merged_data <- map_data %>%
  left_join(df, by = "province")

#Variable of interst
merged_data <- merged_data %>%
  mutate(
    variable = students_middle / population * 1000,
    variable_2 = GDP / population * 1000
  )

data_1963 <- merged_data %>%
  filter(year == 1963)

ggplot(data = data_1963) +
  geom_sf(aes(fill = variable_2), color = "black") +
  scale_colour_gradient() +
  theme_void() +
  theme(legend.position = "bottom")
