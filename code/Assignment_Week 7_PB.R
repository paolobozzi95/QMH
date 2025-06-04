#Uploading data into R

library(readxl)

#call the excel files, now called data, data2, data3
#make sure you change your working directory!
data <- read_excel("C:/Users/paolo/Downloads/data.xlsx")
data2 <- read_excel("C:/Users/paolo/Downloads/data2.xlsx")
data3 <- read_excel("C:/Users/paolo/Downloads/data3.xlsx")


#1, Merging the data sets
# Merge the first two datasets
merged_data_2 <- merge(data, data2, by = c("province", "year"))

# Merge the result with the third dataset by the same two columns
final_merged_data <- merge(merged_data_2, data3, by = c("province", "year"))

#problems with merge
colnames(merged_data_2)

summary(merged_data_2)
#2, Creating new variable
library(dplyr)

df <- final_merged_data %>%
  mutate(num_schools = schools_primary + schools_middle)

head(df)

class(df)

# Remove 'df' from your environment (which is currently a function)
rm(df)

# List all objects in your environment
ls()

#I figgured out what my issue was. I forgot to tell ChatGPT what my data frame was called. 
# Load the dplyr package
library(dplyr)

# Create a new column 'num_schools' that is the sum of 'schools_primary' and 'schools_middle'
final_merged_data <- final_merged_data %>%
  mutate(num_schools = schools_primary + schools_middle)

# View the updated data frame
head(final_merged_data)

# Now calculating the number of schools per capita
final_merged_data <- final_merged_data %>%
  mutate(num_schools_pc = num_schools / population)

# View the updated data frame
head(final_merged_data)

# Calculating GDP per Capita
final_merged_data <- final_merged_data %>%
  mutate(GDP_pc = GDP / population)

# View the updated data frame
head(final_merged_data)

# 3, Filter data for the year 1979
data_1979 <- final_merged_data %>%
  filter(year == 1979)

# View the filtered data
head(data_1979)

#4, Creating plot
View(data_1979)

#### Issue with the data ####


#### 1. Maps  ####

library(ggplot2)
library(sf)
library(dplyr)
library(gridExtra)
library(RColorBrewer)


map_data <- st_read("C:/Users/paolo/Documents/000 - Publications/Data paper Political Economy/Province_1981/Province1981.shp")



map_data <- map_data %>%
  mutate(DEN_PROV = ifelse(row_number() == 40, "Forlì", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Bolzano - Bozen", "Bolzano", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Valle d'Aosta", "Aosta", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Massa-Carrara", "Massa Carrara", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Reggio nell'Emilia", "Reggio Emilia", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Reggio di Calabria", "Reggio Calabria", DEN_PROV),
  )


df_map <- final_merged_data

merged_data <- left_join(map_data, df_map, by = c("DEN_PROV" = "province"))

#Replace the row for Chieti with NAs
merged_data[merged_data$DEN_PROV == "Rieti", c("num_schools_pc", "GDP_pc")] <- NA


#View(merged_data)

map_schools_pc <- merged_data %>%
  filter(year == 1979) %>%
  ggplot() +
  geom_sf(aes(fill = num_schools_pc), color = "black") +
  theme_void() +
  scale_fill_gradient(low = "#F7FBFF", high = "#08306B", na.value = "grey")+
  theme(legend.position="bottom")

map_schools_pc 

map_GDP_pc <- merged_data %>%
  filter(year == 1979) %>%
  ggplot() +
  geom_sf(aes(fill = GDP_pc), color = "black") +
  theme_void() +
  scale_fill_gradient(low = "#F7FBFF", high = "#08306B", na.value = "grey")+
  theme(legend.position="bottom")

map_GDP_pc







# Load the ggplot2 package
library(ggplot2)

# Create a scatter plot with GDP_pc (income) on the x-axis and num_schools_pc (schools per capita) on the y-axis
ggplot(final_merged_data, aes(x = GDP_pc, y = num_schools_pc)) +
  geom_point() +  # Scatter plot
  labs(
    title = "GDP per Capita vs. Number of Schools per Capita",  # Title of the plot
    x = "GDP per Capita",  # Label for the x-axis
    y = "Number of Schools per Capita"  # Label for the y-axis
  ) +
  theme_minimal()  # Optional clean theme

#5, now with regression line
# Load the ggplot2 package
library(ggplot2)

# Create a scatter plot with GDP_pc (income) on the x-axis and num_schools_pc on the y-axis
# and add a regression line
ggplot(final_merged_data, aes(x = GDP_pc, y = num_schools_pc)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line (no confidence interval)
  labs(
    title = "GDP per Capita vs. Number of Schools per Capita", 
    x = "GDP per Capita", 
    y = "Number of Schools per Capita"
  ) +
  theme_minimal()  # Optional clean theme
# 6, The positive regression line shows that the is a correlation. It is a strong one since the points are mostly close to the line. Also it seems to be a linear relationship

# 7, Calculate the Pearson correlation between GDP per capita and number of schools per capita
correlation <- cor(final_merged_data$GDP_pc, final_merged_data$num_schools_pc)

# Print the result
print(correlation)

# Remove rows with missing values in either GDP_pc or num_schools_pc
final_merged_data_clean <- na.omit(final_merged_data[, c("GDP_pc", "num_schools_pc")])

# Calculate the Pearson correlation on the cleaned data
correlation <- cor(final_merged_data_clean$GDP_pc, final_merged_data_clean$num_schools_pc)

# Print the result
print(correlation)

# 8, install.packages("fixest")  # Uncomment if 'fixest' is not installed
install.packages("fixest")

library(fixest)

# Run the fixed effects regression with clustering by "province"
reg <- feols(num_schools_pc ~ GDP_pc, data = final_merged_data, cluster = "province")

# Summarize the regression results
summary(reg)

#Coefficient:0.252265, As the GDP per capita increases, the number of schools per capita increases significantly. This suggests that wealthier areas tend to have more schools per capita.
#SEs:The relatively small SE (0.016107) suggests that the estimated relationship between GDP per capita and number of schools per capita is fairly precise
#R-squared: This is a very high R-squared value (0.967157), suggesting a strong relationship.



#Non linear trends






