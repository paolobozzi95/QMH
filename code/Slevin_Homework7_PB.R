#Margaret Mamie Slevin
#Homework 7 for May 30

# 1. 
# Download and merge data from Moodle
library(readxl)

#call the excel files, now called data, data2, data3
data <- read_excel("C:/Users/paolo/Downloads/data.xlsx")
data2 <- read_excel("C:/Users/paolo/Downloads/data2.xlsx")
data3 <- read_excel("C:/Users/paolo/Downloads/data3.xlsx")

library(dplyr)

#remove our names from data
data <- data %>% select(-student)

#merge data and data3 on the province and year columns
data_merge <- data %>%
  left_join(data2, by = c("year", "province")) %>%
  left_join(data3, by = c("year", "province"))

#found syntax error in data3 Cremona 1976 population that results in GDP_pc over 1
#to fix the mistake before creating GDP_pc
#PB: Very good!
data_merge <- data_merge %>%
  mutate(population = if_else(province == "Cremona" & year == 1976, 332592, population))

#View(data_merge)
# 2.
# Create new variables num_schools and GDP_pc
library(dplyr)

#create num_schools - sum of primary and middle schools
data_merge <- data_merge %>%
  mutate(num_schools = schools_primary + schools_middle)

#create num_schools_pc
data_merge <- data_merge %>%
  mutate(num_schools_pc = num_schools / population)

#create GDP_pc - GDP per capita
data_merge <- data_merge %>%
  mutate(GDP_pc = GDP / population)

summary(data_merge)

# 3. 
# Filter for a single year
data_merge_total <- data_merge

data_merge <- data_merge %>%
  filter(year == 1976)

# 4. 
# Using ggplot2, plot income on the x-axis and number of schools on the y-axis.

#first ggplot remove 5 rows of data, find out why
#View rows with NA or invalid values
data_merge[!complete.cases(data_merge[, c("GDP_pc", "num_schools_pc")]), ]
#no GDP for 1976 - Isernia, Oristano, Pordenone
#mismatched province name in data2 - Massa Carrara, Pesaro Urbino
#fixed naming inconsistencies manually in data2 excel file and reran all code

library(ggplot2)

ggplot(data_merge, aes(x = GDP_pc, y = num_schools_pc)) +
  geom_point() +
  labs(
    title = "Number of Schools per Capita vs. GDP per Capita",
    x = "GDP per Capita",
    y = "Number of Schools per Capita"
  ) +
  theme_minimal()

# 5.
# Draw a regression line
ggplot(data_merge, aes(x = GDP_pc, y = num_schools_pc)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Number of Schools per Capita vs. GDP per Capita",
    x = "GDP per Capita",
    y = "Number of Schools per Capita"
  ) +
  theme_minimal()

# 6. 
# From the plot, is there a correlation? 

#from the plot there appears to be a negative correlation
#between num_schools_pc and GDP_pc

# 7.
# Calculate the Pearson correlation

#data_clean filters out missing values
data_clean <- data_merge %>%
  filter(!is.na(GDP_pc), !is.na(num_schools_pc))

#calculate Pearson correlation
correlation <- cor(data_clean$GDP_pc, data_clean$num_schools_pc, method = "pearson")
print(correlation)

#recommended by ChatGPT
#add correlation to the plot title 
library(ggplot2)

ggplot(data_clean, aes(x = GDP_pc, y = num_schools_pc)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = paste("Number of Schools per Capita vs. GDP per Capita\n(Pearson r =", round(correlation, 2), ")"),
    x = "GDP per Capita",
    y = "Number of Schools per Capita"
  ) +
  theme_minimal()

#or as an annotation in the plot
ggplot(data_clean, aes(x = GDP_pc, y = num_schools_pc)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  annotate("text", x = max(data_clean$GDP_pc), y = max(data_clean$num_schools_pc),
           label = paste("r =", round(correlation, 2)),
           hjust = 1, vjust = 1, size = 5, color = "blue") +
  labs(
    title = "Number of Schools per Capita vs. GDP per Capita",
    x = "GDP per Capita",
    y = "Number of Schools per Capita"
  ) +
  theme_minimal()

# 8.
# Run the regression

library(fixest)

reg <- feols(num_schools_pc ~ GDP_pc, cluster = "province", data = data_clean)
summary(reg)

# Discuss the results
#"Estimate" column reports the estimated coefficient. 
#For GDP_pc the estimate is -0.25 This means that for every 1 unit increase 
#in GDP_pc, there is an estimated -0.25 change in num_schools_pc
#standard error shows the uncertainty of the estimate
#according to ChatGPT the relative size of the standard error in this model (44%) 
#is relatively large. despite the statistical significance of the estimate, 
#there is still a higher amount of uncertainty than might be desirable
#p-value is 0.0244 meaning that there is a 2.44% chance that the estimated 
#coefficient is a result of sampling variation alone.
#It is less than 0.05 and marked with a *, meaning that it is statistically significant.
#This shows that a relationship between GDP_pc and num_schools_pc is likely.
#The (absolute value of the) t-value is greater than 1.96 
#so there is a greater than 95% chance than the coefficient is significantly 
#different from 0 (disproving the null hypothesis.)
#the adjusted R squared is 0.057, meaning that 5.7% of the variation in 
#num_schools_pc is predicted by GDP_pc
#adjusted means that the value is calculated accounting for the number of 
#variables in the model

# Run the regression without cluster
reg2 <- feols(num_schools_pc ~ GDP_pc, data = data_clean)
summary(reg2)

#the estimate remains the same
#standard error is lower than previously, indicating less uncertainty with 
#unclusterd data
#the (absolute value of the) t-value is still greater than 1.96, 
#so the meaning remains the same as above
#the p-value is now 0.0121 and lower than before
#this indicates a stronger statistical relationship
#like the above it is marked with * which indicates the same level of 
#significance (within 5%). if the new p-value were less than 0.01 this would 
#indicate a higher level of significance (within 1%)
#the adjusted R squared is the same
#overall, without clustering the relationship between GDP_pc and num_schools_pc 
#appears less uncertain and more statistically significant this also indicates a 
#less conservative analysis of our data
#however, the results without clustering are no so different that they significantly 
#change our overall interpretation of the relationship between GDP_pc and num_schools_pc






##### Multiple regression #####


data4 <- read_excel("C:/Users/paolo/Downloads/data4.xlsx")

#merge datasets
final_dataset <- data_merge_total %>%
  left_join(data4, by = c("year", "province"))




