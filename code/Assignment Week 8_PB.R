#Thanks Laura Schepp

library(readxl)
#Make sure you change you working directory!!
setwd("C:/Users/paolo/Downloads")

schools <-  read_excel("data.xlsx")
income <- read_excel("data2.xlsx")
population <- read_excel("data3.xlsx")
data4 <- read_excel("data4.xlsx")

#1
#merging the data sets
library(dplyr)

df <- schools %>%
  full_join(income, by = c("province", "year")) %>%
  full_join(population, by = c("province", "year")) %>%
  full_join(data4, by = c("province", "year"))

#2
#Remove number of high schools, the number of high school students, name of the student who entered the data
library(dplyr)

df <- df %>%
  select(-schools_high, -students_high, -student)

#3
#Create an id for each observation, resulting from the union of province name and year
         
library(tidyr)

df <- df %>%
  unite(col = "id", province, year, sep = "", remove = FALSE)

head(df[c("id", "province", "year")])

#4 
#Creating variables
library(dplyr)

df <- df %>%
  mutate(
    n_students = students_primary + students_middle,
    n_schools = schools_primary + schools_middle,
    students_pc = n_students / population,
    schools_pc = n_schools / population,
    GDP_pc = GDP / population
  )

#5
#Visualising the data set
#I had to replace the old data with the cleaned up version
library(dplyr)


# Step 2: Merge the cleaned dataset with the new 'schools_new' dataset
df <- df_cleaned %>%
  full_join(schools_new, by = c("province", "year"))

View(df)

#or

install.packages("writexl")
library(writexl)
write_xlsx(df, "cleaned_dataset.xlsx")

library(dplyr)

df <- df %>%
  mutate(
    n_students = students_primary + students_middle,
    n_schools = schools_primary + schools_middle,
    students_pc = n_students / population,
    schools_pc = n_schools / population,
    GDP_pc = GDP / population
  )

df <- df %>%
  filter(
    students_pc <= 1,
    schools_pc <= 0.5,
    GDP_pc > 0
  )

View(df)

#6
#Plotting data
library(ggplot2)

ggplot(df, aes(x = GDP_pc, y = students_pc, color = as.factor(year))) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Students per Capita vs. GDP per Capita",
    x = "GDP per Capita",
    y = "Students per Capita",
    color = "Year"
  ) +
  theme_minimal()

#Something seems to be off with the year 1962 since it is just concentrated at around 0.00. I do not understand why this is happening. Aksi the year 1981, 1982 and 1983 have a lot of outliers. I feel like I have done something wrong. Could you give me feedback on this?

#7
# New variable
df <- df %>%
  mutate(log_GDP_pc = log(GDP_pc))
df <- df %>% filter(GDP_pc > 0)

ggplot(df, aes(x = log_GDP_pc, y = students_pc, color = as.factor(year))) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Students per Capita vs. Log of GDP per Capita",
    x = "Log(GDP per Capita)",
    y = "Students per Capita",
    color = "Year"
  ) +
  theme_minimal()
#Data is mostly gathered around 0.125 with a slight downwoard trend in later years. So, the GDP lowers slighlty in later years and so do students per capita. There are still quite extreme spikes in students in different provinces in the years 1962, 1981, 1982 and 1983.

#8

install.packages("fixest")
library(fixest)
model <- feols(students_pc ~ log_GDP_pc | province + year, data = df)
summary(model)

#Coefficient for log_GDP_pc: 0.0157 (positive but small), Not statistically significant (p = 0.503), Within-province variation explained (Within R²): ~0.03% (very low)
#No strong evidence that changes in GDP per capita within provinces significantly affect students per capita, Most variation in students per capita is explained by differences between provinces and years (fixed effects), Changes in GDP within provinces over time explain very little of the variation in student numbers

#9
#rereun regression

model2 <- feols(students_pc ~ log_GDP_pc + family_size | province + year, data = df)
summary(model2)

#Adding family_size did not materially change the interpretation of log_GDP_pc. Its coefficient and significance remain nearly the same, family_size itself doesn't explain much either — it's not significant, and the model fit barely improved, Neither GDP_pc nor family_size explains meaningful variation in students_pc within provinces over time, The relationship might be driven more by other structural factors or long-term effects not captured in this regression

#PartII
#Do the family size and income have an effect on female labour force participation? -> Variables: Familiy size and income (independent variables) GDP_pc will be used as an indication for income, labour force participation female (dependent variable)

library(fixest)

# Basic regression with fixed effects
model <- feols(labour_force_participation_female ~ family_size + GDP_pc | province + year, data = df)

# View results
summary(model)

library(ggplot2)

ggplot(df, aes(x = family_size, y = labour_force_participation_female, color = GDP_pc)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis_c(option = "C") +
  labs(
    title = "Female Labour Force Participation vs. Family Size",
    x = "Family Size",
    y = "Female Labour Force Participation",
    color = "GDP per Capita"
  ) +
  theme_minimal()

#I honestly do not know what the graph is trying to tell me. I must have an issue with my data but I cannot find it.