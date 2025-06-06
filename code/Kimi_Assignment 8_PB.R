# Assignment for week 8

library(readxl)
library(tidyverse)
library(ggplot2)
library(fixest)
library(tidyr)
library(writexl)
library(plm)
library(conflicted)

data <- read_excel("C:/Users/pablo/Downloads/data.xlsx")
data2 <- read_excel("C:/Users/pablo/Downloads/data2.xlsx")
data3 <- read_excel("C:/Users/pablo/Downloads/data3.xlsx")
data4 <- read_excel("C:/Users/pablo/Downloads/data4.xlsx")

#Removing unnecessary variables, merging the data and creating the identifier.
data <- data %>% select(-student)
data <- data %>% select(-schools_high)
data <- data %>% select(-students_high)

df <- data %>%
  left_join(data2, by = c("year", "province")) %>%
  left_join(data3, by = c("year", "province")) %>%
  left_join(data4, by = c("year", "province"))

df <- df %>%
  unite("id", province, year, sep = "", remove = FALSE)

#Creating the five new variables.
df <- df %>%
  mutate(
    n_students = students_primary + students_middle,
    n_schools = schools_primary + schools_middle,
    students_pc = n_students / population,
    schools_pc = n_schools / population,
    GDP_pc = GDP / population
  )

#Creating the scatter plot.
ggplot(df, aes(x = GDP_pc, y = students_pc, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  labs(
    x = "GDP_pc",
    y = "students_pc",
    color = "Year"
  ) +
  theme_minimal()

#Creating the logarithm of GDP_pc and running the scatter plot that was used before
df$log_GDP_pc <- log(df$GDP_pc)

ggplot(df, aes(x = log_GDP_pc, y = students_pc, color = factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  labs(
    x = "Log of GDP per Capita",
    y = "Students per Capita",
    color = "Year"
  ) +
  theme_minimal()


#Running the regression with fixed effects
regressionFE <- feols(students_pc ~ log_GDP_pc | province + year, data = df)
summary(regressionFE)

#Running the regression with fixed effects again but with the Adding of a control variable named "family_size"
regressionFEfam <- feols(students_pc ~ log_GDP_pc + family_size | province + year, data = df)
summary(regressionFEfam)

#Concluding the tasks it could be said, that there is neither a correlation between GDP_pc and students_pc, log_GDP_pc and students_pc nor does the regressions show a significant relation (whether with or without fixed effects or an additional control variable).

#Possible question for part 2: Does a correlation exist between the share of votes of the PCI and GDP_pc and employment rate?
#Running the corresponding regressions.
regression2 <- feols(share_votes_PCI ~ GDP_pc | province + year, data = df)
summary(regression2)

ggplot(df, aes(x = GDP_pc, y = share_votes_PCI, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  labs(
    x = "GDP_pc",
    y = "students_pc",
    color = "Year"
  ) +
  theme_minimal()

regression3 <- feols(share_votes_PCI ~ log_GDP_pc | province + year, data = df)
summary(regression3)

ggplot(df, aes(x = log_GDP_pc, y = share_votes_PCI, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  labs(
    x = "GDP_pc",
    y = "students_pc",
    color = "Year"
  ) +
  theme_minimal()

regression4 <- feols(share_votes_PCI ~ employment_rate| province + year, data = df)
summary(regression4)

ggplot(df, aes(x = employment_rate, y = share_votes_PCI, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  labs(
    x = "GDP_pc",
    y = "students_pc",
    color = "Year"
  ) +
  theme_minimal()

#It could be concluded that there is a small correlation between the share of votes from the PCI and GDP_pc and between the share of votes from the PCI and the rate of employment. For further elaboration one could maybe do further inquiries with regard to a north-south division.
