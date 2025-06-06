

#### Logs and levels ####

library(readxl)
setwd("C:/Users/paolo/Downloads")

data <-  read_excel("data.xlsx")
data2 <- read_excel("data2.xlsx")
data3 <- read_excel("data3.xlsx")
data4 <- read_excel("data4.xlsx")

library(dplyr)
#merge the datasets into a new dataset df
df <- data %>%
  left_join(data2, by = c("province", "year")) %>%
  left_join(data3, by = c("province", "year")) %>%
  left_join(data4, by = c("province", "year"))

df <- df %>%
 mutate(GDP_pc = GDP / population)

plot(df$year, df$GDP_pc)

#plot GDP_pc against year with boxplots
library(ggplot2)
left <- ggplot(df, aes(x = as.factor(year), y = GDP_pc)) +
  geom_boxplot() +
  labs(title = "GDP per Capita by Year (boxplots)",
       x = "Year",
       y = "GDP per Capita") +
  scale_x_discrete(breaks = as.character(seq(1962, 1988, 3))) + 
  theme_bw()


right <- ggplot(df, aes(x = as.factor(year), y = log10(GDP_pc))) +
  geom_boxplot() +
  labs(title = "log(GDP) per Capita by Year (boxplots)",
       x = "Year",
       y = "GDP per Capita") +
  scale_x_discrete(breaks = as.character(seq(1962, 1988, 3))) + 
  theme_bw()

library(gridExtra)
grid.arrange(left, right, ncol = 2)


###### Part II - to do in class ####

library(readxl)
setwd("C:/Users/paolo/Downloads")

data <-  read_excel("data.xlsx")
data2 <- read_excel("data2.xlsx")
data3 <- read_excel("data3.xlsx")
data4 <- read_excel("data4.xlsx")

library(dplyr)

df <- df %>%
  mutate(n_students = students_primary + students_middle,
         n_schools = schools_primary + schools_middle,
         students_pc = n_students / population,
         schools_pc = n_schools / population,
         GDP_pc = GDP / population,
         log_GDP_pc = log(GDP_pc))

df <- data %>%
  left_join(data2, by = c("province", "year")) %>%
  left_join(data3, by = c("province", "year")) %>%
  left_join(data4, by = c("province", "year"))


df <- df %>%
  mutate(students_schools = n_students / n_schools)

#remove NAs
df <- df %>%
  filter(!is.na(students_schools))

library(tidyr)
df <- df %>%
  unite(col = id, province, year, sep = "", remove = FALSE)

#How can we interpret this new variable?
library(ggplot2)
ggplot(df, aes(x = as.factor(year), y = students_schools)) +
  geom_boxplot() +
  labs(title = "Students per School by Year",
       x = "Year",
       y = "Students per School") +
  theme_bw()


#plot students_schools as boxplots with years as the x axis and add labels for the outliers
ggplot(df, aes(x = as.factor(year), y = students_schools, label = id)) +
  geom_boxplot() +
  labs(title = "Students per School by Year",
       x = "Year",
       y = "Students per School") +
  theme_bw()

####

ggplot(df, aes(x = as.factor(year), y = students_schools, fill = as.factor(centre_north))) +
  geom_boxplot(position = position_dodge(width = 0.75)) + # Use position_dodge to place them side-by-side
  labs(title = "Students per School by Year (North vs. South)",
       x = "Year",
       y = "Students per School",
       fill = "North") + # Label for the fill legend
  theme_bw()
#What are the determinants of the number of students per schools? 
#Hence, what are the deteterminants of the concentration of schools?

# Run a regression to find the determinants of students per school
library(fixest)
mod_1 <- feols(students_schools ~ centre_north | year, 
               cluster = "province", data = df)
mod_1

#with fixed effects
mod_2 <- feols(students_schools ~ log_GDP_pc + centre_north + family_size + population | year, 
               cluster = "province", data = df)
mod_2

mod_3 <- feols(students_schools ~ log_GDP_pc + centre_north + family_size + population + 
                 share_votes_PCI + share_votes_PSI + share_votes_PRI
               | year, 
               cluster = "province", data = df)
mod_3


mod_4 <- feols(students_schools ~ log_GDP_pc + centre_north + family_size + population + 
                 #        share_votes_PCI + share_votes_PSI + share_votes_PRI +
                 employment_rate_male + employment_rate_female +
                 employment_agriculture + employment_industry
               | year, 
               cluster = "province", data = df)
mod_4

