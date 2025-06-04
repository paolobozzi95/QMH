

#install.packages("readxl")
library(readxl)

data <- read_excel("C:/Users/paolo/Downloads/Data education Italy 1962-1988.xlsx")


####Part 1 - Exploring data####


#some data cleaning first

#install.packages("dplyr")
library(dplyr)

data <- data %>% select(-student)

#I remove data for 1977 since there are too many issues with the source
data <- data %>% filter(year != 1977)


data_yearly <- data %>%
  group_by(year) %>%
  summarise(students_primary_yearly = sum(students_primary, na.rm = TRUE),
            students_middle_yearly  = sum(students_middle, na.rm = TRUE),
            schools_primary_yearly = sum(schools_primary, na.rm = TRUE))

#remove zeros
data_yearly <- data_yearly %>%
  filter(students_primary_yearly > 0 & students_middle_yearly > 0 & schools_primary_yearly > 0,
         year > 1972)

library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)


#plot the number of primary and middle school students over the years

plot <- ggplot(data_yearly, aes(x = year)) +
  geom_line(aes(y = students_primary_yearly/100000, color = "Primary School Students"), size = 1) +
  geom_line(aes(y = schools_primary_yearly/1000, color = "Primary Schools"), size = 1) +
  labs(color = "",
       title = "Number of Primary School Students and Schools in Italy",
       x = "Year",
       y = "Number of students in 100k / Number of schools in k") +
  scale_color_manual(values = c("Primary School Students" = "blue", "Primary Schools" = "red")) +
#change X axis to every 5 years
  scale_x_continuous(breaks = seq(min(data_yearly$year), max(data_yearly$year), by = 5)) +
  theme_economist()

plot

cor(data_yearly$students_primary_yearly, data_yearly$schools_primary_yearly)


#data3 <- read_excel("C:/Users/paolo/Downloads/data3.xlsx")
