#Code for 9 May 2025

5 + 2
5 / 2


#create a vector
a <- c(1, 2, 3, 4, 2, 6, 8)

mean(a)

sd(a)

?sd

install.packages("readxl")
library(readxl)

data <- read_excel("C:/Users/paolo/Downloads/data.xlsx")


summary(data)

library(dplyr)

data_filtered <- data %>%
  filter(!is.na(schools_primary) & schools_primary != '') 


