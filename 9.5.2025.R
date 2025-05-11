
#R very basic operations

#sum

#creating a vector
v <- c(1, 2, 3, 4)
#sum of vectors


#basic functions 

mean(v)

#standard deviation
sd(v)

#if you do not know: use
?sd


#Operator	Description

#  +	  #addition
#  -	  #subtraction
#  *	  #multiplication
#  /	  #division
#  ^ **	#exponentiation
#  x %% y	#modulus (x mod y) 5%%2 is 1
#  x %/% y	#integer division 5%/%2 is 2
#
#  <	  #less than
#  <=	#less than or equal to
#  >	  #greater than
#  >=	#greater than or equal to
#  ==	#exactly equal to - logical operator
#  !=	#not equal to
#  !x	#Not x
#  x | y	#x OR y
#  x & y	#x AND y
#  %>% #pipe operator in "magrittr"

#summary statistics
summary()




#install.packages("readxl")
library(readxl)

data <- read_excel("C:/Users/paolo/Downloads/data.xlsx")


####Part 1 - Exploring data####


#some data cleaning first

#install.packages("dplyr")
library(dplyr)

#we do not want your names in the dataset at this point.
data <- data %>% select(-student)

#Example: What is the province with most schools?

data <- data %>%
  filter(!is.na(schools_primary) & schools_primary != '') 

# Create the bar plot
p <- ggplot(school_counts, aes(x = reorder(province, -schools_primary), y = schools_primary)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Schools per Province",
       x = "Province",
       y = "Number of Schools") +
  theme_bw()

p
