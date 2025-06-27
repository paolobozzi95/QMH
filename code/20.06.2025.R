





library(readxl)
setwd("C:/Users/paolo/Downloads")

data <-  read_excel("data.xlsx")
data2 <- read_excel("data2.xlsx")
data3 <- read_excel("data3.xlsx")
data4 <- read_excel("data4.xlsx")
data5 <- read_excel("data5.xlsx")
data6 <- read_excel("data6.xlsx")
data7 <- read_excel("pensions.xlsx")


#I merge the data
library(dplyr)
df <- data %>%
  left_join(data2, by = c("province", "year")) %>%
  left_join(data3, by = c("province", "year")) %>%
  left_join(data4, by = c("province", "year")) %>%
  left_join(data5, by = c("province", "year")) %>%
  left_join(data6, by = c("province", "year")) %>%
  left_join(data7, by = c("province", "year"))


#Is there a correlation between voting for the DC and the share of pensions over population?

df <- df %>%
  mutate(doctors_population =  medici / population,
         hospitals_population = ospedali / population,
         pensions_population = pensions_invalidity / population,
         schools_population = (schools_primary + schools_middle) / population,
         students_schoosl = (students_primary + students_middle) / 
           (schools_primary + schools_middle),
         tax_burden = tax_revenue / 1000 / GDP)


#years of the elections
#df <- df %>%
#  filter(year %in% c(1962, 1967, 1971, 1978, 1982, 1986))


df <- df %>%
  mutate(gdp_pc = GDP / population)


#Always good to check what i did in the data sheet
#View(df)


#I plot the data 
library(ggplot2)

ggplot(df, aes(x = share_votes_DC, y = tax_burden)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Correlation between tax burden and DC vote share",
       x = "DC vote share",
       y = "Tax burden",
       color = "Year") +
  theme_bw()

#I run the OLS

library(fixest)

reg <- feols(tax_burden ~ share_votes_DC + gdp_pc | province + year, data = df)
summary(reg)


#I create my IV: share of civil marriages over total marriages
#Assumption: religious people marry in church and vote for the DC
df <- df %>%
  mutate(share_civil = civili / (civili + religiosi),
         share_religious = religiosi / (civili + religiosi))

#I make sure it explains something
iv <- feols(share_votes_DC ~ share_religious  | year, data = df)
summary(iv)

################################################################################
model_IV <- feols(tax_burden ~ 1  + factor(year) | share_votes_DC ~ share_religious,
                  cluster ~ province,
                  data = df)
summary(model_IV)
#I could add more controls next to the 1. The independent variable that
#I want to estimate through the instrument only goes after the |

################################################################################


#I print a nice summary
library(tidyverse)
library(modelsummary)

first_stage_model <- model_IV$iv_first_stage[[1]]
class(first_stage_model)  # should be "fixest"


msummary(
  list(
    "First Stage" = first_stage_model,
    "Second Stage" = model_IV
  ),
  coef_map = c(
    "share_religious" = "Share of civil marriages",
    "fit_share_votes_DC" = "Share of DC voting"
  ),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  statistic = "std.error",
  gof_omit = "AIC|BIC|Log.Lik"
)










