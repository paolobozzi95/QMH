





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

#### 5 ####

#I plot the data 
library(ggplot2)

p <- ggplot(df, aes(x = share_votes_DC, y = tax_burden)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Correlation between tax burden and DC vote share",
       x = "DC vote share",
       y = "Tax burden",
       color = "Year") +
  theme_bw()

print(p)

#I run the OLS

library(fixest)

reg <- feols(tax_burden ~ share_votes_DC + gdp_pc | province + year, data = df)
summary(reg)



#### 6 ####

#I create my IV: share of civil marriages over total marriages
#Assumption: religious people marry in church and vote for the DC
df <- df %>%
  mutate(share_civil = civili / (civili + religiosi),
         share_religious = religiosi / (civili + religiosi))


#### 7 ####

reg_fit <- feols(share_votes_DC ~ share_religious, data = df)

summary(reg_fit)

#save the fitted values of the first stage
fit_share_votes_DC <- fitted(reg_fit)

fit_share_votes_DC

#remove obs with share_religious = NA
df <- df %>%
  filter(!is.na(share_religious),
         !is.na(share_votes_DC))

#merge the fitted values in the dataset
df <- df %>%
  mutate(fit_share_votes_DC = fit_share_votes_DC)

#plot both share votes DC and fitted share votes DC
# against share religious

f <- ggplot(df, aes(x = share_religious)) +
  geom_point(aes(y = share_votes_DC), color = "blue", alpha = 0.5) +
  geom_point(aes(y = fit_share_votes_DC), color = "red", alpha = 0.5) +
  labs(title = "Share of votes for DC vs Fitted Share of votes for DC",
       x = "Share of religious marriages",
       y = "Share of votes for DC") +
  theme_bw()

print(f)

#the outlier is the province of Oristano 1982. I checked with the source and it seems correct
#since however the value is highly unplausible, I remove it

df <- df %>%
  filter(!(province == "Oristano" & year == 1982))

#And run everything again:

reg <- feols(tax_burden ~ share_votes_DC + gdp_pc | province + year, data = df)
summary(reg)


reg_fit <- feols(share_votes_DC ~ share_religious, cluster = "province",
                   data = df)

summary(reg_fit)

fit_share_votes_DC <- fitted(reg_fit)

df <- df %>%
  filter(!is.na(share_religious),
         !is.na(share_votes_DC))

df <- df %>%
  mutate(fit_share_votes_DC = fit_share_votes_DC)


ggplot(df, aes(x = share_religious)) +
  geom_point(aes(y = share_votes_DC, color = "Share of votes for DC"), alpha = 0.5) +
  geom_point(aes(y = fit_share_votes_DC, color = "Fitted Share of votes for DC"), alpha = 0.5) +
  labs(title = "Share of votes for DC vs Fitted Share of votes for DC",
       x = "Share of religious marriages",
       y = "Share of votes for DC") +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw() +
  theme(legend.title = element_blank())



#### 8 ####

#I make sure it explains something - Is it a good IV??
iv <- feols(share_votes_DC ~ share_religious | province + year, data = df)
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










