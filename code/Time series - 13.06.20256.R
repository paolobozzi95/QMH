

library(readxl)
setwd("C:/Users/paolo/Downloads")

data <-  read_excel("data.xlsx")
data2 <- read_excel("data2.xlsx")
data3 <- read_excel("data3.xlsx")
data4 <- read_excel("data4.xlsx")

#I merge the data
library(dplyr)
df <- data %>%
  left_join(data2, by = c("province", "year")) %>%
  left_join(data3, by = c("province", "year")) %>%
  left_join(data4, by = c("province", "year"))

#I create two variables: total number of students and total number of schools
df <- df %>%
  mutate(n_students = students_primary + students_middle,
         n_schools = schools_primary + schools_middle)

# Remove rows with NA in tax revenue, schools and students column
test <- df %>%
  filter(!is.na(tax_revenue) & !is.na(n_students) & !is.na(n_schools))

print(unique(test$year))

#interpolate linearly the missing values in GDP, 
#population, tax revenue, n_students, and n_schools

#Package for interpolations
library(zoo)
df <- df %>%
  mutate(GDP = na.approx(GDP, na.rm = FALSE),
         population = na.approx(population, na.rm = FALSE),
         tax_revenue = na.approx(tax_revenue, na.rm = FALSE),
         n_students = na.approx(n_students, na.rm = FALSE),
         n_schools = na.approx(n_schools, na.rm = FALSE))

df <- df %>%
  filter(!is.na(tax_revenue) & !is.na(n_students) & !is.na(n_schools))

print(unique(df$year))

#I aggregate my panel data into time series
library(tidyr)
df_ts <- df %>%
  group_by(year) %>%
  summarise(
    GDP = sum(GDP/1e6, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    tax_revenue = sum(tax_revenue/1e9, na.rm = TRUE),
    students = sum(n_students, na.rm = TRUE),
    schools = sum(n_schools, na.rm = TRUE)
  ) %>%
  ungroup()


head(df_ts)



#From here on, I work on df_ts

print(df_ts, n = 30)

#### ARIMA ####

#I want to predict the GDP for the years 1988 to 1990 using an ARIMA model
library(forecast)

#I tell R that this is a yearly time series starting in 1962 
gdp_ts <- ts(df_ts$GDP, start = c(1962), frequency = 1)

autoplot(gdp_ts) +
  labs(title = "GDP Time Series",
       x = "Year",
       y = "Log(GDP) in Billions lire") +
#  scale_y_continuous(labels = function(x) comma(10^x)) +
  theme_bw()

#R automatically fits the best ARIMA model  
gdp_fit <- auto.arima(gdp_ts)

#Here the model it used:
summary(gdp_fit)


#I forecast the values for 1988 to 1990
gdp_forecast <- forecast(gdp_fit, h = 3)

print(gdp_forecast)

# I plot the forecast
autoplot(gdp_forecast) +
  labs(title = "GDP Time Series with Forecast",
       x = "Year",
       y = "GDP") +
#  scale_y_continuous(labels = function(x) comma(10^x)) +
  theme_bw()



#### ARIMAX ####
library(scales)

#I want to study the impact of tax revenue on GDP
#we need an ARIMAX model

tax_revenue_ts <- ts(df_ts$tax_revenue, start = c(1962), frequency = 1)

autoplot(tax_revenue_ts) +
  labs(title = "Tax Revenue Time Series",
       x = "Year",
       y = "Tax Revenue in Millions lire") +
  theme_bw()

#Plot GDP and tax revenue in the same chart
autoplot(gdp_ts, series = "GDP", size = 1) +
  autolayer(tax_revenue_ts, series = "Tax Revenue", PI = FALSE, size = 1) +
  labs(title = "GDP and Tax Revenue Time Series",
       x = "Year",
       y = "Billions lire") +
  theme_bw() +
  guides(colour = guide_legend(title = "Series"))


#Fit an arimax model with tax revenue as exogenous variable (auto.arima)
library(forecast)
fit_arimax <- auto.arima(df_ts$GDP, xreg = df_ts$tax_revenue)
summary(fit_arimax)


#We do it manually like Romer and Romer (2010)
#i create a few lags (1, 2, and 3 years) for both GDP and tax revenue --> "AR" part
library(dplyr)
df_ts <- df_ts %>%
  arrange(year)

df_ts <- df_ts %>%
  mutate(
    GDP_lag1 = dplyr::lag(GDP, 1),
    GDP_lag2 = dplyr::lag(GDP, 2),
    GDP_lag3 = dplyr::lag(GDP, 3),
    tax_revenue_lag1 = dplyr::lag(tax_revenue, 1),
    tax_revenue_lag2 = dplyr::lag(tax_revenue, 2),
    tax_revenue_lag3 = dplyr::lag(tax_revenue, 3)
  )

#I calculate the deltas for each lag --> "I" part
df_ts <- df_ts %>%
  mutate(
    GDP_delta_lag1 = GDP - GDP_lag1,
    GDP_delta_lag2 = GDP_lag1 - GDP_lag2,
    GDP_delta_lag3 = GDP_lag2 - GDP_lag3,
    tax_revenue_delta_lag1 = tax_revenue - tax_revenue_lag1,
    tax_revenue_delta_lag2 = tax_revenue_lag1 - tax_revenue_lag2,
    tax_revenue_delta_lag3 = tax_revenue_lag2 - tax_revenue_lag3
  )

print(df_ts, n = 30)



#I fit an ARIMAX model with lags

model_1 <- lm(GDP_delta_lag1 ~ 
                     tax_revenue_delta_lag1 + 
                     tax_revenue_delta_lag2 + 
                     tax_revenue_delta_lag3,
                   data = df_ts)

summary(model_1)


model_2 <- lm(GDP_delta_lag1 ~ 
              tax_revenue_delta_lag1 + 
              tax_revenue_delta_lag2 +
              tax_revenue_delta_lag3 +
              GDP_delta_lag2 
              , data = df_ts)

summary(model_2)

#End


