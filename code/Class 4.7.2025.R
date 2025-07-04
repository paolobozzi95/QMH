# Week 12 Assignment - Maya Kurter

# Last week, we examined the relationship between the tax burden and 
# the vote share for the Christian Democratic Party (DC).

# First, we performed a basic correlation and regression analysis between tax burden 
# and DC vote share. Then, we used the Instrumental Variables (IV) approach, employing 
# “share religious” (the proportion of religious marriages) as an instrument for DC 
# vote share in order to test for causality.

# We found a negative relationship: higher DC vote shares were associated with 
# lower tax burdens. However, it was noted that “share religious” may not be a 
# valid instrument, so the results should be interpreted with caution.

# This week, we reverse the perspective:
# Instead of analyzing whether DC voting affects the tax burden,
# We test whether changes in the tax burden (especially after the 1973 reform) 
# affected DC vote shares, and whether this effect was different in the North 
# (treatment group) compared to the South (control group), 
# using a difference-in-differences (diff-in-diff) approach.

# The codes below uses a diff-in-diff approach to test whether the 1973 tax reform 
# had a differential impact on DC vote shares in North vs South Italy.

# 1. Load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(fixest)

setwd("C:/Users/paolo/Downloads")

#load data1, data2, data3, data4, and data5
data1 <- read_excel("data.xlsx")
data2 <- read_excel("data2.xlsx")
data3 <- read_excel("data3.xlsx")
data4 <- read_excel("data4.xlsx")
data5 <- read_excel("data5.xlsx")

# 2. Merge all datasets
# We merge the datasets by "province" and "year" to create a comprehensive panel.
panel <- data4 %>%
  left_join(data2, by = c("province", "year")) %>%
  left_join(data3, by = c("province", "year")) %>%
  left_join(data5, by = c("province", "year"))

# 3. Calculate the tax burden and its lag
# We compute tax burden (tax revenue divided by GDP) and its lagged value 
# for each province.
panel <- panel %>%
  arrange(province, year) %>%
  mutate(
    tax_burden = tax_revenue / GDP,
    tax_burden_lag = lag(tax_burden),
    share_votes_DC_lag = lag(share_votes_DC)
  )

# 4. Filter for election years only
# We restrict the analysis to official election years.
election_years <- c(1963, 1968, 1972, 1979, 1983, 1987)
panel_elec <- panel %>% filter(year %in% election_years)

# 5. Calculate delta (change) variables for tax burden and DC votes
# For each province, we calculate the change in tax burden and
# change in DC vote share compared to the previous election.

panel_elec <- panel_elec %>%
  group_by(province) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    delta_tax_burden = tax_burden - lag(tax_burden),
    delta_share_votes_DC = share_votes_DC - lag(share_votes_DC)
  ) %>%
  ungroup()

df <- panel_elec

# 6. Define treatment (North) and post-1973 period
# We define "north" as the treatment group, "post_1973" as the 
# period after the tax reform.

panel_elec <- panel_elec %>%
  mutate(
    post_1973 = year >= 1979  # 1979 and later are post-reform
  )

# 7. Diff-in-Diff regression (Difference-in-Differences)
# We estimate a diff-in-diff model to assess whether the change in tax burden
# affected DC vote share differently in the North and South after the reform.

did_model <- feols(
  delta_share_votes_DC ~ delta_tax_burden + i(year, north, ref = 1972) | province, 
  data = panel_elec
)

iplot(did_model)

print(summary(did_model))

# 8. Plotting the pattern of coefficients
# We plot the association between the effect of tax burden changes 
# on DC votes, before and after the reform, especially in the North.

iplot(did_model)

# 9. Findings: Interpretation of the Diff-in-Diff Plot

#The plot illustrates the estimated effect of changes in tax burden on changes 
# in the DC vote share for the North, before and after the 1973 tax reform 
#(indicated by the vertical dashed line).

# Before 1973, the estimated effects are close to zero and not statistically 
# significant, as indicated by the confidence intervals crossing zero.

# After 1973, especially in 1979 and 1983, the point estimates become negative 
# and larger in absolute value, with confidence intervals suggesting a potential 
# decline in DC vote share in response to tax burden increases. 

# However, the confidence intervals are wide, so statistical significance is limited.

# Overall, the plot suggests that after the 1973 tax reform, there may have 
# been a more negative reaction in the North: as tax burden increased, 
# DC vote share tended to decrease. However, due to the large confidence intervals, 
# the evidence is suggestive rather than conclusive.

# In conclusion, while the results are not fully conclusive, there is suggestive 
# evidence that the 1973 tax reform triggered a larger negative shift in DC vote 
# shares in the North compared to the South.

# End of Assignment




#### How I would do it ####


head(df)

#I create the interaction myself:
df$delta_tax_burden_north <- df$delta_tax_burden * df$north

did_model <- feols(
  delta_share_votes_DC ~  
    
    delta_tax_burden + 
    #    i(year, delta_tax_burden, ref = 1972) +
    i(year, delta_tax_burden_north, ref=1972)
  
  | province + year,
  data = df)

summary(did_model)

names(coef(did_model))

iplot(did_model)

