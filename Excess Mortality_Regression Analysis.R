# The project for the research seminar "Markets & Straregies II"
# Research question: "What factors can explain excess mortality in Germany?"

### IN THIS FILE I PROCESS THE DATA BEFORE THE REGRESSION ANALYSIS OF PREPARED DATA BASED ON THE MODELS SELECTED

# As the main model I chose linear regression with interaction term and iv regression (can address multicollinearity issue)
# To check robustness I used lag values for COVID-19 cases and excess mortality => results are robust
# For the detailed information on empirical approach and results analysis please refer to the presentation

library(readr)
library(dplyr)
library(AER)
library(sandwich)
library(ggplot2)
library(ivpack)
library(reshape)
library("readxl")
library("mice")
library(tidyr)
library("dlookr")
library("forecast")
library(stargazer)

#### install package for exploratory data analysis

#install.packages("dlookr")
#install.packages("forecast")

########### UPLOAD THE REQUIRED DATA ##################

# Let's work primarily with:
 # -- Data by weeks, BL, no age
 # -- Data by weeks, counties, no age
 # -- Data by weeks, BL, age groups
 # -- Counties before after vaccination

bundesland_age <- read.csv("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATAFRAMES\\Data by weeks, BL, age groups.csv")
germany <- read.csv("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATAFRAMES\\Data by weeks, age groups Germany.csv")

md.pattern(bundesland_age)
md.pattern(germany)

########################### WORK WITH DATA BY WEEkS BL WITH AGE ###########################

colnames(bundesland_age)
summary(bundesland_age)
str(bundesland_age)


present <- bundesland_age %>% 
  select(year, week, Bundesland, age_group, exc_mortality, vacc_per_100_cum, cases_per_100, temperature, rain)
str(present)
summary(present)

present_germany <- germany %>% 
  select(year, week, Bundesland, age_group, exc_mortality, exc_mortality_demo, vacc_per_100_cum, cases_per_100, temperature, rain)

str(present_germany)
summary(present_germany)



############ PREPARE LAG DATA FOR CASES ########################

# 1 week lag
bundesland_age_lag_1_week <- bundesland_age %>%
  mutate(cases_per_100_lag_1_week = lag(cases_per_100, 1))

bundesland_age_lag_1_week <-bundesland_age_lag_1_week[!(bundesland_age_lag_1_week$year_week=="2020 10"),]

germany_lag_1_week <- germany %>%
  mutate(
    cases_per_100_lag_1_week = lag(cases_per_100, 1)
  )

germany_lag_1_week <-germany_lag_1_week[!(germany_lag_1_week$year_week=="2020 10"),]


# 2 weeks lag
bundesland_age_lag_2_weeks <- bundesland_age %>%
  mutate(cases_per_100_lag_2_weeks = lag(cases_per_100, 2))

bundesland_age_lag_2_weeks <-bundesland_age_lag_2_weeks[!(bundesland_age_lag_2_weeks$year_week=="2020 10"| bundesland_age_lag_2_weeks$year_week=="2020 11"),]

germany_lag_2_weeks <- germany %>%
  mutate(
    cases_per_100_lag_2_weeks = lag(cases_per_100, 2)
  )

germany_lag_2_weeks <-germany_lag_2_weeks[!(germany_lag_2_weeks$year_week=="2020 10" | germany_lag_2_weeks$year_week=="2020 11"),]

# 3 weeks lag

bundesland_age_lag_3_weeks <- bundesland_age %>%
  mutate(cases_per_100_lag_3_weeks = lag(cases_per_100, 3))

bundesland_age_lag_3_weeks <-bundesland_age_lag_3_weeks[!(bundesland_age_lag_3_weeks$year_week=="2020 10"| bundesland_age_lag_3_weeks$year_week=="2020 11"
                                                          | bundesland_age_lag_3_weeks$year_week=="2020 12"),]
germany_lag_3_weeks <- germany %>%
  mutate(
    cases_per_100_lag_3_weeks = lag(cases_per_100, 3)
  )

germany_lag_3_weeks <-germany_lag_3_weeks[!(germany_lag_3_weeks$year_week=="2020 10" | germany_lag_3_weeks$year_week=="2020 11"
                                            | germany_lag_3_weeks$year_week=="2020 12"),]

# 4 weeks lag

bundesland_age_lag_4_weeks <- bundesland_age %>%
  mutate(cases_per_100_lag_4_weeks = lag(cases_per_100, 4))

bundesland_age_lag_4_weeks <-bundesland_age_lag_4_weeks[!(bundesland_age_lag_4_weeks$year_week=="2020 10"| bundesland_age_lag_4_weeks$year_week=="2020 11"
                                                          | bundesland_age_lag_4_weeks$year_week=="2020 12" | bundesland_age_lag_4_weeks$year_week=="2020 13"),]

germany_lag_4_weeks <- germany %>%
  mutate(
    cases_per_100_lag_4_weeks = lag(cases_per_100, 4)
  )

germany_lag_4_weeks <-germany_lag_4_weeks[!(germany_lag_4_weeks$year_week=="2020 10" | germany_lag_4_weeks$year_week=="2020 11"
                                            | germany_lag_4_weeks$year_week=="2020 12" | germany_lag_4_weeks$year_week=="2020 13"),]



############# REGRESSIONS ################


################ DATA BUNDESLANDS WEEKS AGE ##################

# LM on all variables without lags

reg1 <- lm(exc_mortality ~ vacc_per_100_cum + cases_per_100 + age_group + temperature + rain + cases_per_100*age_group + BW +BY + BE + BB + HB + HH + HE + MV + NI + NW + RP + SL + SN + 
               ST + SH + TH, data = bundesland_age)
summary(reg1)


#  2SLS on cases_per_100 ~ temperature + rain 

reg2_TSLS = ivreg(exc_mortality ~ vacc_per_100_cum + cases_per_100 + age_group + BW +BY + BE + BB + HB + HH + HE + MV + NI + NW + RP + SL + SN + 
                     ST + SH + TH| temperature + rain + vacc_per_100_cum + age_group + BW +BY + BE + BB + HB + HH + HE + MV + NI + NW + RP + SL + SN + 
                     ST + SH + TH, data = bundesland_age)
summary(reg2_TSLS,vcov = sandwich, diagnostics = TRUE)


# REGRESSIONS with lags
# regression with 1 week lag

spec20 <- lm(exc_mortality ~ vacc_per_100_cum + cases_per_100_lag_1_week + age_group + temperature + rain + cases_per_100_lag_1_week*age_group + BW +BY + BE + BB + HB + HH + HE + MV + NI + NW + RP + SL + SN + 
               ST + SH + TH, data = bundesland_age_lag_1_week)
summary(spec20)

# regression with 2 weeks lag

spec21 <- lm(exc_mortality ~ vacc_per_100_cum + cases_per_100_lag_2_weeks + age_group + temperature + rain + cases_per_100_lag_2_weeks*age_group + BW +BY + BE + BB + HB + HH + HE + MV + NI + NW + RP + SL + SN + 
               ST + SH + TH, data = bundesland_age_lag_2_weeks)
summary(spec21)

# regression with 3 weeks lag

spec22 <- lm(exc_mortality ~ vacc_per_100_cum + cases_per_100_lag_3_weeks + age_group + temperature + rain + cases_per_100_lag_3_weeks*age_group + BW +BY + BE + BB + HB + HH + HE + MV + NI + NW + RP + SL + SN + 
               ST + SH + TH, data = bundesland_age_lag_3_weeks)
summary(spec22)

# regression with 4 weeks lag
spec23 <- lm(exc_mortality ~ vacc_per_100_cum + cases_per_100_lag_4_weeks + age_group + temperature + rain + cases_per_100_lag_4_weeks*age_group + BW +BY + BE + BB + HB + HH + HE + MV + NI + NW + RP + SL + SN + 
               ST + SH + TH, data = bundesland_age_lag_4_weeks)
summary(spec23)




######## AND LASTLY: SPECIFICATION THAT ACCOUNTS FOR DEMOGRAPHIC: GERMANY LEVEL ########### 


# NO ACCOUNT FOR DEMOGRAPHIC
reg3 <- lm(exc_mortality  ~ vacc_per_100_cum + cases_per_100 + age_group + cases_per_100*age_group + temperature + rain, data = germany)
summary(reg3) 

# ACCOUNT FOR DEMOGRAPHIC
reg4 <- lm(exc_mortality_demo  ~ vacc_per_100_cum + cases_per_100 + age_group + cases_per_100*age_group + temperature + rain, data = germany)
summary(reg4)

# REGRESSIONS with lags without demo
# regression with 1 week lag

spec24 <- lm(exc_mortality ~ vacc_per_100_cum + cases_per_100_lag_1_week + age_group + temperature + rain + cases_per_100_lag_1_week*age_group, data = germany_lag_1_week)
summary(spec24)

# regression with 2 weeks lag

spec25 <- lm(exc_mortality ~ vacc_per_100_cum + cases_per_100_lag_2_weeks + age_group + temperature + rain + cases_per_100_lag_2_weeks*age_group, data = germany_lag_2_weeks)
summary(spec25)

# regression with 3 weeks lag

spec26 <- lm(exc_mortality ~ vacc_per_100_cum + cases_per_100_lag_3_weeks + age_group + temperature + rain + cases_per_100_lag_3_weeks*age_group, data = germany_lag_3_weeks)
summary(spec26)

# regression with 4 weeks lag
spec27 <- lm(exc_mortality ~ vacc_per_100_cum + cases_per_100_lag_4_weeks + age_group + temperature + rain + cases_per_100_lag_4_weeks*age_group, data = germany_lag_4_weeks)
summary(spec27)

stargazer(reg3, spec24, spec25, spec26, spec27, type="text")

# REGRESSIONS with lags with demo
# regression with 1 week lag

spec28 <- lm(exc_mortality_demo ~ vacc_per_100_cum + cases_per_100_lag_1_week + age_group + temperature + rain + cases_per_100_lag_1_week*age_group, data = germany_lag_1_week)
summary(spec28)

# regression with 2 weeks lag

spec29 <- lm(exc_mortality_demo ~ vacc_per_100_cum + cases_per_100_lag_2_weeks + age_group + temperature + rain + cases_per_100_lag_2_weeks*age_group, data = germany_lag_2_weeks)
summary(spec29)

# regression with 3 weeks lag

spec30 <- lm(exc_mortality_demo ~ vacc_per_100_cum + cases_per_100_lag_3_weeks + age_group + temperature + rain + cases_per_100_lag_3_weeks*age_group, data = germany_lag_3_weeks)
summary(spec30)

# regression with 4 weeks lag
spec31 <- lm(exc_mortality_demo ~ vacc_per_100_cum + cases_per_100_lag_4_weeks + age_group + temperature + rain + cases_per_100_lag_4_weeks*age_group, data = germany_lag_4_weeks)
summary(spec31)

stargazer(reg4, spec28, spec29, spec30, spec31, type="text")


####### SUMMARY OF ALL THE MODELS ################


stargazer(reg1, reg3, reg4,  type="text")
stargazer(reg1, spec20, spec21, spec22, spec23,  type="text")



### INTERPRETATION ####

bundesland_age <- bundesland_age %>% 
  mutate(
    old = ifelse(age_group == '60+', 1, 0),
    cases_age = cases_per_100*old
    )

sd(bundesland_age$vacc_per_100_cum)
sd(bundesland_age$cases_per_100)
sd(bundesland_age$old)
sd(bundesland_age$temperature)
sd(bundesland_age$rain)
sd(bundesland_age$cases_age)


# For the detailed information on empirical approach and results analysis please refer to the presentation



