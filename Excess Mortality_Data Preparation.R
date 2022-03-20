# The project for the research seminar "Markets & Straregies II"
# Research question: "What factors can explain excess mortality in Germany?"
# Task:
# - get familiar with excess mortality calculation methodology
# - hypothesize what factors could have affected excess mortality except COVID-19
# - perform data collection, data processing, data investigation
# - apply econometric tools to build suitable regression framework
# - make conclusions 


### IN THIS FILE I PROCESS THE DATA BEFORE THE REGRESSION ANALYSIS  

# upload libraries
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


# UPLOAD THE DATA
# daily data by COVID-19 cased by county
urlfile_cases ="https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rl-crowdsource-by-ags.csv"
cases_by_county <- read_csv(url(urlfile_cases))
head(cases_by_county)

# daily data by COVID-19 vaccinations by county
urlfile_vaccinations = "https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv"
vaccinations_by_county <- read_csv(url(urlfile_vaccinations))
head(vaccinations_by_county)

# daily data by COVID-19 deaths by county
urlfile_deaths = "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/deaths-rki-by-ags.csv"
deaths_by_county <- read_csv(url(urlfile_deaths))
head(deaths_by_county)

### DATA PROCESSING ###

# uploaded data are not data frame, need to convert to data.frame
class(cases_by_county) 
cases_by_county <- as.data.frame(cases_by_county)
class(cases_by_county)

deaths_by_county <- as.data.frame(deaths_by_county)
class(deaths_by_county)

vaccinations_by_county<-as.data.frame(vaccinations_by_county)

cases_by_county_long <- melt(cases_by_county, id.vars = "time_iso8601")
deaths_by_county_long <-melt(deaths_by_county, id.vars = "time_iso8601")


# Change data on vaccinations -> before we removed Impfschutz which is crucial for 
# determining the number of vaccinated individuals

#vaccinations_by_county_without_age <- vaccinations_by_county %>% group_by(Impfdatum, LandkreisId_Impfort, Anzahl)


vaccinations_by_county_without_age <- vaccinations_by_county %>%
  filter(Impfschutz == 2)%>% #only fully vaccinated individuals
  group_by(Impfdatum, LandkreisId_Impfort) %>%
  summarize(
    vaccinations = sum(Anzahl)
  )

vaccinations_total <- vaccinations_by_county %>%
  group_by(Impfschutz) %>%
  summarize(
    vaccinations = sum(Anzahl)
  )%>%
  mutate( percentage = vaccinations/83155031) # percentage of population 2020


head(vaccinations_by_county_without_age)

str(cases_by_county_long)
str(deaths_by_county_long)
str(vaccinations_by_county_without_age)

# Problem is that cases_by_county_long and deaths_by_county_long show only cumulative cases and deaths
# I want to separate it by days 

cases_by_county_long <- cases_by_county_long %>%
  group_by(variable)%>%
  mutate(cases = value - lag(value, default = 0))

head(cases_by_county_long)

deaths_by_county_long <- deaths_by_county_long %>%
  group_by(variable)%>%
  mutate(deaths = value - lag(value, default = 0))

#deaths_by_county_long <- select(deaths_by_county_long, -cases) accidentally created a column with a wrong name
head(deaths_by_county_long)

# get column names
colnames(cases_by_county_long)

# rename the columns so they look nicer
cases_by_county_long <- rename(cases_by_county_long, c(value = "cumulative_cases",  time_iso8601 = "date", variable = "county"))
deaths_by_county_long <- rename(deaths_by_county_long, c(value = "cumulative_deaths",  time_iso8601 = "date", variable = "county"))
vaccinations_by_county_without_age <- rename(vaccinations_by_county_without_age, c(Impfdatum = "date", LandkreisId_Impfort = "county"))

# change cases and deaths data format so it is compatible vaccinations format
# split the date into years and weeks so we can work with different date formats 

class(cases_by_county_long$date)

cases_by_county_long <- cases_by_county_long %>% 
  mutate(
    date = as.Date(date),
    week = format(as.Date(date), "%W"),
    year = format(as.Date(date), "%Y"),
    month = format(as.Date(date), "%m"),
    abb_month = format(as.Date(date), "%b"),
    county = as.character(county),
    #county = paste("0", county, sep="") # concetenate with 0 so it is easier to match
  )

# Paste 0 if number of letter is less then 5
cases_by_county_long$county[nchar(cases_by_county_long$county) < 5] <- paste("0", cases_by_county_long$county[nchar(cases_by_county_long$county) < 5], sep="")

class(cases_by_county_long$date)
class(cases_by_county_long$county)

deaths_by_county_long <- deaths_by_county_long %>% 
  mutate(
    date = as.Date(date),
    week = format(as.Date(date), "%W"),
    year = format(as.Date(date), "%Y"),
    month = format(as.Date(date), "%m"),
    abb_month = format(as.Date(date), "%b"),
    county = as.character(county)
  )


# Paste 0 if number of letter is less then 5
deaths_by_county_long$county[nchar(deaths_by_county_long$county) < 5] <- paste("0", deaths_by_county_long$county[nchar(deaths_by_county_long$county) < 5], sep="")



class(deaths_by_county_long$date)
class(deaths_by_county_long$county)

vaccinations_by_county_without_age <- vaccinations_by_county_without_age %>% 
  mutate(
    week = format(as.Date(date), "%W"),
    year = format(as.Date(date), "%Y"),
    month = format(as.Date(date), "%m"),
    abb_month = format(as.Date(date), "%b"),
  )

class(vaccinations_by_county_without_age$date)
class(vaccinations_by_county_without_age$county)


# CHECK ALL THE DATA FOR COMPATIBILITY

# Vaccination data
length(unique(vaccinations_by_county_without_age$county))
unique(vaccinations_by_county_without_age$county)
nchar(unique(vaccinations_by_county_without_age$county))

# Deaths data
length(unique(deaths_by_county_long$county))
unique(deaths_by_county_long$county)
nchar(unique(deaths_by_county_long$county))

# Cases data
length(unique(cases_by_county_long$county))
unique(cases_by_county_long$county)
nchar(unique(cases_by_county_long$county))

# RESULT: mismatch of # of counties

# Finally join all 3 data frames together => 

data <- full_join(cases_by_county_long, deaths_by_county_long)
data <- full_join(data,vaccinations_by_county_without_age)


# SAVE DATA to CSV so we don't need to connect to server git hub all the time 

write.csv(data,"C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Vaccination, Deaths, Cases by days.csv", row.names = FALSE)

# READ saved data

dat <- read.csv("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Vaccination, Deaths, Cases by days.csv")
head(dat)


### ENRICHING THE DATA
# Assigning the name of county, bundesland and population density to the DATA 

counties_codes <- read_excel("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Counties matching.xlsx")
head(counties_codes) #data as of 31.12.2020
str(counties_codes)


# rename `Schlüssel-nummer` for "county"
class(counties_codes$`Schlüssel-nummer`)
counties_codes <- rename(counties_codes, c(`Schlüssel-nummer` = "county"))

dat <- full_join(counties_codes, dat, by = 'county')


# adding data on bundesland
bundesland_county <- read_excel("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Bundesland by county.xlsx")
head(bundesland_county) #data as of 31.12.2020
dat <- full_join(bundesland_county, dat, by = 'county')

bundesland_short_code <-read_excel("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Bundesland_short_code.xlsx")
bundesland_short_code
dat <- full_join(dat, bundesland_short_code)

unique(dat$Bundesland)

# Rename the data
dat <- rename(dat, c("Regionale Bezeichnung" = "regional name", "Kreisfreie Stadt"  = "independent city", 
                     "Fläche \r\nin km2" = 'square km2', "insgesamt" = "total_population",
                     "männlich" = "male", "weiblich" = "female", "je km2" = "population_density per km2"))

bundesland_population <- read.csv("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Population by bundesland.csv")
bundesland_population <- rename(bundesland_population, c("ï..Bundesland" = "Bundesland", "Total" = "Bundesland_population" ))
bundesland_population$Bundesland[2] <- "Baden-Württemberg"
bundesland_population$Bundesland[17] <- "Thüringen"
unique(bundesland_population$Bundesland)

dat <-inner_join(dat, bundesland_population)


### HANDLE NA AND MISSING VALUES ### 

dat$NUTS3 <- NULL
unique(dat$county)

# Change all characters NA to real NA
dat[dat=='NA'] <- NA

# Delete suspicious values for counties + the date 28.12.2021 since there is no data
# + the date 2020-03-02 since there are no data on cases
# we have to delete the whole county 16056 because it doesn't have any data on cases ==> CHANGED DECISION no need for | dat$county == "16056"
dat <-dat[!(dat$county=="sum_deaths" | dat$county=="17000" | dat$county=="u" | dat$county=="sum_cases" 
            | dat$county=="11001" | dat$county=="11002" | dat$county=="11003" | dat$county=="11004"
            | dat$county=="11005" | dat$county=="11006" | dat$county=="11007" | dat$county=="11008"
            | dat$county=="11009" | dat$county=="11010" | dat$county=="11011" | dat$county=="11012" | dat$date == "2021-12-28"
            | dat$date == "2020-03-02" | dat$date == "2021-12-27"),]

# check the data
missing_data <- dat %>%
  mutate( vaccinations = coalesce(vaccinations, 0))%>% # replace vaccinations with zero
  mutate( cases = coalesce(cases, 0))%>% # replace NA cases with zero for county 16056 on order not to lose data on population 
  mutate( cumulative_cases = coalesce(cumulative_cases, 0))%>% # replace NA cases with zero for county 16056 on order not to lose data on population 
  filter(!complete.cases(.))#%>%
  #group_by(county)%>%
  #summarise(
    #cases = n(), # at this point it is clear that there are no data on county level for the county 16056 Eisenach Stadt
    # I believe that converting to 0 will not affect the total sample since n of vaccinations is 79277, deaths are 78 
    #deaths = sum(deaths),
    #vaccinations = sum(vaccinations)
    #) ===> AFTER MANY MANUPULATIONS NO NA


# another package for overview of missing data

md.pattern(missing_data) # finalized to the absence of missing data

# after trying we can apply the same transformation to our data to get rid of NA

dat <- dat %>%
  mutate( vaccinations = coalesce(vaccinations, 0))%>% # replace vaccinations with zero
  mutate( cases = coalesce(cases, 0))%>%  # replace NA cases with zero for county 16056 on order not to lose data on population
  mutate( cumulative_cases = coalesce(cumulative_cases, 0)) 

md.pattern(dat) # 



######### ________________________________ ##########################
# DATA PREPROSSESING FINISHED

# ADD COLUMN Cumulative vaccination
cumulative_vaccinations <- dat %>%
  arrange(county, date)%>%
  group_by(county)%>%
  summarize(
    cumulative_vaccinations = cumsum(vaccinations)
  )

length(cumulative_vaccinations$cumulative_vaccinations)
length(dat$vaccinations)

dat <- dat %>% arrange(county, date) # make sure 2 df are similarly arranged
dat_f <-  bind_cols(dat, cumulative_vaccinations) # check on new data frame first

check <- dat_f %>% filter(dat_f$county...2 != dat_f$county...22) # check if there are any variables not equal to in both columns


# APPLY to the main data frame
dat <- bind_cols(dat, cumulative_vaccinations)
dat <- rename(dat, c("county...2" = "county", "cumulative_vaccinations...22" = "cumulative_vaccinations"))
dat$cumulative_vaccinations...28 <- NULL
dat$county...27 <- NULL
dat$county...24 <- NULL
dat$cumulative_vaccinations...26 <- NULL
dat$cumulative_vaccinations...24 <- NULL
dat$county...23 <- NULL
dat$county...25 <- NULL

colnames(dat)

# before running this need to get rid of NA ==> DONE
# the data indicated that in some counties more that 100% of the population is vaccinated 
# => wrong calculation because of omitted column in the data frame => FIXED

county_total <- dat %>% 
  group_by(county, total_population)%>% 
  summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    vaccinations = sum(vaccinations),
  ) %>% 
  mutate(percentage_vaccinated = round(vaccinations/total_population*100, 1))


# Critical check if the vaccination numbers by Bundeslands match the Robert-Koch Institute 
# => Result: match

bundesland_total <- dat %>% 
  group_by(Bundesland)%>% 
  summarise(
    population = sum(total_population)/664, # not correct, need to take population only for one date ==> 664 observations
    cases = sum(cases),
    deaths = sum(deaths),
    vaccinations = sum(vaccinations),
  ) %>% 
  mutate(percentage_vaccinated = round(vaccinations/population*100, digits = 1)) # => everything matches the official data except Schleswig-Holstein
# difference: 75.3 official data as of 29.12.2021 vs. 76.7 calculated data as of 27.12.2021 => not crucial

# Download data on Excessive mortality (overall, not COVID-19)

excessive_mortality <- read_excel("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Excessive mortality 2020-2021 by weeks BL.xlsx", sheet = 1)
head(excessive_mortality)
colnames(excessive_mortality)

# reshape the data frame
excessive_mortality <- gather(excessive_mortality, "Bundesland", "Excessive_mortality", 3:18)
head(excessive_mortality)

# make sure that keys for bundesland match with previous data frame
key1 <- data.frame(unique(dat$Bundesland)) 
key1 <- rename(key1, c("unique.dat.Bundesland." = "Bundesland"))
colnames(key2)
key2 <- data.frame(unique(excessive_mortality$Bundesland))
key2 <- rename(key2, c("unique.excessive_mortality.Bundesland." = "Bundesland"))
keys <- inner_join(key1,key2) 
keys # => full match of bundeslands names

# CREATE data frame by bundeslands that we will use for analysis
colnames(dat)
bundesland_weeks <- dat %>% 
  group_by(Bundesland, Bundesland_abb, year, week, Bundesland_population)%>% 
  summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    vaccinations = sum(vaccinations),
  ) %>% 
  mutate(percentage_vaccinated = round(vaccinations/Bundesland_population*100, digits = 1))  # => everything matches the official data except Schleswig-Holstein
  #mutate(cumulative_vaccination = cumsum(vaccinations))%>%  # cumulative number of vaccinated
  #mutate(cumulative_vaccination_percentage = round(cumulative_vaccination/Bundesland_population*100, digits = 1)) # cumulative number of vaccinated


# JOIN data frames by bundeslands and excessive mortality
bundesland_weeks <- full_join(bundesland_weeks,excessive_mortality)
unique(bundesland_weeks$Bundesland)

# somehow week with index 0 appeared (maybe it is not full week...) => delete
bundesland_weeks <-bundesland_weeks[!(bundesland_weeks$week=="0"),]

# join with the mortality data by week
# Download data on mortality
mortality <- read.csv("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Mortality by week 2020-2021.csv")
head(mortality)

#rename columns 
colnames(mortality)
unique(dat$Bundesland)
mortality <- rename(mortality, c("ï..year" = "year", "Schleswig.Holstein" = "Schleswig-Holstein", 
                                 "Nordrhein.Westfalen" = "Nordrhein-Westfalen","Rheinland.Pfalz" = "Rheinland-Pfalz",
                                 "Baden.WÃ.rttemberg" = "Baden-Württemberg", "Mecklenburg.Vorpommern" = "Mecklenburg-Vorpommern",
                                 "Sachsen.Anhalt" = "Sachsen-Anhalt", "ThÃ.ringen" = "Thüringen"))
colnames(mortality)

# reshape the data frame
mortality <- gather(mortality, "Bundesland", "Mortality", 3:18)
head(mortality)

mortality <- rename(mortality, c("Excessive_mortality" = "Mortality"))


# JOIN data frames by bundeslands and mortality
bundesland_weeks <- full_join(bundesland_weeks,mortality)
unique(bundesland_weeks$Bundesland)

######### ________________________________ ##########################
################ PREPARE DATA FOR BASIC REGRESSION ##############

# ADD excessive mortality in pp

bundesland_weeks_na <- inner_join(bundesland_weeks_na,excessive_mortality)


# ADD population density
population_density <- read_excel("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Density by bundesland.xlsx")
head(population_density)
colnames(population_density)
# drop unnesessary columns

population_density$Total <- NULL
population_density$Square <- NULL

# join data frames

bundesland_weeks_na <- inner_join(bundesland_weeks_na,population_density)


# cumulative vaccinations 
bundesland_weeks_na_cum <- bundesland_weeks_na %>%
  arrange(year,week)%>%
  group_by(Bundesland)%>%
  summarize(cumulative_vaccinations = cumsum(vaccinations)) # %>%
  #mutate( cumulative_vaccinations = coalesce(cumulative_vaccinations, 0))  # replace NA cases with zero for county 16056 on order not to lose data on population

length(bundesland_weeks_na_cum$cumulative_vaccinations)
length(bundesland_weeks_na$vaccinations)

# combine 2 data frames
bundesland_weeks_na <- bind_cols(bundesland_weeks_na, bundesland_weeks_na_cum)
colnames(bundesland_weeks_na)
bundesland_weeks_na <- rename(bundesland_weeks_na, c("Bundesland...1" = "Bundesland"))
bundesland_weeks_na$Bundesland...12 <- NULL

bundesland_weeks_na <- bundesland_weeks_na %>% mutate(cum_perc_vaccinated = cumulative_vaccinations/Bundesland_population*100)
bundesland_weeks_na$`1` <- NULL #ready to go for regression

### ==> Data on Excessive Mortality without split by the age groups


########### PREPARATION OF THE DATA BY THE AGE GROUPS ###################

# join with bundesland 

vaccinations_age <- rename(vaccinations_by_county, c("LandkreisId_Impfort" = "county", "Altersgruppe" = "age_group"))
head(vaccinations_age)
vaccinations_age <- inner_join(vaccinations_age,counties_codes)
vaccinations_age <- inner_join(bundesland_county, vaccinations_age)

colnames(vaccinations_age)

# Group data by bundesland and weeks
vaccinations_age_grouped <- vaccinations_age %>%
  filter(Impfschutz == 2)%>% #only fully vaccinated individuals
  mutate(
    week = format(as.Date(Impfdatum), "%W"),
    year = format(as.Date(Impfdatum), "%Y"),
    month = format(as.Date(Impfdatum), "%m"),
    abb_month = format(as.Date(Impfdatum), "%b"),
  )%>%
  group_by(Bundesland, year, week, age_group)%>%
  summarize(
    vaccinations = sum(Anzahl)
  )%>%
  mutate(vaccination_sum = sum(vaccinations))%>%
  mutate(weight = vaccinations/vaccination_sum)%>%
  mutate(vaccination_weighted = weighted.mean(vaccinations, weight))%>%
  select(Bundesland, year, week,vaccination_weighted)%>% # remove age groups since average is already calculated
  summarize(vaccination_weighted = mean(vaccination_weighted))%>%
  arrange(Bundesland, year, week)


# check values 
unique(vaccinations_age_grouped$Bundesland)
unique(vaccinations_age_grouped$week)
unique(vaccinations_age_grouped$year)

# remove week 00
vaccinations_age_grouped <-vaccinations_age_grouped[!(vaccinations_age_grouped$week=="00"),]
unique(vaccinations_age_grouped$week)


# cumulative vaccinations 
vaccinations_age_grouped_cum <- vaccinations_age_grouped %>%
  arrange(Bundesland, year,week)%>%
  group_by(Bundesland)%>%
  summarize(cumulative_vaccinations = cumsum(vaccination_weighted)) # %>%
#mutate( cumulative_vaccinations = coalesce(cumulative_vaccinations, 0))  # replace NA cases with zero for county 16056 on order not to lose data on population


length(vaccinations_age_grouped_cum$cumulative_vaccinations)
length(vaccinations_age_grouped$vaccination_weighted)

# combine 2 data frames
vaccinations_age_grouped <- bind_cols(vaccinations_age_grouped, vaccinations_age_grouped_cum)
colnames(vaccinations_age_grouped)
vaccinations_age_grouped <- rename(vaccinations_age_grouped, c("Bundesland...1" = "Bundesland", "cumulative_vaccinations...6" = "cumulative_vaccinations"))
vaccinations_age_grouped$Bundesland...5 <- NULL
vaccinations_age_grouped$Bundesland...7 <- NULL
vaccinations_age_grouped$cumulative_vaccinations...8 <- NULL

colnames(vaccinations_age_grouped) # => vaccinations are ready for regression

# Step 2: prepare the data for excessive mortality


# join with the mortality data by week

# Download data on mortality
mortality_weighted <- read_excel("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Weigted by age Excessive mortality 2020-2021 by weeks BL.xlsx")
head(mortality_weighted)

#rename columns 
str(colnames(mortality_weighted))
unique(mortality_weighted$Bundesland)
colnames(mortality_weighted)

# reshape the data frame
mortality_weighted <- gather(mortality_weighted, "week", "mortality_weighted", 3:54)
head(mortality_weighted) # => ready to be joined with vaccinations

# join mortality and vaccination data

mortality_weighted$year <- as.character(mortality_weighted$year)

bundesland_weighted <- full_join(vaccinations_age_grouped, mortality_weighted)

# add population size
bundesland_weighted <-inner_join(bundesland_weighted, bundesland_population)

# add population density
bundesland_weighted <- inner_join(bundesland_weighted, population_density)
bundesland_weighted <- bundesland_weighted %>% arrange(Bundesland, year, week)

# replace NA for vaccinations

bundesland_weighted <- bundesland_weighted %>% mutate( vaccination_weighted = coalesce(vaccination_weighted, 0))
bundesland_weighted <- bundesland_weighted %>% mutate( cumulative_vaccinations = coalesce(cumulative_vaccinations, 0))
bundesland_weighted <- drop_na(bundesland_weighted)



# combine with cases

cases <- bundesland_weeks_na %>% select (Bundesland, cases)
#cases$year <- as.character(cases$year)
bundesland_weighted$week <- as.double(bundesland_weighted$week)
bundesland_weighted$year <- as.double(bundesland_weighted$year)
#cases$week <- as.character(cases$week)
bundesland_weighted <- full_join(bundesland_weighted, cases)


# deal with NA
bundesland_weighted <- bundesland_weighted %>% mutate(cases = coalesce(cases, 0))


# add population size
bundesland_weighted <-inner_join(bundesland_weighted, bundesland_population)

# add population density
bundesland_weighted <- inner_join(bundesland_weighted, population_density)
bundesland_weighted <- bundesland_weighted %>% select (- Bundesland_abb)
bundesland_weighted <- bundesland_weighted %>% arrange(Bundesland, year, week)
bundesland_weighted <- drop_na(bundesland_weighted)
md.pattern(bundesland_weighted)

    
    

############################################################## 
######### TRY TO GENERATE DATA BY COUNTY ##################

# corona deaths as a proxy for excessive mortality
# Specification: by weeks 

# download data 

county_data <- read.csv("C://Users//khari//Desktop//Siegen//Markets  Strategies II//Term Paper//Term Paper 1//DATA//data_by_county_clean.csv")
head(county_data)

county_data_agg <- county_data %>%
  group_by(county, year, week, total_population, regional.name, population_density.per.km2)%>%
  summarise(
    deaths = sum(deaths),
    vaccinations = sum(vaccinations),
    cases = sum(cases)
  )


# ADD COLUMN Cumulative vaccination
cumulative_vaccinations_conty <- county_data_agg %>%
  arrange(county, year, week)%>%
  group_by(county)%>%
  summarize(
    cumulative_vaccinations = cumsum(vaccinations)
  )

# APPLY to the main data frame
county_data_agg <- bind_cols(county_data_agg, cumulative_vaccinations_conty)
colnames(county_data_agg)


county_data_agg <- rename(county_data_agg, c("county...1" = "county"))
county_data_agg$county...10 <- NULL

colnames(county_data_agg)

#before running this need to get rid of NA ==> DONE

county_data_agg <- county_data_agg %>% 
  mutate(
    vacc_per_100 = round(vaccinations/total_population*100, 1),
    vacc_per_100_cum = round(cumulative_vaccinations/total_population*100, 1),
    deaths_per_100 = round(deaths/total_population*100, 1),
    cases_per_100 = round(cases/total_population*100, 1)
    )



cor(county_data_agg$cumulative_vaccinations, county_data_agg$cases) # vaccination also affects cases? => correlation is high

ggplot(county_data_agg, aes(x = cases_per_100, y= deaths_per_100, 
                                           label=county)) + 
  geom_smooth(method="lm")


# Look at the extreme vaccination values
high_vacc <- county_data_agg %>% filter(vacc_per_100_cum > 100)

ggplot(high_vacc, aes(x=vacc_per_100_cum)) +
  geom_density(fill="orange", alpha=0.5) +
  geom_vline(xintercept=mean(high_vacc$vacc_per_100_cum), size=1, color="black")+
  labs(x= "Coefficients",
       subtitle="OLS coefficients density plots")+
  theme(legend.position="bottom")

# Look at the vaccination distribution

ggplot(county_data_agg, aes(x=vacc_per_100_cum)) +
  geom_density(fill="orange", alpha=0.5) +
  geom_vline(xintercept=mean(county_data_agg$vacc_per_100_cum), size=1, color="black")+
  labs(x= "Coefficients",
       subtitle="OLS coefficients density plots")+
  theme(legend.position="bottom")  # values over 100 can be removed


county_data_agg <- county_data_agg %>% filter(vacc_per_100_cum <= 100)

################ ADD MORE EXPLANATORY DATA TO THE EXSISTING DATA FRAMES ########################

# aggregated data on german level
# temperature by weeks or months for federal states 
# rain by weeks or months for federal states
# for overall german level we can calculate probably excess mortality differently
# dummy variable for age 


# 1. Germany data

germany <- bundesland_weeks_na %>% 
  group_by(year, week) %>% 
  summarise(
    vaccinations = sum(vaccinations),
    deaths = sum(deaths),
    cases = sum(cases),
    excessive_mortality_abs = sum(excessive_mortality_abs),
    cumulative_vaccinations = sum(cumulative_vaccinations)
  ) %>%
  mutate(
    Bundesland_population = 83240000,
    Density = 232.6,
    Bundesland = "Germany",
    Bundesland_abb = "DE", 
    percentage_vaccinated = round(vaccinations/Bundesland_population*100, 1),
    cum_perc_vaccinated = round(cumulative_vaccinations/Bundesland_population*100, 1)
  )

germany_mort <- read_excel("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Excessive mortality 2020-2021 by weeks BL.xlsx", sheet=11)
head(germany_mort)
str(germany_mort)

germany <- inner_join(germany, germany_mort)

spec10 <- lm(Excessive_mortality ~  vaccinations  + cases, data = germany) # not cumulative vaccination rate
summary(spec1)


### Temperature data source https://de.statista.com/statistik/daten/studie/5564/umfrage/monatliche-durchschnittstemperatur-in-deutschland/ 
# https://www.wetterkontor.de/de/wetter/deutschland/monatswerte-temperatur.asp
# Niederschlag https://de.statista.com/statistik/daten/studie/576867/umfrage/durchschnittlicher-niederschlag-pro-monat-in-nordrhein-westfalen/ 

# make a new data frame with vaccinations

vaccinations_by_county_age_old <- vaccinations_by_county %>%
  filter(Impfschutz == 2, Altersgruppe == "60+")%>% #only fully vaccinated individuals
  group_by(Impfdatum, LandkreisId_Impfort, Altersgruppe) %>%
  summarize(
    vaccinations = sum(Anzahl)
  )

vaccinations_by_county_age_young <- vaccinations_by_county %>%
  filter(Impfschutz == 2, Altersgruppe == "12-17" | Altersgruppe == "18-59")%>% #only fully vaccinated individuals
  group_by(Impfdatum, LandkreisId_Impfort) %>%
  summarize(
    vaccinations = sum(Anzahl)
  )%>%
  mutate(Altersgruppe = "0-59")

vaccinations_by_county_age <- full_join(vaccinations_by_county_age_old, vaccinations_by_county_age_young)
vaccinations_by_county_age <- rename(vaccinations_by_county_age, c(Impfdatum = "date", LandkreisId_Impfort = "county", Altersgruppe = "age_group"))

vaccinations_by_county_age <- vaccinations_by_county_age %>% 
  mutate(
    week = format(as.Date(date), "%W"),
    year = format(as.Date(date), "%Y"),
    month = format(as.Date(date), "%m"),
    abb_month = format(as.Date(date), "%b"),
  )

class(vaccinations_by_county_age$date)
class(vaccinations_by_county_age$county)

# assign age groups to the cases + reshare the data frame 
cases_by_county_age <- cases_by_county_long %>% mutate(
  ratio_0_59 = 0.8246,
  ratio_60 = 0.1754,
  "0-59" = cases*ratio_0_59,
  "60+" = cases*ratio_60
)

# remove and reshare
cases_by_county_age <- cases_by_county_age %>% select(-ratio_0_59, -ratio_60)
cases_by_county_age <-  gather(cases_by_county_age, "age_group", "cases", 9:10)

# join 2 data frames
data_age <- full_join(vaccinations_by_county_age, cases_by_county_age)
data_age <- data_age %>% arrange(date, county)

unique(data_age$age_group)

# Enrich your data with additional info: Bundesland code and so on


data_age <- full_join(counties_codes, data_age, by = 'county')

data_age <- rename(data_age, c("Regionale Bezeichnung" = "regional name", "Kreisfreie Stadt"  = "independent city", 
                     "Fläche \r\nin km2" = 'square km2', "insgesamt" = "total_population",
                     "männlich" = "male", "weiblich" = "female", "je km2" = "population_density per km2"))

data_age <- inner_join(bundesland_county, data_age)
data_age <- full_join(data_age, bundesland_short_code)
data_age <-full_join(data_age, bundesland_population)

# add the temperature and rain

#temperature <- read_excel("C://Users//khari//Desktop//Siegen//Markets  Strategies II//Term Paper//Term Paper 1//DATA//temperature rain 2020-2021.xlsx", sheet = 1)
#head(temperature)

# remove unneeded values

#temperature <- temperature %>% select(-Bundesland, -`month num`)

# change the classs type
#temperature$year <- as.character(temperature$year)
#class(temperature$year)

#data_age <- inner_join(data_age,temperature) # added the temperature, wohoo

# Look at NA -> must be a lot 

################# DATA CLEANING #####################################

unique(data_age$county)

# Change all characters NA to real NA


# Delete suspicious values for counties + the date 28.12.2021 and 27.12.2021 since there is no data
# + the date 2020-03-02 since there are no data on cases
# we have to delete the whole county 16056 because it doesn't have any data on cases ==> CHANGED DECISION no need for | dat$county == "16056"
data_age <-data_age[!( data_age$date == "2021-12-27"),]

missing_data_age <- data_age %>%
  mutate( vaccinations = coalesce(vaccinations, 0))%>% # replace vaccinations with zero
  mutate( cases = coalesce(cases, 0))%>% # replace NA cases with zero for county 16056 on order not to lose data on population 
  mutate( cumulative_cases = coalesce(cumulative_cases, 0))%>% # replace NA cases with zero for county 16056 on order not to lose data on population 
  filter(!complete.cases(.))#%>%

# another package for overview of missing data

md.pattern(missing_data_age) # finalized to the absence of missing data

# after trying we can apply the same transformation to our data to get rid of NA

data_age <- data_age %>%
  mutate( vaccinations = coalesce(vaccinations, 0))%>% # replace vaccinations with zero
  mutate( cases = coalesce(cases, 0))%>%  # replace NA cases with zero for county 16056 on order not to lose data on population
  mutate( cumulative_cases = coalesce(cumulative_cases, 0)) 


data_age <- drop_na(data_age)
md.pattern(data_age) # 


# now we can group all the data to the week & Bundesland level

data_age_bundesland <- data_age %>%
  group_by(year, week, Bundesland, Bundesland_abb, Bundesland_population, age_group )%>%
  summarise(
    vaccinations = sum(vaccinations),
    cases = sum(cases)
  )

data_age_bundesland <- inner_join(data_age_bundesland, population_density)


# ADD CUMULATIVE VACCINATIONS, CUMULATIVE CASES = > DONE

# cumulative vaccinations 
data_age_bundesland_cum <- data_age_bundesland %>%
  arrange(year,week)%>%
  group_by(Bundesland, age_group)%>%
  summarize(cumulative_vaccinations = cumsum(vaccinations),
            cumulatvie_cases = cumsum(cases)) # %>%
#mutate( cumulative_vaccinations = coalesce(cumulative_vaccinations, 0))  # replace NA cases with zero for county 16056 on order not to lose data on population


length(data_age_bundesland_cum$cumulative_vaccinations)
length(data_age_bundesland$vaccinations)


data_age_bundesland <- data_age_bundesland %>% arrange(Bundesland,age_group, year,week ) # now the data are in the same format

# combine 2 data frames
data_age_bundesland <- bind_cols(data_age_bundesland, data_age_bundesland_cum)
colnames(data_age_bundesland)
data_age_bundesland <- rename(data_age_bundesland, c("Bundesland...3" = "Bundesland", "age_group...6" = "age_group"))
data_age_bundesland$Bundesland...10 <- NULL
data_age_bundesland$age_group...11 <- NULL

# ADD DATA ON MORTALITY 


exc_mort_by_age <- read_excel("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Excessive mortality 2020-2021 by weeks BL.xlsx", sheet=14)
exc_mort_by_age <-  gather(exc_mort_by_age, "week", "exc_mortality", 4:55)
head(exc_mort_by_age)

# change the classs type
exc_mort_by_age$year <- as.character(exc_mort_by_age$year)
class(exc_mort_by_age$year)

# join data frames

data_age_bundesland <- inner_join(data_age_bundesland, exc_mort_by_age)
md.pattern(data_age_bundesland) # 

# drop NA

data_age_bundesland <- drop_na(data_age_bundesland)
md.pattern(data_age_bundesland) #

data_age_bundesland <- data_age_bundesland %>% 
  mutate(
    percentage_vaccinated = round(vaccinations/Bundesland_population*100, 1),
    cum_perc_vaccinated = round(cumulative_vaccinations/Bundesland_population*100, 1)
  )

# add germany data 

germany_age <- data_age_bundesland %>% 
  group_by(year, week, age_group) %>% 
  summarise(
    vaccinations = sum(vaccinations),
    cases = sum(cases),
    cumulative_vaccinations = sum(cumulative_vaccinations),
    cumulatvie_cases = sum(cumulatvie_cases)
  ) %>%
  mutate(
    Bundesland_population = 83240000,
    Density = 232.6,
    Bundesland = "Germany",
    Bundesland_abb = "DE", 
    percentage_vaccinated = round(vaccinations/Bundesland_population*100, 1),
    cum_perc_vaccinated = round(cumulative_vaccinations/Bundesland_population*100, 1)
  )


temperature_germany <- read_excel("C://Users//khari//Desktop//Siegen//Markets  Strategies II//Term Paper//Term Paper 1//DATA//temperature rain 2020-2021.xlsx", sheet = 2)
head(temperature_germany)

# remove unneeded values

temperature_germany <- temperature_germany %>% select(-abb_month)

# change the classs type
temperature_germany$year <- as.character(temperature_germany$year)
class(temperature_germany$year)
temperature_germany$week <- as.character(temperature_germany$week)
class(temperature_germany$week)

germany_age <- inner_join(germany_age,temperature_germany) # added the temperature, wohoo

# add mortality data 
germany_mort_age <- read_excel("C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATA\\Excessive mortality 2020-2021 by weeks BL.xlsx", sheet=16)
germany_mort_age <-  gather(germany_mort_age, "week", "exc_mortality", 3:54)
head(germany_mort_age)

# change the class

germany_mort_age$year <- as.character(germany_mort_age$year)
class(germany_mort_age$year)


germany_age <- inner_join(germany_age, germany_mort_age)

colnames(germany_age)

spec10 <- lm(exc_mortality ~  vaccinations  + cases + age_group + temperature + rain, data = germany_age) # not cumulative vaccination rate
summary(spec10)


# join the data on Germany 

# add temperature data to bundeslands

data_age_bundesland <- inner_join(data_age_bundesland,temperature_germany) # added the temperature, wohoo)
head(data_age_bundesland)

spec11 <- lm(exc_mortality ~  vaccinations  + cases + age_group + cases*age_group + temperature + rain + Bundesland, data = data_age_bundesland) # not cumulative vaccination rate
summary(spec11)

colnames(germany_age)
colnames(data_age_bundesland)

data_age_bundesland <- bind_rows(data_age_bundesland, germany_age)

# join the data on germany that come without the age 

colnames(germany)
colnames(bundesland_weeks_na)

bundesland_weeks_na <- bind_rows(bundesland_weeks_na, germany)

# add data on temperature

temperature_germany$year <- as.double(temperature_germany$year)
temperature_germany$week <- as.double(temperature_germany$week)

bundesland_weeks_na <- inner_join(bundesland_weeks_na,temperature_germany) # added the temperature, wohoo)
head(bundesland_weeks_na)

head(bundesland_weighted)

bundesland_weighted <-inner_join(bundesland_weighted, temperature_germany)
county_data_agg <- inner_join(county_data_agg, temperature_germany)

###### ONCE AGAIN, WHAT ARE YOUR MASTER DATAFRAMES?

bundesland_weeks_na # BASIC DATA without age by weeks and bundesland 
bundesland_weighted # weighted mortality by age + weighted vaccination by age group, no data on German Level
data_age_bundesland # BASIC DATA with age by weeks and bundesland 
before_after_vacc_another_mort # data before_after vaccination by bundesland 
county_data_agg # data by county, corona deaths are used as proxy for excessive deaths
before_after_vacc_county # data by county, corona deaths are used as proxy for excessive deaths, before / after vaccination started


######### I WILL CREATE ANOTHER R FILE IN ORDER NOT TO GET LOST WITH ALL THE INFO ############ 
####### SAVE MY DATAFRAMES TO CSV FILES ##################### 

write.csv(bundesland_weeks_na,"C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATAFRAMES\\Data by weeks, BL, no age.csv", row.names = FALSE)
write.csv(bundesland_weighted,"C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATAFRAMES\\Data by weeks, BL, no age WEIGHTED.csv", row.names = FALSE)
write.csv(data_age_bundesland,"C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATAFRAMES\\Data by weeks, BL, age groups.csv", row.names = FALSE)
write.csv(before_after_vacc_another_mort,"C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATAFRAMES\\BL before after vaccination.csv", row.names = FALSE)
write.csv(county_data_agg,"C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATAFRAMES\\Data by weeks, counties, no age.csv", row.names = FALSE)
write.csv(before_after_vacc_county,"C:\\Users\\khari\\Desktop\\Siegen\\Markets  Strategies II\\Term Paper\\Term Paper 1\\DATAFRAMES\\Counties before after vaccination.csv", row.names = FALSE)





