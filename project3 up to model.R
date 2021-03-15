# Setup
library(readxl)
library(dplyr)
library(tidyverse)
filepath <- "C:\\Users\\Charles\\Dropbox\\Graduate\\Courses\\(5) Winter 2021\\STA 207\\Projects\\Project 3\\STA207_Project3\\STA207_Project3"

  # replace with location of data

# Read in data
pop <- read_excel(paste0(filepath, "\\World Bank Population.xlsx"), 
                  range = "A5:BM269",
                  col_names = TRUE)

# Two countries have population missing in 2019: "Eritrea" and "Not classified"
  # summary(pop$`2019`)
  # View(pop[is.na(pop$`2019`),])

# use 2011 population for Eritrea, use 2019 population for all other countries
  pop$population <- ifelse(pop$`Country Name` == "Eritrea", pop$`2011`, pop$`2019`)
  pop <- pop[, c('Country Name', 'population')]
  pop <- rename(pop, Country = `Country Name`)

# Standardize names to make merge work (done only for European countries and US)
  pop$Country <- ifelse(pop$Country == 'Czech Republic', 'Czechia', pop$Country)
  pop$Country <- ifelse(pop$Country == 'Slovak Republic', 'Slovakia', pop$Country)
  pop$Country <- ifelse(pop$Country == 'United Kingdom', 'The United Kingdom', pop$Country)
  pop$Country <- ifelse(pop$Country == 'United States', 'United States of America', pop$Country)


# Merge with covid data
  covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
  covid_merged <- left_join(covid, pop, by = 'Country')

# Checking merge
  # List all countries that didn't merge
  # merge_check <- covid_merged[is.na(covid_merged$population)|is.na(covid_merged$Date_reported),]
  # merge_check <- merge_check %>% group_by(Country) %>% summarize(pop_mean = mean(population))
  #
  # Check list countries in covid data with population missing after merge
  #merge_check2 <- covid_merged[is.na(covid_merged$population), c('Country','population')] %>% group_by(Country) %>% summarize(n = n())

  
  stay <- read_csv(paste0(filepath, "\\stay-at-home-covid.csv"))
  stay$Date <- as.Date(stay$Date, "%m/%d/%Y")
  
  face <- read_csv(paste0(filepath, "\\face-covering-policies-covid.csv"))
  face$Date <- as.Date(face$Date, "%m/%d/%Y")
  
  public <- read_csv(paste0(filepath, "\\public-events-covid.csv"))
  public$Date <- as.Date(public$Date, "%m/%d/%Y")
  
  school <- read_csv(paste0(filepath, "\\school-closures-covid.csv"))
  school$Date <- as.Date(school$Date, "%m/%d/%Y")
  
  # Putting all explanatory data together
  data <- inner_join(stay, face, by = c("Entity", "Code","Date"))
  data <- inner_join(data, public, by = c("Entity", "Code","Date"))
  data <- inner_join(data, school, by = c("Entity", "Code","Date"))
  
  # Only keeping dates before February 1, 2021 and aggregate by week
  data <- data[data$Date < "2021-02-01", ]
  
  library(lubridate)
  data <- data %>% 
    group_by(Entity, Code, year = year(Date), month = month(Date)) %>% 
    summarise_if(is.numeric, max)
  
  library(zoo)
  data$Date <- as.yearmon(paste(data$year, data$month), "%Y %m")
  
  # Only keeping European data with total deaths and population
  covid_merged <- covid_merged[covid_merged$WHO_region == "EURO", c("Date_reported", "Country", "Cumulative_deaths", "population")]
  
  covid_merged <- covid_merged %>% 
    group_by(Country, year = year(Date_reported), month = month(Date_reported)) %>% 
    summarise_if(is.numeric, max)
  
  covid_merged$Date <- as.yearmon(paste(covid_merged$year, covid_merged$month), "%Y %m")
  
  
  # Joining exact explanatory variables with data set
  covid_merged <- inner_join(covid_merged, data, by = c("Country" = "Entity", "Date"))
  
  
  # Model
  fit1 <- lm(Cumulative_deaths/population*100000 ~ + Country + Date + stay_home_requirements + facial_coverings + cancel_public_events + school_closures 
             - 1, 
             data = covid_merged)
  
  summary(fit1)$coefficients[49:52,]
  
  fit2 <- lm(Cumulative_deaths/population*100000 ~ + Country + Date + stay_home_requirements + facial_coverings + cancel_public_events 
             - 1, 
             data = covid_merged)
  
  summary(fit2)$coefficients[49:51,]
  
  anova(fit1, fit2)
  
  # fit2 is best model. School closures are not sig variable in model
  
  # diagnostics - CH
  
  plot(fit2)
