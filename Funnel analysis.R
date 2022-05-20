library(tidyverse)
library(dbplyr)
library(lubridate)
library(stringr)
library(ggplot2)
#install.packages("gmodels")
#install.packages("summarytools")
#install.packages("vcd")
library(gmodels)
library(summarytools)
library(vcd)

### Datasets import ###
setwd('/Users/skrundz/Documents/Learning')
data_raw <- as.data.frame(read.csv("Data.csv"))
time_zones <-as.data.frame(read.csv("Geo_data.csv"))

### Structure check ###
str(data_raw, give.attr = FALSE)
str(time_zones)
view(dfSummary(data_raw))

### Adjust the country_codes wording (case sensitive) ###
time_zones <- time_zones %>%
  mutate(country_code = tolower(country_code))

### Data cleaning, joining time zone table, cleaning NAs ###
data <- data_raw %>% 
  mutate(market = case_when(
    market == 'en' ~ 'gb',
    market == 'ja' ~ 'jp',
    market == 'uk' ~ 'gb',
    market == 'ca-fr' ~ 'ca',
    market == 'ko' ~ 'kr',
    TRUE ~ market
  )) %>%
  left_join(time_zones, by= c("market" = "country_code")) %>%
  
  mutate(
    timestamp = as.POSIXct(ymd_hms(timestamp)))

## check - How many NAs do we have? (Time_zone = NA means that market has incorrect values) --> less than 0.005%, rows can be removed from the dataset ###
dfSummary(data_raw)
data_NA <- data %>% filter(is.na(Time_zone)) %>% 
  summarize(count =n())
data_count <- data %>% 
  summarize(count =n())
NA_percentage <- data_NA / data_count
data <- data %>% drop_na(Time_zone)


### convert UTC time to the local time ###
for (i in 1:data_count[1,1]) {
data$local[i] <- as.character(with_tz(data$timestamp[i],tz=data$Time_zone[i]))
}

### Get info of the time of day ###
data <- data %>%
  separate(local, c("date", "time"), sep= " ", remove=F) %>%
  mutate( 
    platform = as.factor(platform),
    device = as.factor(device),
    country_name = as.factor(country_name),
    action = as.factor(action),
    isLoggedIn = as.factor(isLoggedIn),
    date = as_date(date),
    time = hms::as_hms(time),
    day_time = case_when(
      hour(time)>= 6 & hour(time) <12 ~ "morning",
      hour(time)>=12 & hour(time)<18 ~ "afternoon",
      hour(time)>=18 & hour(time)<22 ~ "evening",
      (hour(time)>=22 & hour(time)<24) | (hour(time)>=0 & hour(time)<6)  ~ "night",
      TRUE ~ "night"),
    day_time = as.factor(day_time),
    day_week_num = wday(date), 
    day_week = case_when(
      day_week_num == 1 ~ "Sunday",
      day_week_num == 2 ~ "Monday",
      day_week_num == 3 ~ "Tuesday",
      day_week_num == 4 ~ "Wednesday",
      day_week_num == 5 ~ "Thursday",
      day_week_num == 6 ~ "Friday",
      day_week_num == 7 ~ "Saturday"
    )
)

view(summary <- dfSummary(data))
summary  

table <- round(prop.table(table(data$action, data$day_time),2),2)
table

data_summary <- data %>% select(platform, device, action, country_name, continent_name, day_time, day_week)

view(dfSummary(data_summary))
          # plain.ascii  = FALSE, 
          # style        = "grid", 
          # graph.magnif = 0.75, 
          # valid.col    = FALSE,
          # tmp.img.dir  = "/tmp"))

### the chi-squared test ###
### H0: no relationship exists on the categorical variables in the population - they are independent ###
### H1: variables are dependent, exists correlation ###
### if p-value < 0.05 --> the variables are not independent, there is a statistical relationship between the categorical variables ###

corr_device <- chisq.test(data$action, data$device)
corr_platform <- chisq.test(data$action, data$platform)
corr_isLoggedIn <- chisq.test(data$action, data$isLoggedIn)
corr_country_name <- chisq.test(data$action, data$country_name)
corr_day_time <- chisq.test(data$action, data$day_time)
corr_day_week <- chisq.test(data$action, data$day_week)


corr_device <- unlist(corr_device[c(1,2,3)])
corr_platform <- unlist(corr_platform[c(1,2,3)])
corr_isLoggedIn <- unlist(corr_isLoggedIn[c(1,2,3)])
corr_country_name <- unlist(corr_country_name[c(1,2,3)])
corr_day_time <- unlist(corr_day_time[c(1,2,3)])
corr_day_week <- unlist(corr_day_week[c(1,2,3)])

ctable(
  x = data$action,
  y = data$platform,
  chisq = TRUE, # display results of Chi-square test of independence
  headings = T 
)

ctable(
  x = data$action,
  y = data$day_time,
  chisq = TRUE, # display results of Chi-square test of independence
  headings = T 
)

correlations <- data.frame(corr_device, corr_platform, corr_isLoggedIn, corr_country_name, corr_day_time, corr_day_week)

mosaic(~ action + platform,
             direction = c("v", "h"),
             data = data,
             shade = TRUE
)


##Unpivot the dataset

data_all_pivot <- data %>%
  select(visitorId, market, action, timestamp, isLoggedIn) %>%
  pivot_wider(names_from = action, values_from=c(timestamp, isLoggedIn)) %>%
  filter(is.na(`timestamp_Booking Initialized`)==F) %>%
  mutate(booking_duration = difftime(`timestamp_Booking Confirmed`, `timestamp_Booking Initialized`, units="mins"),
         final_log_status = case_when(
           `isLoggedIn_Booking Confirmed` == `isLoggedIn_Booking Initialized` ~ `isLoggedIn_Booking Initialized`,
           `isLoggedIn_Booking Confirmed`== 'true' & `isLoggedIn_Booking Initialized`== 'false' ~ `isLoggedIn_Booking Confirmed`,
           is.na(`isLoggedIn_Booking Confirmed`) ~ `isLoggedIn_Booking Initialized`,
           is.na(`isLoggedIn_Booking Initialized`) ~ `isLoggedIn_Booking Confirmed` ##logically incorrect but encountered in the dataset
         ),
         logged_during_action = ifelse(`isLoggedIn_Booking Confirmed`== 'true' & `isLoggedIn_Booking Initialized`== 'false', 'true', 'false'))



  
write_csv(data,'/Users/skrundz/Documents/Learning/dataset.csv')
write_csv(data_all_pivot,'/Users/skrundz/Documents/Learning/dataset_all_pivot.csv')


