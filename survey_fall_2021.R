# jung mee park
# Fall Program Survey analysis
# 2021-10-21

# Generate frequencies

# read in the data
setwd("~/Documents/Trellis/Survey/data")

fall_survey <- read.csv("Fall2021TrellisProgramSurveyData.csv", header = T, na.strings=c("","NA"))

# mod_fall_survey <- read.csv("modified_fall_survey.csv", header = T, na.strings=c("","NA"))

head(fall_survey)
# check NA's
sum(is.na(fall_survey$Q3_4))

colSums(is.na(fall_survey)) # for all data
# load in libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(tidyr)
library(forcats)
library(lubridate)
library(dplyr)
library(knitr)
library(lattice)
library(likert)
library(MASS)
library(psych)
library(viridis)
library(here)
library(flextable)
library(devtools)
# library(userfriendlyscience)
# managing dates
# original format mm/dd/yy hh:mm date format in R

# convert Recorded Date 
head(fall_survey$RecordedDate)

# record_dates <- as.POSIXct(fall_survey$RecordedDate,
#                     format = "%m/%d/%y %H:%M", tz = "MST")
#                     
# record_date <- as_datetime(fall_survey$RecordedDate,
#                            format = "%m/%d/%y %H:%m")
# record_dates <-  as.Date(record_date)
##
recorded_dt <- as.POSIXct(as.character(fall_survey[ ,1]), format = "%m/%d/%y %H:%M") 
fall_survey <- dplyr::mutate(fall_survey, recorded_dt) # appended the corrected date time format

fall_survey <- fall_survey %>% 
  mutate(Recorded_Date = as.Date(recorded_dt))


# for created date
head(fall_survey$CreatedDate)
dates <- as.POSIXct(fall_survey$CreatedDate,
                    format = "%m/%d/%y %H:%M", tz = "UTC") 
# change time zones
# change to AZ time 
# t1 <- as.POSIXct(dates, tz = "GMT")
# attributes(t1)$tzone
AZ_created_time <- lubridate::with_tz(dates, "MST")

fall_survey <- dplyr::mutate(fall_survey, AZ_created_time) # AZ created date added

fall_survey <- fall_survey %>% 
  mutate(AZ_created_date = as.Date(AZ_created_time))

#      Break into chunks based on user creation date (CreatedDate)
#      <1 month
#      1-6 months
#      6 months to a year
#      1 year+

# the collection ended on October 1
# subtract years or months
ymd("2021-10-01") - months(1)

nrows = nrow(fall_survey)
period = rep(0, nrows)
period[which(fall_survey$AZ_created_date < "2020-10-01")] = "More than 12 months"			 
period[which(fall_survey$AZ_created_date >= "2020-10-01" & 
               fall_survey$AZ_created_date <= "2021-04-01")] = "Six to 12 months"
period[which(fall_survey$AZ_created_date >= "2021-04-01" & 
               fall_survey$AZ_created_date <= "2021-09-1")] = "One to 6 months"
period[which(fall_survey$AZ_created_date >= "2021-09-1" & 
               fall_survey$AZ_created_date <= "2021-10-10")] = "Less than 1 month" 

# attach period to the data file
fall_survey$period = period

# reorder factor for periods
fall_survey$period = factor(fall_survey$period, 
                          ordered = TRUE,
                          levels = c("One to 6 months", "Six to 12 months", "More than 12 months"))

# when did people drop out
# check Progress variable 
summary(fall_survey$Progress)

# subset to completed surveys only
completed_survey <- fall_survey %>% 
  filter(Finished == "TRUE") 
# %>% 
#   mutate(mean_time = mean(Duration..in.seconds.))

incomplete_survey <- fall_survey %>% 
  filter(Finished == "FALSE") 
# %>% 
#   mutate(mean_time = mean(Duration..in.seconds.))

mean_time <- tapply(fall_survey$Duration..in.seconds., fall_survey$Finished, mean)
mean_time

quant_time <- tapply(fall_survey$Duration..in.seconds., fall_survey$Finished, quantile)
quant_time

sum_time <- tapply(fall_survey$Duration..in.seconds., fall_survey$Finished, summary)
sum_time

# rename variables 

# reorder factors responses to Q3
fall_survey$Q3_1 = factor(fall_survey$Q3_1, 
                   ordered = TRUE,
                   levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                              "Somewhat agree", "Strongly agree"))

fall_survey$Q3_2 = factor(fall_survey$Q3_2, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

fall_survey$Q3_3 = factor(fall_survey$Q3_3, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

fall_survey$Q3_4 = factor(fall_survey$Q3_4, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))
# reorder responses to question 4
fall_survey$Q4 = factor(fall_survey$Q4, 
                          ordered = TRUE,
                          levels = c("Extremely dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied", 
                                      "Somewhat satisfied", "Extremely satisfied"))
# reorder responses to question 6
fall_survey$Q6_1 = factor(fall_survey$Q6_1, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

fall_survey$Q6_2 = factor(fall_survey$Q6_2, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

fall_survey$Q6_3 = factor(fall_survey$Q6_3, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))
# reorder responses to question 9
fall_survey$Q9_1 = factor(fall_survey$Q9_1, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

fall_survey$Q9_2 = factor(fall_survey$Q9_2, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

fall_survey$Q9_3 = factor(fall_survey$Q9_3, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

fall_survey$Q9_4 = factor(fall_survey$Q9_4, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))
# reorder responses to question 11
fall_survey$Q11_1 = factor(fall_survey$Q11_1, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

fall_survey$Q11_2 = factor(fall_survey$Q11_2, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

fall_survey$Q11_3 = factor(fall_survey$Q11_3, 
                          ordered = TRUE,
                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                                     "Somewhat agree", "Strongly agree"))

# save file to csv
write.csv(fall_survey, file = "modified_fall_survey.csv")

