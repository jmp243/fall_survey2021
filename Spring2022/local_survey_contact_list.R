##Trellis users by product
##can be used for Program Survey Contact List - March 2022 and User Community Emails

#SET UP Environment
library('tidyverse')
library('dplyr')
library('readxl')
library('salesforcer')
library('RForcecom')
library('lubridate')

rm(list=ls(all=TRUE)) 
options(digits=3)

today <- Sys.Date()
#capture current month
this_month <- month(today)
this_year <- year(today)

format(today, format="%B %d %Y")

#### set working directory ####
getwd()
setwd("~/Documents/Trellis/Survey/Spring2022")

for_use <- read.csv("20220304_SFTrellisUsersByProduct.csv")

# drop the first column
for_use <- for_use[,-c(1)] # drop X column

# rename Scheduling and Notes to Scheduling & Notes and Email.x
library(dplyr)
for_use <- for_use %>%
  mutate(SchedulingNotes = recode(SchedulingNotes, 'Scheduling and Notes' = 'Scheduling & Notes'))
# for_use$Email <- for_use$Email.x

for_use %>% 
rename(Email = Email.x)
#### create a concatenated column ####
# create a new column
library(dplyr)
library(tibble)
library(tidyr)

# for_use2 <- for_use %>%
#   rownames_to_column %>%
#   gather(colName, Value, -rowname) %>%
#   na.omit %>%
#   select(-colName) %>%
#   nest(Value, .key = allproducts) %>%
#   arrange(rowname) %>%
#   left_join(cbind(for_use %>% rownames_to_column), .) %>% 
#   select(-rowname)

# for_use2 <- for_use %>%
#   unite(allproducts, SchedulingNotes, Events, MarketingSF, SAFER, ExternalPartners,
#         remove = FALSE,
#         sep = ', ') %>%
#   # Replace NAs and "NA, "s with ''
#   mutate(allproducts = gsub('NA[, ]*', '', allproducts)) %>%
#   # Replace ', ' with '' if it is at the end of the line
#   mutate(allproducts = gsub(', $', '', allproducts)) 

library(dplyr)
library(tidyr)
library(stringi)
# Data with four cases
# my_prod <- data.frame(Col1 = c("a", NA, "b", "i"),
#                       Col2 = c("d", "c", "e", NA),
#                       Col3 = c("f", NA, NA, "j"),
#                       Col4 = c("g", NA, "h", NA))

for_use2 <- for_use %>%
  # count the number of non-NA values in columns of interest
  mutate(col_count = rowSums(!across(SchedulingNotes:ExternalPartners, is.na))) %>%
  # use unite to join everything by commas, dropping any NA values
  unite(allproducts, SchedulingNotes:ExternalPartners,
        sep = ", ",
        remove = FALSE,
        na.rm = TRUE) %>%
  # update that column based on our count of non-NA values
  mutate(allproducts = if_else(col_count == 2,
                               gsub(pattern = ",", 
                                    replacement = " and",
                                    x = allproducts),
                               stringi::stri_replace_last_fixed(str = allproducts,
                                                                pattern = ", ",
                                                                replacement = ", and "))) %>%
  # remove the helper column from output
  select(-col_count)

# add in sf data for reporting
for_use2$allproducts[for_use2$allproducts==""]<- "Salesforce data for reporting"

for_use2 <- for_use2 %>% 
  rename(Email = Email.x)
write.csv(for_use2, "for_use2_March31.csv")
