# EPR survey 2023
# Jung Mee Park
# 2023-03-06
# analyzing Feb 2023 survey of EPR recipients

setwd("~/Documents/Trellis/Survey/Fall2021/EPR_Survey_2023")

# read in library
library(tidyverse)
library(stringi)
library(stringr)
# read in data
EPR_data <- read.csv("EPR+CatCloud+Survey_March+6,+2023_09.14.csv", skip=2)

# empty_lat <- EPR_data %>% 
#   filter(X..ImportId...locationLatitude.. != " ")
# 
completed_surveys <- EPR_data %>% 
  filter(X..ImportId...progress.. >= 34)

# remove long prefix before variable names
completed_surveys <- completed_surveys  %>%
  rename_with(.fn= ~str_remove(.x, "X..ImportId...") %>%
                str_replace("\\.", " "))

sapply(completed_surveys, class)

completed_surveys %>% 
  filter(if_all(-c(`QID1 .`, `QID2_1 .`), ~ is.na(.)), 
         if_all(c(`QID1 .`, `QID2_1 .`), ~ !is.na(.)))


# fill one First question as indifferent if blank
completed_surveys$`QID1 .` <- as.factor(completed_surveys$`QID1 .`)
summary(completed_surveys$`QID1 .`)
# completed_surveys$`QID1 .`[is.na(completed_surveys$`QID1 .`)] = "Indifferent"
# colSums(is.na(completed_surveys))
completed_surveys$`QID1 .`[completed_surveys$`QID1 .`==""]<-"Indifferent"
# 
# QID1_filled <- completed_surveys %>% 
#   # mutate(QID1 = ifelse(diagnosis == "confirmed" | PC_R == "pos", "pos", "neg"))
#   mutate(QID1 = case_when(`QID1 .` == NA ~ "Indifferent", 
#                           `QID1 .` == "Yes" ~ "Yes", 
#                           `QID1 .` == "No" ~ "No",
#                           `QID1 .` == "Indifferent" ~ "Indifferent"
#                           ))
# 
# QID1_filled <- completed_surveys %>% mutate(
#     QID1 = case_when(
#     grepl("Indifferent", `QID1 .`) ~ "Indifferent",
#     grepl(NA, `QID1 .`) ~ "Indifferent",
#     grepl("Yes", `QID1 .`) ~ "Yes",
#     grepl("No", `QID1 .`) ~ "No",
#   ))
