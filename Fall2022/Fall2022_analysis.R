# Jung Mee Park
# jmpark@email.arizona.edu
# Fall 2022 survey
# transpose questions
# 2022-11-28

# clear all 
rm(list=ls(all=TRUE)) 

#### load in libraries ####
# library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(tidyr)
library(forcats)
library(lubridate)
library(dplyr)
# library(knitr)
# library(lattice)
library(likert)
# library(MASS)
# library(psych)
# library(viridis)
# library(here)
# library(flextable)
# library(devtools)
# library(broom)
library(stringr)
# load library
library(tidyr)
library(ggthemes)
library(reshape2)
library(RColorBrewer)
library(scales)
library(stringi)
library(urltools)
library(purrr)
library(stringr)

# get wd
getwd()
setwd("~/Documents/Trellis/Survey/Fall2021/Fall2022/Fall2022_data")

# read in questions
questions <- read.csv("fall2022_survey_questions.csv", header = T)
transposed_qu <- t(questions)
print(transposed_qu)

# read in responses
# April12 <- read.csv("Trellis Program Survey - Spring 2022_April 12, 2022_11.20.csv", header = T)
Nov28 <- read.csv("Trellis+Program+Survey+-+Fall+2022_December+7,+2022_08.48.csv", header = T)
# Nov28 <- Nov28 %>% 
#   filter(StartDate != "Start Date") 
Nov28 <- Nov28[-c(1,2), ]
Nov28 <- Nov28 %>% 
  dplyr::select(-c(CreatedDate, counter.Events, counter.External.Partners, 
                   counter.Marketing...SF, counter.Scheduling.Notes, counter.SAFER, 
                   counter.Service.Desk, counter.Reports, allproducts))

# Nov28$RecipientEmail[Nov28$RecipientEmail=="jcbetts@email.arizona.edu"] <- "jcbetts@arizona.edu"

# emails <- c("xyz@gmail.com", "abc@hotmail.com")
# emails_new <- gsub("@(.+)$", "\\1", emails)

# Nov28$NetID <- stri_match_first_regex(Nov28$RecipientEmail, "(.*?)\\@")[,2]

Nov28$RecipientEmail <- gsub('email.', '', Nov28$RecipientEmail)
# merge in some SF data
SF_list <- read.csv("2022_Dec7_SFTrellisUsersByProduct(1).csv")
SF_list$Email.x <- gsub('email.', '', SF_list$Email.x)
# SF_list$NetID <- stri_match_first_regex(SF_list$Email.x, "(.*?)\\@")[,2]

names(SF_list)

SF_list <- SF_list %>% 
  dplyr::select(c(CreatedDate, Email.x, allproducts, counter.Service.Desk, 
                  counter.Events, counter.Scheduling.Notes, counter.Marketing...SF, 
                  counter.External.Partners, counter.SAFER, counter.Reports)) # add in accurate counter information from SF

Nov28 <- left_join(Nov28, SF_list, by = c("RecipientEmail" = "Email.x"))
duplicated(Nov28$RecipientEmail)
# Nov28 <- left_join(Nov28, SF_list, by = c("NetID" = "NetID"))
# look for duplicates
# duplicated(Nov28$NetID)
# # remove row 8 and 51 
# Nov28 <- Nov28[-c(8,51), ] # eromero and victoria p

# check for NA's
Nov28 %>%
  mutate(across(.fns = ~+(!is.na(.))))
  # mutate(across(.cols = everything(), is.na)) %>%
  # summarise(across(.cols = everything(), ~ sum(.x)/n()))

Nov28 %>% 
  mutate(across(.cols = 18:41,
                .fns = ~ ifelse(is.na(.x) == TRUE, 0, 1)))

# table_na <- as.data.frame(sapply(Nov28_2, function(x) sum(is.na(x))))

Nov28 %>% 
  # filter(Finished == TRUE) %>% 
  group_by(Finished) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count)) 

# remove test email cases or blank emails
Nov28 <- Nov28[!(Nov28$RecipientEmail=="park.schlawin@gmail.com" | Nov28$RecipientEmail=="jmpark@arizona.edu"| Nov28$RecipientEmail=="jmpark@email.arizona.edu" | Nov28$RecipientEmail=="fkmiller@arizona.edu" | Nov28$RecipientEmail=="" ),]
# sum(is.na(Nov28$RecipientEmail))

# for created date
head(Nov28$CreatedDate)
Nov28$dates <- as.POSIXct(Nov28$CreatedDate,
                          format="%Y-%m-%d %H:%M:%S") 
# change time zones
# change to AZ time 
# t1 <- as.POSIXct(dates, tz = "GMT")
# attributes(t1)$tzone
# AZ_created_time <- lubridate::with_tz(dates, "MST")

Nov28 <- Nov28 %>% 
  dplyr::mutate(AZ_created_date = as.Date(dates)) # AZ created date added


Nov28 <- Nov28 %>%
  mutate(
    period = case_when(AZ_created_date <= "2021-08-01" ~ "More than two semesters",
                       AZ_created_date <= "2022-01-15" ~ "Last semester",
                       # AZ_created_date <= "2021-12-01" ~ "This semester"
                       TRUE ~ "This semester"
    )
  )

# reorder factor for periods
# April25$period = factor(April25$period, 
#                             ordered = TRUE,
#                             levels = c("Less than 1 month", "One to 6 months", "Six to 12 months", "More than 12 months"))

Nov28$period = factor(Nov28$period, 
                        ordered = TRUE,
                        levels = c("More than two semesters", "Last semester", "This semester"))
Nov28 %>% 
  # filter(Finished == TRUE) %>% 
  group_by(period) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count)) 

#### set up likert questions####
Nov28 <- Nov28 %>% 
  mutate(across(.cols=c("Q6_1","Q6_2", "Q6_3",
                        "Q10_1", "Q10_2", "Q12_1", "Q12_2"),
                .fns = ~ factor(.x, 
                                levels = c("Strongly disagree", "Somewhat disagree", 
                                           "Neither agree nor disagree", 
                                           "Somewhat agree", "Strongly agree"),
                                ordered = TRUE)))

# April25_2$Q4 = factor(April25_2$Q4, 
#                       ordered = TRUE,
#                       levels = c("Extremely dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied", 
#                                  "Somewhat satisfied", "Extremely satisfied"))
# 
# # recode advising base
Nov28$Profile.Name[Nov28$Profile.Name=="advising base"] <- "Advising Base"

# breakdown of respondents
Nov28$Progress <- as.numeric(Nov28$Progress)
Nov28_2 <- Nov28 %>% 
  dplyr::filter(Progress > 30)

# library(stringr)
# April25_2[c('prod_val1', 'prod_val2', 'prod_val3', 'prod_val4', 'prod_val5', 'prod_val6')] <- str_split_fixed(April25_2$allproducts, ',', 6)
# # April12_3 <- April12_2 %>% separate(Q8, c('Q8_val1', 'Q8_val2', 'Q8_val3', 'Q8_val4', 'Q8_val5', 'Q8_val6'))
# products <- April25_2[,75:80] # these are the columns of created Q8_val1 through 6

# pivot the table longer
library(data.table)

# April25_2 %>% 
#   gather("key", "value", counter.Events, counter.External.Partners, counter.Marketing...SF, counter.Reports, counter.SAFER, counter.Scheduling.Notes, counter.Service.Desk) %>% 
#   group_by(value) %>%
#   summarise(n=n())
# table(April25_2$counter.Events)
# table(April25_2$counter.External.Partners)
# table(April25_2$counter.Marketing...SF)
# table(April25_2$counter.Reports)
# table(April25_2$counter.SAFER)
# table(April25_2$counter.Scheduling.Notes)
# table(April25_2$counter.Service.Desk)

Nov28_2 %>% 
  dplyr::count(allproducts)

# remove duplicate
Nov28_2 <- Nov28_2[-c(8), ] #  victoria p, check eromero too

Nov28_2 %>% 
  # filter(Finished == TRUE) %>% 
  group_by(period) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count)) 

Nov28_2 %>% 
  # filter(Finished == TRUE) %>% 
  group_by(Profile.Name) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count)) 


## LIKERT for q6 
names(Nov28_2)[names(Nov28_2)=="Q6_1"] <- "Q6_1 I understand what resources are available to support my use of Trellis."
names(Nov28_2)[names(Nov28_2)=="Q6_2"] <- "Q6_2 I continue to feel supported in my use of Trellis."
names(Nov28_2)[names(Nov28_2)=="Q6_3"] <- "Q6_3 I am able to get answers to my questions when I need them through Trellis resources."

q6_plot <- plot(likert(Nov28_2[,26:28]), ordered = FALSE, wrap= 40) # this looks good. but add more to it. 

q6_plot <- q6_plot + labs(title = "Support for and knowledge of Trellis",
                          subtitle = "Question 6 - full data") 

print(q6_plot)


Nov28_2 %>% 
  mutate(across(.cols = 26:28,
                .fns = ~ ifelse(is.na(.x) == TRUE, 0, 1))) %>% 
  mutate(num_na_Q6 = `Q6_1 I understand what resources are available to support my use of Trellis.` +
           `Q6_2 I continue to feel supported in my use of Trellis.` + 
           `Q6_3 I am able to get answers to my questions when I need them through Trellis resources.`) %>% 
  # filter(num_na_Q1 < 3)
  count(num_na_Q6 == 3)

## Q8
# Q8_1[Q8_1==""] <- NA
# Q8_2[Q8_2==""] <- NA
# Q8_3[Q8_3==""] <- NA
# Q8_5[Q8_5==""] <- NA
# Q8_6[Q8_6==""] <- NA
# Q8_7[Q8_7==""] <- NA
# 
Nov28_2 %>%
  # group_by(Company) %>%
  summarise_at(18:25, funs(sum(is.na(.))))
Nov28_2 %>% 
  mutate(across(.cols = 18:25,
                .fns = ~ ifelse(is.na(.x) == TRUE, 0, 1))) %>% 
  mutate(num_na_Q8 = `Q8_1` + `Q8_2` + `Q8_3` +  `Q8_5` + `Q8_6` + `Q8_7`) %>% 
  # filter(num_na_Q1 < 3)
  count(num_na_Q8 >= 6)

names(Nov28_2)[names(Nov28_2)=="Q8_1"] <- "Q8_1 Technical Issue Form"
names(Nov28_2)[names(Nov28_2)=="Q8_2"] <- "Q8_2 Monthly Trellis Demos"
names(Nov28_2)[names(Nov28_2)=="Q8_3"] <- "Q8_3 Trellis User Community Meetings"
names(Nov28_2)[names(Nov28_2)=="Q8_5"] <- "Q8_4 Asking my peers"
names(Nov28_2)[names(Nov28_2)=="Q8_6"] <- "Q8_5 Trellis Teams (MS Teams)"
# names(Nov28_2)[names(Nov28_2)=="Q8_"] <- "Q8_6 Aware of None of these"
names(Nov28_2)[names(Nov28_2)=="Q8_7"] <- "Q8_7 None of these"
names(Nov28_2)[names(Nov28_2)=="Q8_8"] <- "Q8_6 Ask Trellis team members directly"
names(Nov28_2)[names(Nov28_2)=="Q8_8_TEXT"] <- "Q8_text Team Member"
# names(April25_2)[names(April25_2)=="Q7.2_3"] <- "Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)"

Nov28_2 <- Nov28_2 %>%
  relocate("Q8_7 None of these", .after = "Q8_text Team Member")
#count missing
# 
Q8_1 <- Nov28_2 %>%
  pivot_longer(cols = c("Q8_1 Technical Issue Form","Q8_2 Monthly Trellis Demos",
                        "Q8_3 Trellis User Community Meetings","Q8_4 Asking my peers",
                        "Q8_5 Trellis Teams (MS Teams)", "Q8_6 Ask Trellis team members directly",
                        "Q8_7 None of these"),
               names_to = "tools",
               values_to = "response") %>%
  # drop_na("Tech Issues Form") %>%
  distinct() %>%
  dplyr::select(period, tools, response, Profile.Name)
# 
# Q8_1 %>%
#   filter(response != "") %>% 
#   count(response) 
# 
# # Q7_1[Q7_1==""] <- NA
# 
# Q8_1 %>% 
#   filter(response != "") %>% 
#   ggplot(aes(factor(response))) +
#   # drop_na() +
#   geom_bar(stat="count", position = "dodge") + 
#   scale_fill_brewer(palette = "Set1")

# Q8graph <- April25_2[,75:80] # these are the columns of created Q8_val1 through 6


# pivot the table longer
# long_Q8 <- Q8graph %>% 
#   pivot_longer(everything(), names_to = "question", values_to = "response", values_drop_na = TRUE)

# replace with NA
Q8_1[Q8_1==""] <- NA
Q8_1 <- Q8_1 %>%
  dplyr::mutate(response = dplyr::recode(response, "Ask Trellis team members directly. Please share who you most often contact below." = 'Ask Trellis team members'))
# create a table to calculate percentages
Q8_pct <- Q8_1%>% 
  drop_na() %>% 
  group_by(response) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count)) %>% 
  mutate(response = factor(response, 
                           levels = c("Technical Issue Form", "Asking my peers ", 
                                      "Monthly Trellis Demos","Trellis Teams (MS Teams)",
                                      "Trellis User Community Meetings", 
                                      "Ask Trellis team members", 
                                      "None of these" )))

Q8_pct

Q8graph <- 
  ggplot(Q8_pct, aes(response,count, fill = response)) +
  geom_bar(stat='identity') +
  labs(x = "", y = "") +
  # coord_flip() +
  theme(legend.position="none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count)))) +
  xlab("") + 
  ylab("Count") 


Q8graph <- Q8graph + labs(title = "Which of these tools do you use to get help with Trellis?",
                          subtitle = "Question 8 - full data") + 
  theme(legend.position="none")

print(Q8graph)

# # longer Q8 with fill
# longer_Q8 <- April25_2[, c(61, 68, 75, 76,77,78,79,80)]
# longer_Q8_2 <- longer_Q8 %>% 
#   pivot_longer(cols = c(3:8), names_to = "question", values_to = "response", values_drop_na = TRUE) %>% 
#   mutate(response = factor(response, 
#                            levels = c("24/7 ", "Asking my peers ", "Drop in hours/Online Training ", 
#                                       "Knowledge Articles ",
#                                       "Trellis Teams chat", "Trellis user groups ", "None of these")))
# replace with NA
# longer_Q8_2[longer_Q8_2==""] <- NA

# for the profile name division
options(dplyr.summarise.inform = FALSE)
Q8_pct2 <- Q8_1 %>%
  drop_na() %>%
  group_by(response, Profile.Name) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

Q8graph2 <- Q8_pct2 %>%
  ggplot(aes(response,count, fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  # theme(axis.title.y = element_text(size = 9)) +
  xlab("") +
  ylab("Count")

Q8graph2 <- Q8graph2 + labs(title = "Which of these tools do you use to get help with Trellis?",
                            subtitle = "Question 8 - full data", fill= "Profile Name")
print(Q8graph2)


Q8_longer <- Nov28_2 %>% 
    pivot_longer(cols = c("Q8_1 Technical Issue Form","Q8_2 Monthly Trellis Demos",
                          "Q8_3 Trellis User Community Meetings","Q8_4 Asking my peers",
                          "Q8_5 Trellis Teams (MS Teams)", "Q8_6 Ask Trellis team members directly",
                          "Q8_7 None of these"),
                 names_to = "tools",
                 values_to = "response") %>%
    # drop_na("Tech Issues Form") %>%
    distinct() %>%
  pivot_longer(cols = c(Events, ExternalPartners, MarketingSF, SchedulingNotes, ServiceDesk, SAFER),
               names_to = "group",
               names_prefix = "counter.",
               values_to = "product") %>%
  dplyr::select(period, tools, response, product) %>% 
  drop_na() %>% 
# %>%
#   group_by(response) %>%
#   summarize(count = n()) %>%  # count records by species
#   # mutate(pct = count/sum(count)) %>%
  mutate(response = factor(response,
                           levels = c("Technical Issue Form", "Asking my peers ",
                                      "Monthly Trellis Demos","Trellis Teams (MS Teams)",
                                      "Trellis User Community Meetings",
                                      "Ask Trellis team members",
                                      "None of these" ))) 
# Q8_products <- Nov28_2 %>%
#   pivot_longer(cols = c(Events, ExternalPartners, MarketingSF, SchedulingNotes, ServiceDesk, SAFER),
#                names_to = "group",
#                names_prefix = "counter.",
#                values_to = "product") %>%
  # unique()
Q8_pct2 <- Q8_longer %>%
  filter(response != "") %>% 
  filter(product != "") %>%
  group_by(response, product) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

q8_plota <- Q8_pct2 %>%
  # count how often each class occurs in each sample.
  # drop_na(product) %>%
  count(response, product, count)%>%
  # group_by(response)%>%
  # # drop_na(response) %>%
  mutate(pct = count/sum(count)) %>% 
  ggplot(aes(x = response, y = count, fill = product)) +
  geom_bar(aes(fill = product), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set3") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3)


q8_plota <- q8_plota + labs(title = "Tools used to resolve issues",
                                  subtitle = "Question 8 - full data", fill = "Product Name")

print(q8_plota) # not a good graph because it is double counting a whole lot of people

# graph with time breakdown 
Q8_pct3 <- Q8_1 %>% 
  drop_na() %>% 
  group_by(period, response) %>% 
  summarize(count = n()) %>% 
  mutate(pct = count/sum(count))

Q8_pct3

Q8graph3 <- Q8_pct3 %>% 
  ggplot(aes(response,count, fill = period)) +
  geom_bar(aes(fill = period), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Set2") + 
  xlab("") + 
  ylab("Count") 

Q8graph3 <- Q8graph3 + labs(title = "Which of these tools do you use to get help with Trellis?",
                            subtitle = "Question 8 - full data", fill= "Period")  

print(Q8graph3)

# Likert Q10
# rename questions for Q10
names(Nov28_2)[names(Nov28_2)=="Q10_1"] <- "Q10_1 I feel comfortable using Trellis."
names(Nov28_2)[names(Nov28_2)=="Q10_2"] <- "Q10_2 Trellis makes it easy for me to access the information I need to do my job."

# check for missing values
Nov28_2 %>%
  dplyr::select(starts_with("Q10_")) %>%
  is.na %>%
  colSums

# Nov28_2 %>% 
#   dplyr::select(`Q10_1 I feel comfortable using Trellis.`) %>% 
#   mutate(NA_10_1 = sum(is.na))

# LIKERT for q10
q10_plot <- plot(likert(Nov28_2[,29:30]), ordered = T, wrap= 40) # this looks good. but add more to it. 
q10_plot <- q10_plot + labs(title = "Ease of Use",
                            subtitle = "Question 10 - full data") 
q10_plot

# graph with period for Q10_1
q10_1_plot <- Nov28_2%>%
  # count how often each class occurs in each sample.
  count(period,`Q10_1 I feel comfortable using Trellis.`)%>%
  group_by(`Q10_1 I feel comfortable using Trellis.`)%>%
  drop_na(`Q10_1 I feel comfortable using Trellis.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q10_1 I feel comfortable using Trellis.`, y = n, fill = period)) +
  geom_bar(aes(fill = period), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set2") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q10_1_plot <- q10_1_plot + labs(title = "I feel comfortable using Trellis",
                                subtitle = "Question 10_1 - full data", fill = "Period")

print(q10_1_plot) #survivor bias

# Q10_1 without Profile Name but by products
Q10_1_longer <- Nov28_2 %>%
  pivot_longer(cols = c(Events, ExternalPartners, MarketingSF, SchedulingNotes, 
                        ServiceDesk, SAFER),
               names_to = "group",
               names_prefix = "counter.",
               values_to = "product") %>%
  unique() 


q10_1_plota <- Q10_1_longer %>%
  # count how often each class occurs in each sample.
  drop_na(product) %>%
  count(product,`Q10_1 I feel comfortable using Trellis.`)%>%
  group_by(`Q10_1 I feel comfortable using Trellis.`)%>%
  drop_na(`Q10_1 I feel comfortable using Trellis.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q10_1 I feel comfortable using Trellis.`, y = n, fill = product)) +
  geom_bar(aes(fill = product), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set3") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q10_1_plota <- q10_1_plota + labs(title = "I feel comfortable using Trellis",
                                subtitle = "Question 10_1 - full data", fill = "Product Name")

print(q10_1_plota) #survivor bias

### job easier by period

#   dplyr::select(counter.Service.Desk, counter.Scheduling.Notes, 
#                 counter.Events, counter.Marketing...SF, counter.External.Partners, 
#                 counter.SAFER) %>% 
#   rename_with(.fn= ~str_remove(.x, "counter.") %>%
#                 str_replace("\\.", " ")) %>% 
#   rename(`Marketing in SF` = `Marketing ..SF`) 


q10_2_plot <- Nov28_2 %>%   # count how often each class occurs in each sample.
  count(period, `Q10_2 Trellis makes it easy for me to access the information I need to do my job.`)%>%
  group_by(`Q10_2 Trellis makes it easy for me to access the information I need to do my job.`)%>%
  drop_na(`Q10_2 Trellis makes it easy for me to access the information I need to do my job.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q10_2 Trellis makes it easy for me to access the information I need to do my job.`, y = n, fill = period)) +
  geom_bar(aes(fill = period), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set2") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q10_2_plot <- q10_2_plot + labs(title = "Easy Access to Information",
                                subtitle = "Question 10_2 - full data", fill = "Period")

print(q10_2_plot) #survivor bias

# job easier by product
# Q10_1_longer <- Nov28_2 %>%
#   pivot_longer(cols = c(Events, ExternalPartners, MarketingSF, SchedulingNotes, ServiceDesk, SAFER),
#                names_to = "group",
#                names_prefix = "counter.",
#                values_to = "product") %>%
#   unique() 

q10_2_plot2 <- Q10_1_longer %>%
  drop_na(product) %>%
  # count how often each class occurs in each sample.
  count(product, `Q10_2 Trellis makes it easy for me to access the information I need to do my job.`)%>%
  group_by(`Q10_2 Trellis makes it easy for me to access the information I need to do my job.`)%>%
  drop_na(`Q10_2 Trellis makes it easy for me to access the information I need to do my job.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q10_2 Trellis makes it easy for me to access the information I need to do my job.`, 
             y = n, fill = product)) +
  geom_bar(aes(fill = product), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set3") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q10_2_plot2 <- q10_2_plot2 + labs(title = "Easy Access to Information",
                                  subtitle = "Question 10_2 - full data", fill = "Product Name")

print(q10_2_plot2) #  850 by 479 

# job easier by profile
q10_2_plot2b <- Nov28_2%>%
  # count how often each class occurs in each sample.
  count(Profile.Name, `Q10_2 Trellis makes it easy for me to access the information I need to do my job.`)%>%
  group_by(`Q10_2 Trellis makes it easy for me to access the information I need to do my job.`)%>%
  drop_na(`Q10_2 Trellis makes it easy for me to access the information I need to do my job.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q10_2 Trellis makes it easy for me to access the information I need to do my job.`, 
             y = n, fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  # scale_fill_brewer(palette = "Set2") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q10_2_plot2b <- q10_2_plot2b + labs(title = "Easy Access to Information",
                                  subtitle = "Question 10_2 - full data", fill = "Profile Name")

print(q10_2_plot2b) #survivor bias
##
# Likert Q12
# rename questions for Q12
names(Nov28_2)[names(Nov28_2)=="Q12_1"] <- "Q12_1 I believe access to the Trellis data allows me to provide better service."
names(Nov28_2)[names(Nov28_2)=="Q12_2"] <- "Q12_2 I would recommend Trellis capabilities to someone else."

# LIKERT for q12
q12_plot <- plot(likert(Nov28_2[,38:39]), ordered = T, wrap= 40) # this looks good. but add more to it. 
q12_plot <- q12_plot + labs(title = "Value of Trellis",
                            subtitle = "Question 12 - full data") 
q12_plot

Nov28_2 %>%
     dplyr::select(starts_with("Q12_")) %>%
     is.na %>%
     colSums

# graph with period for Q12_1 with period
q12_1_plot <- Nov28_2%>%
  # count how often each class occurs in each sample.
  count(period,`Q12_1 I believe access to the Trellis data allows me to provide better service.`)%>%
  group_by(`Q12_1 I believe access to the Trellis data allows me to provide better service.`)%>%
  drop_na(`Q12_1 I believe access to the Trellis data allows me to provide better service.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q12_1 I believe access to the Trellis data allows me to provide better service.`, y = n, fill = period)) +
  geom_bar(aes(fill = period), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set2") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q12_1_plot <- q12_1_plot + labs(title = "Trellis helps me provide better service",
                                subtitle = "Question 12_1 - full data", fill = "Period")

print(q12_1_plot) #survivor bias

# # q 12_1 with profile Name
q12_1_plota <- Nov28_2%>%
  # count how often each class occurs in each sample.
  count(Profile.Name,`Q12_1 I believe access to the Trellis data allows me to provide better service.`)%>%
  group_by(`Q12_1 I believe access to the Trellis data allows me to provide better service.`)%>%
  drop_na(`Q12_1 I believe access to the Trellis data allows me to provide better service.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q12_1 I believe access to the Trellis data allows me to provide better service.`, y = n, fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  # scale_fill_brewer(palette = "Set2") +
  xlab("") +
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3)

q12_1_plota <- q12_1_plota + labs(title = "Trellis helps me provide better service",
                                  subtitle = "Question 12_1 - full data", fill = "Profile Name")

print(q12_1_plota) #survivor bias

# Q10_1_longer <- Nov28_2 %>%
#   pivot_longer(cols = c(Events, ExternalPartners, MarketingSF, SchedulingNotes, 
#                         ServiceDesk, SAFER),
#                names_to = "group",
#                names_prefix = "counter.",
#                values_to = "product") %>%
#   unique() 

# plot of 12_1 with product
q12_1_plota <- Q10_1_longer %>%
  # count how often each class occurs in each sample.
  drop_na(product) %>%
  count(product,Q12_1)%>%
  group_by(Q12_1)%>%
  drop_na(Q12_1) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = Q12_1, y = n, fill = product)) +
  geom_bar(aes(fill = product), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set3") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q12_1_plota <- q12_1_plota + labs(title = "Trellis helps me provide better service",
                                  subtitle = "Question 12_1 - full data", fill = "Product Name")

print(q12_1_plota) #survivor bias
### Q12_2 with period
q12_2_plot <- Nov28_2%>%
  # count how often each class occurs in each sample.
  count(period, `Q12_2 I would recommend Trellis capabilities to someone else.`)%>%
  group_by(`Q12_2 I would recommend Trellis capabilities to someone else.`)%>%
  drop_na(`Q12_2 I would recommend Trellis capabilities to someone else.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q12_2 I would recommend Trellis capabilities to someone else.`, y = n, fill = period)) +
  geom_bar(aes(fill = period), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set2") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q12_2_plot <- q12_2_plot + labs(title = "Would Recommend Trellis",
                                subtitle = "Question 12_2 - full data", fill = "Period")

print(q12_2_plot) #survivor bias

# job easier by profile
q12_2_plot2 <- Nov28_2%>%
  # count how often each class occurs in each sample.
  count(Profile.Name, `Q12_2 I would recommend Trellis capabilities to someone else.`)%>%
  group_by(`Q12_2 I would recommend Trellis capabilities to someone else.`)%>%
  drop_na(`Q12_2 I would recommend Trellis capabilities to someone else.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q12_2 I would recommend Trellis capabilities to someone else.`, 
             y = n, fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  # scale_fill_brewer(palette = "Set3") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q12_2_plot2 <- q12_2_plot2 + labs(title = "Would Recommend Trellis",
                                  subtitle = "Question 12_2 - full data", fill = "Profile Name")

print(q12_2_plot2) 
# plot of 12_1 with product
q12_2_plota <- Q10_1_longer %>%
  # count how often each class occurs in each sample.
  drop_na(product) %>%
  count(product,Q12_2)%>%
  group_by(Q12_2)%>%
  drop_na(Q12_2) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = Q12_2, y = n, fill = product)) +
  geom_bar(aes(fill = product), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set3") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q12_2_plota <- q12_2_plota + labs(title = "Would Recommend Trellis",
                                  subtitle = "Question 12_2 - full data", fill = "Product Name")
print(q12_2_plota)
# Question 15
names(Nov28_2)[names(Nov28_2)=="Q15_1"] <- "Q15_1 Case/Notes"
names(Nov28_2)[names(Nov28_2)=="Q15_2"] <- "Q15_2 Enrollments"
names(Nov28_2)[names(Nov28_2)=="Q15_3"] <- "Q15_3 EPR"
names(Nov28_2)[names(Nov28_2)=="Q15_4"] <- "Q15_4 Appointments"
names(Nov28_2)[names(Nov28_2)=="Q15_5"] <- "Q15_5 Event Attendance"
names(Nov28_2)[names(Nov28_2)=="Q15_7"] <- "Q15_7 Emails Received"
names(Nov28_2)[names(Nov28_2)=="Q15_6"] <- "Q15_6 None of these"

# names(April25_2)[names(April25_2)=="Q7.2_3"] <- "Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)"

Nov28_2 <- Nov28_2 %>%
  relocate("Q15_6 None of these", .after = "Q15_7 Emails Received")

Q15 <- Nov28_2 %>% 
  pivot_longer(cols = c("Q15_1 Case/Notes", "Q15_2 Enrollments", "Q15_3 EPR", "Q15_4 Appointments",
                        "Q15_5 Event Attendance","Q15_7 Emails Received","Q15_6 None of these"),
               names_to = "tools", 
               values_to = "response") %>% 
  # drop_na("Tech Issues Form") %>% 
  distinct() %>% 
  dplyr::select(period, tools, response, Profile.Name)

Q15 %>%
  filter(response != "") %>% 
  count(response) 

Q15 %>% 
  filter(response != "") %>% 
  ggplot(aes(factor(response), fill=response)) +
  # drop_na() +
  theme(legend.position="none") +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set3")


# replace with NA
# Q15[Q15==""] <- NA
# # check for missing values
# Nov28_2 %>%
#   dplyr::select(starts_with("Q15_")) %>%
#   is.na %>%
#   colSums
# Nov28_2$`Q15_1 Case/Notes`[Nov28_2$`Q15_1 Case/Notes`==""] <- NA
# Nov28_2$`Q15_2 Enrollments`[Nov28_2$`Q15_2 Enrollments`==""] <- NA
# Nov28_2$`Q15_3 EPR`[Nov28_2$`Q15_3 EPR`==""] <- NA
# Nov28_2$`Q15_4 Appointments`[Nov28_2$`Q15_4 Appointments`==""] <- NA
# Nov28_2$`Q15_5 Event Attendance`[Nov28_2$`Q15_5 Event Attendance`==""] <- NA
# Nov28_2$`Q15_6 None of these`[Nov28_2$`Q15_6 None of these`==""] <- NA
# Nov28_2$`Q15_7 Emails Received`[Nov28_2$`Q15_7 Emails Received`==""] <- NA

Nov28_2 %>% 
  mutate(across(.cols = 31:37,
                .fns = ~ ifelse(is.na(.x) == TRUE, 0, 1))) %>% 
  mutate(num_na_Q15 = `Q15_1 Case/Notes` + `Q15_2 Enrollments` + `Q15_3 EPR` + `Q15_4 Appointments` +
           `Q15_5 Event Attendance` + `Q15_6 None of these` + `Q15_7 Emails Received`) %>% 
  count(num_na_Q15 >= 7)
  # count(num_na_Q15 == 0)

Q15 <- Q15 %>%
  dplyr::mutate(response = dplyr::recode(response, "Early Progress Reports" = 'EPR'))
# create a table to calculate percentages
Q15_pct <- Q15%>% 
  # drop_na() %>% 
  group_by(response) %>%
  filter(response != "") %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count)) %>% 
  mutate(response = factor(response, 
                           levels = c("Cases/Notes", "Enrollments", "EPR", "Appointments",
                                      "Event Attendance","Emails Received","None of These")))
Q15_pct

Q15graph <- 
  ggplot(Q15_pct, aes(response,count, fill = response)) +
  geom_bar(stat='identity') +
  labs(x = "", y = "") +
  # coord_flip() +
  theme(legend.position="none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count)))) +
  xlab("") + 
  ylab("Count")


Q15graph <- Q15graph + labs(title = "Valuable Information within Trellis",
                          subtitle = "Question 15 - full data") + 
  theme(legend.position="none")

print(Q15graph)

# # longer Q8 with fill
# longer_Q8 <- April25_2[, c(61, 68, 75, 76,77,78,79,80)]
# longer_Q8_2 <- longer_Q8 %>% 
#   pivot_longer(cols = c(3:8), names_to = "question", values_to = "response", values_drop_na = TRUE) %>% 
#   mutate(response = factor(response, 
#                            levels = c("24/7 ", "Asking my peers ", "Drop in hours/Online Training ", 
#                                       "Knowledge Articles ",
#                                       "Trellis Teams chat", "Trellis user groups ", "None of these")))
# replace with NA
# longer_Q8_2[longer_Q8_2==""] <- NA

options(dplyr.summarise.inform = FALSE)
Q15_pct2 <- Q15 %>% 
  drop_na() %>% 
  group_by(response, Profile.Name) %>%
  filter(response != "") %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

Q15graph2 <- Q15_pct2 %>% 
  filter(response != "") %>% 
  ggplot(aes(response,count, fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  # theme(axis.title.y = element_text(size = 9)) +
  xlab("") + 
  ylab("Count") 

Q15graph2 <- Q15graph2 + labs(title = "Valuable Information through Trellis",
                            subtitle = "Question 15 - full data", fill= "Profile Name")  
print(Q15graph2)

# graph with time breakdown 
Q15_pct3 <- Q15 %>% 
  drop_na() %>% 
  group_by(period, response) %>% 
  filter(response != "") %>% 
  summarize(count = n()) %>% 
  mutate(pct = count/sum(count))

Q15_pct3

Q15graph3 <- Q15_pct3 %>% 
  # drop_na() %>% 
  ggplot(aes(response,count, fill = period)) +
  geom_bar(aes(fill = period), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Set2") + 
  xlab("") + 
  ylab("") 

Q15graph3 <- Q15graph3 + labs(title = "Valuable Information through Trellis",
                              subtitle = "Question 15 - full data", fill= "Period")  
print(Q15graph3)

#####
Q15_longer <- Nov28_2 %>% 
  pivot_longer(cols = c(`Q15_1 Case/Notes`, `Q15_2 Enrollments`, `Q15_3 EPR`, 
                        `Q15_4 Appointments`, `Q15_5 Event Attendance`, `Q15_6 None of these`, 
                        `Q15_7 Emails Received`),
               names_to = "tools",
               values_to = "response") %>%
  # drop_na("Tech Issues Form") %>%
  distinct() %>%
  pivot_longer(cols = c(Events, ExternalPartners, MarketingSF, SchedulingNotes, ServiceDesk, SAFER),
               names_to = "group",
               names_prefix = "counter.",
               values_to = "product") %>%
  dplyr::select(period, tools, response, product) %>% 
  drop_na()
  # %>% 
  # # %>%
  # #   group_by(response) %>%
  # #   summarize(count = n()) %>%  # count records by species
  # #   # mutate(pct = count/sum(count)) %>%
  # mutate(response = factor(response,
  #                          levels = c("Technical Issue Form", "Asking my peers ",
  #                                     "Monthly Trellis Demos","Trellis Teams (MS Teams)",
  #                                     "Trellis User Community Meetings",
  #                                     "Ask Trellis team members",
  #                                     "None of these" ))) 

Q15_pct2 <- Q15_longer %>%
  filter(response != "") %>% 
  filter(product != "") %>%
  group_by(response, product) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

q15_plota <- Q15_pct2 %>%
  # count how often each class occurs in each sample.
  # drop_na(product) %>%
  count(response, product, count)%>%
  # group_by(response)%>%
  # # drop_na(response) %>%
  mutate(pct = count/sum(count)) %>% 
  ggplot(aes(x = response, y = count, fill = product)) +
  geom_bar(aes(fill = product), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set2") + 
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3)

q15_plota <- q15_plota + labs(title = "Valuable ways to see information",
                            subtitle = "Question 15 - full data", fill = "Product Name")

print(q15_plota) # not a good graph because it is double counting a whole lot of people


#### upset plot for Q8 and Q 15 ####
# https://krassowski.github.io/complex-upset/articles/Examples_R.html
library(ComplexUpset)
library(UpSetR)
upset_plot_Q8 <- Nov28_2 %>% 
  dplyr::select(ResponseId, `Q8_1 Technical Issue Form`, `Q8_2 Monthly Trellis Demos`,
         `Q8_3 Trellis User Community Meetings`, `Q8_4 Asking my peers`, `Q8_5 Trellis Teams (MS Teams)`,
         `Q8_6 Ask Trellis team members directly`, `Q8_7 None of these`) %>% 
  dplyr::distinct() 
upset_plot_Q8 <- upset_plot_Q8 %>% 
  dplyr::select(-ResponseId) %>% 
  data.frame() %>%
  # t() %>% # transpose the result, ugh
  as_tibble()

upset_plot_Q8 <- upset_plot_Q8 %>% 
  dplyr::mutate(`Technical form` = ifelse(`Q8_1.Technical.Issue.Form` == "Technical Issue Form", 1, 0)) %>% 
  dplyr::mutate(`Trellis Demos` = ifelse(`Q8_2.Monthly.Trellis.Demos` == "Monthly Trellis Demos", 1, 0)) %>% 
  dplyr::mutate(`Comm user meetings` = ifelse(`Q8_3.Trellis.User.Community.Meetings` == "Trellis User Community Meetings", 1, 0)) %>% 
  dplyr::mutate(`Ask a peer` = ifelse(`Q8_4.Asking.my.peers` == "Asking my peers ", 1, 0)) %>% 
  dplyr::mutate(`MS Teams` = ifelse(`Q8_5.Trellis.Teams..MS.Teams.` == "Trellis Teams (MS Teams)", 1, 0)) %>% 
  dplyr::mutate(`Trellis team member` = ifelse(`Q8_6.Ask.Trellis.team.members.directly` == "Ask Trellis team members directly. Please share who you most often contact below.", 1, 0)) %>% 
  dplyr::mutate(`None of these` = ifelse(`Q8_7.None.of.these` == "None of these", 1, 0)) 

upset_plot_Q8 <- upset_plot_Q8 %>% 
  dplyr::select(`Technical form`, `Trellis Demos`, `Comm user meetings`, `Ask a peer`, `MS Teams`,
                `Trellis team member`, `None of these`)

upset_plot_Q8 %>% 
  filter(`Ask a peer` ==1) %>% 
  count()
  # Cat_SF_enroll <- Cat_SF_enroll %>%
  # dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
  # dplyr::mutate(Edit = ifelse(Edit.Sessions > 0, 1, 0)) %>% 
  # dplyr::mutate(Goal = ifelse(Goal.Sessions > 0, 1, 0))

set_vars <- c("Technical form", "Trellis Demos", "Comm user meetings", "Ask a peer",
              "MS Teams", "Trellis team member", "None of these")
upset_plot_Q8 %>% print(n = nrow(upset_plot_Q8))

Q8_upset_plot <- ComplexUpset::upset(data = upset_plot_Q8, intersect = set_vars, ) +
    labs(title = "Q8 responses")

Q8_upset_plot
# Q 15 
upset_plot_Q15 <- Nov28_2 %>% 
  dplyr::select(ResponseId, `Q15_1 Case/Notes`, `Q15_2 Enrollments`,
                `Q15_3 EPR`, `Q15_4 Appointments`, `Q15_5 Event Attendance`,
                `Q15_7 Emails Received`, `Q15_6 None of these`) %>% 
  dplyr::distinct() 
upset_plot_Q15 <- upset_plot_Q15 %>% 
  dplyr::select(-ResponseId) %>% 
  data.frame() %>%
  # t() %>% # transpose the result, ugh
  as_tibble()

upset_plot_Q15 <- upset_plot_Q15 %>% 
  dplyr::mutate(`Case/Notes` = ifelse(`Q15_1.Case.Notes` == "Cases/Notes", 1, 0)) %>% 
  dplyr::mutate(`Enrollments` = ifelse(`Q15_2.Enrollments` == "Enrollments", 1, 0)) %>% 
  dplyr::mutate(`EPR` = ifelse(`Q15_3.EPR` == "Early Progress Reports", 1, 0)) %>% 
  dplyr::mutate(`Appointments` = ifelse(`Q15_4.Appointments` == "Appointments", 1, 0)) %>% 
  dplyr::mutate(`Event Attendance` = ifelse(`Q15_5.Event.Attendance` == "Event Attendance", 1, 0)) %>% 
  dplyr::mutate(`Emails recieved` = ifelse(`Q15_7.Emails.Received` == "Emails Received", 1, 0)) %>% 
  dplyr::mutate(`None of these` = ifelse(`Q15_6.None.of.these` == "None of These", 1, 0)) 

upset_plot_Q15 <- upset_plot_Q15 %>% 
  dplyr::select(`Case/Notes`, Enrollments, EPR, Appointments, `Event Attendance`,
                `Emails recieved`, `None of these`) 

upset_plot_Q15 <- upset_plot_Q15 %>%
  dplyr::filter(`Case/Notes` == 1 | Enrollments == 1 | EPR == 1 | Appointments == 1 |
                  `Event Attendance` == 1 | `Emails recieved` == 1 | `None of these` == 1)

set_vars <- c("Case/Notes", "Enrollments", "EPR", "Appointments", "Event Attendance", 
              "Emails recieved", "None of these")

upset_plot_Q15 %>% print(n = nrow(upset_plot_Q15))

Q15_upset_plot <- ComplexUpset::upset(data = upset_plot_Q15, intersect = set_vars, ) +
  labs(title = "Q15 responses")

Q15_upset_plot

### upset plot for intersection of products used ###
Nov28_2 %>%
  summarise(across(.cols = c(counter.Events, counter.External.Partners, counter.Marketing...SF, counter.Reports,
                             counter.SAFER, counter.Scheduling.Notes, counter.Service.Desk),
                   .fns = sum, na.rm=TRUE)) %>%
  # function and tilda to modify the string and what you want to remove from it.
  rename_with(.fn= ~str_remove(.x, "counter.") %>%
                str_replace("\\.", " ")) %>%
  rename(`Marketing in SF` = `Marketing ..SF`) 

upset_plot_products <- Nov28_2 %>% 
  dplyr::select(ResponseId, counter.Service.Desk, counter.Scheduling.Notes, 
                counter.Events, counter.Marketing...SF, counter.External.Partners, 
                counter.SAFER, counter.Reports) %>% 
  dplyr::distinct() 

upset_plot_products <- upset_plot_products %>% 
  dplyr::select(-ResponseId) %>% 
  data.frame() %>%
  as_tibble()

upset_plot_products <- upset_plot_products %>% 
  dplyr::select(counter.Service.Desk, counter.Scheduling.Notes, 
                counter.Events, counter.Marketing...SF, counter.External.Partners, 
                counter.SAFER) %>% 
  rename_with(.fn= ~str_remove(.x, "counter.") %>%
                str_replace("\\.", " ")) %>% 
  rename(`Marketing in SF` = `Marketing ..SF`) 

# upset_plot_products[upset_plot_products==NA] <- 0
set_vars2 <- c("Service Desk","Scheduling Notes", "Events", "Marketing in SF","External Partners",
               "SAFER")

upset_plot_products %>% 
  # summarise(n=n())
  print(n = nrow(upset_plot_products))

sapply(X = upset_plot_products,
       FUN = table)

upset_plot_prods_graph <- ComplexUpset::upset(data = upset_plot_products, 
                                              intersect = set_vars2,
                                              ) +
  labs(title = "Product Usage Intersections")

upset_plot_prods_graph 



# output a new dataset with comments
Fall_survey_comments <- Nov28_2 %>% 
  dplyr::select(CreatedDate, allproducts, Q13) %>% 
  dplyr::filter(Q13 != "")

write.csv(Fall_survey_comments, "Fall2022_survey_comments.csv")

# subset of EPR data
# EPR_data <- Nov28_2 %>% 
#   dplyr::filter(`Q15_3 EPR` == "Early Progress Reports")
# # set_vars <- c("Appt", "Edit", "Goal")
# upset_plot_Q8 <- upset_plot_Q8 %>% 
#   filter(Appt== 1 | Edit ==1 |Goal==1) %>%
#   ungroup()
# Q7_1[Q7_1==""] <- NA

# ### col sums for questions
# April25_2$`Q3_1 My interaction with the Trellis team has been positive.`[April25_2$`Q3_1 My interaction with the Trellis team has been positive.`==""] <- NA
# 
# sum(is.na(April25_2$`Q3_1 My interaction with the Trellis team has been positive.`)) # 29 

# # Q4 
# #colSums(!is.na(dat))
# April25_2$Q4[April25_2$Q4==""] <- NA
# 
# sum(is.na(April25_2$Q4)) #29 
# 
# # Q8
# April25_2$Q8[April25_2$Q8==""] <- NA
# 
# sum(is.na(April25_2$Q8)) 

## lesson on 
# %in% only does exact match. under the hood, it is all.equal
# remotes::install_github(("MCMaurer/inclose"))
# library(inclose) # this uses tolerance for decimals. 
# unit testing
# continuous develping
df1 <- data.frame(ResponseId = c("R_1QuS", "R_2s0Z", "R_3pgw", "R_27xs","R_1jVV", "R_25zP", "R_3O1y"), 
                 Q1_1_visited = c("McDonalds",NA, "McDonalds", NA, NA, NA, "McDonalds"), 
                 Q1_2_visited = c("Wendys","Wendys", "Wendys", NA, NA, NA, "Wendys"), 
                 Q1_3_visited = c(NA, NA, NA, "Subway", "Subway", NA, "Subway"), 
                 Q2_1_food_is_good = c("Somewhat agree","Somewhat agree", "Somewhat agree", "Neither agree nor disagree",
                          "Strongly agree","Somewhat agree","Strongly agree"), 
                 Q2_2_price_is_good = c("Strongly disagree",NA, "Somewhat disagree", "Neither agree nor disagree",
                          "Strongly agree","Somewhat agree","Somewhat agree")
                 )
# df1 %>%
#   mutate(across(.fns = ~+(!is.na(.))))

df1 %>%
  mutate(across(.cols = c(2:4), is.na)) %>%
  summarise(across(.cols = c(2:4), ~ 1- sum(.x)/n()))
df1 %>%
 mutate(across(.cols = everything(), is.na)) %>%
 summarise(across(.cols = everything(), ~ 1- sum(.x)/n()))


sapply(df1, function(x) sum(is.na(x)))

df1_table_na <- as.data.frame(sapply(df1, function(x) sum(is.na(x))))

df2 <- df1 %>% 
  mutate(across(.cols = 2:6,
                .fns = ~ ifelse(is.na(.x) == TRUE, 0, 1)))
df2 %>% 
  mutate(num_na_Q1 = (Q1_1_visited) + (Q1_2_visited) + (Q1_3_visited)) %>% 
  # filter(num_na_Q1 < 3)
  count(num_na_Q1 ==3 )
