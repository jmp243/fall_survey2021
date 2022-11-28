# Jung Mee Park
# jmpark@email.arizona.edu
# Fall 2022 survey
# transpose questions
# 2022-11-28

# clear all 
rm(list=ls(all=TRUE)) 

#### load in libraries ####
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
# library(psych)
library(viridis)
library(here)
# library(flextable)
# library(devtools)
library(broom)
library(stringr)
# load library
library(tidyr)
library(ggthemes)
library(reshape2)
library(RColorBrewer)
library(scales)

# get wd
getwd()
setwd("~/Documents/Trellis/Survey/Fall2021/Fall2022")

# read in questions
questions <- read.csv("fall2022_survey_questions.csv", header = T)
transposed_qu <- t(questions)
print(transposed_qu)

# read in responses
# April12 <- read.csv("Trellis Program Survey - Spring 2022_April 12, 2022_11.20.csv", header = T)
Nov28 <- read.csv("Fall2022_survey_analysis.csv", header = T)
Nov28 <- Nov28 %>% 
  dplyr::select(-c(CreatedDate))
# merge in some SF data
SF_list <- read.csv("2022_Nov28_SFTrellisUsersByProduct.csv")
names(SF_list)

SF_list <- SF_list %>% 
  dplyr::select(2,3)

Nov28 <- left_join(Nov28, SF_list, by = c("RecipientEmail" = "Email.x"))

# check for NA's
Nov28 %>% 
  mutate(across(.cols = everything(), is.na)) %>% 
  summarise(across(.cols = everything(), ~ sum(.x)/n()))

# remove test email cases or blank emails
Nov28 <- Nov28[!(Nov28$RecipientEmail=="park.schlawin@gmail.com" | Nov28$RecipientEmail=="jmpark@arizona.edu"| Nov28$RecipientEmail=="jmpark@email.arizona.edu" | Nov28$RecipientEmail=="fkmiller@arizona.edu" | Nov28$RecipientEmail=="" ),]
sum(is.na(Nov28$RecipientEmail))

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

# Nov28 <- Nov28 %>% 
#   mutate(AZ_created_date = as.Date(AZ_created_time))

# create periods
# dec to now 2022 (this semester)
# july to end of november (2nd )
#
# April25 <- April25 %>%
#   mutate(
#     period = case_when(AZ_created_date < "2021-05-01" ~ "More than 12 months",
#                        AZ_created_date <= "2021-11-01" ~ "Six to 12 months",
#                        AZ_created_date <= "2022-04-01" ~ "One to 6 months",
#                        TRUE ~ "Less than 1 month"
#     )
#   )

Nov28 <- Nov28 %>%
  mutate(
    period = case_when(AZ_created_date <= "2021-11-01" ~ "More than two semesters",
                       AZ_created_date <= "2022-08-15" ~ "Last semester",
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
  count(period)

#### set up likert questions####
Nov28_2 <- Nov28 %>% 
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
Nov28_2$Profile.Name[Nov28_2$Profile.Name=="advising base"] <- "Advising Base"

# breakdown of respondents
Nov28_2 %>% 
  count(Profile.Name)
# 
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
# library(kableExtra)
Nov28_2 %>%
  summarise(across(.cols = c(counter.Events, counter.External.Partners, counter.Marketing...SF, counter.Reports,
                             counter.SAFER, counter.Scheduling.Notes, counter.Service.Desk),
                   .fns = sum, na.rm=TRUE)) %>%
  # function and tilda to modify the string and what you want to remove from it.
  rename_with(.fn= ~str_remove(.x, "counter.") %>%
                str_replace("\\.", " ")) %>%
  rename(`Marketing in SF` = `Marketing ..SF`) 
# %>%
#   kable()

# products_count <- t(sapply(April25_2[ , 45:51],    # Create table with counts
#                        function(x) tapply(x, April25_2[ , 6], sum)))
# products_count  
# rename questions for Q3
# names(April25_2)[names(April25_2)=="Q3_1"] <- "Q3_1 My interaction with the Trellis team has been positive."
# names(April25_2)[names(April25_2)=="Q3_2"] <- "Q3_2 My interactions with the Trellis team have been informative."
# names(April25_2)[names(April25_2)=="Q3_3"] <- "Q3_3 The training sessions prepared me to use Trellis."
# names(April25_2)[names(April25_2)=="Q3_4"] <- "Q3_4 Trellis staff supported me in my initial adoption of Trellis."


# LIKERT for q3
# adjust output to 1000 by 484
# q3_plot <- plot(likert(April25_2[,18:21]), ordered = F, wrap= 40) # this looks good. but add more to it. 
# q3_plot <- q3_plot + labs(title = "Interactions with Trellis staff",
#                           subtitle = "Question 3 - full data") 
# q3_plot

# count up by group
# , `Q3_2 My interactions with the Trellis team have been informative.`,
# `Q3_3 The training sessions prepared me to use Trellis.`, `Q3_4 Trellis staff supported me in my initial adoption of Trellis.`
# length(April25_2$`Q3_1 My interaction with the Trellis team has been positive.`)

# LIKERT for q4
# names(April12_2)[names(April12_2)=="Q4"] <- "Q4 How satisfied were you with your initial training in Trellis?"

# q4_plot <- plot(likert(April25_2[,22, drop=FALSE]), ordered = F, wrap= 40) # this looks good. but add more to it. 
# q4_plot <- q4_plot + labs(title = "Q4 How satisfied were you with your initial training in Trellis?",
#                           subtitle = "Question 4 - full data") 
# q4_plot

## fill response with another variable like Profile.Name


# April25_2%>%
#   # count how often each class occurs in each sample.
#   count(Profile.Name, Q4)%>% 
#   group_by(Q4)%>%
#   drop_na(Q4) %>% 
#   mutate(Percent = n /sum(n))%>%
#   ggplot(aes(x = Q4, y = Percent, fill = Profile.Name)) + 
#   coord_flip() +
#   geom_col(width=0.7)+
#   geom_text(aes(label = paste0(round(Percent * 100), '%')),
#             position = position_stack(vjust = 0.5))
# 
# 
# Q4graph <- April25_2 %>%
#   group_by(Profile.Name) %>% 
#   # mutate(Q4 = fct_infreq(Q4)) %>%
#   drop_na(Q4) %>% 
#   ggplot(aes(x = factor(Q4)), y = ..count.., fill = Profile.Name) +
#   geom_bar(aes(fill = Profile.Name)) +
#   # coord_flip() +
#   labs(x = "", y = "responses")
# 
# Q4graph <- Q4graph + labs(title = "How satisfied were you with your initial training in Trellis?",
#                           subtitle = "Question 4 - full data", fill = "Profile Name")
# 
# print(Q4graph)
# 
# # perhaps an overly complicated stacked count graph with percentages
# Q4graph2 <- April25_2%>%
#   # count how often each class occurs in each sample.
#   count(Profile.Name,Q4)%>%
#   group_by(Q4)%>%
#   drop_na(Q4) %>%
#   mutate(Percent = n /sum(n))%>%
#   ggplot(aes(x = Q4, y = n, fill = Profile.Name)) +
#   geom_bar(aes(fill = Profile.Name), stat = "identity") +
#   geom_col(width=0.7)+
#   scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   geom_text(aes(label = paste0(round(Percent * 100), '%')),
#             position = position_stack(vjust = 0.5)) +
#   xlab("Answer") + 
#   ylab("Count") 
# 
# Q4graph2 <- Q4graph2 + labs(title = "How satisfied were you with your initial training in Trellis?",
#                             subtitle = "Question 4 - full data", fill = "Profile Name")
# 
# print(Q4graph2)
# 
# ### periods added
# Q4graph3 <- April25_2%>%
#   # count how often each class occurs in each sample.
#   count(period,Q4)%>%
#   group_by(Q4)%>%
#   drop_na(Q4) %>%
#   mutate(Percent = n /sum(n))%>%
#   ggplot(aes(x = Q4, y = n, fill = period)) +
#   geom_bar(aes(fill = period), stat = "identity") +
#   geom_col(width=0.7)+
#   scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   scale_fill_brewer(palette = "Set2") + 
#   xlab("Answer") + 
#   ylab("Count") +
#   geom_text(aes(label = paste0(round(Percent * 100), '%')),
#             position = position_stack(vjust = 0.5)) 
# 
# Q4graph3 <- Q4graph3 + labs(title = "How satisfied were you with your initial training in Trellis?",
#                             subtitle = "Question 4 - full data", fill = "Period")
# 
# print(Q4graph3) #survivor bias

## LIKERT for q6 
names(Nov28_2)[names(Nov28_2)=="Q6_1"] <- "Q6_1 I understand what resources are available to support my use of Trellis."
names(Nov28_2)[names(Nov28_2)=="Q6_2"] <- "Q6_2 I continue to feel supported in my use of Trellis."
names(Nov28_2)[names(Nov28_2)=="Q6_3"] <- "Q6_3 I am able to get answers to my questions when I need them through Trellis resources."

q6_plot <- plot(likert(Nov28_2[,26:28]), ordered = T, wrap= 40) # this looks good. but add more to it. 

q6_plot <- q6_plot + labs(title = "Suport for and knowledge of Trellis",
                          subtitle = "Question 6 - full data") 

print(q6_plot)

## Q7 - two part analysis 

# relocate columns 
# April25_2 <- April25_2 %>% 
#   relocate(Q7.2_1, .after = Q7.1_1)
# 
# April25_2 <- April25_2 %>% 
#   relocate(Q7.2_2, .after = Q7.1_2)
# 
# April25_2 <- April25_2 %>% 
#   relocate(Q7.2_3, .after = Q7.1_3)
# 
# April25_2 <- April25_2 %>% 
#   relocate(Q7.2_4, .after = Q7.1_4)
# 
# April25_2 <- April25_2 %>% 
#   relocate(Q7.2_5, .after = Q7.1_5)
# 
# April25_2 <- April25_2 %>% 
#   relocate(Q7.2_6, .after = Q7.1_6)

# pairwise results don't make a ton of sense

# # rename the questions
# names(April25_2)[names(April25_2)=="Q7.1_1"] <- "Q7.1_1 Aware of Release notes"
# names(April25_2)[names(April25_2)=="Q7.1_2"] <- "Q7.1_2 Aware of Bi weekly live demonstrations (i.e. Sprint demos)"
# names(April25_2)[names(April25_2)=="Q7.1_3"] <- "Q7.1_3 Aware of Communities of practice (i.e. Trellis user groups)"
# names(April25_2)[names(April25_2)=="Q7.1_4"] <- "Q7.1_4 Aware of Monthly information sessions"
# names(April25_2)[names(April25_2)=="Q7.1_5"] <- "Q7.1_5 Aware of Newsletter/Digest"
# names(April25_2)[names(April25_2)=="Q7.1_6"] <- "Q7.1_6 Aware of None of these"
# names(April25_2)[names(April25_2)=="Q7.2_1"] <- "Q7.2_1 Have used Release notes"
# names(April25_2)[names(April25_2)=="Q7.2_2"] <- "Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)"
# names(April25_2)[names(April25_2)=="Q7.2_3"] <- "Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)"
# names(April25_2)[names(April25_2)=="Q7.2_4"] <- "Q7.2_4 Have attended Monthly information sessions"
# names(April25_2)[names(April25_2)=="Q7.2_5"] <- "Q7.2_5 Have used Newsletter/Digest"
# names(April25_2)[names(April25_2)=="Q7.2_6"] <- "Q7.2_6 Have attended None of these"

# # release notes columns 26, 27
# April25_2 %>% 
#   # count how often each class occurs in each sample.
#   count(`Q7.1_1 Aware of Release notes`, `Q7.2_1 Have used Release notes`)%>%
#   # count(`Q7.1_1`, `Q7.2_1`)%>% 
#   drop_na() %>% 
#   mutate(pct = n /sum(n)) 
# # %>%
# #   geom_boxplot()
# #   geom_text(aes(label = paste0(round(pct * 100), '%')),
# #             position = position_stack(vjust = 0.5))
# April25_2 <- April25_2 %>% 
#   mutate(Q7.1_1_modified = ifelse(`Q7.2_1 Have used Release notes` == "I have used/attended.",
#                                   "I am aware.", 
#                                   `Q7.1_1 Aware of Release notes`))




# Q7_1 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_1_modified, `Q7.2_1 Have used Release notes`),
#                names_to = "col_name_release", 
#                values_to = "release_notes") %>% 
#   drop_na("release_notes") %>% 
#   distinct() 
# 
# Q7_1 %>% 
#   count(release_notes) 
# 
# # Q7_1[Q7_1==""] <- NA
# 
# Q7_1 %>% 
#   filter(release_notes != "") %>% 
#   ggplot(aes(factor(release_notes))) +
#   # drop_na() +
#   geom_bar(stat="count", position = "dodge") + 
#   scale_fill_brewer(palette = "Set1")
# 
# # facet wrap by column. All questions in 1, all questions in the other. 
# # Q7 part 2 sprint demos
# April25_2 %>% 
#   # count how often each class occurs in each sample.
#   count(`Q7.1_2 Aware of Bi weekly live demonstrations (i.e. Sprint demos)`, `Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)`)%>%
#   drop_na() %>% 
#   mutate(pct = n /sum(n)) 
# 
# April25_2 <- April25_2 %>% 
#   mutate(Q7.1_2_modified = ifelse(`Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)` == "I have used/attended.",
#                                   "I am aware.", 
#                                   `Q7.1_2 Aware of Bi weekly live demonstrations (i.e. Sprint demos)`))
# Q7_2 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_2_modified, `Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)`),
#                names_to = "col_name_demos", 
#                values_to = "sprint_demos") %>% 
#   drop_na("sprint_demos") %>% 
#   distinct() 
# 
# 
# Q7_2 %>% 
#   count(sprint_demos) 
# 
# Q7_2 %>% 
#   filter(sprint_demos != "") %>% 
#   ggplot(aes(factor(sprint_demos))) +
#   geom_bar(stat="count", position = "dodge") + 
#   scale_fill_brewer(palette = "Set1")
# 
# 
# # Q7 part 3 Trellis user groups
# April25_2 %>% 
#   # count how often each class occurs in each sample.
#   count(`Q7.1_3 Aware of Communities of practice (i.e. Trellis user groups)`, `Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)`)%>%
#   drop_na() %>% 
#   mutate(pct = n /sum(n)) 
# 
# April25_2 <- April25_2 %>% 
#   mutate(Q7.1_3_modified = ifelse(`Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)` == "I have used/attended.",
#                                   "I am aware.", 
#                                   `Q7.1_3 Aware of Communities of practice (i.e. Trellis user groups)`))
# Q7_3 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_3_modified, `Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)`),
#                names_to = "col_name_user", 
#                values_to = "user_groups") %>% 
#   drop_na("user_groups") %>% 
#   distinct() 
# 
# 
# Q7_3 %>% 
#   count(user_groups) 
# 
# Q7_3 %>% 
#   filter(user_groups != "") %>% 
#   ggplot(aes(factor(user_groups))) +
#   geom_bar(stat="count", position = "dodge") + 
#   scale_fill_brewer(palette = "Set1")
# 
# # Q7 part 4 Monthly information sessions
# April25_2 %>% 
#   # count how often each class occurs in each sample.
#   count(`Q7.1_4 Aware of Monthly information sessions`, `Q7.2_4 Have attended Monthly information sessions`)%>%
#   drop_na() %>% 
#   mutate(pct = n /sum(n)) 
# 
# April25_2 <- April25_2 %>% 
#   mutate(Q7.1_4_modified = ifelse(`Q7.2_4 Have attended Monthly information sessions` == "I have used/attended.",
#                                   "I am aware.", 
#                                   `Q7.1_4 Aware of Monthly information sessions`))
# Q7_4 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_4_modified, `Q7.2_4 Have attended Monthly information sessions`),
#                names_to = "col_name_monthly", 
#                values_to = "monthly") %>% 
#   drop_na("monthly") %>% 
#   distinct() 
# 
# 
# Q7_4 %>% 
#   count(monthly) 
# 
# Q7_4 %>% 
#   filter(monthly != "") %>% 
#   ggplot(aes(factor(monthly))) +
#   geom_bar(stat="count", position = "dodge") + 
#   scale_fill_brewer(palette = "Set1")
# 
# # Q7 part 5 Newsletter
# April25_2 %>% 
#   # count how often each class occurs in each sample.
#   count(`Q7.1_5 Aware of Newsletter/Digest`, `Q7.2_5 Have used Newsletter/Digest`)%>%
#   drop_na() %>% 
#   mutate(pct = n /sum(n)) 
# 
# April25_2 <- April25_2 %>% 
#   mutate(Q7.1_5_modified = ifelse(`Q7.2_5 Have used Newsletter/Digest` == "I have used/attended.",
#                                   "I am aware.", 
#                                   `Q7.1_5 Aware of Newsletter/Digest`))
# Q7_5 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_5_modified, `Q7.2_5 Have used Newsletter/Digest`),
#                names_to = "col_name_newsletter", 
#                values_to = "newsletter") %>% 
#   drop_na("newsletter") %>% 
#   distinct() 
# 
# 
# Q7_5 %>% 
#   count(newsletter) 
# 
# Q7_5 %>% 
#   filter(newsletter != "") %>% 
#   ggplot(aes(factor(newsletter))) +
#   geom_bar(stat="count", position = "dodge") + 
#   scale_fill_brewer(palette = "Set1")
# 
# # Q7 part 6 none
# April25_2 %>% 
#   # count how often each class occurs in each sample.
#   count(`Q7.1_6 Aware of None of these`, `Q7.2_6 Have attended None of these`)%>%
#   drop_na() %>% 
#   mutate(pct = n /sum(n)) 
# 
# April25_2 <- April25_2 %>% 
#   mutate(Q7.1_6_modified = ifelse(`Q7.2_6 Have attended None of these` == "I have used/attended.",
#                                   "I am aware.", 
#                                   `Q7.1_6 Aware of None of these`))
# Q7_6 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_6_modified, `Q7.2_6 Have attended None of these`),
#                names_to = "col_name_none", 
#                values_to = "none") %>% 
#   drop_na("none") %>% 
#   distinct() 
# 
# 
# Q7_6 %>% 
#   count(none) 
# 
# Q7_6 %>% 
#   filter(none != "") %>% 
#   ggplot(aes(factor(none))) +
#   geom_bar(stat="count", position = "dodge") + 
#   scale_fill_brewer(palette = "Set1")
# 
# # q7 new data.frame
# Q7 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_1_modified, `Q7.2_1 Have used Release notes`),
#                names_to = "col_name", 
#                values_to = "Release_Notes") %>% 
#   drop_na("Release_Notes") %>% 
#   distinct() 
# 
# Q7a <- Q7_1[, c(68, 69) ] # recipient email, dept, profile.name, period, colname, value
# 
# Q7 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_2_modified, `Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)`),
#                names_to = "col_name_demos", 
#                values_to = "sprint_demos") %>% 
#   drop_na("sprint_demos") %>% 
#   distinct() 
# 
# Q7b <- Q7_2[, c(69, 70) ] # recipient email, dept, profile.name, period, colname, value
# 
# Q7 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_3_modified, `Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)`),
#                names_to = "col_name_user", 
#                values_to = "user_groups") %>% 
#   drop_na("user_groups") %>% 
#   distinct() 
# 
# Q7c <- Q7_3[, c(70, 71) ] # recipient email, dept, profile.name, period, colname, value
# 
# Q7<- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_4_modified, `Q7.2_4 Have attended Monthly information sessions`),
#                names_to = "col_name_monthly", 
#                values_to = "monthly") %>% 
#   drop_na("monthly") %>% 
#   distinct() 
# 
# Q7d <- Q7_4[, c(71, 72) ] # recipient email, dept, profile.name, period, colname, value
# 
# Q7 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_5_modified, `Q7.2_5 Have used Newsletter/Digest`),
#                names_to = "col_name_newsletter", 
#                values_to = "newsletter") %>% 
#   drop_na("newsletter") %>% 
#   distinct() 
# Q7e <- Q7_5[, c(72, 73) ] # recipient email, dept, profile.name, period, colname, value
# 
# Q7 <- April25_2 %>% 
#   pivot_longer(cols = c(Q7.1_6_modified, `Q7.2_6 Have attended None of these`),
#                names_to = "col_name_none", 
#                values_to = "none") %>% 
#   drop_na("none") %>% 
#   distinct()
# 
# Q7f <- Q7_6[, c(73, 74) ] # recipient email, dept, profile.name, period, colname, value
# 
# # data %>% 
# #   mutate(q1_mod = ifelse...) %>% 
# #   select(-q1) %>% 
# #   pivot_longer(cols = everything(), names_to = "question", values_to = "answer") %>% 
# #   ggplot(aes(x = answer)) + 
# #   geom_bar() + 
# #   facet_wrap(vars(question), scales = "free_x", ncol=1) # scales = "free" 
# 
# # merge the Q7 files
# Q7 <- merge(Q7a, Q7b, by = 0, all = TRUE)
# Q7 <- merge(Q7, Q7c, by = 0, all = TRUE)
# Q7 <- merge(Q7, Q7d, by = 0, all = TRUE)
# Q7 <- merge(Q7, Q7e, by = 0, all = TRUE)
# Q7 <- merge(Q7, Q7f, by = 0, all = TRUE)
# 
# Q7_new <- subset(Q7, select= - c(1:5)) # recheck the numbers
# #
# # Q7_new2 <- Q7_new                              # Replicate data
# # Q7_new2$group <- factor(data_new$group,      # Reordering group factor levels
# #                          levels = c("B", "A", "C", "D"))
# 
# Trellis_resources <- c(
#   `monthly` = "Monthly Info Sessions",
#   `newsletter` = "Newsletters",
#   `release_notes` = "Release Notes",
#   `sprint_demos` = "Sprint Demos",
#   `user_groups` = "User Groups",
#   `none` = "None of These"
# )
# 
# Q7_new2 <- Q7_new %>%
#   pivot_longer(cols = c(release_notes, sprint_demos, user_groups, monthly, newsletter, none),
#                names_to = "question", values_to = "answer") 
# # %>% 
# #   mutate(question = as.factor(question), 
# #          levels = c("monthly", "newsletter", "release_notes", "sprint_demos", "user_groups", "none")) 
# 
# # #col_name_release, col_name_demos, col_name_user, col_name_monthly, col_name_newsletter, col_name_none
# # Q7_new2$question <- factor(Q7_new2$question,      # Reordering group factor levels
# #        levels = c("monthly", "newsletter", "release_notes", "sprint_demos", "user_groups", "none"))
# 
# Q7_new %>% 
#   pivot_longer(cols = c(none, release_notes, sprint_demos, user_groups, monthly, newsletter), 
#                names_to = "question", values_to = "answer") %>% 
#   mutate(question = factor(question, levels = c("monthly", "newsletter", "release_notes", "sprint_demos", "user_groups", "none"))) %>%
#   # mutate(question = factor(question),
#   #        levels = c("monthly", "newsletter", "release_notes", "sprint_demos", "user_groups", "none")) %>%
#   filter(answer != "") %>% 
#   ggplot(aes(x = factor(answer), fill = factor(answer))) + 
#   geom_bar(stat="count", position = "dodge") + 
#   facet_wrap(vars(question), scales = "free_x", ncol=3, labeller = as_labeller(Trellis_resources)) +  # scales = "free" 
#   scale_fill_brewer(palette = "Set2") + 
#   labs(title = "Question 7 - Usage of Trellis Resources",
#        subtitle = "full data", fill = "Answer") +
#   scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   xlab("Answer") + 
#   ylab("Count") 

# Q7_new2 %>% 
#   filter(answer != "") %>% 
#   ggplot(aes(x = factor(answer), fill = factor(answer))) + 
#   geom_bar(stat="count", position = "dodge") + 
#   facet_wrap(vars(question), scales = "free_x", ncol=3, labeller = as_labeller(Trellis_resources)) +  # scales = "free" 
#   scale_fill_brewer(palette = "Set2") + 
#   scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   labs(title = "Question 7 - Usage of Trellis Resources",
#        subtitle = "full data", fill = "Answer") +
#   xlab("Answer") + 
#   ylab("Count") 


## Q8
# Nov28_2$Q8[Nov28_2$Q8==""] <- NA
# 
# sum(is.na(Nov28_2$Q8)) 

# disperse answers into long, currently comma separated

# April25_2[c('Q8_val1', 'Q8_val2', 'Q8_val3', 'Q8_val4', 'Q8_val5', 'Q8_val6', 
#             'Q8_val7')] <- str_split_fixed(Nov28_2$Q8, ',', 7)
# # April12_3 <- April12_2 %>% separate(Q8, c('Q8_val1', 'Q8_val2', 'Q8_val3', 'Q8_val4', 'Q8_val5', 'Q8_val6'))

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

Q8_1 %>%
  filter(response != "") %>% 
  count(response) 

# Q7_1[Q7_1==""] <- NA

Q8_1 %>% 
  filter(response != "") %>% 
  ggplot(aes(factor(response))) +
  # drop_na() +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

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
  xlab("Answer") + 
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
  xlab("Answer") + 
  ylab("Count") 

Q8graph2 <- Q8graph2 + labs(title = "Which of these tools do you use to get help with Trellis?",
                            subtitle = "Question 8 - full data", fill= "Profile Name")  
print(Q8graph2)

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
  xlab("Answer") + 
  ylab("Count") 

Q8graph3 <- Q8graph3 + labs(title = "Which of these tools do you use to get help with Trellis?",
                            subtitle = "Question 8 - full data", fill= "Period")  
print(Q8graph3)

# Likert Q10
# rename questions for Q10
names(Nov28_2)[names(Nov28_2)=="Q10_1"] <- "Q10_1 I feel comfortable using Trellis."
names(Nov28_2)[names(Nov28_2)=="Q10_2"] <- "Q10_2 Trellis makes it easy for me to access the information I need to do my job."

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
  xlab("Answer") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q10_1_plot <- q10_1_plot + labs(title = "I feel comfortable using Trellis",
                                subtitle = "Question 10_1 - full data", fill = "Period")

print(q10_1_plot) #survivor bias

### job easier by period
q10_2_plot <- Nov28_2%>%
  # count how often each class occurs in each sample.
  count(period, `Q10_2 Trellis makes it easy for me to access the information I need to do my job.`)%>%
  group_by(`Q10_2 Trellis makes it easy for me to access the information I need to do my job.`)%>%
  drop_na(`Q10_2 Trellis makes it easy for me to access the information I need to do my job.`) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = `Q10_2 Trellis makes it easy for me to access the information I need to do my job.`, y = n, fill = period)) +
  geom_bar(aes(fill = period), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  scale_fill_brewer(palette = "Set2") + 
  xlab("Answer") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q10_2_plot <- q10_2_plot + labs(title = "Easy Access to Information",
                                subtitle = "Question 10_2 - full data", fill = "Period")

print(q10_2_plot) #survivor bias

# job easier by profile
q10_2_plot2 <- Nov28_2%>%
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
  scale_fill_brewer(palette = "Set2") + 
  xlab("Answer") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) 

q10_2_plot2 <- q10_2_plot2 + labs(title = "Easy Access to Information",
                                  subtitle = "Question 10_2 - full data", fill = "Profile Name")

print(q10_2_plot2) #survivor bias
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
  relocate("Q15_7 None of these", .after = "Q15_6 Emails Received")

Q15 <- Nov28_2 %>% 
  pivot_longer(cols = c("Q15_1 Case/Notes", "Q15_2 Enrollments", "Q15_3 EPR", "Q15_4 Appointments",
                        "Q15_5 Event Attendance","Q15_6 Emails Received","Q15_7 None of these"),
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
  scale_fill_brewer(palette = "Set1")


# replace with NA
Q15[Q15==""] <- NA
Q15 <- Q15 %>%
  dplyr::mutate(response = dplyr::recode(response, "Early Progress Reports" = 'EPR'))
# create a table to calculate percentages
Q15_pct <- Q15%>% 
  drop_na() %>% 
  group_by(response) %>%
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
  xlab("Answer") + 
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
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

Q15graph2 <- Q15_pct2 %>% 
  ggplot(aes(response,count, fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  # theme(axis.title.y = element_text(size = 9)) +
  xlab("Answer") + 
  ylab("Count") 

Q15graph2 <- Q15graph2 + labs(title = "Valuable Information through Trellis",
                            subtitle = "Question 15 - full data", fill= "Profile Name")  
print(Q15graph2)

# graph with time breakdown 
Q15_pct3 <- Q15 %>% 
  drop_na() %>% 
  group_by(period, response) %>% 
  summarize(count = n()) %>% 
  mutate(pct = count/sum(count))

Q15_pct3

Q15graph3 <- Q15_pct3 %>% 
  ggplot(aes(response,count, fill = period)) +
  geom_bar(aes(fill = period), stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  geom_text(aes(label = paste0(round(pct * 100), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Set2") + 
  xlab("Answer") + 
  ylab("Count") 

Q15graph3 <- Q15graph3 + labs(title = "Valuable Information through Trellis",
                              subtitle = "Question 15 - full data", fill= "Period")  
print(Q15graph3)


#### upset plot for Q8 and Q 15 ####
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
  # Cat_SF_enroll <- Cat_SF_enroll %>%
  # dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
  # dplyr::mutate(Edit = ifelse(Edit.Sessions > 0, 1, 0)) %>% 
  # dplyr::mutate(Goal = ifelse(Goal.Sessions > 0, 1, 0))

set_vars <- c("Technical form", "Trellis Demos", "Comm user meetings", "Ask a peer",
              "MS Teams", "Trellis team member", "None of these")
upset_plot_Q8 %>% print(n = nrow(upset_plot_Q8))

ComplexUpset::upset(data = upset_plot_Q8, intersect = set_vars, ) +
    labs(title = "Q8 responses")

# Q 15 
upset_plot_Q15 <- Nov28_2 %>% 
  dplyr::select(ResponseId, `Q15_1 Case/Notes`, `Q15_2 Enrollments`,
                `Q15_3 EPR`, `Q15_4 Appointments`, `Q15_5 Event Attendance`,
                `Q15_6 Emails Received`, `Q15_7 None of these`) %>% 
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
  dplyr::mutate(`None of these` = ifelse(`Q15_6.Emails.Received` == "None of These", 1, 0)) %>% 
  dplyr::mutate(`None of these` = ifelse(`Q15_7.None.of.these` == "Emails Received", 1, 0)) 

upset_plot_Q15 <- upset_plot_Q15 %>% 
  dplyr::select()
# Cat_SF_enroll <- Cat_SF_enroll %>%
# dplyr::mutate(Appt = ifelse(Appt.Sessions > 0, 1, 0)) %>% 
# dplyr::mutate(Edit = ifelse(Edit.Sessions > 0, 1, 0)) %>% 
# dplyr::mutate(Goal = ifelse(Goal.Sessions > 0, 1, 0))

set_vars <- c("Technical form", "Trellis Demos", "Comm user meetings", "Ask a peer",
              "MS Teams", "Trellis team member", "None of these")
upset_plot_Q8 %>% print(n = nrow(upset_plot_Q8))

ComplexUpset::upset(data = upset_plot_Q8, intersect = set_vars, ) +
  labs(title = "Q8 responses")

# set_vars <- c("Appt", "Edit", "Goal")
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

