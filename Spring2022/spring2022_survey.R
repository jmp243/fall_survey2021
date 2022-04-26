# Jung Mee Park
# jmpark@email.arizona.edu
# Spring 2022 survey
# transpose questions
# 2022-04-12

# clear all 
rm(list=ls(all=TRUE)) 

# get wd
getwd()
setwd("~/Documents/Trellis/Survey/Fall2021/Spring2022")

# read in questions
questions <- read.csv("spring2022_survey_questions.csv", header = T)
transposed_qu <- t(questions)
print(transposed_qu)

# read in responses
# April12 <- read.csv("Trellis Program Survey - Spring 2022_April 12, 2022_11.20.csv", header = T)
April25 <- read.csv("Trellis Program Survey - Spring 2022_April 25, 2022_09.38.csv", header = T)
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
library(psych)
library(viridis)
library(here)
library(flextable)
library(devtools)
library(broom)


# check for NA's
April25 %>% 
  mutate(across(.cols = everything(), is.na)) %>% 
  summarise(across(.cols = everything(), ~ sum(.x)/n()))

# remove test email cases
April25 <- April25[!(April25$RecipientEmail=="park.schlawin@gmail.com" | April25$RecipientEmail=="jmpark@arizona.edu"| April25$RecipientEmail=="jmpark@email.arizona.edu" | April25$RecipientEmail=="fkmiller@arizona.edu"  ),]
April25

# for created date
head(April25$CreatedDate)
dates <- as.POSIXct(April25$CreatedDate,
                    format = "%m/%d/%y %H:%M") 
# change time zones
# change to AZ time 
# t1 <- as.POSIXct(dates, tz = "GMT")
# attributes(t1)$tzone
AZ_created_time <- lubridate::with_tz(dates, "MST")

April25 <- dplyr::mutate(April25, AZ_created_time) # AZ created date added

April25 <- April25 %>% 
  mutate(AZ_created_date = as.Date(AZ_created_time))

# create periods
April25 <- April25 %>% 
  mutate(
    period = case_when(AZ_created_date < "2021-05-01" ~ "More than 12 months",
                       AZ_created_date <= "2021-11-01" ~ "Six to 12 months",
                       AZ_created_date <= "2022-04-01" ~ "One to 6 months",
                       TRUE ~ "Less than 1 month"
    )
  )


# reorder factor for periods
April25$period = factor(April25$period, 
                            ordered = TRUE,
                            levels = c("Less than 1 month", "One to 6 months", "Six to 12 months", "More than 12 months"))
April25 %>% 
  count(period)

#### set up likert questions####
April25_2 <- April25 %>% 
  mutate(across(.cols=c("Q3_1", "Q3_2", "Q3_3", "Q3_4", "Q6_1","Q6_2", "Q6_3",
                        "Q10_1", "Q10_2", "Q12_1", "Q12_2"),
                .fns = ~ factor(.x, 
                                levels = c("Strongly disagree", "Somewhat disagree", 
                                           "Neither agree nor disagree", 
                                           "Somewhat agree", "Strongly agree"),
                                ordered = TRUE)))

April25_2$Q4 = factor(April25_2$Q4, 
                        ordered = TRUE,
                        levels = c("Extremely dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied", 
                                   "Somewhat satisfied", "Extremely satisfied"))

# recode advising base
April25_2$Profile.Name[April25_2$Profile.Name=="advising base"] <- "Advising Base"

# breakdown of respondents
April25_2 %>% 
  count(Profile.Name)
# 
# library(stringr)
# April25_2[c('prod_val1', 'prod_val2', 'prod_val3', 'prod_val4', 'prod_val5', 'prod_val6')] <- str_split_fixed(April25_2$allproducts, ',', 6)
# # April12_3 <- April12_2 %>% separate(Q8, c('Q8_val1', 'Q8_val2', 'Q8_val3', 'Q8_val4', 'Q8_val5', 'Q8_val6'))
# products <- April25_2[,75:80] # these are the columns of created Q8_val1 through 6

# pivot the table longer
April25_2 %>% 
  count(c(counter.Events, counter.External.Partners, counter.Marketing...SF, counter.Reports, counter.SAFER, counter.Scheduling.Notes, counter.Service.Desk))

# rename questions for Q3
names(April25_2)[names(April25_2)=="Q3_1"] <- "Q3_1 My interaction with the Trellis team has been positive."
names(April25_2)[names(April25_2)=="Q3_2"] <- "Q3_2 My interactions with the Trellis team have been informative."
names(April25_2)[names(April25_2)=="Q3_3"] <- "Q3_3 The training sessions prepared me to use Trellis."
names(April25_2)[names(April25_2)=="Q3_4"] <- "Q3_4 Trellis staff supported me in my initial adoption of Trellis."


# LIKERT for q3
q3_plot <- plot(likert(April25_2[,18:21]), ordered = F, wrap= 40) # this looks good. but add more to it. 
q3_plot <- q3_plot + labs(title = "Interactions with Trellis staff",
                          subtitle = "Question 3 - full data") 
q3_plot

# count up by group
# , `Q3_2 My interactions with the Trellis team have been informative.`,
# `Q3_3 The training sessions prepared me to use Trellis.`, `Q3_4 Trellis staff supported me in my initial adoption of Trellis.`
length(April25_2$`Q3_1 My interaction with the Trellis team has been positive.`)

# LIKERT for q4
# names(April12_2)[names(April12_2)=="Q4"] <- "Q4 How satisfied were you with your initial training in Trellis?"

q4_plot <- plot(likert(April25_2[,22, drop=FALSE]), ordered = F, wrap= 40) # this looks good. but add more to it. 
q4_plot <- q4_plot + labs(title = "Q4 How satisfied were you with your initial training in Trellis?",
                          subtitle = "Question 4 - full data") 
q4_plot

## fill response with another variable like Profile.Name
library(scales)


April25_2%>%
  # count how often each class occurs in each sample.
  count(Profile.Name, Q4)%>% 
  group_by(Q4)%>%
  drop_na(Q4) %>% 
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = Q4, y = Percent, fill = Profile.Name)) + 
  coord_flip() +
  geom_col(width=0.7)+
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5))


Q4graph <- April25_2 %>%
  group_by(Profile.Name) %>% 
  # mutate(Q4 = fct_infreq(Q4)) %>%
  drop_na(Q4) %>% 
  ggplot(aes(x = factor(Q4)), y = ..count.., fill = Profile.Name) +
  geom_bar(aes(fill = Profile.Name)) +
  # coord_flip() +
  labs(x = "", y = "responses")

Q4graph <- Q4graph + labs(title = "How satisfied were you with your initial training in Trellis?",
                          subtitle = "Question 4 - full data", fill = "Profile Name")

print(Q4graph)

# perhaps an overly complicated stacked count graph with percentages
Q4graph2 <- April25_2%>%
  # count how often each class occurs in each sample.
  count(Profile.Name,Q4)%>%
  group_by(Q4)%>%
  drop_na(Q4) %>%
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = Q4, y = n, fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name), stat = "identity") +
  geom_col(width=0.7)+
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5))

Q4graph2 <- Q4graph2 + labs(title = "How satisfied were you with your initial training in Trellis?",
                          subtitle = "Question 4 - full data", fill = "Profile Name")

print(Q4graph2)
## LIKERT for q6 
names(April25_2)[names(April25_2)=="Q6_1"] <- "Q6_1 I understand what resources are available to support my use of Trellis."
names(April25_2)[names(April25_2)=="Q6_2"] <- "Q6_2 I continue to feel supported in my use of Trellis."
names(April25_2)[names(April25_2)=="Q6_3"] <- "Q6_3 I am able to get answers to my questions when I need them through Trellis resources."

q6_plot <- plot(likert(April25_2[,23:25]), ordered = T, wrap= 40) # this looks good. but add more to it. 

q6_plot <- q6_plot + labs(title = "Suport for and knowledge of Trellis",
                          subtitle = "Question 6 - full data") 

print(q6_plot)

## Q7 - two part analysis 

# relocate columns 
April25_2 <- April25_2 %>% 
  relocate(Q7.2_1, .after = Q7.1_1)

April25_2 <- April25_2 %>% 
  relocate(Q7.2_2, .after = Q7.1_2)

April25_2 <- April25_2 %>% 
  relocate(Q7.2_3, .after = Q7.1_3)

April25_2 <- April25_2 %>% 
  relocate(Q7.2_4, .after = Q7.1_4)

April25_2 <- April25_2 %>% 
  relocate(Q7.2_5, .after = Q7.1_5)

April25_2 <- April25_2 %>% 
  relocate(Q7.2_6, .after = Q7.1_6)

# pairwise results don't make a ton of sense

# rename the questions
names(April25_2)[names(April25_2)=="Q7.1_1"] <- "Q7.1_1 Aware of Release notes"
names(April25_2)[names(April25_2)=="Q7.1_2"] <- "Q7.1_2 Aware of Bi weekly live demonstrations (i.e. Sprint demos)"
names(April25_2)[names(April25_2)=="Q7.1_3"] <- "Q7.1_3 Aware of Communities of practice (i.e. Trellis user groups)"
names(April25_2)[names(April25_2)=="Q7.1_4"] <- "Q7.1_4 Aware of Monthly information sessions"
names(April25_2)[names(April25_2)=="Q7.1_5"] <- "Q7.1_5 Aware of Newsletter/Digest"
names(April25_2)[names(April25_2)=="Q7.1_6"] <- "Q7.1_6 Aware of None of these"
names(April25_2)[names(April25_2)=="Q7.2_1"] <- "Q7.2_1 Have used Release notes"
names(April25_2)[names(April25_2)=="Q7.2_2"] <- "Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)"
names(April25_2)[names(April25_2)=="Q7.2_3"] <- "Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)"
names(April25_2)[names(April25_2)=="Q7.2_4"] <- "Q7.2_4 Have attended Monthly information sessions"
names(April25_2)[names(April25_2)=="Q7.2_5"] <- "Q7.2_5 Have used Newsletter/Digest"
names(April25_2)[names(April25_2)=="Q7.2_6"] <- "Q7.2_6 Have attended None of these"

# release notes columns 26, 27
April25_2 %>% 
  # count how often each class occurs in each sample.
  count(`Q7.1_1 Aware of Release notes`, `Q7.2_1 Have used Release notes`)%>%
  # count(`Q7.1_1`, `Q7.2_1`)%>% 
  drop_na() %>% 
  mutate(pct = n /sum(n)) 
# %>%
#   geom_boxplot()
#   geom_text(aes(label = paste0(round(pct * 100), '%')),
#             position = position_stack(vjust = 0.5))
April25_2 <- April25_2 %>% 
  mutate(Q7.1_1_modified = ifelse(`Q7.2_1 Have used Release notes` == "I have used/attended.",
                                  "I am aware.", 
                                  `Q7.1_1 Aware of Release notes`))
  
# load library
library(ggplot2)
library(tidyr)
library(ggthemes)
library(reshape2)
library(RColorBrewer)


Q7_1 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_1_modified, `Q7.2_1 Have used Release notes`),
               names_to = "col_name_release", 
               values_to = "release_notes") %>% 
  drop_na("release_notes") %>% 
  distinct() 

Q7_1 %>% 
  count(release_notes) 

# Q7_1[Q7_1==""] <- NA

Q7_1 %>% 
  filter(release_notes != "") %>% 
  ggplot(aes(factor(release_notes))) +
  # drop_na() +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

# facet wrap by column. All questions in 1, all questions in the other. 
# Q7 part 2 sprint demos
April25_2 %>% 
  # count how often each class occurs in each sample.
  count(`Q7.1_2 Aware of Bi weekly live demonstrations (i.e. Sprint demos)`, `Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)`)%>%
  drop_na() %>% 
  mutate(pct = n /sum(n)) 

April25_2 <- April25_2 %>% 
  mutate(Q7.1_2_modified = ifelse(`Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)` == "I have used/attended.",
                                  "I am aware.", 
                                  `Q7.1_2 Aware of Bi weekly live demonstrations (i.e. Sprint demos)`))
Q7_2 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_2_modified, `Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)`),
               names_to = "col_name_demos", 
               values_to = "sprint_demos") %>% 
  drop_na("sprint_demos") %>% 
  distinct() 


Q7_2 %>% 
  count(sprint_demos) 

Q7_2 %>% 
  filter(sprint_demos != "") %>% 
  ggplot(aes(factor(sprint_demos))) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")


# Q7 part 3 Trellis user groups
April25_2 %>% 
  # count how often each class occurs in each sample.
  count(`Q7.1_3 Aware of Communities of practice (i.e. Trellis user groups)`, `Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)`)%>%
  drop_na() %>% 
  mutate(pct = n /sum(n)) 

April25_2 <- April25_2 %>% 
  mutate(Q7.1_3_modified = ifelse(`Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)` == "I have used/attended.",
                                  "I am aware.", 
                                  `Q7.1_3 Aware of Communities of practice (i.e. Trellis user groups)`))
Q7_3 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_3_modified, `Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)`),
               names_to = "col_name_user", 
               values_to = "user_groups") %>% 
  drop_na("user_groups") %>% 
  distinct() 


Q7_3 %>% 
  count(user_groups) 

Q7_3 %>% 
  filter(user_groups != "") %>% 
  ggplot(aes(factor(user_groups))) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

# Q7 part 4 Monthly information sessions
April25_2 %>% 
  # count how often each class occurs in each sample.
  count(`Q7.1_4 Aware of Monthly information sessions`, `Q7.2_4 Have attended Monthly information sessions`)%>%
  drop_na() %>% 
  mutate(pct = n /sum(n)) 

April25_2 <- April25_2 %>% 
  mutate(Q7.1_4_modified = ifelse(`Q7.2_4 Have attended Monthly information sessions` == "I have used/attended.",
                                  "I am aware.", 
                                  `Q7.1_4 Aware of Monthly information sessions`))
Q7_4 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_4_modified, `Q7.2_4 Have attended Monthly information sessions`),
               names_to = "col_name_monthly", 
               values_to = "monthly") %>% 
  drop_na("monthly") %>% 
  distinct() 


Q7_4 %>% 
  count(monthly) 

Q7_4 %>% 
  filter(monthly != "") %>% 
  ggplot(aes(factor(monthly))) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

# Q7 part 5 Newsletter
April25_2 %>% 
  # count how often each class occurs in each sample.
  count(`Q7.1_5 Aware of Newsletter/Digest`, `Q7.2_5 Have used Newsletter/Digest`)%>%
  drop_na() %>% 
  mutate(pct = n /sum(n)) 

April25_2 <- April25_2 %>% 
  mutate(Q7.1_5_modified = ifelse(`Q7.2_5 Have used Newsletter/Digest` == "I have used/attended.",
                                  "I am aware.", 
                                  `Q7.1_5 Aware of Newsletter/Digest`))
Q7_5 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_5_modified, `Q7.2_5 Have used Newsletter/Digest`),
               names_to = "col_name_newsletter", 
               values_to = "newsletter") %>% 
  drop_na("newsletter") %>% 
  distinct() 


Q7_5 %>% 
  count(newsletter) 

Q7_5 %>% 
  filter(newsletter != "") %>% 
  ggplot(aes(factor(newsletter))) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

# Q7 part 6 none
April25_2 %>% 
  # count how often each class occurs in each sample.
  count(`Q7.1_6 Aware of None of these`, `Q7.2_6 Have attended None of these`)%>%
  drop_na() %>% 
  mutate(pct = n /sum(n)) 

April25_2 <- April25_2 %>% 
  mutate(Q7.1_6_modified = ifelse(`Q7.2_6 Have attended None of these` == "I have used/attended.",
                                  "I am aware.", 
                                  `Q7.1_6 Aware of None of these`))
Q7_6 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_6_modified, `Q7.2_6 Have attended None of these`),
               names_to = "col_name_none", 
               values_to = "none") %>% 
  drop_na("none") %>% 
  distinct() 

 
Q7_6 %>% 
  count(none) 

Q7_6 %>% 
  filter(none != "") %>% 
  ggplot(aes(factor(none))) +
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

# q7 new data.frame
Q7 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_1_modified, `Q7.2_1 Have used Release notes`),
               names_to = "col_name", 
               values_to = "Release_Notes") %>% 
  drop_na("Release_Notes") %>% 
  distinct() 

Q7a <- Q7_1[, c(68, 69) ] # recipient email, dept, profile.name, period, colname, value

Q7 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_2_modified, `Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)`),
               names_to = "col_name_demos", 
               values_to = "sprint_demos") %>% 
  drop_na("sprint_demos") %>% 
  distinct() 

Q7b <- Q7_2[, c(69, 70) ] # recipient email, dept, profile.name, period, colname, value

Q7 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_3_modified, `Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)`),
               names_to = "col_name_user", 
               values_to = "user_groups") %>% 
  drop_na("user_groups") %>% 
  distinct() 

Q7c <- Q7_3[, c(70, 71) ] # recipient email, dept, profile.name, period, colname, value

Q7<- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_4_modified, `Q7.2_4 Have attended Monthly information sessions`),
               names_to = "col_name_monthly", 
               values_to = "monthly") %>% 
  drop_na("monthly") %>% 
  distinct() 

Q7d <- Q7_4[, c(71, 72) ] # recipient email, dept, profile.name, period, colname, value

Q7 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_5_modified, `Q7.2_5 Have used Newsletter/Digest`),
               names_to = "col_name_newsletter", 
               values_to = "newsletter") %>% 
  drop_na("newsletter") %>% 
  distinct() 
Q7e <- Q7_5[, c(72, 73) ] # recipient email, dept, profile.name, period, colname, value

Q7 <- April25_2 %>% 
  pivot_longer(cols = c(Q7.1_6_modified, `Q7.2_6 Have attended None of these`),
               names_to = "col_name_none", 
               values_to = "none") %>% 
  drop_na("none") %>% 
  distinct()

Q7f <- Q7_6[, c(73, 74) ] # recipient email, dept, profile.name, period, colname, value

# data %>% 
#   mutate(q1_mod = ifelse...) %>% 
#   select(-q1) %>% 
#   pivot_longer(cols = everything(), names_to = "question", values_to = "answer") %>% 
#   ggplot(aes(x = answer)) + 
#   geom_bar() + 
#   facet_wrap(vars(question), scales = "free_x", ncol=1) # scales = "free" 

# merge the Q7 files
Q7 <- merge(Q7a, Q7b, by = 0, all = TRUE)
Q7 <- merge(Q7, Q7c, by = 0, all = TRUE)
Q7 <- merge(Q7, Q7d, by = 0, all = TRUE)
Q7 <- merge(Q7, Q7e, by = 0, all = TRUE)
Q7 <- merge(Q7, Q7f, by = 0, all = TRUE)

Q7_new <- subset(Q7, select= - c(1:5)) # recheck the numbers

# Q7_new2 <- Q7_new                              # Replicate data
# Q7_new2$group <- factor(data_new$group,      # Reordering group factor levels
#                          levels = c("B", "A", "C", "D"))

Trellis_resources <- c(
  `monthly` = "Monthly Info Sessions",
  `newsletter` = "Newsletters",
  `release_notes` = "Release Notes",
  `sprint_demos` = "Sprint Demos",
  `user_groups` = "User Groups",
  `none` = "None of These"
)

Q7_new2 <- Q7_new %>%
  pivot_longer(cols = c(release_notes, sprint_demos, user_groups, monthly, newsletter, none),
               names_to = "question", values_to = "answer")
# Q7_new2 <- factor(Q7$new2$answer,      # Reordering group factor levels
#        levels = c("monthly", "newsletter", "release_notes", "sprint_demos", "user_groups", "none"))

Q7_new %>% 
  pivot_longer(cols = c(release_notes, sprint_demos, user_groups, monthly, newsletter, none), 
               names_to = "question", values_to = "answer") %>% 
  filter(answer != "") %>% 
  ggplot(aes(x = factor(answer), fill = factor(answer))) + 
  geom_bar(stat="count", position = "dodge") + 
  facet_wrap(vars(question), scales = "free_x", ncol=3, labeller = as_labeller(Trellis_resources)) +  # scales = "free" 
  scale_fill_brewer(palette = "Set2") + 
  labs(title = "Question 7 - Usage of Trellis Resources",
       subtitle = "full data", fill = "Answer") +
  xlab("Answer") + 
  ylab("Count") 

Q7_new2 %>% 
  filter(answer != "") %>% 
  ggplot(aes(x = factor(answer), fill = factor(answer))) + 
  geom_bar(stat="count", position = "dodge") + 
  facet_wrap(vars(question), scales = "free_x", ncol=3, labeller = as_labeller(Trellis_resources)) +  # scales = "free" 
  scale_fill_brewer(palette = "Set2") + 
  labs(title = "Question 7 - Usage of Trellis Resources",
       subtitle = "full data", fill = "Answer") +
  xlab("Answer") + 
  ylab("Count") 
## Q8
April25_2$Q8[April25_2$Q8==""] <- NA

sum(is.na(April25_2$Q8)) 

# disperse answers into long, currently comma separated
library(stringr)
April25_2[c('Q8_val1', 'Q8_val2', 'Q8_val3', 'Q8_val4', 'Q8_val5', 'Q8_val6')] <- str_split_fixed(April25_2$Q8, ',', 6)
# April12_3 <- April12_2 %>% separate(Q8, c('Q8_val1', 'Q8_val2', 'Q8_val3', 'Q8_val4', 'Q8_val5', 'Q8_val6'))
Q8graph <- April25_2[,75:80] # these are the columns of created Q8_val1 through 6

# pivot the table longer
long_Q8 <- Q8graph %>% 
  pivot_longer(everything(), names_to = "question", values_to = "response", values_drop_na = TRUE)

# replace with NA
long_Q8[long_Q8==""] <- NA

# create a table to calculate percentages
Q8_pct <- long_Q8 %>% 
  drop_na() %>% 
  group_by(response) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

Q8_pct

Q8graph <- 
  ggplot(Q8_pct, aes(response,count, fill = response)) +
  geom_bar(stat='identity') +
  labs(x = "", y = "") +
  # coord_flip() +
  theme(legend.position="none") +
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))


Q8graph <- Q8graph + labs(title = "Which of these tools do you use to get help with Trellis?",
                          subtitle = "Question 8 - full data") + 
  theme(legend.position="none")

print(Q8graph)

# Likert Q10
# rename questions for Q10
names(April25_2)[names(April25_2)=="Q10_1"] <- "Q10_1 I feel comfortable using Trellis."
names(April25_2)[names(April25_2)=="Q10_2"] <- "Q10_2 Trellis makes it easy for me to access the information I need to do my job."

# LIKERT for q10
q10_plot <- plot(likert(April25_2[,39:40]), ordered = F, wrap= 40) # this looks good. but add more to it. 
q10_plot <- q10_plot + labs(title = "Ease of Use",
                          subtitle = "Question 10 - full data") 
q10_plot

# Likert Q12
# rename questions for Q12
names(April25_2)[names(April25_2)=="Q12_1"] <- "Q12_1 I believe access to the Trellis data allows me to provide better service."
names(April25_2)[names(April25_2)=="Q12_2"] <- "Q12_2 I would recommend Trellis capabilities to someone else."

# LIKERT for q12
q12_plot <- plot(likert(April25_2[,41:42]), ordered = F, wrap= 40) # this looks good. but add more to it. 
q12_plot <- q12_plot + labs(title = "Value of Trellis",
                            subtitle = "Question 12 - full data") 
q12_plot

### col sums for questions
April25_2$`Q3_1 My interaction with the Trellis team has been positive.`[April25_2$`Q3_1 My interaction with the Trellis team has been positive.`==""] <- NA

sum(is.na(April25_2$`Q3_1 My interaction with the Trellis team has been positive.`)) # 29 

# Q4 
#colSums(!is.na(dat))
April25_2$Q4[April25_2$Q4==""] <- NA
 
sum(is.na(April25_2$Q4)) #29 

# Q8
April25_2$Q8[April25_2$Q8==""] <- NA

sum(is.na(April25_2$Q8)) 