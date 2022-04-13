# Jung Mee Park
# jmpark@email.arizona.edu
# Spring 2022 survey
# transpose questions
# 2022-04-12

getwd()
setwd("~/Documents/Trellis/Survey/Fall2021/Spring2022")

# read in questions
questions <- read.csv("spring2022_survey_questions.csv", header = T)
transposed_qu <- t(questions)
print(transposed_qu)

# read in responses
April12 <- read.csv("Trellis Program Survey - Spring 2022_April 12, 2022_11.20.csv", header = T)

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
April12 %>% 
  mutate(across(.cols = everything(), is.na)) %>% 
  summarise(across(.cols = everything(), ~ sum(.x)/n()))

# for created date
head(April12$CreatedDate)
dates <- as.POSIXct(April12$CreatedDate,
                    format = "%m/%d/%y %H:%M") 
# change time zones
# change to AZ time 
# t1 <- as.POSIXct(dates, tz = "GMT")
# attributes(t1)$tzone
AZ_created_time <- lubridate::with_tz(dates, "MST")

April12 <- dplyr::mutate(April12, AZ_created_time) # AZ created date added

April12 <- April12 %>% 
  mutate(AZ_created_date = as.Date(AZ_created_time))

# create periods
April12 <- April12 %>% 
  mutate(
    period = case_when(AZ_created_date < "2021-05-01" ~ "More than 12 months",
                       AZ_created_date <= "2021-11-01" ~ "Six to 12 months",
                       AZ_created_date <= "2022-04-01" ~ "One to 6 months",
                       TRUE ~ "Less than 1 month"
    )
  )


# reorder factor for periods
April12$period = factor(April12$period, 
                            ordered = TRUE,
                            levels = c("Less than 1 month", "One to 6 months", "Six to 12 months", "More than 12 months"))
April12 %>% 
  count(period)

#### set up likert questions####
April12_2 <- April12 %>% 
  mutate(across(.cols=c("Q3_1", "Q3_2", "Q3_3", "Q3_4", "Q6_1","Q6_2", "Q6_3",
                        "Q10_1", "Q10_2", "Q12_1", "Q12_2"),
                .fns = ~ factor(.x, 
                                levels = c("Strongly disagree", "Somewhat disagree", 
                                           "Neither agree nor disagree", 
                                           "Somewhat agree", "Strongly agree"),
                                ordered = TRUE)))

April12_2$Q4 = factor(April12_2$Q4, 
                        ordered = TRUE,
                        levels = c("Extremely dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied", 
                                   "Somewhat satisfied", "Extremely satisfied"))

# recode advising base
April12_2$Profile.Name[April12_2$Profile.Name=="advising base"] <- "Advising Base"

# rename questions for Q3
names(April12_2)[names(April12_2)=="Q3_1"] <- "Q3_1 My interaction with the Trellis team has been positive."
names(April12_2)[names(April12_2)=="Q3_2"] <- "Q3_2 My interactions with the Trellis team have been informative."
names(April12_2)[names(April12_2)=="Q3_3"] <- "Q3_3 The training sessions prepared me to use Trellis."
names(April12_2)[names(April12_2)=="Q3_4"] <- "Q3_4 Trellis staff supported me in my initial adoption of Trellis."


# LIKERT for q3
q3_plot <- plot(likert(April12_2[,18:21]), ordered = F, wrap= 40) # this looks good. but add more to it. 
q3_plot <- q3_plot + labs(title = "Interactions with Trellis staff",
                          subtitle = "Question 3 - full data") 
q3_plot

# LIKERT for q4
q4_plot <- plot(likert(April12_2[,22, drop=FALSE]), ordered = F, wrap= 40) # this looks good. but add more to it. 
q4_plot <- q4_plot + labs(title = "Initial Training",
                          subtitle = "Question 4 - full data") 
q4_plot

## fill response with another variable like Profile.Name
library(scales)


April12_2%>%
  # count how often each class occurs in each sample.
  count(Profile.Name,Q4)%>% 
  group_by(Q4)%>%
  drop_na(Q4) %>% 
  mutate(Percent = n /sum(n))%>%
  ggplot(aes(x = Q4, y = Percent, fill = Profile.Name)) + 
  coord_flip() +
  geom_col(width=0.7)+
  geom_text(aes(label = paste0(round(Percent * 100), '%')),
            position = position_stack(vjust = 0.5))


Q4graph <- April12_2 %>%
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
April12_2%>%
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


## LIKERT for q6 
names(April12_2)[names(April12_2)=="Q6_1"] <- "Q6_1 I understand what resources are available to support my use of Trellis."
names(April12_2)[names(April12_2)=="Q6_2"] <- "Q6_2 I continue to feel supported in my use of Trellis."
names(April12_2)[names(April12_2)=="Q6_3"] <- "Q6_3 I am able to get answers to my questions when I need them through Trellis resources."
q6_plot <- plot(likert(April12_2[,23:25]), ordered = T, wrap= 40) # this looks good. but add more to it. 

q6_plot <- q6_plot + labs(title = "Suport for and knowledge of Trellis",
                          subtitle = "Question 6 - full data") 

print(q6_plot)

## Q7 - two part analysis 

# relocate columns 
# iris <- iris %>% relocate(Species, .before = Sepal.Length)
April12_2 <- April12_2 %>% 
  relocate(Q7.2_1, .after = Q7.1_1)

April12_2 <- April12_2 %>% 
  relocate(Q7.2_2, .after = Q7.1_2)

April12_2 <- April12_2 %>% 
  relocate(Q7.2_3, .after = Q7.1_3)

April12_2 <- April12_2 %>% 
  relocate(Q7.2_4, .after = Q7.1_4)

April12_2 <- April12_2 %>% 
  relocate(Q7.2_5, .after = Q7.1_5)

April12_2 <- April12_2 %>% 
  relocate(Q7.2_6, .after = Q7.1_6)

# pairwise results don't make a ton of sense

# rename the questions
names(April12_2)[names(April12_2)=="Q7.1_1"] <- "Q7.1_1 Aware of Release notes"
names(April12_2)[names(April12_2)=="Q7.1_2"] <- "Q7.1_2 Aware of Bi weekly live demonstrations (i.e. Sprint demos)"
names(April12_2)[names(April12_2)=="Q7.1_3"] <- "Q7.1_3 Aware of Communities of practice (i.e. Trellis user groups)"
names(April12_2)[names(April12_2)=="Q7.1_4"] <- "Q7.1_4 Aware of Monthly information sessions"
names(April12_2)[names(April12_2)=="Q7.1_5"] <- "Q7.1_5 Aware of Newsletter/Digest"
names(April12_2)[names(April12_2)=="Q7.1_6"] <- "Q7.1_6 Aware of None of these"
names(April12_2)[names(April12_2)=="Q7.2_1"] <- "Q7.2_1 Have used Release notes"
names(April12_2)[names(April12_2)=="Q7.2_2"] <- "Q7.2_2 Have attended Bi weekly live demonstrations (i.e. Sprint demos)"
names(April12_2)[names(April12_2)=="Q7.2_3"] <- "Q7.2_3 Have used Communities of practice (i.e. Trellis user groups)"
names(April12_2)[names(April12_2)=="Q7.2_4"] <- "Q7.2_4 Have attended Monthly information sessions"
names(April12_2)[names(April12_2)=="Q7.2_5"] <- "Q7.2_5 Have used Newsletter/Digest"
names(April12_2)[names(April12_2)=="Q7.2_6"] <- "Q7.2_6 Have attended None of these"

# release notes columns 26, 27
April12_2 %>% 
  # count how often each class occurs in each sample.
  count(`Q7.1_1 Aware of Release notes`, `Q7.2_1 Have used Release notes`)%>% 
  drop_na() %>% 
  mutate(pct = n /sum(n)) 
# %>%
#   geom_boxplot()
#   geom_text(aes(label = paste0(round(pct * 100), '%')),
#             position = position_stack(vjust = 0.5))

  
# load library
  library(ggplot2)
  library(tidyr)
  library(ggthemes)
  library(reshape2)

# reorganize data
Release_notes <- April12_2 %>% 
  gather(key="question", value="response", 26:27) 

ggplot(data = Release_notes, aes(x = question, y = response)) + 
  geom_boxplot()


  
## Q8
April12_2$Q8[April12_2$Q8==""] <- NA

sum(is.na(April12_2$Q8)) 

# disperse answers into long, currently comma separated
library(stringr)
April12_2[c('Q8_val1', 'Q8_val2', 'Q8_val3', 'Q8_val4', 'Q8_val5', 'Q8_val6')] <- str_split_fixed(April12_2$Q8, ',', 6)
# April12_3 <- April12_2 %>% separate(Q8, c('Q8_val1', 'Q8_val2', 'Q8_val3', 'Q8_val4', 'Q8_val5', 'Q8_val6'))
Q8graph <- April12_2[,69:74] # these are the columns of created Q8_val1 through 6

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

# Likert Q12


