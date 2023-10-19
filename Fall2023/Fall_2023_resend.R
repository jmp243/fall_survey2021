# Correcting Fall 2023
# jmpark@arizona.edu
# October 19, 2023

# libraries
library(tidyverse)
library(readr)
library(likert)
library(dplyr)
library(stringr)

# export_EMD <- read_csv("Fall2023/fall2023_history_Oct16_2023.csv")
# Fall_2023_contact_list <- read_csv("Fall2023/Fall-2023-contact-list.csv")
# 
# # merge two datasets
# merged_fall2023 <- export_EMD %>% 
#   left_join(Fall_2023_contact_list)
# 
# # remove External Data Reference and rename User: Features
# # merged_fall2023a <- merged_fall2023 %>% 
# #   select(-`External Data Reference`) %>% 
# #   rename("External Data Reference" = "User: Feature")
# 
# # email only those who did not finish survey
# table(merged_fall2023$Status)
# 
# merged_fall2023b <- merged_fall2023 %>% 
#   filter(Status != "Survey Finished")
# 
# merged_fall2023b <- merged_fall2023b %>% 
#   select(`Last Name`, `First Name`, `Contact Id`, `Lookup Id`, Email, `User: Feature`, Status, Link)
# 
# write.csv(merged_fall2023b, "~/Documents/Trellis/Survey/Fall2021/Fall2023/resend_list_Oct16_2023.csv", row.names=FALSE)
# 
# # look at who started the survey
# survey_started <- merged_fall2023b %>% 
#   filter(Status == "Survey Started")

# separate User: Feature
library(tidyr)
longer_contact_list <- Fall_2023_contact_list |>
  separate_rows(c(`User: Feature`, Email), sep = "; ")


# reading in the survey results
Fall_Survey2023_Oct19 <- read_csv("Fall2023/data/Trellis+Program+Survey+-+Fall+2023_October+19,+2023_09.38.csv")

# delete top two rows
Fall_Survey2023_Oct19 <- Fall_Survey2023_Oct19 %>% 
  filter(`Progress-Progress` <= 8)

# remove my test emails
Fall_Survey2023_Oct19 <- Fall_Survey2023_Oct19 %>% 
  filter(`RecipientFirstName-Recipient First Name` != "Jung")

# delete unused columns
Fall_Survey2023_Oct19 <-Fall_Survey2023_Oct19[ , -c(45:49) ] # be careful when you run this


# open SF contact list
SF_Contacts <- read_csv("Fall2023/data/SF_Contact_list_w_CreatedDate.csv")

# merge survey with contacts
Fall_Survey2023_Oct19a <- left_join(Fall_Survey2023_Oct19, SF_Contacts, by = join_by("RecipientEmail-Recipient Email" == "Email"))

# check to see if any NA's in NetID field
Fall_Survey2023_Oct19a  %>% 
  filter(is.na(NetID)) %>% 
  tally()

# incorporate created date
# for created date
head(Fall_Survey2023_Oct19a$`User: Created Date`)
Fall_Survey2023_Oct19a$CreatedDate <- as.POSIXct(Fall_Survey2023_Oct19a$`User: Created Date`,
                          format= "%m/%d/%y") 


Fall_Survey2023_Oct19a <- Fall_Survey2023_Oct19a %>% 
  dplyr::mutate(CreatedDate = as.Date(CreatedDate)) # AZ created date added


Fall_Survey2023_Oct19a <- Fall_Survey2023_Oct19a %>%
  mutate(
    period = case_when(CreatedDate <= "2022-08-01" ~ "More than two semesters",
                       CreatedDate <= "2023-01-15" ~ "Last semester",
                       # AZ_created_date <= "2021-12-01" ~ "This semester"
                       TRUE ~ "This semester"
    )
  )

Fall_Survey2023_Oct19b$period <- factor(Fall_Survey2023_Oct19b$period, 
                                        levels = c("This semester", "Last semester", "More than two semesters"), 
                                        ordered = TRUE)
# last login for context
head(Fall_Survey2023_Oct19a$`User: Last Login`)
Fall_Survey2023_Oct19a$LastLogin <- as.POSIXct(Fall_Survey2023_Oct19a$`User: Last Login`,
                                                 format= "%m/%d/%y") 


Fall_Survey2023_Oct19a <- Fall_Survey2023_Oct19a %>% 
  dplyr::mutate(LastLogin = as.Date(LastLogin)) # AZ created date added


Fall_Survey2023_Oct19a <- Fall_Survey2023_Oct19a %>%
  mutate(
    LastLogin_group = case_when(LastLogin <= "2023-08-01" ~ "More than 60 days",
                                LastLogin <= "2023-09-01" ~ "More than 30 days",
                       # AZ_created_date <= "2021-12-01" ~ "This semester"
                       TRUE ~ "Recent"
    )
  )

Fall_Survey2023_Oct19b$LastLogin_group <- factor(Fall_Survey2023_Oct19b$LastLogin_group, 
                                        levels = c("Recent", "More than 30 days", "More than 60 days"), 
                                        ordered = TRUE)
# trim out some unused variables
Fall_Survey2023_Oct19b <- Fall_Survey2023_Oct19a %>% 
  select(-`Trellis User`, -`ExternalReference-External Data Reference`, -`DistributionChannel-Distribution Channel`, 
         -`LocationLatitude-Location Latitude`, -`LocationLongitude-Location Longitude`, -`UserLanguage-User Language`,
         -`Status-Response Type`, -`IPAddress-IP Address`)


### analysis
# rename questions
## Rename Q2.1 
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.1_1-Please rate your level of agreement regarding your introduction and training for Trellis. - Trellis staff supported me in my initial adoption of Trellis."
                              ] <- "Q2.1_1 Trellis staff supported me in my initial adoption of Trellis."
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.1_2-Please rate your level of agreement regarding your introduction and training for Trellis. - The initial training sessions prepared me to use Trellis."
                              ] <- "Q2.1_2 The initial training sessions prepared me to use Trellis."
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.1_3-Please rate your level of agreement regarding your introduction and training for Trellis. - I am satisfied with my Trellis onboarding experience."
                              ] <- "Q2.1_3 I am satisfied with my Trellis onboarding experience." 
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.1_4-Please rate your level of agreement regarding your introduction and training for Trellis. - I've had positive interactions with the Trellis team."
                              ] <- "Q2.1_4 I've had positive interactions with the Trellis team."

names(Fall_Survey2023_Oct19b)

## Rename Q2.3
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.3_1-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Technical issue form/ Submitting a support ticket" 
                              ] <- "Q2.3_1 Technical issue form/ Submitting a support ticket"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.3_2-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Trellis training workshops"
                              ] <- "Q2.3_2 Trellis training workshops"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.3_9-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - On-Screen guided help (Whatfix)"  
                              ] <- "Q2.3_3 On-Screen guided help (Whatfix)"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.3_5-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Asking my peers"                                                                  
                              ] <- "Q2.3_4 Asking my peers"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.3_6-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Trellis teams (MS Teams)"                                                         
                              ] <- "Q2.3_5 Trellis teams (MS Teams)"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.3_8-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Ask Trellis team members directly. Please share who you most often contact below."
                              ] <- "Q2.3_6 Ask Trellis team members directly"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.3_8_TEXT-Which of these tools do you use to get help with Trellis? (select all that apply) - Ask Trellis team members directly. Please share who you most often contact below. - Text" 
                              ] <- "Q2.3_6a Team Member Name"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)== "Q2.3_7-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - None of these"
                              ] <- "Q2.3_7 None of these"

## Rename Q2.4
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.4_1-Please rate your level of agreement with the following statements: - I am familiar with the Trellis resources available to me." 
                              ] <- "Q2.4_1 I am familiar with the Trellis resources available to me."
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.4_2-Please rate your level of agreement with the following statements: - I can find answers to my questions with Trellis resources."                                                    
                              ] <- "Q2.4_2 I can find answers to my questions with Trellis resources."
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q2.4_3-Please rate your level of agreement with the following statements: - I feel supported in my use of Trellis."                                                                        
                              ] <- "Q2.4_3 I feel supported in my use of Trellis."

## Rename Q3.2
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q3.2_1-Please rate your level of agreement with the following statements: - I feel comfortable using Trellis." 
                              ] <- "Q3.2_1 I feel comfortable using Trellis."
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q3.2_2-Please rate your level of agreement with the following statements: - Trellis makes it easy for me to access the information I need."
                              ] <- "Q3.2_2 Trellis makes it easy for me to access the information I need."

## Rename Q4.2
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q4.2_1-Please rate your level of agreement or disagreement with the following statements: - I believe access to the Trellis data allows me to provide better service."  
                              ] <- "new"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q4.2_2-Please rate your level of agreement or disagreement with the following statements: - I would recommend Trellis capabilities to someone else." 
                              ] <- "new"

## Rename Q5.1
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="old"] <- "new"

## Rename Q 3.3 which should be Q5.2
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="old"] <- "new"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="old"] <- "new"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="old"] <- "new"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="old"] <- "new"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="old"] <- "new"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="old"] <- "new"
#### set up likert questions####
Fall_Survey2023_Oct19b %>% 
  mutate(across(.cols=c(),
                .fns = ~ factor(.x, 
                                levels = c("Strongly disagree", "Somewhat disagree", 
                                           "Neither agree nor disagree", 
                                           "Somewhat agree", "Strongly agree"),
                                ordered = TRUE)))



# Load in datafile of counts from FKM
October02_usercounts <- read_csv("Fall2023/data/2023_October02_ACTIVEuser_act_counts.csv")

# remove the n column
October02_usercounts <-October02_usercounts %>% 
  select(-n)

# merge count data to short data
Fall_Survey2023_Oct19c <- Fall_Survey2023_Oct19b %>% 
  left_join(October02_usercounts, by = join_by("Full Name" == "Name.x"))

# create a longer list based on Features
longer_Fall_Survey2023_Oct19 <- Fall_Survey2023_Oct19 |>
  separate_rows(c(`User: Feature-User: Feature`, `RecipientEmail-Recipient Email`), sep = "; ")