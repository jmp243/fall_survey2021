# Correcting Fall 2023
# jmpark@arizona.edu
# October 19, 2023

# libraries
library(tidyverse)
library(readr)
library(likert)
library(dplyr)
library(stringr)
# library(dplyr)
library(tidyr)

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



# reading in the survey results
Fall_Survey2023_Oct19 <- read_csv("Fall2023/data/Trellis+Program+Survey+-+Fall+2023_October+30,+2023_09.11.csv")

# delete top two rows
Fall_Survey2023_Oct19 <- Fall_Survey2023_Oct19 %>% 
  filter(`Progress-Progress` <= 8)

# remove my test emails
Fall_Survey2023_Oct19 <- Fall_Survey2023_Oct19 %>% 
  filter(`RecipientFirstName-Recipient First Name` != "Jung")

# delete unused columns
# Fall_Survey2023_Oct19 <-Fall_Survey2023_Oct19[ , -c(45) ] # be careful when you run this
# Fall_Survey2023_Oct19 <-Fall_Survey2023_Oct19[ , -c(45:49) ] # be careful when you run this

# open SF contact list
SF_Contacts <- read_csv("Fall2023/data/SF_Contact_list_w_CreatedDate.csv")

# separate User: Feature
library(tidyr)
longer_contact_list <- SF_Contacts |>
  separate_rows(c(`User: Feature`, Email), sep = "; ")

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
    Period = case_when(CreatedDate <= "2022-08-01" ~ "More than two semesters",
                       CreatedDate <= "2023-01-15" ~ "Last semester",
                       # AZ_created_date <= "2021-12-01" ~ "This semester"
                       TRUE ~ "This semester"
    )
  )

Fall_Survey2023_Oct19a$Period <- factor(Fall_Survey2023_Oct19a$Period, 
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

Fall_Survey2023_Oct19a$LastLogin_group <- factor(Fall_Survey2023_Oct19a$LastLogin_group, 
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
                              ] <- "Q4.2_1 I believe access to the Trellis data allows me to provide better service."
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q4.2_2-Please rate your level of agreement or disagreement with the following statements: - I would recommend Trellis capabilities to someone else." 
                              ] <- "Q4.2_2 I would recommend Trellis capabilities to someone else."

## Rename Q4.3
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q4.3-What do you like best about Trellis?"  
                              ] <- "Q4.3 What do you like best about Trellis?"

## Rename Q5.1
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q5.1_1-Please check this box if you are interested in participating in future Trellis user experience research. - Yes"                                                                     
                              ] <- "Q5.1_1 Yes to participating in future Trellis user experience research" 

## Rename Q 3.3 which should be Q5.2
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q3.3_1-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - Make it easier to do common, smaller tasks"
                              ] <- "Q5.2_1 Make it easier to do common, smaller tasks"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q3.3_30-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - Improved shared work with team members"
                              ] <- "Q5.2_2 Improved shared work with team members"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q3.3_31-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - Make it easier to work with large, more complicated data"
                              ] <- "Q5.2_3 Make it easier to work with large, more complicated data"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q3.3_2-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - More control over product options" 
                              ] <- "Q5.2_4 More control over product options"
names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Q3.3_3-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - Bring more Trellis data together in one place" 
                              ] <- "Q5.2_5 Bring more Trellis data together in one place"


# #### Rename Numeric Question Labels ####
# ## Rename Q2.1 
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.1_1-Please rate your level of agreement regarding your introduction and training for Trellis. - Trellis staff supported me in my initial adoption of Trellis."
# ] <- "Num-Q2.1_1 Trellis staff supported me in my initial adoption of Trellis."
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.1_2-Please rate your level of agreement regarding your introduction and training for Trellis. - The initial training sessions prepared me to use Trellis."
# ] <- "Num-Q2.1_2 The initial training sessions prepared me to use Trellis."
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.1_3-Please rate your level of agreement regarding your introduction and training for Trellis. - I am satisfied with my Trellis onboarding experience."
# ] <- "Num-Q2.1_3 I am satisfied with my Trellis onboarding experience." 
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.1_4-Please rate your level of agreement regarding your introduction and training for Trellis. - I've had positive interactions with the Trellis team."
# ] <- "Num-Q2.1_4 I've had positive interactions with the Trellis team."
# 
# names(Fall_Survey2023_Oct19b)
# 
# ## Rename Q2.3
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.3_1-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Technical issue form/ Submitting a support ticket" 
# ] <- "Num-Q2.3_1 Technical issue form/ Submitting a support ticket"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.3_2-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Trellis training workshops"
# ] <- "Num-Q2.3_2 Trellis training workshops"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.3_9-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - On-Screen guided help (Whatfix)"  
# ] <- "Num-Q2.3_3 On-Screen guided help (Whatfix)"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.3_5-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Asking my peers"                                                                  
# ] <- "Num-Q2.3_4 Asking my peers"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.3_6-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Trellis teams (MS Teams)"                                                         
# ] <- "Num-Q2.3_5 Trellis teams (MS Teams)"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.3_8-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - Ask Trellis team members directly. Please share who you most often contact below."
# ] <- "Num-Q2.3_6 Ask Trellis team members directly"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.3_8_TEXT-Which of these tools do you use to get help with Trellis? (select all that apply) - Ask Trellis team members directly. Please share who you most often contact below. - Text" 
# ] <- "Num-Q2.3_6a Team Member Name"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)== "Num-Q2.3_7-Which of these tools do you use to get help with Trellis? (select all that apply) - Selected Choice - None of these"
# ] <- "Num-Q2.3_7 None of these"
# 
# ## Rename Q2.4
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.4_1-Please rate your level of agreement with the following statements: - I am familiar with the Trellis resources available to me." 
# ] <- "Num-Q2.4_1 I am familiar with the Trellis resources available to me."
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.4_2-Please rate your level of agreement with the following statements: - I can find answers to my questions with Trellis resources."                                                    
# ] <- "Num-Q2.4_2 I can find answers to my questions with Trellis resources."
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q2.4_3-Please rate your level of agreement with the following statements: - I feel supported in my use of Trellis."                                                                        
# ] <- "Num-Q2.4_3 I feel supported in my use of Trellis."
# 
# ## Rename Q3.2
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q3.2_1-Please rate your level of agreement with the following statements: - I feel comfortable using Trellis." 
# ] <- "Num-Q3.2_1 I feel comfortable using Trellis."
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q3.2_2-Please rate your level of agreement with the following statements: - Trellis makes it easy for me to access the information I need."
# ] <- "Num-Q3.2_2 Trellis makes it easy for me to access the information I need."
# 
# ## Rename Q4.2
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q4.2_1-Please rate your level of agreement or disagreement with the following statements: - I believe access to the Trellis data allows me to provide better service."  
# ] <- "Num-Q4.2_1 I believe access to the Trellis data allows me to provide better service."
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q4.2_2-Please rate your level of agreement or disagreement with the following statements: - I would recommend Trellis capabilities to someone else." 
# ] <- "Num-Q4.2_2 I would recommend Trellis capabilities to someone else."
# 
# ## Rename Q4.3
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q4.3-What do you like best about Trellis?"  
# ] <- "Num-Q4.3 What do you like best about Trellis?"
# 
# ## Rename Q5.1
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q5.1_1-Please check this box if you are interested in participating in future Trellis user experience research. - Yes"                                                                     
# ] <- "Num-Q5.1_1 Yes to participating in future Trellis user experience research" 
# 
# ## Rename Q 3.3 which should be Q5.2
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q3.3_1-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - Make it easier to do common, smaller tasks"
# ] <- "Num-Q5.2_1 Make it easier to do common, smaller tasks"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q3.3_30-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - Improved shared work with team members"
# ] <- "Num-Q5.2_2 Improved shared work with team members"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q3.3_31-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - Make it easier to work with large, more complicated data"
# ] <- "Num-Q5.2_3 Make it easier to work with large, more complicated data"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q3.3_2-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - More control over product options" 
# ] <- "Num-Q5.2_4 More control over product options"
# names(Fall_Survey2023_Oct19b)[names(Fall_Survey2023_Oct19b)=="Num-Q3.3_3-Which part of user experience for online tools is most important? [rank from most important = 1 to least important = 5] - Bring more Trellis data together in one place" 
# ] <- "Num-Q5.2_5 Bring more Trellis data together in one place"



# Load in datafile of counts from FKM
October02_usercounts <- read_csv("Fall2023/data/2023_October02_ACTIVEuser_act_counts.csv")

# remove the n column
October02_usercounts <-October02_usercounts %>% 
  select(-n)

# merge count data to short data
Fall_Survey2023_Oct19c <- Fall_Survey2023_Oct19b %>% 
  left_join(October02_usercounts, by = join_by("Full Name" == "Name.x"))

Fall_Survey2023_Oct19c <- Fall_Survey2023_Oct19c %>% 
  distinct(`ResponseId-Response ID`, Emplid, .keep_all = TRUE)
  
# create a longer list based on Features
longer_Fall_Survey2023_Oct19 <- Fall_Survey2023_Oct19c |>
  separate_rows(c(`User: Feature-User: Feature`, `RecipientEmail-Recipient Email`), sep = "; ")

# drop some redundant columns
longer_Fall_Survey2023_Oct19 <- longer_Fall_Survey2023_Oct19 %>% 
  select(-`StartDate-Start Date`, -`EndDate-End Date`, -`Duration (in seconds)-Duration (in seconds)`, 
         -`RecipientLastName-Recipient Last Name`, -`RecipientFirstName-Recipient First Name`,
         -`RecordedDate-Recorded Date`)

# relocate column
longer_Fall_Survey2023_Oct19 <- longer_Fall_Survey2023_Oct19 %>% 
  relocate(`User: Feature-User: Feature`, .before = NULL, .after = NULL)

# save to csv
write_csv(Fall_Survey2023_Oct19c, "Fall2023/data/Merged_Data/Fall_survey2023_Oct30.csv")

# save to csv
write_csv(longer_Fall_Survey2023_Oct19, "Fall2023/data/Merged_Data/longer_Fall_Survey2023_Oct30.csv")

# analyzing name data
# column 22
Col22_analysis <- Fall_Survey2023_Oct19c %>%
  select(`ResponseId-Response ID`, Period, LastLogin_group, `Q2.3_6a Team Member Name`) %>% 
  dplyr::rename(team_member = `Q2.3_6a Team Member Name`) %>% 
  separate_rows(team_member)

Col22_analysis_original <- Fall_Survey2023_Oct19c %>%
  select(`ResponseId-Response ID`, `Q2.3_6a Team Member Name`) 

Col22_analysis <- left_join(Col22_analysis, Col22_analysis_original)

write_csv(Col22_analysis_original, "Fall2023/data/Merged_Data/Col22_original_2023_Oct30.csv")

write_csv(Col22_analysis, "Fall2023/data/Merged_Data/Col22_Fall_Survey2023_Oct30.csv")

# raw_count_Col22 <- Col22_analysis %>% 
#   select(team_member) %>% 
#   # group_by(`ResponseId-Response ID`) %>% 
#   count()

# read in analyzed data
library(readr)
Col22_original_hand_analysis <- read_csv("Fall2023/data/Col22_original_2023_Oct30_hand_analysis.csv")

# count data
Col22_pivot <- Col22_original_hand_analysis %>%
  pivot_longer(cols = c(
    "Ali Jeffords",             
    "Cindy Rupp-Valdez",         "Kim Winchell"      ,        "Emily Stultz"             ,
    "Zach Potter"       ,        "Raina O'Brien"      ,       "Kayli Hill"               ,
    "Makayla Nichols"    ,       "Frances Miller"      ,      "Rebecca \"Becky\" Pickett",
    "Sarah Wieland"       ,      "Rachel"                   
  ),
  names_to = "names",
  values_to = "team") %>%
  # drop_na("Tech Issues Form") %>%
  distinct() %>%
  dplyr::select(names, team, `ResponseId-Response ID`)


Col22_pivot %>% 
  select(team, names) %>% 
  filter(team == 1) %>% 
  group_by(names) %>% 
  count()

# Analysis of who answered which questions
library(purrr)
purrr::map(Fall_Survey2023_Oct19c, ~sum(is.na(.)))

finished_surveys <- Fall_Survey2023_Oct19c %>% 
  filter(`Finished-Finished`==TRUE)

purrr::map(finished_surveys, ~sum(is.na(.)))

Fall_Survey2023_Oct19c %>%
  select(c(15:21)) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

# 
# ggplot(data = Q5,
#   aes(x=task,fill=rank)+
#   geom_bar()+
#   coord_flip()+
#   scale_fill_manual(values = colours) + 
#   ylab("position_levels")
# 
#   #To keep track of each row of data 
#   dplyr::mutate(row = row_number()) %>%
#   # Bring data in separate rows splitting on comma
#   separate_rows(team_member, sep = ",\\s*") %>%
#   # Split data on colon to get data in two columns
#   separate_wider_delim(team_member, ", ", names = c("col", "value")) %>%
#   # Get data in wide format
#   pivot_wider(names_from = col, values_from = value) %>%
#   # Drop row column
#   select(-row)

# # likert analysis
# # Q 2.1
# Fall_Survey2023_Oct19b <- as.data.frame(Fall_Survey2023_Oct19b)
# q2_plot <- plot(likert(Fall_Survey2023_Oct19b[,11:14]), ordered = TRUE,
#                 auto.key = list(columns = 2)) # this looks good. but add more to it. 
# 
# 
# q2_plot <- q2_plot + labs(title = "Support and Adoption of Trellis",
#                           subtitle = "Question 2.1 - full data") 
# 
# print(q2_plot)
# 
# # q2.4
# q2_4plot <- plot(likert(Fall_Survey2023_Oct19b[,23:25]), ordered = TRUE, wrap= 40) # this looks good. but add more to it. 
# 
# q2_4plot <- q2_4plot + labs(title = "Ongoing Support for Trellis",
#                           subtitle = "Question 2.4 - full data") 
# 
# print(q2_4plot)
# 
# # q3.2
# q3_2plot <- plot(likert(Fall_Survey2023_Oct19b[,26:27]), ordered = TRUE, wrap= 40) # this looks good. but add more to it. 
# 
# q3_2plot <- q3_2plot + labs(title = "Ease of Using Trellis",
#                             subtitle = "Question 3.2 - full data") 
# 
# print(q3_2plot)
# 
# # q4.2
# q4_2plot <- plot(likert(Fall_Survey2023_Oct19b[,28:29]), ordered = TRUE, wrap= 40) # this looks good. but add more to it. 
# 
# q4_2plot <- q4_2plot + labs(title = "Recommending Trellis to Others",
#                             subtitle = "Question 4.2 - full data") 
# 
# print(q4_2plot)
# 
# # multiselect question Q2.3
# Q2_3 <- Fall_Survey2023_Oct19b %>%
#   pivot_longer(cols = c(
#     "Q2.3_1 Technical issue form/ Submitting a support ticket",                         
#     "Q2.3_2 Trellis training workshops",                                               
#     "Q2.3_3 On-Screen guided help (Whatfix)",                                          
#     "Q2.3_4 Asking my peers",                                                          
#     "Q2.3_5 Trellis teams (MS Teams)",                                                 
#     "Q2.3_6 Ask Trellis team members directly",                                        
#     "Q2.3_7 None of these"                                                            
#   ),
#                names_to = "tools",
#                values_to = "response") %>%
#   # drop_na("Tech Issues Form") %>%
#   distinct() %>%
#   dplyr::select(period, tools, response, `ResponseId-Response ID`)
# # 
# # replace with NA
# Q2_3[Q2_3==""] <- NA
# Q2_3 <- Q2_3 %>%
#   dplyr::mutate(response = dplyr::recode(response, "Ask Trellis team members directly. Please share who you most often contact below." = 'Ask Trellis team members'))
# 
# # # create a table to calculate percentages
# Q2_3_pct <- Q2_3%>% 
#   drop_na() %>% 
#   group_by(response) %>%
#   summarize(count = n()) %>%  # count records by species
#   mutate(pct = scales::percent(count/sum(count), accuracy = 0.1)) %>% 
#   # mutate(pct = count/sum(count))  %>% 
#   mutate(response = factor(response,
#                            levels = c("Technical issue form/ Submitting a support ticket", 
#                                       "Asking my peers",
#                                       "Trellis training workshops", 
#                                       "On-Screen guided help (Whatfix)",
#                                       "Trellis teams (MS Teams)",
#                                       "Ask Trellis team members",
#                                       "None of these" )))
# 
# Q2_3_pct
# 
# # show in a graph
# Q2_3_graph <- 
#   ggplot(Q2_3_pct, aes(response, count, fill = response)) +
#   geom_bar(stat='identity') +
#   labs(x = "", y = "") +
#   # coord_flip() +
#   theme(legend.position="none") +
#   scale_x_discrete(guide = guide_axis(n.dodge=2))+
#   # geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count)))) +
#   # geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count)))) +
#   xlab("") + 
#   ylab("Count") 
# 
# 
# Q2_3_graph <- Q2_3_graph + labs(title = "Which of these tools do you use to get help with Trellis?",
#                           subtitle = "Question 2.3 - full data") + 
#   theme(legend.position="none")
# 
# Q2_3_graph
