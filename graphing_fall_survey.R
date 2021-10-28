# tables and graphs
# create some tables by department
table(fall_survey$Primary_Department__r.Name)

overview_pct <- fall_survey %>% 
  group_by(Primary_Department__r.Name) %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

pct_graph <- 
  ggplot(overview_pct, aes(Primary_Department__r.Name, 
                           count, fill = Primary_Department__r.Name)) +
  geom_bar(stat='identity') +
  labs(x = "Units", y = "Respondents") +
  coord_flip() +
  theme(legend.position="none") +
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))

pct_graph <- pct_graph + labs(title = "Respondents across departments", 
                              subtitle = "full data",  fill = "Department Name")

print(pct_graph)

### redo the graph with parent organizations
overview_pct1 <- fall_survey %>% 
  group_by(Parent_Organization__c) %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

pct_graph1 <- 
  ggplot(overview_pct1, aes(Parent_Organization__c, 
                            count, fill = Parent_Organization__c)) +
  geom_bar(stat='identity') +
  labs(x = "Units", y = "Respondents") +
  coord_flip() +
  theme(legend.position="none") +
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))

pct_graph1 <- pct_graph1 + labs(title = "Respondents across parent organizations", 
                                subtitle = "full data",  fill = "Department Name")

print(pct_graph1)



# descriptive responses to questions
## responses to Q3 
bargraph0 <- fall_survey %>%
  pivot_longer(Q3_1:Q3_4, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = question, colour = question)) +
  geom_bar() +
  geom_text(stat="count", aes(label=..count..), vjust=1, hjust=1) +
  # geom_text(stat='count', aes(label = ..count..), position = position_stack(vjust = 0.5)) +
  geom_boxplot() +
  labs(x = "response", y = "number of respondents")

bargraph0 <- bargraph0 + labs(title = "",
                              subtitle = "")
print(bargraph0)

# # rename variable names
names(fall_survey)[names(fall_survey)=="Q3_1"] <- "Q3_1 My interaction with the Trellis team has been positive."
names(fall_survey)[names(fall_survey)=="Q3_2"] <- "Q3_2 My interactions with the Trellis team have been clear."
names(fall_survey)[names(fall_survey)=="Q3_3"] <- "Q3_3 The training sessions prepared me to use Trellis."
names(fall_survey)[names(fall_survey)=="Q3_4"] <- "Q3_4 Trellis staff supported me in my initial adoption of Trellis."
# LIKERT for q3
q3_plot <- plot(likert(fall_survey[,18:21]), ordered = F, wrap= 40) # this looks good. but add more to it. 
q3_plot <- q3_plot + labs(title = "Interactions with Trellis staff",
                          subtitle = "Question 3 - full data") 

print(q3_plot)

# responses to Q4 
Q4graph <- fall_survey %>%
  # mutate(Q4 = fct_infreq(Q4)) %>%
  drop_na(Q4) %>% 
  ggplot(aes(x = factor(Q4)), count) +
  geom_bar(fill = "blue") +
  # coord_flip() +
  labs(x = "", y = "responses")

## fill response with another variable like Profile.Name
Q4graph <- fall_survey %>%
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

# plot likert


# for q6 
names(fall_survey)[names(fall_survey)=="Q6_1"] <- "Q6_1 I understand what resources are available to support my use of Trellis."
names(fall_survey)[names(fall_survey)=="Q6_2"] <- "Q6_2 I continue to feel supported in my use of Trellis."
names(fall_survey)[names(fall_survey)=="Q6_3"] <- "Q6_3 I feel knowledgeable about the types of tools that are available through Trellis."

q6_plot <- plot(likert(fall_survey[,23:25]), ordered = T, wrap= 40) # this looks good. but add more to it. 

q6_plot <- q6_plot + labs(title = "Suport for and knowledge of Trellis",
                            subtitle = "Question 6 - full data") 

print(q6_plot)

# response for Q7 
# drop the legend
Q7graph <- fall_survey[,26:31]

# pivot the table longer
long_Q7 <- Q7graph %>% 
  pivot_longer(everything(), names_to = "question", values_to = "response", values_drop_na = TRUE)

# create a table to calculate percentages
Q7_pct <- long_Q7 %>% 
  group_by(response) %>%
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count))

Q7graph <- 
  ggplot(Q7_pct, aes(response,count, fill = response)) +
  geom_bar(stat='identity') +
  labs(x = "", y = "") +
  # coord_flip() +
  theme(legend.position="none") +
  geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))


# Q7graph <- fall_survey %>%
#   pivot_longer(Q7_1:Q7_6, names_to = "question", values_to = "response", 
#                values_drop_na = TRUE) %>%
#   ggplot(aes(x = response, fill = question, colour = question)) +
#   geom_bar() +
#   # geom_text(stat='count', aes(label=..count..), vjust=1, hjust=1) +
#   geom_text(stat='count', aes(label = ..count..), position = position_stack(vjust = 0.5)) +
#   geom_boxplot() +
#   labs(x = "groups", y = "number of respondents")

Q7graph <- Q7graph + labs(title = "Which of these tools do you use to get help with Trellis?",
                          subtitle = "Question 7 - full data") + 
                          theme(legend.position="none")

print(Q7graph)

# for q9 
# # rename variable names
names(fall_survey)[names(fall_survey)=="Q9_1"] <- "Q9_1 Trellis makes it easier to do my job."
names(fall_survey)[names(fall_survey)=="Q9_2"] <- "Q9_2 Trellis is easy to use."
names(fall_survey)[names(fall_survey)=="Q9_3"] <- "Q9_3 I feel comfortable using Trellis."
names(fall_survey)[names(fall_survey)=="Q9_4"] <- "Q9_4 I have had positive experiences using Trellis."

q9_plot <- plot(likert(fall_survey[,32:35]), ordered = F, wrap= 40) # this looks good. but add more to it. 
q9_plot <- q9_plot + labs(title = "Ease of Trellis",
                            subtitle = "Question 9 - full data") 

print(q9_plot)

# for q11
# # rename variable names
names(fall_survey)[names(fall_survey)=="Q11_1"] <- "Q11_1 Trellis can help students access resources when they need them."
names(fall_survey)[names(fall_survey)=="Q11_2"] <- "Q11_2 Trellis CRM has allowed me to be more strategic in my interactions."
names(fall_survey)[names(fall_survey)=="Q11_3"] <- "Q11_3 I would recommend Trellis capabilities to someone else."

q11_plot <- plot(likert(fall_survey[,36:38]), ordered = F, wrap= 40) # this looks good. but add more to it. 
q11_plot <- q11_plot + labs(title = "Benefits of Trellis",
                          subtitle = "Question 11 - full data") 

print(q11_plot)

### looking at q12
comments <- table(fall_survey$Q12)

# table for all products
# starts with counter
counter_table <- fall_survey %>%
  select(starts_with("counter"))

fall_survey %>%
  pivot_longer(counter.Events:counter.Service.Desk, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response)) +
  geom_bar()

counter_graph <- counter_graph + labs(title = "",
                              subtitle = "")
print(counter_graph)

# group by Profile.Name


profile_graph <- fall_survey %>%
  group_by(Profile.Name) %>% 
  # summarise(count = n()) %>% 
  # mutate(Q4 = fct_infreq(Q4)) %>%
  # drop_na(Profile.Name) %>% 
  ggplot(aes(x = Profile.Name), count) +
  geom_bar(fill = "blue") +
  coord_flip() +
  labs(x = "", y = "responses")

profile_graph <- profile_graph + labs(title = "",
                          subtitle = "full data")

print(profile_graph)

# textured plots with fill of Profile.Name
# subset to completed Q4 only
completed_survey <- fall_survey %>% 
  filter(Finished == "TRUE") 
# %>% 

# redo question 4 with profile name
profile_graph <- fall_survey %>% 
  filter(!is.na(Q4)) %>% 
  filter(!is.na(Profile.Name)) %>% 
  ggplot(aes(x=Q4, y=stat(count), fill = Profile.Name)) +
  # geom_step(data = overview_pct) +
  # geom_bar(data = first_mode)
  geom_bar(stat='count',  
           position=position_stack(vjust=0.5)) 
  # labs(x = "", y = "") +
  # geom_text(aes(label = ..count..), position=position_stack(vjust=0.5))

profile_graph <- profile_graph + labs(title = "How satisfied were you with your initial training in Trellis?", 
                                subtitle = "Q4 based on profile names",  fill = "Profile Name") 

print(profile_graph)

# time of user initiation 
# re evaluate Q4 

user_Q4 <- fall_survey %>%
  group_by(period) %>% 
  # mutate(Q4 = fct_infreq(Q4)) %>%
  drop_na(Q4) %>% 
  ggplot(aes(x = factor(Q4)), y = ..count.., fill = period) +
  geom_bar(aes(fill = period)) +
  coord_flip() +
  scale_color_brewer(palette = "Blues") +
  labs(x = "", y = "responses")

user_Q4 <- user_Q4 + labs(title = "How satisfied were you with your initial training in Trellis?",
                          subtitle = "Question 4 based on user created date", fill = "User Created Date")+ 
                          scale_fill_brewer(palette = "Dark2")

print(user_Q4)

# reevaluate Q7 with periods 
# create a table to calculate percentages
# Q7_pct <- long_Q7 %>% 
#   group_by(response) %>%
#   summarize(count = n()) %>%  # count records by species
#   mutate(pct = count/sum(count))
# 
# Q7subset <- subset(fall_survey, select = c(period,Q7_1,Q7_2,Q7_3,Q7_4,Q7_5,Q7_6))

# pivot the table longer
# long_Q7 <- Q7subset %>% 
#   pivot_longer(everything(), names_to = "question", values_to = "response", values_drop_na = TRUE)
# Q7 with period fill

Q7graph <- fall_survey %>% 
  # group_by(period) %>% 
  pivot_longer(Q7_1:Q7_6, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = period)) +
  geom_bar(aes(fill = period)) +
  coord_flip() +
  # geom_text(stat="count", aes(label=..count..), vjust=1, hjust=1) +
  # # geom_text(stat='count', aes(label = ..count..), position = position_stack(vjust = 0.5)) +
  # geom_boxplot() +
  labs(x = "response", y = "number of respondents") 
  # theme(legend.position=element_blank())
  # geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))


Q7graph <- Q7graph + labs(title = "Which of these tools do you use to get help with Trellis?",
                          subtitle = "Question 7 - full data", fill = "User Created Date") + 
                          scale_fill_brewer(palette = "Dark2")

print(Q7graph)

# Q7 with profile name fill
Q7graph1 <- fall_survey %>% 
  # group_by(period) %>% 
  pivot_longer(Q7_1:Q7_6, names_to = "question", values_to = "response", 
               values_drop_na = TRUE) %>%
  ggplot(aes(x = response, fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name)) +
  # geom_text(stat="count", aes(label=..count..), vjust=1, hjust=1) +
  # # geom_text(stat='count', aes(label = ..count..), position = position_stack(vjust = 0.5)) +
  # geom_boxplot() +
  labs(x = "response", y = "number of respondents") 
# theme(legend.position=element_blank())
# geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count))))


Q7graph1 <- Q7graph1 + labs(title = "Which of these tools do you use to get help with Trellis?",
                          subtitle = "Question 7 - full data", fill = "Profile Name") 

print(Q7graph1)
## breakdown of user created date and profile name
user_graph <- fall_survey %>%
  group_by(Profile.Name) %>% 
  drop_na(period) %>% 
  ggplot(aes(x = period, y = ..count.., fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name)) +
  # coord_flip() +
  labs(x = "time using Trellis", y = "responses")

user_graph <- user_graph + labs(title = "Breakdown of users based on experience",
                          subtitle = "full data", fill = "Profile Name")

print(user_graph)

### check for when people drop out of the study using Progress
summary(fall_survey$Progress)
fall_survey$Progress_factor <- as.factor(fall_survey$Progress)

table(fall_survey$Progress_factor)

# looking at graphing drop outs with violin plots
user_graph1 <- fall_survey %>%
  group_by(Progress_factor) %>% 
  # drop_na(period) %>% 
  ggplot(aes(x = Progress_factor, y = ..count.., fill = Profile.Name)) +
  geom_bar(aes(fill = Profile.Name)) +
  # coord_flip() +
  labs(x = "Progress", y = "responses")

user_graph1 <- user_graph1 + labs(title = "",
                                subtitle = "full data", fill = "Profile Name")

print(user_graph1)

# check on the counter stuff
# all products has some unparsed information
#
# rename counter variables

counter_data <- subset(fall_survey, select=c(period, 
         counter.Events,counter.Marketing...SF,counter.Scheduling.Notes,counter.Service.Desk))

summary(counter_data)
# labels <- c(counter.Events = "Events", counter.Marketing...SF = "Marketing SF",
#             counter.Scheduling.Notes = "Scheduling Notes", counter.Service.Desk = "Service Desk")

counter_data <- counter_data %>% mutate_all(as.factor)

df_long <- counter_data %>%
  pivot_longer(cols = -period, names_to = "expo", 
               values_drop_na = TRUE)  
  
counter_graph <- ggplot(df_long) + 
  geom_bar(aes(x = expo,  fill = period)) + 
  scale_x_discrete(labels = c('Events','Marketing SF','Scheduling Notes','Service Desk')) +
  labs(x = "tools", y = "number of respondents") +
  coord_flip() 

counter_graph <- counter_graph + labs(title = "What tools users use",
                                      subtitle = "full data", fill = "User Created Date") +
  scale_fill_brewer(palette = "Dark2")

print(counter_graph)

##
counter_graph0 <- fall_survey %>%
  pivot_longer(counter.Events:counter.Service.Desk, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response, fill = question, colour = question)) +
  geom_bar() +
  # geom_text(stat='count', aes(label=..count..), vjust=-1) +
  geom_boxplot() +
  labs(x = "", y = "Number of Users") 

counter_graph0 <- counter_graph0 + labs(title = "",
                                  subtitle = "full data", fill = "Profile Name")

print(counter_graph0)


# violin plot for time to complete survey
# compare completed and incomplete surveys based on time to complete in sec

fall_survey %>% 
  group_by(as.factor(Progress)) %>%
  summarise(mean_min = mean(Duration..in.seconds.)/60)

# violin_first <- first_mode %>%
#   group_by(Mode) %>%
#   drop_na(Mode) %>% 
#   mutate(Mode_start = n()) %>% 
#   ungroup()

violin_first <- ggplot(fall_survey, aes(x = period, y = Progress, fill = period)) +
  geom_violin(scale = "count") +
  ylab("Percent complete") +
  xlab("User created date") 

violin_first <- violin_first +  labs(title = "Who completed the survey", 
                                     subtitle = "full data") 

print(violin_first)

##### Table of profile names
table(fall_survey$Profile.Name)

