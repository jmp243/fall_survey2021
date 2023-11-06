# Jung Mee Park
# Fall 2023 Survey Analysis Visuals
# last run 10-31-2023

library("RColorBrewer")   

Fall_Survey2023_Oct19a %>% 
  # Fall_Survey2023_Oct19a %>% 
  group_by(Period) %>%
  dplyr::summarize(n = n()) %>%  # count records
  dplyr::mutate(rel.freq =  scales::percent(n/sum(n), accuracy = 0.1))

#### set up likert questions####
names(Fall_Survey2023_Oct19b)

Fall_Survey2023_Oct19b <-Fall_Survey2023_Oct19b %>% 
  dplyr::mutate(dplyr::across(.cols=c(11:14, 23:29),
                .fns = ~ factor(.x, 
                                levels = c("Strongly disagree", "Somewhat disagree", 
                                           "Neither agree nor disagree", 
                                           "Somewhat agree", "Strongly agree"),
                                ordered = TRUE)))

# levels <- c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", 
#             "Somewhat disagree","Strongly disagree")
# 
# Fall_Survey2023_Oct19b  %>%
#   tibble::as_tibble() %>%
#   dplyr::mutate(dplyr::across(c(Comp1Letter, Comp2Letter, Comp3Letter) ,
#                               ~forcats::parse_factor(., levels = levels)))


# likert analysis
# Q 2.1
Fall_Survey2023_Oct19b <- as.data.frame(Fall_Survey2023_Oct19b)

q2_plot <- plot(likert(Fall_Survey2023_Oct19b[,11:14]), ordered = FALSE,) # this looks good. but add more to it. 


q2_plot <- q2_plot + labs(title = "Support and Adoption of Trellis",
                          subtitle = "Question 2.1 - full data") 

print(q2_plot) # 1040 by 300

# q2.4 minus 34 unanswered
q2_4plot <- plot(likert(Fall_Survey2023_Oct19b[,23:25]), 
                 ordered = FALSE,
                 wrap= 40,
                 # auto.key=list(columns = 2)
                 ) # this looks good. but add more to it. 

q2_4plot <- q2_4plot + labs(title = "Ongoing Support for Trellis",
                            subtitle = "Question 2.4 - full data") 

print(q2_4plot) # dimension 980 by 300
# getwd()
# ggsave("Fall2023/Fall2023_visuals/q2_4plot.png", limitsize = FALSE, width = 980, height = 300, units = "cm")

# q3.2
q3_2plot <- plot(likert(Fall_Survey2023_Oct19b[,26:27]), ordered = TRUE, wrap= 40,) # this looks good. but add more to it. 

q3_2plot <- q3_2plot + labs(title = "Ease of Using Trellis",
                            subtitle = "Question 3.2 - full data") 

print(q3_2plot)

# q4.2
q4_2plot <- plot(likert(Fall_Survey2023_Oct19b[,28:29]), ordered = TRUE, wrap= 40) # this looks good. but add more to it. 

q4_2plot <- q4_2plot + labs(title = "Recommending Trellis to Others",
                            subtitle = "Question 4.2 - full data") 

print(q4_2plot)

# multiselect question Q2.3
Q2_3 <- Fall_Survey2023_Oct19b %>%
  pivot_longer(cols = c(
    "Q2.3_1 Technical issue form/ Submitting a support ticket",                        
    "Q2.3_2 Trellis training workshops",                       
    "Q2.3_3 On-Screen guided help (Whatfix)",                      
    "Q2.3_4 Asking my peers",                     
    "Q2.3_5 Trellis teams (MS Teams)",                    
    "Q2.3_6 Ask Trellis team members directly",                   
    "Q2.3_7 None of these"
  ),
  names_to = "tools",
  values_to = "response") %>%
  # drop_na("Tech Issues Form") %>%
  distinct() %>%
  dplyr::select(Period, tools, response, `ResponseId-Response ID`)
# 
# replace with NA
Q2_3[Q2_3==""] <- NA
Q2_3 <- Q2_3 %>%
  dplyr::mutate(response = dplyr::recode(response, "Ask Trellis team members directly. Please share who you most often contact below." = 'Ask Trellis team members'))

# # create a table to calculate percentages
Q2_3_pct <- Q2_3%>% 
  drop_na() %>% 
  group_by(Period, response) %>% # removed Period, 
  dplyr::summarize(count = n()) %>%  # count records by species
  dplyr::mutate(pct = scales::percent(count/sum(count), accuracy = 0.1)) %>% 
  # mutate(pct = count/sum(count))  %>% 
  mutate(response = factor(response,
                           levels = c("Technical issue form/ Submitting a support ticket", 
                                      "Asking my peers",
                                      "Trellis training workshops", 
                                      "On-Screen guided help (Whatfix)",
                                      "Trellis teams (MS Teams)",
                                      "Ask Trellis team members",
                                      "None of these" )))

Q2_3_pct
# # create a percentage graph
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
#   names_to = "tools",
#   values_to = "response") %>%
#   # drop_na("Tech Issues Form") %>%
#   distinct() %>%
#   dplyr::select(Period, tools, response)


# replace with NA
# Q2_3[Q2_3==""] <- NA

Q8graph <- 
  ggplot(Q2_3_pct, aes(response,count, fill = Period)) +
  geom_bar(stat='identity') +
  labs(x = "", y = "") +
  # coord_flip() +
  theme(legend.position="none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  # geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count)))) +
  # geom_text(aes(label = scales::percent(pct))) +
  
  xlab("") + 
  ylab("Count") +
  scale_fill_brewer(palette = "Set2") 


Q8graph <- Q8graph + labs(title = "Which of these tools do you use to get help with Trellis?",
                          subtitle = "Question 8 - full data") + 
  theme(legend.position="none")

print(Q8graph)

# show in a graph
 
# label_df = my_df %>% group_by(percentage_int) %>% summarise(n=n())
# sum_count <- 
#   Q2_3_pct %>%
#   group_by(Period) %>%
#   summarise(max_pos = sum(count))

# label_df = my_df %>% group_by(percentage_int) %>% summarise(n=n())

library(plyr)
# Q2_3_pct <- ddply(Q2_3_pct, .(Period), transform, pos = 
#                 cumsum(count) - (0.5 * count));
library(ggplot2)
# THIS WORKS
Q2_3_graph <- 
  ggplot(Q2_3_pct, aes(response, count)) +
  geom_bar(aes(fill = Period), stat='identity') +
  # labs(x = "", y = "") +
  # coord_flip() +
  # theme(legend.position="none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  # geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count)))) +
  # geom_text(aes(label = scales::percent(pct), y = if_else(count > 0.1*max(count), count/2, count+ 0.05*max(count)))) +
  xlab("") + 
  ylab("Count") + 
  geom_text(
    aes(label = after_stat(y), group = response), 
    stat = 'summary', fun = sum, vjust = -0.5
  ) +
  scale_fill_brewer(palette = "Set2") 
# +
#   geom_text(aes(label = count,y = pos), size = 3, vjust = 1, colour = "black") 


  # geom_text(aes(label = count), vjust = 1.5, colour = "black")
  # geom_text(data = sum_count,
  #   aes(y = max_pos, label = max_pos), size = 4,
  #   vjust = -.5)

Q2_3_graph <- Q2_3_graph + labs(title = "Which of these tools do you use to get help with Trellis?",
                                subtitle = "Question 2.3 - full data") + 
  guides(fill=guide_legend(title='Initiation Period'))


  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="none")
  # theme(legend.position="none") 


print(Q2_3_graph) #1150 times 450

# different percentages for tools use
Q2_3_pct2 <- Q2_3%>% 
  drop_na() %>% 
  group_by(response) %>%
  dplyr::summarize(count = n()) %>%  # count records by species
  dplyr::mutate(pct = scales::percent(count/sum(count), accuracy = 0.1)) %>% 
  # mutate(pct = count/sum(count))  %>% 
  mutate(response = factor(response,
                           levels = c("Technical issue form/ Submitting a support ticket", 
                                      "Asking my peers",
                                      "Trellis training workshops", 
                                      "On-Screen guided help (Whatfix)",
                                      "Trellis teams (MS Teams)",
                                      "Ask Trellis team members",
                                      "None of these" )))


  # unique()
# Q8_pct2 <- Q8_longer %>%
#   filter(response != "") %>% 
#   filter(product != "") %>%
#   group_by(response, product) %>%
#   summarize(count = n()) %>%  # count records by species
#   mutate(pct = count/sum(count))

q8_plota <- Q2_3_pct2  %>%
  # count how often each class occurs in each sample.
  # group_by(response)%>%
  # # drop_na(response) %>%
  dplyr::mutate(pct = count/sum(count)) %>% 
  ggplot(aes(x = response, y = count), fill = response) +
  geom_bar(stat = "identity") +
  geom_col(width=0.7)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  xlab("") + 
  ylab("Count") +
  geom_text(aes(label = paste0(round(pct * 100), '%')),colour="white", 
            position = position_stack(vjust = 0.5), size = 3) 


q8_plota <- q8_plota + labs(title = "Tools used to resolve issues",
                                  subtitle = "Question 2.3 - full data") 
print(q8_plota) # not a 

# upset plot for question 
library(ComplexUpset)
# library(UpSetR)
# for col 15 to 21 in c
upset_data_Q8 <- as.data.frame(Fall_Survey2023_Oct19c)

upset_data_Q8 <- upset_data_Q8 %>% 
  dplyr::select(`ResponseId-Response ID`, `Q2.3_1 Technical issue form/ Submitting a support ticket`, 
                `Q2.3_2 Trellis training workshops`, `Q2.3_3 On-Screen guided help (Whatfix)`,
                `Q2.3_4 Asking my peers`, `Q2.3_5 Trellis teams (MS Teams)`, 
                `Q2.3_6 Ask Trellis team members directly`, `Q2.3_7 None of these`,
                Period, `User: Feature`) %>% 
  dplyr::distinct() 

# Add NA to missing period
# upset_data_Q8[upset_data_Q8$Period == '', 'Period'] = NA

# # Add NA to missing Feature
# upset_data_Q8[upset_data_Q8$`User: Feature`== '', '`User: Feature`'] = NA
# 

upset_data_Q8 <- upset_data_Q8 %>% 
  # dplyr::select(-`ResponseId-Response ID`) %>% 
  data.frame() %>%
  # t() %>% # transpose the result, ugh
  as_tibble()

upset_data_Q8b <- upset_data_Q8 %>% 
  dplyr::mutate(`Technical form` = ifelse(Q2.3_1.Technical.issue.form..Submitting.a.support.ticket == "Technical issue form/ Submitting a support ticket", 1, 0)) %>% 
  dplyr::mutate(`Trellis training` = ifelse(Q2.3_2.Trellis.training.workshops == "Trellis training workshops", 1, 0)) %>% 
  dplyr::mutate(`On-Screen guide (Whatfix)` = ifelse(Q2.3_3.On.Screen.guided.help..Whatfix. == "On-Screen guided help (Whatfix)", 1, 0)) %>% 
  dplyr::mutate(`Ask a peer` = ifelse(Q2.3_4.Asking.my.peers == "Asking my peers", 1, 0)) %>% 
  dplyr::mutate(`MS Teams` = ifelse(Q2.3_5.Trellis.teams..MS.Teams. == "Trellis teams (MS Teams)", 1, 0)) %>% 
  dplyr::mutate(`Trellis team member` = ifelse(Q2.3_6.Ask.Trellis.team.members.directly  == "Ask Trellis team members directly. Please share who you most often contact below.", 1, 0)) %>% 
  dplyr::mutate(`None of these` = ifelse(Q2.3_7.None.of.these == "None of these", 1, 0)) 
# %>% 
#   mutate_if(is.character, ~replace(., is.na(.), 0))

upset_data_Q8b <- upset_data_Q8b %>% 
  data.frame() %>%
  as_tibble()

write_csv(upset_data_Q8b, "Fall2023/data/Merged_Data/Fall2023_upset_Q2-3_Oct30.csv")

# Fall_Survey2023_Oct19b %>%
#   select(c(15:21)) %>%  # replace to your needs
#   summarise_all(funs(sum(is.na(.))))

# upset_data_Q8b[is.na(upset_data_Q8b)] <- 0

# Check how many actually answered the question
upset_data_Q8b %>% 
  # mutate(across(.cols = 11:17,
  #               .fns = ~ ifelse(is.na(.x) == TRUE, 0, 1))) %>%
  mutate_at(c(11:17), ~replace(., is.na(.), 0)) %>% 
  mutate(num_na_Q15 = Technical.form + Trellis.training + On.Screen.guide..Whatfix. + Ask.a.peer + 
           MS.Teams + Trellis.team.member + None.of.these) %>% 
  dplyr::count(num_na_Q15 == 0) # 211 folks answered the prompt


# get color hex code
brewer.pal(3, "Set2") #"#66C2A5" "#FC8D62" "#8DA0CB"
## ComplexUpset tutorial
# subset just the question upset plot
Q8 = colnames(upset_data_Q8)[2:8]

# upset_plot_Q8 <- upset_data_Q8b %>%
#   dplyr::select(`Technical form`, `Trellis training`, `On-Screen guide (Whatfix)`, `Ask a peer`, `MS Teams`,
#                 `Trellis team member`, `None of these`) %>%
#   mutate_if(is.character, ~replace(., is.na(.), 0))

# 
# upset_plot_Q8 <- upset_data_Q8 %>% 
#   dplyr::select(-`ResponseId-Response ID`)
  

# upset_plot_Q8 %>%
#   filter(`Ask a peer` ==1) %>%
#   count()
names(upset_data_Q8b)
# set_vars8 <- c("Technical.form", "Trellis.training", "On.Screen.guide..Whatfix.", 
#                "Ask.a.peer",
#               "MS.Teams", "Trellis.team.member", "None.of.these")
set_vars8 = colnames(upset_data_Q8b)[11:17]
# upset_data_Q8b$Period <- as.numeric(upset_data_Q8b$Period)
# upset_plot_Q8 %>% print(n = nrow(upset_plot_Q8))
# 


Q8_upset_plot <- ComplexUpset::upset(data = upset_data_Q8b,  
                                     intersect = set_vars8,
                                     set_sizes=FALSE,
                                     name = "",
                                     min_size = 3,
                                     min_degree=1,
                                     themes = upset_themes, 
                                     # base_annotations=list(
                                     #   'Intersection size'=intersection_size(
                                     #     mode = mode,
                                     #     counts=FALSE,
                                     #     mapping=aes(fill=Period)
                                     # 
                                     #   ))
                                     annotations = list(
                                       'Initiation Period'=(
                                       ggplot(mapping = aes(fill=Period))
                                       +geom_bar(stat = 'count', position='fill')
                                     + scale_y_continuous(labels=scales::percent_format())
                                     + scale_fill_manual(values=c(
                                       'This semester'='#66C2A5', 'Last semester'='#FC8D62',
                                       'More than two semesters'='#8DA0CB'
                                       ))
                                     # + ylab('Initiation Period')
                                     +  labs(title = "Q2.3 Which of these tools do you use to get help with Trellis?")
                                       )
                                     ),
                                  width_ratio=0.1)


                                     # width_ratio=0.1)  


Q8_upset_plot

# fill the bars

upset(
  upset_data_Q8b,
  set_vars8,
  set_sizes = FALSE,
  min_size = 3,
  min_degree=1,
  base_annotations=list(
    'Intersection size'=intersection_size(
      # counts=FALSE,
      mapping=aes(fill=Period)
    ) + scale_fill_manual(values=c(
      'This semester'='#66C2A5', 'Last semester'='#FC8D62',
      'More than two semesters'='#8DA0CB'
    ))
    +  labs(title = "Q2.3 Which of these tools do you use to get help with Trellis?")
  ),
  width_ratio=0.1
)

# # upset test
# upset_test(upset_data_Q8b, set_vars8)

### UPSET PLOT for Features used
upset_plot_Features <- Fall_Survey2023_Oct19c %>% 
  dplyr::select(`ResponseId-Response ID`, `User: Feature-User: Feature`) %>% 
  dplyr::distinct() 

# upset_plot_Features  <- upset_plot_Features  |>
#   separate_rows(c(`User: Feature`, `ResponseId-Response ID`), sep = "; ")

wider_upset_plot_Features <- upset_plot_Features %>%
  separate_rows(`User: Feature-User: Feature`, sep = "; ") %>%
  # mutate(`User: Feature-User: Feature` = 1) %>%
  pivot_wider(names_from = `User: Feature-User: Feature`, values_from = `User: Feature-User: Feature`)
# write to CSV for analysis in Tableau
write_csv(wider_upset_plot_Features, "Fall2023/data/Merged_Data/Fall2023_wider_upset_plot_Features.csv")

# turn features into binary
upset_plot_Q15 <- wider_upset_plot_Features %>% 
  dplyr::mutate(`Marketing Cloud` = ifelse(`Marketing Cloud` == "Marketing Cloud" , 1, 0)) %>% 
  dplyr::mutate(`External Partners` = ifelse(`External Partners` ==  "External Partners", 1, 0)) %>% 
  dplyr::mutate(`Case Notes` = ifelse(`Case Notes` == "Case Notes", 1, 0)) %>% 
  dplyr::mutate(`Custom Forms` = ifelse(`Custom Forms` == "Custom Forms" , 1, 0)) %>% 
  dplyr::mutate(`Plans` = ifelse(`Plans` == "Plans"  , 1, 0)) %>% 
  dplyr::mutate(`Scheduling` = ifelse(`Scheduling` ==  "Scheduling", 1, 0)) %>% 
  dplyr::mutate(`Events` = ifelse(`Events` ==  "Events"   , 1, 0)) %>% 
  dplyr::mutate(`Service Desk` = ifelse(`Service Desk` ==  "Service Desk"   , 1, 0)) %>% 
  dplyr::mutate(`Texting` = ifelse(`Texting` == "Texting", 1, 0)) %>% 
  dplyr::mutate(`Email to Case` = ifelse(`Email to Case` == "Email to Case" , 1, 0)) %>% 
  dplyr::mutate(`Chat to Case` = ifelse(`Chat to Case` == "Chat to Case"  , 1, 0)) 


set_vars15 = colnames(upset_plot_Q15)[2:12]

# merge in Period, last login or other
q15_addendum <- Fall_Survey2023_Oct19c %>% 
  select(`ResponseId-Response ID`, Period, LastLogin_group, `Parent Organization`)

upset_plot_Q15 <- upset_plot_Q15 %>% 
  left_join(q15_addendum)
upset_plot_Q15 %>% print(n = nrow(upset_plot_Q15))

Q15_upset_plot <- ComplexUpset::upset(data = upset_plot_Q15, intersect = set_vars15, )
Q15_upset_plot

## fancier upset plot for 15
Q5_upset_plot <- ComplexUpset::upset(data = upset_plot_Q15,  
                                     intersect = set_vars15,
                                     # set_sizes=FALSE,
                                     name = "",
                                     # min_size = 3,
                                     min_degree=1,
                                     themes = upset_themes, 
                                     # base_annotations=list(
                                     #   'Intersection size'=intersection_size(
                                     #     mode = mode,
                                     #     counts=FALSE,
                                     #     mapping=aes(fill=Period)
                                     # 
                                     #   ))
                                     annotations = list(
                                       'Initiation Period'=(
                                         ggplot(mapping = aes(fill=Period))
                                         +geom_bar(stat = 'count', position='fill')
                                         + scale_y_continuous(labels=scales::percent_format())
                                         + scale_fill_manual(values=c(
                                           'This semester'='#66C2A5', 'Last semester'='#FC8D62',
                                           'More than two semesters'='#8DA0CB'
                                         ))
                                         # + ylab('Initiation Period')
                                         +  labs(title = "Features Accessed")
                                       )
                                     ),
                                     width_ratio=0.1)


# width_ratio=0.1)  


Q15_upset_plot
# fill the bars

upset(
  upset_plot_Q15,
  set_vars15,
  set_sizes = FALSE,
  # min_size = 3,
  min_degree=1,
  base_annotations=list(
    'Intersection size'=intersection_size(
      # counts=FALSE,
      mapping=aes(fill=Period)
    ) + scale_fill_manual(values=c(
      'This semester'='#66C2A5', 'Last semester'='#FC8D62',
      'More than two semesters'='#8DA0CB'
    ))
    +  labs(title = "Features used")
  ),
  width_ratio=0.1
)

# multiselect for feature
Q15 <- upset_plot_Q15 %>%
  pivot_longer(cols = c(
    "Marketing Cloud",        "External Partners",      "Case Notes" ,           
    "Custom Forms"    ,       "Plans"             ,     "Scheduling"  ,           "Events"  ,              
    "Service Desk"     ,      "Texting"            ,    "Email to Case",          "Chat to Case"  
  ),
  names_to = "features",
  values_to = "response") %>%
  # drop_na("Tech Issues Form") %>%
  distinct() %>%
  dplyr::select(Period, features, response, `ResponseId-Response ID`)

# replace with NA
# Q2_3[Q2_3==""] <- NA
# Q2_3 <- Q2_3 %>%
#   dplyr::mutate(response = dplyr::recode(response, "Ask Trellis team members directly. Please share who you most often contact below." = 'Ask Trellis team members'))

# # create a table to calculate percentages
Q15_pct <- Q15 %>% 
  drop_na() %>%
  group_by(Period, features, `ResponseId-Response ID`) %>%
  dplyr::summarize(count = n()) %>%  # count records by species
  dplyr::mutate(pct = scales::percent(count/sum(count), accuracy = 0.1)) 
  # # mutate(pct = count/sum(count))  %>% 
  # mutate(response = factor(response,
  #                          levels = c("Technical issue form/ Submitting a support ticket", 
  #                                     "Asking my peers",
  #                                     "Trellis training workshops", 
  #                                     "On-Screen guided help (Whatfix)",
  #                                     "Trellis teams (MS Teams)",
  #                                     "Ask Trellis team members",
  #                                     "None of these" )))

Q15_pct

# show in a graph

library(plyr)
# Q2_3_pct <- ddply(Q2_3_pct, .(Period), transform, pos = 
#                 cumsum(count) - (0.5 * count));
library(ggplot2)
Q15_graph <- 
  ggplot(Q15_pct, aes(features, count)) +
  geom_bar(aes(fill = Period), stat='identity') +
  # theme(legend.position="none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  xlab("") + 
  ylab("Count") + 
  geom_text(
    aes(label = after_stat(y), group = features), 
    stat = 'summary', fun = sum, vjust = -0.5
  ) +
  scale_fill_brewer(palette = "Set2") 

Q15_graph <- Q15_graph + labs(title = "What features do you have access to",
                                subtitle = "from metadata") + 
  guides(fill=guide_legend(title='Initiation Period'))


print(Q15_graph) #1150 times 450


# optional UX research question

Fall_Survey2023_Oct19c %>%
  dplyr::group_by(`Q5.1_1 Yes to participating in future Trellis user experience research`) %>%
  dplyr::summarize(n = n())

library(RankAggreg)
library(e1071)
library(xlsx)
library(stringr)



# # and now let's apply mutate_if to convert all the other variables (numeric) into factors (categorical variables).
library(dplyr)
library(tidyr)


# columns 32 to 36 include the rank orders
# out of 75 respondents

#Calculate means
Columns1 <- Fall_Survey2023_Oct19c %>% select(c(32:36))
# Columns1[] <- lapply(Columns1, as.numeric)
Col1_tall <- Columns1 %>% gather(key = Feature, value = Rank, 
                                 `Q5.2_1 Make it easier to do common, smaller tasks`:`Q5.2_5 Bring more Trellis data together in one place`)

# Stats1 <- Col1_tall %>% group_by(Feature) %>% summarize(Avg = mean(Rank))
# Stats1

# colours <- c("firebrick4","firebrick3", "firebrick1", "gray70", "blue3" ,"darkblue")
# colours <- c("darkblue", "lightblue", "gray70", "firebrick3", "firebrick4")
colours <- c("darkblue","lightblue",  "gray70", "red3", "red4")

library(forcats)
# about 75 folks answered the last UX question

ggplot(subset(Col1_tall,!is.na(Rank)))+
  aes(x=forcats::fct_rev(Feature),fill=Rank)+
  geom_bar()+
  coord_flip()+
  scale_fill_manual(values = colours) +
  xlab("") +
  ylab("position_levels") +
  ggtitle("Optional UX rank-order question")
