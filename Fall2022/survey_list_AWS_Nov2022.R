##Trellis users by product
##can be used for Program Survey Contact List - March 2022 and User Community Emails
# April 7, 2022
# updated Nov. 1, 2022

#SET UP Environment
library('tidyverse')
library('dplyr')
library('readxl')
library('salesforcer')
# library('RForcecom')
library('lubridate')

getwd()
setwd("D:/Users/jmpark/Box/Trellis/Program Team/Trellis Metrics Reports/Trellis KPIs/Raw data")

#setwd("~/Trellis Users KPIs")
# setwd("C:/Users/fkmiller/Desktop/for feb 2022 program kpis/")

# rm(list=ls(all=TRUE)) 
options(digits=3)

today <- Sys.Date()
#capture current month
this_month <- month(today)
this_year <- year(today)


format(today, format="%B %d %Y")

############################################

##look at data to see format and variables
library(readxl)
perms2prods<-read_xlsx("20220224_Permissionsets_mapped_to_Products.xlsx")

############################################
##STEP 2: Import data from SF ------ Paused for now while Brett fixes something

#Oauth
sf_auth()
# 
# sf_auth(login_url="https://login.salesforce.com")

###bring in Contact Records
my_soql_contact <- sprintf("SELECT Id,	Emplid__c, Email, User__c, Primary_Department__r.Name, 
                           NetId__c, hed__Primary_Organization__c, MDM_Primary_Type__c
                           FROM Contact")

contact_records <- sf_query(my_soql_contact, object_name="Contact", api_type="Bulk 1.0")

###bring in User object fields
soql_users<-sprintf("select Id, Email, UserType, Name, NetID__c, Profile.Name, ContactId, CreatedDate,
                    Department, ProfileId, UserRoleId, 
                    UserRole.Name, Title, Username, LastLoginDate 
                    from User
                    WHERE IsActive=TRUE ")
users_SF <- sf_query(soql_users, object_name="User", api_type="Bulk 1.0")


###bring in affiliation Records
my_soql_aff <- sprintf("SELECT Academic_Department__c, hed__Affiliation_Type__c, hed__Contact__c, 
                        hed__Primary__c, hed__Account__r.Name, Parent_Organization__c
                           FROM hed__Affiliation__c")

affiliations <- sf_query(my_soql_aff, object_name="hed__Affiliation__c", api_type="Bulk 1.0")


###bring in permissionsets Records
solq_perms<-sprintf("select AssigneeId, PermissionSet.Name, PermissionSet.Type, PermissionSet.ProfileId, 
                    PermissionSetGroupId from PermissionSetAssignment")
permissionsets <- sf_query(solq_perms, object_name="PermissionSetAssignment", api_type="Bulk 1.0")



############################################
##STEP 3: Merge files

##restrict to users with last logins this month
names(users_SF)

users<-users_SF[,-c(4,6,9:13)] #to remove Department, LastLoginDate, ProfileId, 
# Title, Username, UserRoleId, UserType

users<-unique(users)

users %>% count(Profile.Name) 
##restrict User file to those we want
users<-subset(users, users$Profile.Name %in% c("Salesforce Base", 
                                               "Advising Base", "Service Desk Base"))

##reshape the permissionsets file

names(users)
names(permissionsets)
names(perms2prods)
# perms2prods<-perms2prods[,-c(2)] # drop n

# users_perms<-merge(users, permissionsets, by.x = "Id", by.y = "AssigneeId")
# users_perms_prods<-merge(users_perms, perms2prods, by.x = "PermissionSet.Name", by.y = "PermissionSet.Name", all.x = TRUE)
# upp_c<-merge(users_perms_prods, contact_records, by.x = "Id", by.y = "User__c", all.x = TRUE)

# names(upp_c)
# upp_c<-upp_c[,-c(1,3,10:12,15,17,18)] # to remove Id, CreatedDate, PermissionSetGroupId,
# PermissionSet.ProfileId, PermissionSet.Type, Email.y, hed_Primary_Organization__c, 
# MDM_Primary_Type__c

upp_c <- left_join(users, permissionsets, by = c("Id" = "AssigneeId")) %>%
  left_join(perms2prods) %>%
  left_join(contact_records, by = c("Id" = "User__c")) %>%
  select(CreatedDate, ContactId, Email.x, Name, NetID__c.x, PermissionSet.Name,
         Profile.Name, UserRole.Name, Product, Primary_Department__r.Name, Id.y) %>%
  filter(PermissionSet.Name!="X00ef4000001uSgMAAU" & PermissionSet.Name!="X00e2S000000G6SIQA0") %>% 
  distinct()


upp_c %>% 
  count(Profile.Name)

# upp_c<-subset(upp_c, upp_c$PermissionSet.Name!="X00ef4000001uSgMAAU" & upp_c$PermissionSet.Name!="X00e2S000000G6SIQA0")

# upp_c<-distinct(upp_c)

##create 'base' product from profile name
#upp_c$Product[is.na(upp_c$Product)]<-upp_c$Profile.Name
upp_c$Product[upp_c$UserRole.Name=="Data Analyst" & is.na(upp_c$Product)]<-"Reports"
upp_c$Product[(upp_c$UserRole.Name=="SAFER Volunteer" | upp_c$UserRole.Name=="SAFER") & is.na(upp_c$Product)]<-"SAFER"
upp_c$Product[upp_c$Profile.Name=="Advising Base" & is.na(upp_c$Product)]<-"Scheduling/Notes"
upp_c$Product[upp_c$Profile.Name=="Service Desk Base" & is.na(upp_c$Product)]<-"Service Desk"
upp_c$Product[upp_c$UserRole.Name=="Marketer" & is.na(upp_c$Product)]<-"Marketing - SF"

upp_c2<-unique(upp_c)

upp_c2<-subset(upp_c2, !is.na(upp_c2$Product))

#
#Merge in Affiliations
#
names(affiliations)
affiliation1s <- subset(affiliations, affiliations$hed__Primary__c==TRUE)
# affiliation1s <- affiliation1s[, -c(4)] #remove hed_Primary__c
affiliation1s %>% 
  select(-hed__Primary__c) %>% 
  distinct()

df_foruse<-merge(upp_c2, affiliation1s, by.x = "Id.y", by.y = "hed__Contact__c",
                 all.x = TRUE) %>%
  select(CreatedDate, ContactId, Email.x, Name, Profile.Name, UserRole.Name,
         Product, Primary_Department__r.Name, 
         Parent_Organization__c, hed__Account__r.Name)

names(df_foruse)
# df_foruse<-df_foruse[,-c(1,2,11,13,14)] #remove Id.y, PermissionSet.Name, 
#NetID__c.y, Academic_Department__c, hed_Affiliation_TYpe__c


# df_foruse <- left_join(upp_c2, affiliation1s, by = c("Id.y" = "hed__Contact__c")) %>% 
#   select(-c(Id.y, PermissionSet.Name, NetID__c.y, 
#             Academic_Department__c, hed_Affiliation_TYpe__c))

# ##reshape for one line per user
names(df_foruse)
for_reshape<-df_foruse
for_reshape$counter<-1
for_reshape<-unique(for_reshape)
for_reshape %>% count(Profile.Name)

#test<-merge(for_reshape, users_stripped, by.x = "NetID__c.x", by.y = "NetID__c", all = TRUE)

for_use<-reshape(for_reshape, v.names="counter", timevar="Product", idvar=c("CreatedDate", "Email.x", "Name", "Profile.Name", "UserRole.Name"),
                 direction="wide")

##export for checking
write.csv(for_use, "who is missing_active_users.csv")

names(for_use)
for_use$SchedulingNotes[for_use$`counter.Scheduling/Notes` ==1]<-"Scheduling and Notes"
for_use$Events[for_use$counter.Events==1]<-"Events"
for_use$ServiceDesk[for_use$`counter.Service Desk`==1]<-"Service Desk"
for_use$MarketingSF[for_use$`counter.Marketing - SF`==1]<-"Marketing in SF"
for_use$SAFER[for_use$`counter.SAFER`==1]<-"SAFER"
for_use$ExternalPartners[for_use$`counter.External Partners`==1]<-"External Partners"

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

##now need to figure out how to concatenate the product vars into one in a way that looks like English for export
#for_use$AllProducts<-as.character(paste(for_use$SchedulingNotes, for_use$Events, for_use$ServiceDesk, for_use$MarketingSF, sep = ", "), na.rm = TRUE)
##can't figure out how to do it without the NAs showing up. will try to trim them out
#for_use$AllProducts1<-str_remove_all(for_use$AllProducts, "NA,")
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
  select(-c(col_count, ContactId)) # add back in CreatedDate

write.csv(for_use2, "2022_Nov2_SFTrellisUsersByProduct.csv")