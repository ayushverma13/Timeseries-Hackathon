###########################################################################################################
###########################################################################################################
## Data preprocessing

library(fpp2)
library(dplyr)
library(mltools)
library(data.table)
library(xgboost)
library(tidyverse)
library(caret)
library(readr)
library(stringr)
library(car)
library(DMwR) # smote
library(tictoc)
# library(plyr)
# install.packages("ROSE")
library(ROSE)

fill_rate_function<-function(data){
  # data<-DF3
  Data<-data%>%select(BranchMaster,Ethnicity,DerivedMaritalStatus, 
                      DerivedChildren,DerivedOccupation,CarSize,CarSegment, 
                      DerivedCreditCard,DerivedAgeGroup,EmailDomain,MobileOperator,
                      DerivedIncomeRange,Tier,B_OB)
  
  
  
  # grepl("nknown", as.character(Data$B_OB))
  Data$BranchMaster_NA <- ifelse( grepl("nknown", as.character(Data$BranchMaster)),0, 1)%>%as.numeric()
  Data$Ethnicityr_NA <- ifelse( grepl("nknown", as.character(Data$Ethnicity)),0, 1)%>%as.numeric()
  Data$DerivedMaritalStatus_NA <- ifelse( grepl("nknown", as.character(Data$DerivedMaritalStatus)),0, 1)%>%as.numeric()
  Data$DerivedChildren_NA <- ifelse( grepl("nknown", as.character(Data$DerivedChildren)),0, 1)%>%as.numeric()
  Data$DerivedOccupation_NA <- ifelse( grepl("nknown", as.character(Data$DerivedOccupation)),0, 1)%>%as.numeric()
  Data$CarSize_NA <- ifelse( grepl("nknown", as.character(Data$CarSize)),0, 1)%>%as.numeric()
  Data$CarSegment_NA <- ifelse( grepl("nknown", as.character(Data$CarSegment)),0, 1)%>%as.numeric()
  Data$DerivedCreditCard_NA <- ifelse( grepl("nknown", as.character(Data$DerivedCreditCard)),0, 1)%>%as.numeric()
  Data$DerivedAgeGroup_NA <- ifelse( grepl("nknown", as.character(Data$DerivedAgeGroup)),0, 1)%>%as.numeric()
  Data$EmailDomain_NA <- ifelse( grepl("nknown", as.character(Data$EmailDomain)),0, 1)%>%as.numeric()
  Data$MobileOperator_NA <- ifelse( grepl("nknown", as.character(Data$MobileOperator)),0, 1)%>%as.numeric()
  Data$DerivedIncomeRange_NA <- ifelse( grepl("nknown", as.character(Data$DerivedIncomeRange)),0, 1)%>%as.numeric()
  Data$Tier_NA <- ifelse( grepl("nknown", as.character(Data$Tier)),0, 1)%>%as.numeric()
  Data$B_OB_NA <- ifelse( grepl("nknown", as.character(Data$B_OB)),0, 1)%>%as.numeric()  
  
  df_na<-Data%>%select(ends_with("_NA"))
  df_na$fill_rate<-rowSums(df_na)
  
  data$fill_rate<-df_na$fill_rate
  
  # data_fillrate<-data%>%group_by(CampaignType, fill_rate) %>% 
  #   summarise(n(),sum(Sale_Churn1),sum(Sale_Churn1)/sum(n()))
  
  return(data)
}  

raw_data<- read.csv("Desktop/Cristano_Ronaldo_Final_v1/data.csv",
                       header = T, stringsAsFactors = F)


raw_data_test<-raw_data%>%filter(is.na(is_goal))


raw_data_train<-anti_join(raw_data,raw_data_test)
table(raw_data_train$is_goal)
raw_data$X

#match id
sum(ifelse(is.na(raw_data_train$match_event_id),1,0))
unique(raw_data_train$match_event_id)

sum(ifelse(is.na(raw_data_train$location_x),1,0))
unique(raw_data_train$location_x)

sum(ifelse(is.na(raw_data_train$location_y),1,0))
unique(raw_data_train$location_y)


sum(ifelse(is.na(raw_data_train$remaining_min),1,0))
sum(ifelse(is.na(raw_data_train$remaining_min.1),1,0))
ifelse(raw_data_train$remaining_min==raw_data_train$remaining_min.1,"pass","fail")
unique(raw_data_train$remaining_min)
unique(raw_data_train$remaining_min.1)
table(raw_data_train$remaining_min,raw_data_train$is_goal)
table(raw_data_train$remaining_min.1,raw_data_train$is_goal)#dont use


sum(ifelse(is.na(raw_data_train$power_of_shot),1,0))
sum(ifelse(is.na(raw_data_train$power_of_shot.1),1,0))
unique(raw_data_train$power_of_shot)
unique(raw_data_train$power_of_shot.1)
table(raw_data_train$power_of_shot,raw_data_train$is_goal)
table(raw_data_train$power_of_shot.1,raw_data_train$is_goal)#dont use

sum(ifelse(is.na(raw_data_train$knockout_match),1,0))
sum(ifelse(is.na(raw_data_train$knockout_match.1),1,0))
unique(raw_data_train$knockout_match)
unique(raw_data_train$knockout_match.1)
table(raw_data_train$knockout_match,raw_data_train$is_goal)
table(raw_data_train$knockout_match.1,raw_data_train$is_goal)#dont use

sum(ifelse(is.na(raw_data_train$game_season),1,0))
unique(raw_data_train$game_season)
table(raw_data_train$knockout_match,raw_data_train$game_season)

sum(ifelse(is.na(raw_data_train$remaining_sec),1,0))
sum(ifelse(is.na(raw_data_train$remaining_sec.1),1,0))
unique(raw_data_train$remaining_sec)
unique(raw_data_train$remaining_sec.1)
table(raw_data_train$remaining_sec,raw_data_train$is_goal)
table(raw_data_train$remaining_sec.1,raw_data_train$is_goal)#dont use

sum(ifelse(is.na(raw_data_train$distance_of_shot),1,0))
sum(ifelse(is.na(raw_data_train$distance_of_shot.1),1,0))

sum(ifelse(is.na(raw_data_train$is_goal),1,0))

sum(ifelse(is.na(raw_data_train$area_of_shot),1,0))

sum(ifelse(is.na(raw_data_train$shot_basics),1,0))

sum(ifelse(is.na(raw_data_train$range_of_shot),1,0))

sum(ifelse(is.na(raw_data_train$team_name),1,0))

sum(ifelse(is.na(raw_data_train$date_of_game),1,0))

sum(ifelse(is.na(raw_data_train$home.away),1,0))

sum(ifelse(is.na(raw_data_train$shot_id_number),1,0))#continous variable for a time seiers perspective

sum(ifelse(is.na(raw_data_train$lat.lng),1,0))

sum(ifelse(is.na(raw_data_train$type_of_shot),1,0))
sum(ifelse(is.na(raw_data_train$type_of_combined_shot),1,0))




# Creating vectors for the possible value of the variables 
BranchMaster <- c('Ahmedabad', 'Mumbai Andheri', 'Hyderabad', 'New Delhi', 'Kolkata', 'Pune', 'Surat', 'Thane', 'Lucknow', 'Chandigarh', 'Indore', 'Chennai', 'Bangalore', 'Coimbatore', 'Kochi')



################################################################################################################################################################################################################
################################################################################################################################################################################################
################
## Convert it to Factor

#to see number of leads from different tier cities
DF2 %>% dplyr::filter(CampaignType %in% c("Web", "Primary", "Secondary")) %>%
  dplyr::filter(BranchMaster %in% c('Ahmedabad', 'Mumbai Andheri', 'Hyderabad', 'New Delhi', 'Kolkata', 
                                    'Pune', 'Surat', 'Thane', 'Lucknow', 'Chandigarh', 'Indore', 'Chennai', 
                                    'Bangalore', 'Coimbatore', 'Kochi')) %>%
  dplyr::group_by(Tier) %>%
  summarise(dplyr::n())


DF2$BranchMaster <- factor(DF2$BranchMaster, levels = BranchMaster)
DF2$Ethnicity <- factor(DF2$Ethnicity, levels = Ethnicity)
DF2$DerivedMaritalStatus <- factor(DF2$DerivedMaritalStatus, levels = DerivedMaritalStatus)
DF2$DerivedChildren <- factor(DF2$DerivedChildren, levels = DerivedChildren)
DF2$DerivedOccupation <- factor(DF2$DerivedOccupation, levels = DerivedOccupation)
DF2$CarSize <- factor(DF2$CarSize, levels = CarSize)
DF2$CarSegment <- factor(DF2$CarSegment, levels = CarSegment)
DF2$DerivedCreditCard <- factor(DF2$DerivedCreditCard, levels = DerivedCreditCard)
DF2$DerivedAgeGroup <- factor(DF2$DerivedAgeGroup, levels = DerivedAgeGroup)
DF2$EmailDomain <- factor(DF2$EmailDomain, levels = EmailDomain)
DF2$MobileOperator <- factor(DF2$MobileOperator, levels = MobileOperator)
DF2$DerivedIncomeRange <- factor(DF2$DerivedIncomeRange, levels = DerivedIncomeRange)
DF2$Tier <- factor(DF2$Tier, levels = Tier)
DF2$CampaignCategory <- factor(DF2$CampaignCategory, levels = CampaignCategory)
DF2$B_OB<-factor(DF2$B_OB,levels = c(1,0))
class(DF2$App_Met)
DF2$App_Met<-as.numeric(DF2$App_Met)
a<-DF2[is.na(DF2$App_Met),]
# view(a)
# DF2$App_Met<-as.numeric(levels(DF2$App_Met))[DF2$App_Met]
sum(is.na(DF2$App_Met))
# DF2%>%filter(App_Met=="NA")


unique(DF1$BranchMaster)
unique(DF2$BranchMaster)
unique(DF1$Ethnicity)
unique(DF2$Ethnicity)
unique(DF1$DerivedMaritalStatus)
unique(DF2$DerivedMaritalStatus)
unique(DF1$DerivedChildren)
unique(DF2$DerivedChildren)
unique(DF1$DerivedOccupation)
unique(DF2$DerivedOccupation)
unique(DF1$CarSize)
unique(DF2$CarSize)
unique(DF1$CarSegment)
unique(DF2$CarSegment)
unique(DF1$DerivedAgeGroup)

unique(DF1$DerivedCreditCard)
unique(DF2$DerivedCreditCard)
unique(DF2$DerivedAgeGroup)
unique(DF1$EmailDomain)
unique(DF2$EmailDomain)
unique(DF1$MobileOperator)
unique(DF2$MobileOperator)
unique(DF1$DerivedIncomeRange)
unique(DF2$DerivedIncomeRange)
unique(DF1$Tier)
unique(DF2$Tier)
unique(DF1$CampaignCategory)
unique(DF2$CampaignCategory)


## Filter Data
# 
# DF3 <- DF2 %>% filter(CampaignType %in% c("Web", "Primary", "Secondary")) %>%
#   filter(BranchMaster %in% c("Chennai","Chandigarh","Ahmedabad","Pune",
#                              "Mumbai Andheri","Surat","Lucknow","New Delhi",
#                              "Hyderabad","Kochi","Coimbatore","Bangalore",
#                              "Indore","Thane","Kolkata"))

##########################################################################################
################################################################################################
######################################## Make changes Here######################################
#################################################################################################

## Creating data frames for web based leads on the basis of the Branches(Ahmedabad, Lucknow, Indore, Pune and Coimbatore) 

setwd("c:/Users/ayush.b.verma/Documents/mumbai model")


DF3 <- DF2 %>% filter(CampaignType %in% c("Primary","Web"))%>%
  filter(BranchMaster %in% c("Mumbai Andheri"))

sum(DF3$Sale1)
sum(DF3$Sale_Churn1)

unique(DF3$CampaignCategory)
unique(DF3$BranchMaster)
table(DF3$CampaignType)

###################################################################################################
## Filtering the dataframe by fill rate 

# DF3<-fill_rate_function(DF3)
# DF3<-DF3%>%filter(fill_rate>=8)
table(DF3%>%filter(Sale_Churn==1)%>%select(fill_rate))
table(DF3%>%select(fill_rate))
table(DF2%>%select(BranchMaster,fill_rate))
# write.csv(table(DF2%>%select(BranchMaster,fill_rate)),file = "fillrates.csv")
##########################################################################################################################################
##########################################################################################################################################
#create train dataset for data between 1-7-2018 to 31-12-2018

testperiod_1<- DF3[which(DF3$CreatedDate1 >= as.Date(strptime("01-07-2018", "%d-%m-%Y")) &
                           DF3$CreatedDate1 <= as.Date(strptime("31-10-2019", "%d-%m-%Y"))),]

train <- DF3[which(DF3$CreatedDate1 >= as.Date(strptime("01-11-2018", "%d-%m-%Y")) &
                     DF3$CreatedDate1 <= as.Date(strptime("31-3-2019", "%d-%m-%Y"))|
                     (DF3$fill_rate%in%c(14) & DF3$Sale_Churn1==1)),]
#find the variables that might be malignant


table(train%>%filter(Sale_Churn==1)%>%select(fill_rate))
table(train%>%select(fill_rate))

train <- train%>%filter(fill_rate>=9)

#create train_ dataset for data containing the leads to which dials >=1, and is type of lead is prim, second or a web based lead only
train_ <- train[which(is.na(train$App_Met) == FALSE & 
                        train$Dials >= 1 & 
                        as.character(train$CampaignType) %in% c("Web","Primary")),]


train_ %>% group_by(CampaignType) %>% summarise(n(), sum(Sale1), sum(Sale_Churn1))
train_ %>% summarise(Met = sum(App_Met), Sale1 = sum(Sale1), Sale_Churn = sum(Sale_Churn1),
                     Sale1_C = sum(Sale1)/n(), Sale_Churn_C = sum(Sale_Churn1)/n())
nrow(train_)
table(train_$DerivedMaritalStatus)
#created a dataframe for 2019 variables
Y2019_1 <- DF3[which(DF3$CreatedDate1 >= as.Date(strptime("01-04-2019", "%d-%m-%Y")) &
                       DF3$CreatedDate1 <= as.Date(strptime("30-04-2019", "%d-%m-%Y")) &
                       DF3$CampaignType %in% c("Primary", "Secondary", "Web")),]

Y2019_2 <- DF3[which(DF3$CreatedDate1 >= as.Date(strptime("1-05-2019", "%d-%m-%Y")) &
                       DF3$CampaignType %in% c("Primary", "Secondary", "Web")),]
# 
# 
# Y2019_3 <- DF3[which(DF3$CreatedDate1 >= as.Date(strptime("01-05-2019", "%d-%m-%Y")) & 
#                        DF3$CampaignType %in% c("Primary", "Secondary", "Web")),]
# 

Y2019 <- DF3[which(DF3$CreatedDate1 >= as.Date(strptime("01-03-2019", "%d-%m-%Y")) & 
                     DF3$CampaignType %in% c("Primary", "Secondary", "Web")),]



#created a dataframe for 2018 variables that occured in the 1st half of 2018
Y2018_0106 <- DF3[which(DF3$CreatedDate1 >= as.Date(strptime("01-01-2018", "%d-%m-%Y")) &
                          DF3$CreatedDate1 <= as.Date(strptime("30-06-2018", "%d-%m-%Y")) & 
                          DF3$CampaignType %in% c("Primary", "Secondary", "Web")),]

# ifelse(nrow(DF3) == (nrow(train) + nrow(Y2018_0106) + nrow(Y2019)), "PASS", "FAIL")

Y2019_1 %>% group_by(CampaignType) %>% summarise(n(), sum(Sale_Churn1),sum(Sale_Churn1)/n())
Y2019_2 %>% group_by(CampaignType) %>% summarise(n(), sum(Sale_Churn1),sum(Sale_Churn1)/n())
# Y2019_3 %>% group_by(CampaignType) %>% summarise(n(), sum(Sale1),sum(Sale1)/n())
Y2019 %>% group_by(CampaignType) %>% summarise(n(), sum(Sale_Churn1),sum(Sale_Churn1)/n())



set.seed(101) 


train_1 <- train_
test_1 <- train_

train_1 <- train_1[complete.cases(train_1[,c('BranchMaster',
                                             'Ethnicity',
                                             'DerivedMaritalStatus',
                                             'DerivedChildren',
                                             'DerivedOccupation',
                                             'CarSize',
                                             'CarSegment',
                                             'DerivedCreditCard',
                                             'DerivedAgeGroup',
                                             'EmailDomain',
                                             'MobileOperator',
                                             'DerivedIncomeRange',
                                             'Tier',
                                             'CampaignCategory',
                                             "App_Met",
                                             "APP_Met")]),]
sum(is.na(train_1$App_Met))
train_11 <- train_1[which(train_1$Sale_Churn1 == 1),]
train_12 <- train_1[which(train_1$Sale_Churn1 == 0),]

# training with all the won examples and only 10% of lost examples from training data
set.seed(123)
ind <- sample(2, nrow(train_12), replace = TRUE, prob = c(0.05,0.95))
train_12_ <- train_12[ind==1,]
train_1_ <- rbind(train_11, train_12_)
table(train_1_$Sale_Churn1)

table(train_1_$DerivedMaritalStatus,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$DerivedMaritalStatus,train_1_$Sale_Churn1)
table(train_1_$DerivedChildren,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$DerivedChildren,train_1_$Sale_Churn1)
table(train_1_$DerivedOccupation,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$DerivedOccupation,train_1_$Sale_Churn1)####### REMOVE DERIVED OCCUPATION
table(train_1_$DerivedAgeGroup,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$DerivedAgeGroup,train_1_$Sale_Churn1)
table(train_1_$DerivedIncomeRange,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$DerivedIncomeRange,train_1_$Sale_Churn1)####### REMOVE DERIVED INCOME
table(train_1_$DerivedCreditCard,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$DerivedCreditCard,train_1_$Sale_Churn1)####### REMOVE DERIVED CREDIT CARD
table(train_1_$Ethnicity,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$Ethnicity,train_1_$Sale_Churn1)####### REMOVE ETHNICITY
table(train_1_$CarSize,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$CarSize,train_1_$Sale_Churn1)
table(train_1_$CarSegment,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$CarSegment,train_1_$Sale_Churn1)
table(train_1_$EmailDomain,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$EmailDomain,train_1_$Sale_Churn1)####### REMOVE EMAIL DOMAIN
table(train_1_$MobileOperator,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$MobileOperator,train_1_$Sale_Churn1)####### HIGH P SCORE
table(train_1_$Tier,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$Tier,train_1_$Sale_Churn1)####### HIGH P SCORE
table(train_1_$B_OB,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$B_OB,train_1_$Sale_Churn1)
table(train_1_$CampaignCategory,train_1_$Sale_Churn1,train_1_$CampaignType)
chisq.test(train_1_$CampaignCategory,train_1_$Sale_Churn1)


ifelse(train_1_$Sale_Churn==train_1_$Sale_Churn1,"pass","fail")
###########################################################################################################
###########################################################################################################

library(fpp2)
library(dplyr)
library(mltools)
library(data.table)
library(xgboost)
library(tidyverse)
library(caret)
library(readr)
library(stringr)
library(car)
library(DMwR) # smote
train_1_$Sale_Churn<-factor(train_1_$Sale_Churn, levels = c(1,0))
train_1_$App_Met<-factor(train_1_$App_Met, levels = c(1,0))
train_1_$B_OB<-factor(train_1_$B_OB, levels = c(1,0))

table(train_1_$fill_rate)
table(train_1_%>%filter(Sale_Churn1==1)%>%select(fill_rate))
df_SMOTE1<-train_1_%>%filter(fill_rate>=8)
table(df_SMOTE1$Sale_Churn1)
table(train_1_$Sale_Churn1)
train_wo_smote1<-anti_join(train_1_,df_SMOTE1)
table(train_wo_smote1$Sale_Churn1)

# featuresel<-chisq.test()


# onehot_train_smote1<-SMOTE(Sale_Churn~App_Met+Ethnicity + DerivedMaritalStatus + DerivedChildren + DerivedOccupation + CarSize + CarSegment +
#                             DerivedCreditCard + DerivedAgeGroup + EmailDomain + MobileOperator +
#                             DerivedIncomeRange + Tier +B_OB+fill_rate +CampaignCategory, data = df_SMOTE1%>%select('Sale_Churn','App_Met','Ethnicity' ,'DerivedMaritalStatus',
#                                                                                       'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment',
#                                                                                       'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator',
#                                                                                       'DerivedIncomeRange', 'Tier','B_OB','CampaignCategory','fill_rate'), seed = 1,perc.over = 100,perc.under =100*(nrow(df_SMOTE1%>%filter(Sale_Churn1==0))/nrow(df_SMOTE1%>%filter(Sale_Churn1==1))))
onehot_train_smote1<-SMOTE(Sale_Churn~App_Met+Ethnicity + DerivedMaritalStatus + DerivedChildren + DerivedOccupation +
                             DerivedCreditCard + DerivedAgeGroup + EmailDomain + MobileOperator +
                             DerivedIncomeRange + Tier +B_OB +fill_rate+CampaignCategory, data = df_SMOTE1%>%select('Sale_Churn','App_Met','Ethnicity' ,'DerivedMaritalStatus', 
                                                                                                                    'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment', 
                                                                                                                    'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator', 
                                                                                                                    'DerivedIncomeRange', 'Tier','B_OB','CampaignCategory','fill_rate'), seed = 1,perc.over = 100,perc.under =100*(nrow(df_SMOTE1%>%filter(Sale_Churn1==0))/nrow(df_SMOTE1%>%filter(Sale_Churn1==1))))
dim(onehot_train_smote1)
table(df_SMOTE1$Sale_Churn1)
table(onehot_train_smote1$Sale_Churn)

onehot_train_smote<-rbind(onehot_train_smote1,train_wo_smote1%>%select(as.vector(colnames(onehot_train_smote1))))
dim(onehot_train_smote)
table(train_1_$Sale_Churn1)
table(onehot_train_smote$Sale_Churn)
########################################################################################################################
########################################################################################################################
# convert the data frame into a one hot encoded format

# onehot_train<-one_hot(as.data.table(onehot_train_smote%>%select('Ethnicity' ,'DerivedMaritalStatus',
#                                                                'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment',
#                                                                'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator',
#                                                                'DerivedIncomeRange', 'Tier','B_OB','CampaignCategory','Sale_Churn')))%>%as.data.frame()
onehot_train<-one_hot(as.data.table(onehot_train_smote%>%select('Ethnicity' ,'DerivedMaritalStatus',
                                                                'DerivedChildren', 'DerivedOccupation',
                                                                'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain',
                                                                'DerivedIncomeRange', 'Tier','B_OB','CampaignCategory','Sale_Churn')))%>%as.data.frame()

onehot_train$fill_rate<-round(onehot_train_smote$fill_rate)%>%factor(,levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))


dim(onehot_train)


####################################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################

#specifically for the mumbai model for now

Campaign_mum<-onehot_train%>%select(CampaignCategory_eDM,CampaignCategory_Google,`CampaignCategory_National Alliance`,CampaignCategory_Organic,CampaignCategory_Facebook,CampaignCategory_Web,CampaignCategory_Yahoo)
dim(Campaign_mum)
onehot_train<-one_hot(as.data.table(onehot_train_smote%>%select('Ethnicity' ,'DerivedMaritalStatus',
                                                                'DerivedChildren', 'DerivedOccupation',
                                                                'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain',
                                                                'DerivedIncomeRange', 'Tier','B_OB','Sale_Churn')))%>%as.data.frame()
dim(onehot_train)
onehot_train<-cbind(onehot_train,Campaign_mum)
dim(onehot_train)
####################################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################

X_train <- xgb.DMatrix(as.matrix(onehot_train))
Y_train <- onehot_train$Sale_Churn_1%>%as.numeric()
class(Y_train)
onehot_train$B_OB<-onehot_train$B_OB_1
onehot_train<-onehot_train%>%select(-B_OB_1,-B_OB_0,-Sale_Churn_1,-Sale_Churn_0)
dim(onehot_train)
onehot_train<-onehot_train%>%select(-DerivedMaritalStatus_single,-DerivedOccupation_unemployed)


dim(onehot_train)

onehot_train<-onehot_train%>%select(-contains("nknown"))
dim(onehot_train)

onehot_test<-one_hot(as.data.table(Y2019%>%select('Ethnicity' ,'DerivedMaritalStatus',
                                                  'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment',
                                                  'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator',
                                                  'DerivedIncomeRange', 'Tier','B_OB')))%>%as.data.frame()

dim(onehot_test)


X_test = xgb.DMatrix(as.matrix(onehot_test))
Y_test = Y2019$Sale_Churn1





##########################################################################################################################
##########################################################################################################################
##  XGboost using the datascience plus tutorial



xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api.
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)
set.seed(2)
tic("datascience plus")
xgb_model = train(
  onehot_train, Y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)
toc()
saveRDS(xgb_model, "14-9SMOTE prim+web mumbai 9frnew camp yes xgb_35to65classbalance.rds")

# xgb_model<-readRDS("west_primweb_xgb_ROSE.rds")

best_params<-xgb_model$bestTune

predicted <- predict(xgb_model, X_test)
predicted <- (predicted-min(predicted))/(max(predicted)-min(predicted))
residuals <- Y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = Y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) +
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predecited Power Output ") + ylab("Observed Power Output") +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

Model<-xgb_model
Data<-Y2019
test_data_dmatrix<-X_test
segments<-20

Model_Performance_xgboost <- function(Model,Data, Col1, Data_Output,segments){
  
  # test_data_dmatrix<-one_hot(as.data.table(Data%>%select('Ethnicity' ,'DerivedMaritalStatus',
  #                                                        'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment',
  #                                                        'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator',
  #                                                        'DerivedIncomeRange', 'Tier','B_OB')))%>%as.data.frame()
  
  test_data_dmatrix<-one_hot(as.data.table(Data%>%select('Ethnicity' ,'DerivedMaritalStatus',
                                                         'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment',
                                                         'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator',
                                                         'DerivedIncomeRange', 'Tier','B_OB','CampaignCategory')))%>%as.data.frame()
  # 
  
  
  # test_data_dmatrix = xgb.DMatrix(as.matrix(test_data_dmatrix))
  
  test_data_dmatrix$B_OB<-test_data_dmatrix$B_OB_1
  test_data_dmatrix<-test_data_dmatrix%>%select(-B_OB_1,-B_OB_0)
  
  xgb_perf<-predict(Model, newdata = test_data_dmatrix%>%select(as.vector(Model$finalModel$xNames)))%>%as.numeric()
  
  # Data$Col1<- (xgb_perf-min(xgb_perf))/(max(xgb_perf)-min(xgb_perf))
  Data$Col1<- xgb_perf
  
  
  #create deciles for the data in column one , rating them or scoring the leads in by the new data
  Data <- Data %>% mutate(D_Col1 = ntile(Col1, as.numeric(segments)))
  
  Data <- Data[complete.cases(Data[,c('BranchMaster',
                                      'Ethnicity',
                                      'DerivedMaritalStatus',
                                      'DerivedChildren',
                                      'DerivedOccupation',
                                      'CarSize',
                                      'CarSegment',
                                      'DerivedCreditCard',
                                      'DerivedAgeGroup',
                                      'EmailDomain',
                                      'MobileOperator',
                                      'DerivedIncomeRange',
                                      'Tier',
                                      'CampaignCategory',
                                      "App_Met",
                                      "APP_Met")]),]
  d_output<-list()
  
  d_output[[1]] <- Data %>% group_by(D_Col1) %>%
    summarise(leads=n(), Apps_met = sum(App_Met), Sales = sum(Sale_Churn1),Max_prob = max(Col1),Min_prob = min(Col1), 
              Leads_per_Sale = sum(Sale_Churn1)/n(), Leads_per_App = sum(App_Met)/n())%>%arrange(desc(D_Col1))
  
  # ohm<-one_hot(as.data.table(Data%>%select('Ethnicity' ,'DerivedMaritalStatus',
  #                                           'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment',
  #                                           'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator',
  #                                           'DerivedIncomeRange', 'Tier','B_OB')))%>%as.data.frame()
  ohm<-Data%>%select('Ethnicity' ,'DerivedMaritalStatus',
                     'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment',
                     'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator',
                     'DerivedIncomeRange', 'Tier','B_OB','fill_rate')
  ohm$Sale_Churn1<-Data$Sale_Churn1
  ohm$LeadID<-Data$LeadID
  ohm$Score<-Data$Col1  
  ohm$percentile<-Data$D_Col1
  
  d_output[[2]] <-ohm%>%filter(Sale_Churn1==1)%>%arrange(desc(percentile))
  
  return(d_output)
  # Data_Output <- Data %>% group_by(D_Col1,  BranchMaster) %>%
  #   summarise(leads=n(), Apps_met = sum(App_Met), Sales = sum(Sale1),Max_prob = max(Col1),Min_prob = min(Col1), 
  #             Leads_per_Sale = sum(Sale1)/n(), Leads_per_App = sum(App_Met)/n())%>%arrange(desc(D_Col1))
}

train_1_ModelPerformance <- Model_Performance_xgboost(xgb_model,train_1_, D_9056, test_1_ModelPerformance,10)
write.csv(train_1_ModelPerformance[[1]], "train1_ModelPerformance_sale_chennai9frnew_campno_nounknown_prim_xgboost_ROSE 2421 7FR.csv")

test_1_ModelPerformance <- Model_Performance_xgboost(xgb_model,testperiod_1, D_9056, test_1_ModelPerformance,20)
write.csv(test_1_ModelPerformance[[1]], "Test1_ModelPerformance_sale_chennai9frnew_campno_nounknown_prim_xgboost_ROSE 2421 7FR.csv")

Y2019_1_ModelPerformance <-  Model_Performance_xgboost(xgb_model,Y2019_1, D_9056, Y2019_ModelPerformance,20)
write.csv(Y2019_1_ModelPerformance[[1]], "Y2019_april_ModelPerformance_sale_chennai9frnew_campno_nounknown_prim_xgboost_ROSE 2421 7FR.csv")

Y2019_2_ModelPerformance <-  Model_Performance_xgboost(xgb_model,Y2019_2, D_9056, Y2019_ModelPerformance,20)
write.csv(Y2019_2_ModelPerformance[[1]], "Y2019_may_ModelPerformance_sale_chennai9frnew_campno_nounknown_prim_xgboost_ROSE 2421 7FR.csv")

# Y2019_3_ModelPerformance <-  Model_Performance_xgboost(xgb_model,Y2019_3, D_9056, Y2019_ModelPerformance,20)
# write.csv(Y2019_3_ModelPerformance, "Y2019_may_ModelPerformance_appmet_coimb9fr_campyes_prim_xgboost_ROSE 2421 7FR.csv")
l
Y2019_total_ModelPerformance <-  Model_Performance_xgboost(xgb_model,Y2019, D_9056, Y2019_ModelPerformance,20)
write.csv(Y2019_total_ModelPerformance[[1]], "Y2019_ModelPerformance_sale_luck9frnew_campyes_nounknown_prim_xgboost_ROSE 2421 7FR.csv")

write.csv(Y2019_total_ModelPerformance[[2]],file = "saleleads_y2019_chennai9fr.csv")

###########################################################################################################################

onehot_train_all<-one_hot(as.data.table(train_1_%>%select('Ethnicity' ,'DerivedMaritalStatus', 
                                                          'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment', 
                                                          'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator', 
                                                          'DerivedIncomeRange', 'Tier','B_OB','App_Met','Sale_Churn1')))%>%as.data.frame()

onehot_train_all$fillrate<-train_1_$fill_rate

onehot_test_all<-one_hot(as.data.table(Y2019%>%select('Ethnicity' ,'DerivedMaritalStatus', 
                                                      'DerivedChildren', 'DerivedOccupation', 'CarSize', 'CarSegment', 
                                                      'DerivedCreditCard', 'DerivedAgeGroup', 'EmailDomain', 'MobileOperator', 
                                                      'DerivedIncomeRange', 'Tier','B_OB','App_Met','Sale_Churn1')))%>%as.data.frame()

write.csv(onehot_train_all, "All Data one hot train_fillrate.csv")
write.csv(onehot_test_all, "All Data one hot test.csv")

xgb_imp <- xgb.importance(feature_names = colnames(X_train),
                          model = xgb_model$finalModel)

class(xgb_imp)write.csv(xgb_imp,file = "varimp_luck_9fr.csv")
class(xgb_imp)