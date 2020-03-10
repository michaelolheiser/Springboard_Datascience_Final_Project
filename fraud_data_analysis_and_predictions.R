# This code corresponds to the final project for Springboard's Introduction to Data Science course. 
# See powerpoint presentation in this repository for more information
# This code reads, cleans, analyzes, and predicts fraud data for MoneyGram International. 
# All sensitive data fields have been masked


rm(list=ls())
require(dplyr)
require(tidyr)  
require(lubridate)
require(ggplot2)
require(geosphere)
require(xtable)
require(ROCR)
require(rpart)
require(rpart.plot)
require(tree)
require(randomForest)
require(caTools)

#read MGI data
raw_MGI_data<-read.csv("Z:/Springboard/raw_MGI_data.csv",header=TRUE,stringsAsFactors=FALSE,
                       colClasses=c("ZIPCODE"="character")) 
fraud<-read.csv("Z:/Springboard/Fraud_2014to2017.csv",header=TRUE,stringsAsFactors=FALSE)

#join Fraud Data and edit flag
raw_MGI_data<-left_join(raw_MGI_data, fraud, by="A_ID2") 
raw_MGI_data$Fraud[is.na(raw_MGI_data$Fraud)]<-0
rm(fraud)

#format dates 
raw_MGI_data$LAST_CREDIT_UPDATE<-as.Date(raw_MGI_data$LAST_CREDIT_UPDATE, "%m/%d/%Y")
raw_MGI_data$C_INSTALL_DATE<-as.Date(raw_MGI_data$C_INSTALL_DATE, "%m/%d/%Y")
raw_MGI_data$Fraud_Start_Date<-as.Date(raw_MGI_data$Fraud_Start_Datetime, "%m/%d/%y")
raw_MGI_data$Fraud_End_Date<-as.Date(raw_MGI_data$Fraud_End_Datetime, "%m/%d/%y")


#Filters
raw_MGI_data = raw_MGI_data %>%
  filter(ADDRESS!="1550 UTICA AVE S",   #excludes Internal test accounts     
         A_CLASS %in% unique(na.omit(A_CLASS)), #excludes agents with no contract
         C_SW_TYPE=="DW1.0",
         !(DAILY_CREDIT_LIMIT<=100 & Fraud==0), #excludes PC's with no send ability that had no fraud
         !(DAILY_CREDIT_LIMIT<=100 & LAST_CREDIT_UPDATE<Fraud_Start_Date & Fraud==1), #excludes PC's with no send ability prior to fraud
         !(TRANSACT_CODE=="") #ensures that transactng contract exists
  ) %>%
  subset(!(C_STATUS=="I" & 
             as.Date(C_CREATEDATE,"%m/%d/%Y")>as.Date("2016-08-07"))) %>%  #Excludes un-installed computers over a year old
  subset(is.na(DAILY_CREDIT_LIMIT)==FALSE) %>% #excludes PC's that never could send
  subset(C_INSTALL_DATE>as.Date("2015-01-01") | is.na(C_INSTALL_DATE)==TRUE) #excludes agents installed before fraud tracking started

#Duplicates
duplicate_IDs<-raw_MGI_data$C_ID[duplicated(raw_MGI_data$C_ID)]  #find duplicate C_IDs
raw_MGI_data<-raw_MGI_data[!duplicated(raw_MGI_data$C_ID),] #delete duplicates
raw_MGI_data$TRANSACT_CODE[raw_MGI_data$C_ID %in% duplicate_IDs]<-"A" #force Transact Codes to "A" for duplicated IDs
rm(duplicate_IDs) 


#missing audit data (blocks and credit changes)
raw_MGI_data$MEAN_CREDIT[is.na(raw_MGI_data$MEAN_CREDIT)]<-raw_MGI_data$DAILY_CREDIT_LIMIT[is.na(raw_MGI_data$MEAN_CREDIT)] #credit did not change. mean is current Credit
raw_MGI_data$CREDIT_CHANGES[is.na(raw_MGI_data$CREDIT_CHANGES)]<-0 #force credit change=NA to be zero
raw_MGI_data$STDEV_CREDIT[is.na(raw_MGI_data$STDEV_CREDIT)]<-0 #force STDEV(CREDIT)=NA to be zero
raw_MGI_data$ACCT_BLOCKS[is.na(raw_MGI_data$ACCT_BLOCKS)]<-0 #force ACCT_Blocks=NA to be zero

#Group data by Agent
raw_MGI_data = raw_MGI_data %>%
  group_by(A_ID, A_NAME, HIERARCHY_CD, M_ID, M_NAME, A_CREATEDATE, ADDRESS, CITY, STATE_ABBR, STATE, ZIPCODE, COUNTY, LAT,
           LON, A_TYPE, A_STATUS, A_STATUS_UPDATE, A_STATUS_REASON, A_CLASS, OPEN_24X7, Fraud, Fraud_Start_Date, Fraud_End_Date,
           TRANSACT_CODE, SERVICE_CODE ) %>%
  summarise(MEAN_CREDIT = sum(MEAN_CREDIT),
            INSTALL_DATE = min(C_INSTALL_DATE),
            STDDEV_CREDIT = mean(STDEV_CREDIT),
            CREDIT_CHANGES = sum(CREDIT_CHANGES),
            ACCT_BLOCKS = sum(ACCT_BLOCKS)
  )

#Join data by zipcode
#SOURCE: MGI Database
raw_MGI_data$ZIPCODE<-substr(raw_MGI_data$ZIPCODE,0,5) #format zipcode to 5 digits
agents_by_zipcode<-read.csv("Z:/Springboard/agents_by_zipcode.csv",header=TRUE,stringsAsFactors=FALSE,
                            colClasses=c("ZIPCODE"="character"))
raw_MGI_data<-left_join(raw_MGI_data, agents_by_zipcode, by="ZIPCODE")
rm(agents_by_zipcode)

#Join population data by zip code
#SOURCE: census.gov
zip_pop<-read.csv("Z:/Springboard/External_Data/Population_and_age_by_ZIPCODE.csv",header=TRUE,stringsAsFactors=FALSE,
                  colClasses=c("ZIPCODE"="character"))
colnames(zip_pop)[2]<-"ZipCode_Pop"
raw_MGI_data<-left_join(raw_MGI_data,zip_pop, by="ZIPCODE")
rm(zip_pop)

#Join Population data by County
#Source census.gov
county_pop<-read.csv("Z:/Springboard/External_Data/2015_pop_by_county.csv",header=TRUE,stringsAsFactors=FALSE)
raw_MGI_data<-left_join(raw_MGI_data, county_pop , by=c("STATE", "COUNTY"))


#Join Population data by City
#Source census.gov
city_pop<-read.csv("Z:/Springboard/External_Data/2015_pop_by_city.csv",header=TRUE,stringsAsFactors=FALSE)
city_pop = city_pop %>%
  group_by(STATE,CITY) %>%
  summarise(City_Pop=sum(POPULATION)) #group/sum population for duplicate city names
raw_MGI_data<-left_join(raw_MGI_data, city_pop , by=c("STATE", "CITY"))


#Join crime data by City
#SOURCE: ucr.fbi.gov/crime-in-the-u.s/2015/crime-in-the-u.s.-2015/offenses-known-to-law-enforcement -> table 10
crime<-read.csv("Z:/Springboard/External_Data/2015_crime_by_city.csv",header=TRUE,stringsAsFactors=FALSE)   
crime = crime %>%
  select(STATE, CITY, Total_Crime) %>%
  group_by(STATE, CITY) %>%
  summarise(Total_Crime_byCity = sum(Total_Crime))
raw_MGI_data<-left_join(raw_MGI_data, crime, by=c("STATE", "CITY"))
raw_MGI_data = raw_MGI_data %>%
  mutate(City_Crime_per_Capita = Total_Crime_byCity/City_Pop) #calculate crime per capita


#import latlon data to find nearest neighbor for missing data
#SOURCE federalgovernmentzipcodes.us
latlon_city<-read.csv("Z:/Springboard/External_Data/latlon.csv",header=TRUE,stringsAsFactors=FALSE)  
states<-read.csv("Z:/Springboard/External_Data/states.csv",header=TRUE,stringsAsFactors=FALSE)  
latlon_city<-inner_join(latlon_city, states, by="STATE_ABBR")
latlon_city = latlon_city %>%
  group_by(STATE, CITY) %>%
  summarise(LAT=mean(LAT), LON=mean(LON))
latlon_city<-inner_join(latlon_city, crime, by=c("STATE", "CITY"))
latlon_city<-inner_join(latlon_city, city_pop, by=c("STATE", "CITY"))
latlon_city = latlon_city %>%
  mutate(City_Crime_per_Capita = Total_Crime_byCity/City_Pop) %>%
  filter(!is.na(LAT), !is.na(LON))


#Find agents with missing city crime data to calculate nearest neighbor using latlon_city
crime_missing<-data.frame(A_ID = raw_MGI_data$A_ID ,                          
                          City_Crime_per_Capita = raw_MGI_data$City_Crime_per_Capita,
                          LAT = raw_MGI_data$LAT,
                          LON = raw_MGI_data$LON)
crime_missing = crime_missing %>%
  filter(is.na(City_Crime_per_Capita),
         !is.na(LAT),
         !is.na(LON))
crime_missing$City_Crime_per_Capita<-NULL

######################################################################################################################
#Use nested FOR Loop to calculate nearest neighbor for city crime
#Save Ouput and comment this section out to avoid having to run the loop everytime the code is run
# list<-rep(NA, length(latlon_city$City_Crime_per_Capita))
# neighbor_index<-matrix(NA, length(crime_missing$A_ID), 2)
# 
# for (j in 1:length(neighbor_index)) {
# 
# for (i in 1:length(list)){
# 
# list[i]<-distGeo(c(crime_missing$LON[j], crime_missing$LAT[j]),c(latlon_city$LON[i], latlon_city$LAT[i]))/1000
#                           }
# neighbor_index[j,1]<-which.min(list) 
# neighbor_index[j,2]<-min(list) 
#                                     }
# 
# neighbors<-latlon_city[neighbor_index[,1],]
# neighbors$Dist_to_Nearest_City<-neighbor_index[,2]
# 
# # 
# # #write out the neighbors file as backup. FOR loop is a long calculation that only needs to be done once
# # #the For loop will need to be run when new data is introduced to run through the model
# # #The rows of this file match with the crime_missing file, hence bind_cols used below
#  write.csv(neighbors, "Z:/Springboard/city_crime_neighbors_DW.csv") 
#######################################################################################################################

#import saved result from section above
neighbors<-read.csv("Z:/Springboard/city_crime_neighbors_DW.csv", stringsAsFactors=FALSE)


#edit the crime_missing data and join it to raw_MGI_data
crime_missing<-bind_cols(crime_missing,neighbors)
crime_missing$Dist_to_Nearest_City<-neighbor_index[,2]
crime_missing$LAT<-NULL #delete Lat
crime_missing$LON<-NULL #delete Lon 
crime_missing$X<-NULL #primary key X
crime_missing$City_Pop<-NULL
crime_missing = crime_missing %>%
  rename(STATE_NEAREST = STATE,
         CITY_NEAREST = CITY,
         Total_Crime_byCity_Nearest = Total_Crime_byCity,
         City_Crime_per_Capita_Nearest = City_Crime_per_Capita)
raw_MGI_data<-left_join(raw_MGI_data, crime_missing, by="A_ID")

#coalesce city crime fields. replyr package has coalesce function but I was unable to install it
raw_MGI_data$City_Crime_per_Capita[is.na(raw_MGI_data$City_Crime_per_Capita)]<-
  raw_MGI_data$City_Crime_per_Capita_Nearest[is.na(raw_MGI_data$City_Crime_per_Capita)]
raw_MGI_data$Total_Crime_byCity[is.na(raw_MGI_data$Total_Crime_byCity)]<-
  raw_MGI_data$Total_Crime_byCity_Nearest[is.na(raw_MGI_data$Total_Crime_byCity)]
raw_MGI_data$Total_Crime_byCity_Nearest<-NULL
raw_MGI_data$City_Crime_per_Capita_Nearest<-NULL



#join poverty data by county
poverty<-read.csv("Z:/Springboard/External_Data/2015_poverty_by_county.csv",header=TRUE,stringsAsFactors=FALSE)  
poverty = poverty %>%
  group_by(STATE_ABBR, COUNTY) %>%
  summarise(Poverty_Percent_All_Ages=mean(Poverty_Percent_All_Ages)) 
raw_MGI_data<-left_join(raw_MGI_data, poverty, by=c("STATE_ABBR", "COUNTY"))

#only 20 agents are missing Poverty data therefore we wont calculate nearest neighbor

#join education data by county
education<-read.csv("Z:/Springboard/External_Data/2015_education_rates_by_county.csv",header=TRUE,stringsAsFactors=FALSE)  
education = education %>%
  group_by(STATE, COUNTY) %>%
  summarise(Percent_bachelors_or_higher = mean(Percent_bachelors_or_higher),
            Percent_high_school_or_higher = mean(Percent_high_school_or_higher),
            Percent_Graduate_degree = mean(Percent_Graduate_degree))
raw_MGI_data<-left_join(raw_MGI_data, education, by=c("STATE", "COUNTY"))

#find agents with missing county data for education
education_missing<-data.frame(A_ID = raw_MGI_data$A_ID ,                          
                              Percent_bachelors_or_higher = raw_MGI_data$Percent_bachelors_or_higher,
                              Percent_high_school_or_higher = raw_MGI_data$Percent_high_school_or_higher,
                              Percent_Graduate_degree = raw_MGI_data$Percent_Graduate_degree,
                              LAT = raw_MGI_data$LAT.y,
                              LON = raw_MGI_data$LON.y)
education_missing = education_missing %>%
  filter(is.na(Percent_bachelors_or_higher),
         !is.na(LAT),
         !is.na(LON))


#import latlon data for counties to be used in nearest neighbor for missing data
latlon_cnty<-read.csv("Z:/Springboard/External_Data/latlon_county.csv",header=TRUE,stringsAsFactors=FALSE)
latlon_cnty = latlon_cnty %>%
  select(STATE_ABBR, COUNTY, LAT, LON) %>%
  group_by(STATE_ABBR, COUNTY) %>%
  summarise(LAT=mean(LAT), LON=mean(LON))
latlon_cnty<-inner_join(latlon_cnty, states, by="STATE_ABBR")
latlon_cnty<-inner_join(latlon_cnty, education, by=c("STATE", "COUNTY"))

######################################################################################################################
# Use nested FOR Loop to calculate nearest neighbor for county education
# Save Ouput and comment this section out to avoid having to run the loop everytime the code is run
#  list<-rep(NA, length(latlon_cnty$Percent_bachelors_or_higher))
#  neighbor_index<-matrix(NA, length(education_missing$A_ID), 2)
# 
# for (j in 1:length(neighbor_index)) {
# 
#  for (i in 1:length(list)){
# 
# list[i]<-distGeo(c(education_missing$LON[j], education_missing$LAT[j]),c(latlon_cnty$LON[i], latlon_cnty$LAT[i]))/1000
#                           }
# neighbor_index[j,1]<-which.min(list) 
# neighbor_index[j,2]<-min(list) 
#                                     }
# # 
# neighbors<-latlon_cnty[neighbor_index[,1],]
# neighbors$Dist_to_Nearest_County<-neighbor_index[,2]
# 
# # 
# 
# write out the neighbors file as backup. FOR loop is a long calculation that only needs to be done once
# The rows of this file match with the education_missing file, hence bind_cols used below
# write.csv(neighbors, "Z:/Springboard/county_education_neighbors_DW.csv") 
#######################################################################################################################

# Import saved result from the section above
neighbors<-read.csv("Z:/Springboard/county_education_neighbors_DW.csv", stringsAsFactors=FALSE)

#edit the education_missing data and join it to raw_MGI_data
education_missing<-bind_cols(education_missing,neighbors) 
education_missing$LAT<-NULL
education_missing$LON<-NULL
education_missing[2]<-NULL
education_missing[2]<-NULL
education_missing[2]<-NULL
education_missing[2]<-NULL
education_missing[2]<-NULL
education_missing = education_missing %>%
  rename(STATE_CNTY_NEAREST = STATE,
         COUNTY_NEAREST = COUNTY,
         Percent_bachelors_or_higher_nearest = Percent_bachelors_or_higher,
         Percent_high_school_or_higher_nearest = Percent_high_school_or_higher,
         Percent_Graduate_degree_nearest = Percent_Graduate_degree
  )
raw_MGI_data<-left_join(raw_MGI_data, education_missing, by="A_ID")

#coalesce city crime fields. replyr package has coalesce function but I was unable to install it. 

raw_MGI_data$Percent_bachelors_or_higher[is.na(raw_MGI_data$Percent_bachelors_or_higher)]<-
  raw_MGI_data$Percent_bachelors_or_higher_nearest[is.na(raw_MGI_data$Percent_bachelors_or_higher)]

raw_MGI_data$Percent_high_school_or_higher[is.na(raw_MGI_data$Percent_high_school_or_higher)]<-
  raw_MGI_data$Percent_high_school_or_higher_nearest[is.na(raw_MGI_data$Percent_high_school_or_higher)]

raw_MGI_data$Percent_Graduate_degree[is.na(raw_MGI_data$Percent_Graduate_degree)]<-
  raw_MGI_data$Percent_Graduate_degree_nearest[is.na(raw_MGI_data$Percent_Graduate_degree)]

raw_MGI_data$Percent_bachelors_or_higher_nearest<-NULL
raw_MGI_data$Percent_high_school_or_higher_nearest<-NULL
raw_MGI_data$Percent_Graduate_degree_nearest<-NULL

rm(latlon_cnty, city_pop, county_pop, crime, poverty, states, crime_missing, education, education_missing, latlon_city,  neighbors)

#cleanup data frame and get rid of fields we dont need
raw_MGI_data$LAT.y<-NULL
raw_MGI_data$LON.y<-NULL
raw_MGI_data$LAT<-NULL
raw_MGI_data$LON<-NULL
colnames(raw_MGI_data)[13]<-"LAT"
colnames(raw_MGI_data)[14]<-"LON"


#set a limit for how close a nearest neighbor should be for missing values. 
model_data = raw_MGI_data %>%
  filter(Dist_to_Nearest_City<100 | is.na(Dist_to_Nearest_City),  #nearest city for crime proxy is at most 100km away.
         Dist_to_Nearest_County<200 | is.na(Dist_to_Nearest_County) #nearest county for education proxy is at most 200km away.
  )


#Use  this section to run t tests to analyze significance of numeric variables. 
# t.test(model_data$City_Crime_per_Capita[model_data$Fraud==0], 
#        model_data$City_Crime_per_Capita[model_data$Fraud==1], 
#        alternative="less")  , na.rm=TRUE) 
# sd(model_data$City_Crime_per_Capita[model_data$Fraud==0], na.rm=TRUE)
# sum(is.na(model_data$City_Crime_per_Capita))
# 
# t.test(model_data$ACCT_BLOCKS[model_data$Fraud==0], 
#        model_data$ACCT_BLOCKS[model_data$Fraud==1], 
#        alternative="less") 
# sd(model_data$ACCT_BLOCKS[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$ACCT_BLOCKS[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$ACCT_BLOCKS))
# 
# t.test(model_data$City_Pop[model_data$Fraud==0], 
#        model_data$City_Pop[model_data$Fraud==1], 
#        alternative="greater") 
# sd(model_data$City_Pop[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$City_Pop[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$City_Pop))
# 
# t.test(model_data$County_Pop[model_data$Fraud==0], 
#        model_data$County_Pop[model_data$Fraud==1], 
#        alternative="greater") 
# sd(model_data$County_Pop[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$County_Pop[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$County_Pop))
# 
# 
# t.test(model_data$ZipCode_Pop[model_data$Fraud==0], 
#        model_data$ZipCode_Pop[model_data$Fraud==1] 
# ) 
# sd(model_data$ZipCode_Pop[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$ZipCode_Pop[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$ZipCode_Pop))
# 
# 
# t.test(model_data$MEAN_CREDIT[model_data$Fraud==0], 
#        model_data$MEAN_CREDIT[model_data$Fraud==1], 
#        alternative="greater") 
# sd(model_data$MEAN_CREDIT[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$MEAN_CREDIT[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$MEAN_CREDIT))
# 
# 
# t.test(model_data$CREDIT_CHANGES[model_data$Fraud==0], 
#        model_data$CREDIT_CHANGES[model_data$Fraud==1], 
#        alternative="greater") 
# sd(model_data$CREDIT_CHANGES[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$CREDIT_CHANGES[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$CREDIT_CHANGES))
# 
# 
# t.test(model_data$AGENTS_IN_ZIP[model_data$Fraud==0], 
#        model_data$AGENTS_IN_ZIP[model_data$Fraud==1], 
#        alternative="greater") 
# sd(model_data$AGENTS_IN_ZIP[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$AGENTS_IN_ZIP[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$AGENTS_IN_ZIP))
# 
# t.test(model_data$Median_age[model_data$Fraud==0], 
#        model_data$Median_age[model_data$Fraud==1], 
#        alternative="less")  
# sd(model_data$Median_age[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$Median_age[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$Median_age))
# 
# 
# t.test(model_data$Poverty_Percent_All_Ages[model_data$Fraud==0], 
#        model_data$Poverty_Percent_All_Ages[model_data$Fraud==1], 
#        alternative="less")  
# sd(model_data$Poverty_Percent_All_Ages[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$Poverty_Percent_All_Ages[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$Poverty_Percent_All_Ages))
# 
# t.test(model_data$Percent_high_school_or_higher[model_data$Fraud==0], 
#        model_data$Percent_high_school_or_higher[model_data$Fraud==1], 
#        alternative="greater")  
# sd(model_data$Percent_high_school_or_higher[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$Percent_high_school_or_higher[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$Percent_high_school_or_higher))
# 
# 
# t.test(model_data$Percent_bachelors_or_higher[model_data$Fraud==0], 
#        model_data$Percent_bachelors_or_higher[model_data$Fraud==1], 
#        alternative="greater") 
# sd(model_data$Percent_bachelors_or_higher[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$Percent_bachelors_or_higher[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$Percent_bachelors_or_higher))
# 
# 
# t.test(model_data$Percent_Graduate_degree[model_data$Fraud==0], 
#        model_data$Percent_Graduate_degree[model_data$Fraud==1], 
#        alternative="greater")  
# sd(model_data$Percent_Graduate_degree[model_data$Fraud==1], na.rm=TRUE) 
# sd(model_data$Percent_Graduate_degree[model_data$Fraud==0], na.rm=TRUE) 
# sum(is.na(model_data$Percent_Graduate_degree))


#Create a data frame "cor_data" to calculate correlation values
cor_data<-model_data[, c(
  "City_Crime_per_Capita", "County_Pop", "City_Pop", "ZipCode_Pop", "Poverty_Percent_All_Ages", "Median_age", 
  "Percent_high_school_or_higher", "Percent_bachelors_or_higher", "Percent_Graduate_degree", 
  "AGENTS_IN_ZIP", "MEAN_CREDIT", "CREDIT_CHANGES", "ACCT_BLOCKS")]

#Use this COde Chunk to calculate a correlation table\

cor_tab<-round(cor(cor_data, use = "complete.obs"), 2)
upper<-cor_tab
xtable(round(cor(cor_data, use = "complete.obs"), 2))


#Gather data used for modelling. Will split into Train and Predict subsets based on Fraud date and Create Date
model_data<-model_data[, c("A_ID","A_NAME", "M_NAME","A_CREATEDATE", "Fraud", "Fraud_Start_Date", 
                           "City_Crime_per_Capita", "County_Pop", "City_Pop", "ZipCode_Pop", "Poverty_Percent_All_Ages", "Median_age", 
                           "Percent_high_school_or_higher", "Percent_bachelors_or_higher", "Percent_Graduate_degree", 
                           "AGENTS_IN_ZIP", "MEAN_CREDIT", "CREDIT_CHANGES","ACCT_BLOCKS")]

#Write out final data set to avoid having to run previous sections before to model
write.csv(model_data, "Z:/Springboard/model_data_groupedbyagent_DW.csv")
----------------------------------------------------------------------------------------------------------------
#begin modelling with cleaned up data#########################################################################  

#Import cleaned data set
model_data<-read.csv("Z:/Springboard/model_data_groupedbyagent_DW.csv")

#Split data for training and testing
train_data<-model_data
set.seed(123)
split = sample.split(model_data$Fraud, SplitRatio=0.75)
train_data1<-subset(model_data, split == TRUE)
predict_data1<-subset(model_data, split == FALSE)


train_data1 = train_data1 %>% #filter out rows with missing values for predictor variables
  filter(!is.na(MEAN_CREDIT),
         !is.na(ZipCode_Pop),
         !is.na(Poverty_Percent_All_Ages),
         !is.na(Median_age),
         !is.na(ACCT_BLOCKS)
         
  )

predict_data1 = predict_data1 %>% #filter out rows with missing values for predictor variables
  filter(!is.na(MEAN_CREDIT),
         !is.na(Poverty_Percent_All_Ages),
         !is.na(Median_age),
         !is.na(ACCT_BLOCKS)
  )


#logistic regression model 1
model1<- glm(Fraud ~ Poverty_Percent_All_Ages + MEAN_CREDIT + ACCT_BLOCKS + Median_age , data=train_data1, family=binomial)
summary(model1) 

predictTest<- predict(model1, type="response", newdata=predict_data1)
summary(predictTest)
table(predict_data1$Fraud, predictTest>0.02) #return confusion matrix. repeat for various threshold values


#Use this code chunck to find ROC curve and calculate AUC
ROCRpred<-prediction(prediction_glm1, train_data1$Fraud)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf<-performance(ROCRpred, "tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.005), text.adj=c(-0.2,1.7))

#Build and analyze random forest model
forest1 <- randomForest(Fraud ~  Poverty_Percent_All_Ages + MEAN_CREDIT + ACCT_BLOCKS + Median_age , data=train_data1, nodesize=1, ntree=500)
predictforest<-predict(forest1, newdata=predict_data1)
table(predict_data1$Fraud, predictforest>.04) #return confusion matrix. repeat for various threshold values

prediction_forest1 <- predict(forest1, type="response")
summary(prediction_forest1)
tapply(prediction_forest1, train_data1$Fraud, mean)

#AUC and ROCR for Random forest
ROCRpred<-prediction(prediction_forest1, train_data1$Fraud)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf<-performance(ROCRpred, "tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.005), text.adj=c(-0.2,1.7))


#Build and analyze regression tree
cart1<-rpart(Fraud ~ Poverty_Percent_All_Ages + MEAN_CREDIT + ACCT_BLOCKS + Median_age, data=train_data1, control=rpart.control(minsplit=1, minbucket=20, cp=0.001))
predictioncart<-predict(cart1, newdata=predict_data1)
table(predict_data1$Fraud,predictioncart>0.025) #return confusion matrix. repeat for various threshold values
summary(cart1)
prp(cart1)

#use code chunk to plot ROC curve and calculate AUC
prediction_cart1 <- predict(cart1)
ROCRpred<-prediction(prediction_cart1, train_data1$Fraud)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf<-performance(ROCRpred, "tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.005), text.adj=c(-0.2,1.7))



