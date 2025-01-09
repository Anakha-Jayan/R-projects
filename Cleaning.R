df=read.csv('C:/Users/lenovo/Downloads/R files/Bank Marketing Campaign - Copy.csv',skip=1) # the first has nothing meaningful for analysis so skipped
df
head(df,10)
tail(df)
dim(df)
ncol(df)
nrow(df)
str(df)
names(df)
sum(is.na(df))
colSums(is.na(df))
library(tidyr)
x=drop_na(df)
x
colSums(is.na(x))

#drop clm
drop=subset(x,select = -c(DaySince_PrevCampaign,NoOf_Contacts_PrevCampaign))
drop
names(drop)

#drop rows
data=drop[-c(2:5)]
data
head(data)

#add a new,modified clm
data$newclm=(data$AccountBalance)*0.1
data$newclm
names(data)

#rename clm name
colnames(data)[colnames(data)=='newclm']='Acc balance 10%'
names(data)

#change clm values
data$Gender[data$Gender=='male']='m'
data$Gender[data$Gender=='female']='f'
data$Gender

#removing duplicates
library(dplyr)
data1=data %>%
  distinct(data$Contact_ID,.keep_all = TRUE)
data1
nrow(data1)

dim(df)
dim(data1)

#convert to csv
write.csv(data1,file = 'C:/Users/lenovo/Desktop/ANAKHA/BCOM/6/Bank marketing.csv')
