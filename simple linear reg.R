#give interpretations at each step also every step should be followed
df=Temperature_ice_cream_sales
head(df)

sum(is.na(df))
#no null values

#check for duplicates because regression works only for unique items
any(duplicated(df))
#none present

#to check for extreme outliers that will affect analysis
boxplot(df$Temperature_Celsius)
boxplot(df$Ice_Cream_Sales)
#no much outliers or extreme values in dataset

#to check normality
hist(df$Ice_Cream_Sales) #dependent variable is sales
#it is normally distributed.# most of the sales is from 300-400 range
#so we can increase the sales of 300-400


#Check how sales are related with other variables
library(ggplot2)
ggplot(df,aes(x=df$Temperature_Celsius,y=df$Ice_Cream_Sales))+
  geom_point()+
  labs(x='Temperature',y='Sales($)')
#Temperature is strongly,positively,linearly correlated 
#with target variable

#heatmap
cor(df)
heatmap(cor(df),symm = TRUE)
#temperature seems to be most correlated with sales at .74 very close to 1

#simple linear regression
lr_model=lm(df$Ice_Cream_Sales~df$Temperature_Celsius)
summary(lr_model)

#In the result:
#-32.7208 shows a negative interception.  This shows that in some rows the sales are decreasing as temperature increases.As we add 
#more to the data, the intercept will become positive.

# The slope(16.12), shows for every 1 degree rise in temperature,there will be sales of 16.12 units .
#It shows a positive relation between variables that as temperature increases sales increases as well.

#the p value is 2e-16 that is less than 0.05, so we can reject the null hypothesis
#there is significant difference between variables

#the r square value is at 0.55 or 55% that says the model is 55% accurate and 55%
# variation in dependent variable can be explained by the model.  


