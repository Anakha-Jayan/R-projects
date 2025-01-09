df=Performance_analysis
head(df)
sum(is.na(df))

#u test is used when there is no symmetry distribution.And all factors are independent variables.
#u test ranks the data based on variables to know efficiency
#shapiro test - to check if the data is normally distributed
#if below 0.05 the data is not normally distributed


#Does the use of AI-powered logistics result in a significantly higher profit margin compared to
#traditional logistics systems?
traditional<-df[df$`Logistics Systems`=='Traditional',] # , to convert to dataframe
ai_powered<-df[df$`Logistics Systems`=='AI-powered',]

#to check normality of profit margin
shapiro.test(traditional$`ProfitMargin(%)`)
shapiro.test(ai_powered$`ProfitMargin(%)`)
#here the p value is below 0.05,so it is not normally distributed so can be used for u test

#normality of monthly revenue
shapiro.test(traditional$MonthlyRevenue)
shapiro.test(ai_powered$MonthlyRevenue)
#the p value is less than 0.05,so not normal distribution.

#normality of customer satisfaction scores
shapiro.test(traditional$CustomerSatisfactionScore)
shapiro.test(ai_powered$CustomerSatisfactionScore)
#the p value is below 0.05,so it is not normally distributed

#normality of employee retention
shapiro.test(traditional$`EmployeeRetentionRate(%)`)
shapiro.test(ai_powered$`EmployeeRetentionRate(%)`)
#the p value is below 0.05,so it is not normally distributed

#filter profit margin for group A and group B
group_a<-df$`ProfitMargin(%)`[df$`Logistics Systems`=='Traditional']
group_b<-df$`ProfitMargin(%)`[df$`Logistics Systems`=='AI-powered']

#perform mann whitney u test
u_test_result<-wilcox.test(group_a,group_b,alternative = 'two.sided') #alternative =two.sided because it has two conditions one profit next logistic
u_test_result


if(u_test_result$p.value<0.05){
  print('There is significant difference in profit margins between the logistic systems')
}else{
  print('There is no significant difference in profit margins between the logistic systems')
}

#Monthly Revenue Comparison
group_c<-df$MonthlyRevenue[df$`Logistics Systems`=='Traditional']
group_d<-df$MonthlyRevenue[df$`Logistics Systems`=='AI-powered']
u_test_result2<-wilcox.test(group_c,group_d,alternative = 'two.sided')
u_test_result2
if(u_test_result$p.value<0.05){
  print('There is significant difference in monthly revenue between the logistic systems')
}else{
  print('There is no significant difference in monthly revenue between the logistic systems')
}

#Customer Satisfaction Review
grp_a<-df$CustomerSatisfactionScore[df$`Logistics Systems`=='Traditional']
grp_b<-df$CustomerSatisfactionScore[df$`Logistics Systems`=='AI-powered']
u_test_result3<-wilcox.test(grp_a,grp_b,alternative = 'two.sided')
u_test_result3
if(u_test_result$p.value<0.05){
  print('There is significant difference in customer satisfaction scores between the logistic systems')
}else{
  print('There is no significant difference in customer satisfaction scores between the logistic systems')
}

#Employee Retention Assessment
grp_c<-df$`EmployeeRetentionRate(%)`[df$`Logistics Systems`=='Traditional']
grp_d<-df$`EmployeeRetentionRate(%)`[df$`Logistics Systems`=='AI-powered']
u_test_result4<-wilcox.test(grp_c,grp_d,alternative = 'two.sided')
u_test_result4
if(u_test_result$p.value<0.05){
  print('There is significant difference in employee retention rate between the logistic systems')
}else{
  print('There is no significant difference in employee retention rate between the logistic systems')
}

#So as per the results it does not matter which logistic systems we use, the effect would not have any significant difference