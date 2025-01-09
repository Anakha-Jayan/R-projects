#u test ranks while chi square counts. chi square for categorical variables
df=HR_Analytics
head(df)
sum(is.na(df))

#1 Is there an association between Employee Type and Training Completion?
table_employee_training=table(df$Employment_Type,df$Training_Completed)
chisq_result=chisq.test(table_employee_training)
chisq_result
if(chisq_result$p.value<0.05){
  cat('The test is significant')
}else{
  cat('The test is not significant')
}

#2 Is there an association between Department and Performance Score?
#performance score is not categorical so convert
m=mean(df$Performance_Score)
df$Performance_Score_rate=ifelse(df$Performance_Score>=m,'high','low')
table_dept=table(df$Department,df$Performance_Score_rate)
chisq_result1=chisq.test(table_dept)
chisq_result1
if(chisq_result1$p.value<0.05){
  cat('The test is significant')
}else{
  cat('The test is not significant')
}

#3 Is there an association between Marital Status and Performance Score?
table_marital<-table(df$Marital_Status,df$Performance_Score_rate)
chisq_result2<-chisq.test(table_marital)
chisq_result2
if(chisq_result2$p.value<0.05){
  cat('There is significant association between marital status and performance score.')
}else{
  cat('There is no significant association between marital status and performance score.')
}

#4 Is there a relationship between Gender and Employee Type?
table_gender<-table(df$Gender,df$Employment_Type)
chisq_result3<-chisq.test(table_gender)
chisq_result3
if(chisq_result3$p.value<0.05){
  cat('The test is significant')
}else{
  cat('The test is not significant')
}

#Training Completion: Similar rates for both permanent and temporary employees.
#Department Performance: No significant difference in performance scores across departments.
#Marital Status: No impact on employees' performance scores.
#Gender and Employment Type: Equal distribution of employment types for males and females.
#None of the tested categorical variables show a statistically significant relationship. 
#This implies that factors such as employee type, department, marital status, and gender
#do not significantly influence or show association to training completion, 
#performance score, or employee type in this dataset.
#Therefore, no strong patterns or associations were seen.

#None of the tested categorical variables show a statistically significant relationship. This implies that factors such as employee type, department, marital status, and gender do not significantly influence training completion, performance score, or employee type in this dataset. Therefore, no strong patterns or associations were detected in these aspects.