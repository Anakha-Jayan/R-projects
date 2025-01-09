# r uses proportion test for z test

df=Patients_Dataset
head(df)
sum(is.na(df))

#outcome table to check if patients have diabetes
outcome_table<-table(df$Outcome,df$Diabetes)
outcome_table

#proportion test
prop_test_result=prop.test(outcome_table)
print(prop_test_result)

if(prop_test_result$p.value<0.05){
  cat('There is statistical significance in proportion of patients having diabetes.')
} else{
  cat('There is no statistical significance in proportion of patients having diabetes.')
}

#Is there a significant difference in the proportion of improved outcomes between male
#and female patients?
gender_table1=table(df$Outcome,df$Gender)
gender_table1
prop_test_result1=prop.test(gender_table1)
prop_test_result1
if(prop_test_result1$p.value<0.05){
  cat('There is statistical significance in outcomes between male and female patients.')
} else{
  cat('There is no statistical significance outcomes between male and female patients.')
}


#Is there a significant difference in the proportion of improved outcomes between
#patients with high BMI (over 30) and those with a normal BMI (30 or below)?

df$BMI_rate=ifelse(df$BMI>30,'High BMI','Low BMI')
BMI_table=table(df$Outcome,df$BMI_rate)
BMI_table
prop_result2=prop.test(BMI_table)
prop_result2
if(prop_result2$p.value<0.05){
  cat('There is statistical significance in BMI rates.')
} else{
  cat('There is no statistical significance in BMI rates')
}
