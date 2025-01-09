df=ABC_Incorporation
head(df)
tail(df)
sum(is.na(df))
dim(df)
names(df)

#1 How many employees are there at ABC Incorporation?
count(df)

#2 What is the average number of years of experience for employees in the company?
mean(df$Experience)

#3 How many employees have additional certifications?
sum(df$`Additional Certifications`>0)

#4 How many employees have no additional certifications?
sum(df$`Additional Certifications`==0)

#5 What is the average number of leaves taken by employees?
mean(df$`No of Leave`)

#6 How many employees have taken more than 10 leaves?
sum(df$`No of Leave`>10)

#7 What is the most common performance rating in the company?
Mode(df$`Performance rating`)

#8. What is the percentage of employees who work over time and don't?
overtime=table(df$`Over Time`)
overtime
prop_overtime=prop.table(overtime)*100
round(prop_overtime,2)

#9 How many employees have joined in each year?
table(df$`Year of joining`)
hist(df$`Year of joining`,col='yellow',border = 'green',main = 'Employees joining per year')

#10 What is the employee distribution at ABC Inc on the basis of gender?
table(df$Gender)


#11 Find the average salary of 2021 among malesand females in ABC Inc 
avg_salary=df%>%
  group_by(Gender)%>%
  summarise(avg_salary2021=mean(`Salary 2021`))
print(avg_salary)

#12 What departments does ABC Incorporation have?
unique(df$Department)

#13 How many employees are categorized under each marital status at ABC Incorporation?
marital_dist=table(df$`Marital Status`)
library(lessR)
PieChart(marital_dist,labels = '%',main = 'Marital status wise distribution')


#14 How can the employees be distributed by age in ABC Incorporation?
agewise=table(df$Age)
hist(agewise,main='Employee agewise distribution',col='cyan',border='blue')

#15 Calculate the standard deviation of No of Leave taken by employees. 
sd(df$`No of Leave`)

#16 How has the company's compensation structure in 2013 changed over  time? #skewness
Skew(df$`Salary 2013`)

#17 Evaluate the concentration of employee experience levels.
Kurt(df$Experience)

#18 How many employees are there in each department?
dept=table(df$Department)

#19 What is the average age of employees in each department?
avg_age=df%>%
  group_by(Department)%>%
  summarise(mean_age=mean(Age))
print(avg_age)

#20 Is there a gender imbalance within each department?
table(df$Department,df$Gender)

#21 How has the salary in 2013 changed over the years for each department?
salary_change_over_years=df%>%
  group_by(Department,`Year of joining`)%>%
  summarise(salary_change_mean=mean(`Salary 2013`))
salary_change_over_years

#22 How many employees are working overtime in each department?
table(df$Department,df$`Over Time`)

#23 Can you give us a summary of how ages differ between male and female employees?
summary_by_gender <- df %>%
  group_by(Gender) %>%
  summarise(Mean = mean(Age),
            Median = median(Age),
            Min = min(Age),
            Max = max(Age),
            SD = sd(Age))
print(summary_by_gender)

#24.Is there a change in employee salary over the years?
diff=df$`Salary 2021`-df$`Salary 2013`
diff
summary(diff)

#25.How many employees have changes in salary (increment and decrement)?
increment=sum((df$`Salary 2021`-df$`Salary 2013`)>0)
decrement=sum((df$`Salary 2021`-df$`Salary 2013`)<0)
increment
decrement

#26 Analyse the salary structure from 1980 to 2010 in decades!
salary_structure_over_years=df%>%
  mutate(decade=(`Year of joining`%/%10)*10)%>%
  group_by(decade)%>%
  summarise(mean_salary=mean(`Salary 2021`))
salary_structure_over_years

#27.Could you explain the gender distribution within different departments at our company?
library(ggplot2)
ggplot(df,aes(x=`Department`,fill = `Gender`))+
  geom_bar(position = 'dodge')+
  labs(title = 'Department wise gender distribution',x='Department',y='Gender distribution')

#28. 	Find the department wise average salary in 2021.
tapply(df$`Salary 2021`,df$Department,FUN = mean)
      
#29 How do number of meetings attended varies among the employees?
var(df$`No of meetings attended`)

#30 Which is the most common salary amount in 2021 in each department,
#and how many employees are under that?
tapply(df$`Salary 2021`,df$Department,FUN=Mode)
