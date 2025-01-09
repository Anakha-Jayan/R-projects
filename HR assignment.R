dm=HR_data
library(tidyr)
x=drop_na(dm)
x
sum(is.na(x))

#1 how many employees are there in the company?
count(x)

library(dplyr)
#average age of employees
avg_age1=mean(x$age)
avg_age1
#age distribution 
hist(x$age,col = 'pink',border = 'magenta',main='Age distribution')
summary(x$age)

#4 age by dept
avgage_by_dept1=tapply(x$age,x$department,FUN = mean)
library(plotly)
plot_ly(labels=names(avgage_by_dept1),values=avgage_by_dept1,type='pie',marker=list(colors=c('orange','green','brown')))%>%
  layout(title='Age distribution by department')

#avg age for both females and males
avgage_gender=tapply(x$age,x$gender,FUN = mean)
avgage_gender
library(lessR)
PieChart(avgage_gender,labels = '%',main='Average age by gender')

#6 no.of depts
unique(x$department)

#7 Employees in each department (like value count)
table(x$department)

#8 is there a gender imbalance in each dept
table(x$gender,x$department)

#9 How has the salary in 2017 changed over the years for each dept
avg_salary_in_2017_by_dept1=x%>%
  group_by(x$department,x$year_of_joining)%>%
  summarise(mean(x$salary_2017))
print(avg_salary_in_2017_by_dept1,n=75)

#10 no . of overtime workers per dept
o=table(x$department,x$over_time)
o
prop.table(o,margin=1)*100

#11 skewness of the distribution of ages among employees in the company 
library(DescTools)
skew(x$age)

#12 gender wise employee distribution
g=table(x$department, x$gender)
g2=prop.table(g,margin=1)
round(g2*100,2)

library(ggplot2)
ggplot(x,aes(x=department,fill = gender))+
geom_bar(position = 'Stack')

#14 salary of 2023 dept wise
tapply(x$salary_2023,x$department,FUN = min)
tapply(x$salary_2023,x$department,FUN = mean)

#15 gender based diff in salary 2023
tapply(x$salary_2023,x$gender,FUN = min)
tapply(x$salary_2023,x$gender,FUN = mean)

#16 gender wise 2017 salary mean
tapply(x$salary_2017,x$gender,FUN = mean)

#17 how average current salary evolved over different decades 
salary_change=x%>%
  mutate(decade=(year_of_joining%/%10)*10)%>%
  group_by(decade)%>%
  summarise(average_current_salary=mean(salary_2023))
salary_change

#18 kurtosis of working hours
Kurt(x$total_working_years)

#19 average salary difference between 2023 and 2017
mean(x$salary_2017)
mean(x$salary_2023)
mean(x$salary_2023-x$salary_2017)

#20 salary change by dept
salary_change_by_dept=x%>%
  group_by(department)%>%
summarise(mean=mean(salary_2023-salary_2017))
print(salary_change_by_dept)

#21 gender base difference in salary change mean
tapply(x$salary_2023-x$salary_2017,x$gender,FUN = mean)

#22 summary
summary(x$salary_2017)
summary(x$salary_2023)
summary(x$salary_2023-x$salary_2017)

#23 salary changes increase or decrease
salary_diff=x$salary_2023-x$salary_2017
increase=sum(salary_diff>0)
decrease=sum(salary_diff<0)
increase
decrease

#24 std of salary change over the years
sd(salary_diff)

#25 variance in age
var(x$age)

#26 most no.of emp joining year
Mode(x$year_of_joining)    

#27 max current salary received by an employee filtered
max=max(x$salary_2023)
x[x$salary_2023==max,]
