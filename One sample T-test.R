df=Marketing
head(df)
tail(df)
dim(df)
sum(is.na(df))

#Given the dataset Marketing with a column Performance score, perform a hypothesis test to
#determine if the mean performance score of a random sample is significantly different from
#80. Use a sample size of 20 and a significance level of 0.05. Provide the p-value and the
#decision regarding the null hypothesis.

library(dplyr)

scores_mean=mean(df$`Performance score`)
scores_mean

set.seed(123) #put this and then take sample size

sample_size=20

score_sample=sample(df$`Performance score`,sample_size)
score_sample

t.test(score_sample,m=80)#m is what we expect

#to extract p value among many results give a name
result=t.test(score_sample,mu=80) #mu is a fixed variable
p_value=result$p.value
p_value

#test
alpha=.05
if (p_value>alpha) {
  cat('Accept null hypothesis(H0 :mean=80),which shows means are equal')
}else{
  cat('Reject null hypothesis(H0) and accept H1')
}
  
