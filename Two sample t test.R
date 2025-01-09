dm=Marketing
head(dm)
dim(dm)

#Calculate mean satisfaction scores by gender
#here we use two sample t test

library(dplyr)
male=dm$`Performance score`[dm$Gender=='Male']
female=dm$`Performance score`[dm$Gender=='Female']

set.seed(123)
sample_size=20

male_sample=sample(male,sample_size)
female_sample=sample(female,sample_size)

result2=t.test(male_sample,female_sample)
result2
p_value=result2$p.value
p_value

alpha=0.05

  
if (p_value > alpha) {
  cat('Accept the null hypothesis: There is no significant difference between the variables')
} else {
  cat('Reject the null hypothesis: There is a significant difference between the variables')
}

  
