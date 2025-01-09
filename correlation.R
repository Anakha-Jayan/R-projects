df=Advertsing_Campign
head(df)

cor(df$YouTube,df$Sales)
cor(df$`News Paper`,df$Sales)
cor(df$TV,df$Sales)
# The correlation of TV advertising and sales is at 0.978 with the strongest positive correlation.

#drop company name clm to find data corr
x=subset(df,select = -(`Company Name`)) #c if more than one clm

library(ggplot2)
d=cor(x)
heatmap(d,symm = TRUE)
