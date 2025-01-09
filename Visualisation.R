data=Sales_data
data

#barplot
library(ggplot2)
ggplot(data,aes(x=Product,y=sales))+
  geom_bar(stat = "identity",fill="black")+
  labs(title="Sales by product",x="product",y="Sales")
  
#summary table of total sales per zone
total_sales_per_zone=aggregate(sales~Zone,data=data,FUN = sum)
View(total_sales_per_zone)

ggplot(total_sales_per_zone,aes(x=Zone,y=sales))+
  geom_bar(stat ='identity',fill='blue')+
  labs(title='Sales per zone',x='zone',y='Sales')

#summary table of total sales per product and manager
sales_summary=aggregate(sales~Product+Manager,data=data,FUN = sum)
sales_summary

ggplot(sales_summary,aes(x=Product,y=sales,fill = Manager))+
  geom_bar(stat='identity')+
  labs(title='Sales by product and managers',x='product',y='sales')

# Total sales of different zones vary across various managers
zone_by_manager=aggregate(sales ~ Manager + Zone , data=data,FUN="sum")
zone_by_manager

#for not taking stacked
ggplot(zone_by_manager,aes(x=Zone,y=sales,fill=Manager))+
  geom_bar(stat="identity",position="dodge")+
  labs(title="Sales of Zones by Manager",x="Zone",y="Sales")

#sales over time
#line chart showing sales over time
library(dplyr)
data_yearly<-data%>%
  mutate(Year=as.numeric(format(Date,'%Y')))%>%
  group_by(Year)%>%
  summarise(Total_Sales=sum(sales,na.rm = TRUE))
data_yearly

ggplot(data_yearly,aes(x=Year,y=Total_Sales))+
  geom_line(stat = 'identity',color='maroon',size=2)+
  labs(title="Yearly sales",x="time period",y="Sales")

#FILTER DATA FOR PRODUCT B229
product_yearly<-data%>%
filter(Product=='B229')%>%
  mutate(Year=as.numeric(format(Date,'%Y')))%>%
  group_by(Year)%>%
  summarise(Total_Sales=sum(sales,na.rm=TRUE))
product_yearly

ggplot(product_yearly,aes(x=Year,y=Total_Sales))+
  geom_line(stat = 'identity')+
  labs(title='Yearly sales of product B229',x='time period',y='Sales')
  
library(plotly)
#pie chart showing proportion of sales by zone 
plot_ly(data,labels= ~Zone,values= ~sales,type="pie")%>%
  layout(title="Proportion of.Sales.by.Zone")

#area chart
ggplot(data_yearly,aes(x=Year,y=Total_Sales))+
  geom_area(fill='skyblue',alpha=.5)+
  geom_line(color='blue')+
  labs(title='Total Sales per year',x='Year',y='Total sales')+
  theme_minimal()

#histogram
library(ggplot2)
ggplot(data,aes(x=sales))+
  geom_histogram(binwidth=2,fill='yellow',color='green')+
  labs(title = 'Distribution of sales',x='sales',y='frequency')

#boxplot
ggplot(data,aes(y=sales))+
  geom_boxplot(fill='skyblue',color='black')+
  labs(title = 'Box plot of sales',y='sales')#boxplot does not use x axis for analysis
#11
ggplot(data,aes(x=Zone,y=sales))+
  geom_boxplot(fill='yellow',color='black')+
  labs(title = 'Box plot of zone wise sales',y='sales',x='zone')