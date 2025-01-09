data=Walmart
data
# 1)Create a visualization for find the yearly revenue.
year=tapply(data$`Total Revenue`,data$`Order year`,sum)#instead of tapply we can use by too
barplot(year,col = "blue",xlab = "Order year",ylab = "Total revenue",main="Yearly Revenue")
# 2)Find out which year has the highest order.
order=tapply(data$`Order ID`,data$`Order year`,length)
barplot(order,col = "red",xlab = "Order year",ylab = "Total Orders",main="Orders per Year")
# 3)Create a bar chart to find which month has the best revenue.
data$`Order Month`<- factor(data$`Order Month`, levels = 1:12, labels = c("January", "February", "March", "April", "May", "June", 
                                                                          "July", "August", "September", "October", "November", "December"))
month=tapply(data$`Total Revenue`,data$`Order Month`,sum)
barplot(month,col = "purple",xlab = "Order Month",ylab = "Total Revenue",main="Revenue per Month")
# 4)Create a stacked bar chart to find the order priority by item type.
library(ggplot2)
ggplot(data, aes(x = `Item Type`, fill = `Order Priority`)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Order Priority by Item Type", x = "Item Type", y = "Count of Orders")
# 5)Create a clustered bar chart to find the region wise sale in offline channel.
offline_data <- subset(data, `Sales Channel` == "Offline")
offline_data
ggplot(offline_data, aes(x = Region, y = `Total Revenue`, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' to create clustered bars
  labs(title = "Region-wise Sales in Offline Channel", x = "Region", y = "Total Revenue")
# 6)Plot a boxplot to find the region wise revenue.
ggplot(data,aes(x=Region,y=`Total Revenue`))+
  geom_boxplot(fill="cyan",color='red')+
  labs(title="Box plot of the Region wise Revenue")
# 7)Identify the distribution of profit.
ggplot(data,aes(x=`Total Profit`))+
  geom_histogram(binwidth =2,color="purple")+
  labs(title="Distribution of Profit",x="Profit",y="Frequency")
# 8) Plot a scatterplot to identify the relation between total profit and total revenue.
ggplot(data, aes(x = `Total Revenue`, y = `Total Profit`)) +
  geom_point() +  # Create scatter plot points
  labs(title = "Scatter Plot of Total Profit and Total Revenue",x = "Total Revenue",y = "Total Profit")
# 9) Plot the relation between total cost and total revenue by item type using scatter plot.
ggplot(data, aes(x = `Total Revenue`, y = `Total Cost`, color = `Item Type`)) +
  geom_point() + 
  labs(title = "Relationship Between Total Cost and Total Revenue by Item Type",x = "Total Revenue",y = "Total Cost") 
#10) Create a donut chart to find the total units sold in each item type.
colnames(data)
units_sold <- aggregate(`Units Sold` ~ `Item Type`, data = data, sum)
units_sold
library(ggplot2)
units_df <- data.frame(`Item Type` = names(units_sold), `Units Sold` = as.numeric(units_sold))
units_df
ggplot(units_df, aes(x = 2, y = `Units Sold`, fill = `Item Type`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  xlim(0.5, 2.5) +
  theme_void() +
  labs(title = "Orders by Region") +
  geom_text(aes(label = Orders), position = position_stack(vjust = 0.5))+
  annotation_custom(grob = grid::circleGrob(gp = grid::gpar(fill = "white", col = NA)),
                    xmin = -0.5, xmax = 0.5, ymin = -0.5, ymax = 0.5)
#11) Create a pie chart to find the total units sold by region.
region_sales <- aggregate(`Units Sold` ~ `Region`, data = data, sum)
ggplot(region_sales, aes(x = "", y = `Units Sold`, fill = `Region`)) +
  geom_bar(stat = "identity", width = 1, color = "cyan") + 
  coord_polar(theta = "y")
labs(title = "Total Units Sold by Region")
# 12) Plot a line chart to find how units sold vary over time.
library(dplyr)
data_yearly <-data %>%
  mutate(Year=as.numeric(format(`Ship Date`,"%Y"))) %>%
  group_by(Year) %>%
  summarise(Unit_Sold=sum(`Units Sold`,na.rm=TRUE))
data_yearly 
ggplot(data_yearly,aes(x=Year,y=`Unit_Sold`))+
  geom_line(color="black",size=1,stat="identity")+
  labs(title="Unit Sold over time",x="Year",y="Units")
# 13)Create visualization to how total revenue change over time.
library(dplyr)
data_yearly_rev <-data %>%
  mutate(Year=as.numeric(format(`Ship Date`,"%Y"))) %>%
  group_by(Year) %>%
  summarise(Total_Revenue=sum(`Total Revenue`,na.rm=TRUE))
data_yearly_rev
ggplot(data_yearly_rev, aes(x = Year, y = Total_Revenue)) +
  geom_line(color = "black", size = 1, stat = "identity") +
  labs(title = "Revenue Over Time", x = "Year", y = "Revenue")
# 14) Create a bubble chart identify how the units sold and profit relate considering the order month.
monthly_data <- aggregate(cbind(`Units Sold`, `Total Profit`) ~ `Order Month`, data = data, sum)
ggplot(monthly_data, aes(x = `Units Sold`, y = `Total Profit`,size=`Total Profit`,color = `Order Month`)) +
  geom_point(alpha = 0.6)+
  labs(title = "Relationship Between Units Sold and Profit by Order Month", x = "Units Sold", y = "Profit", size = "Total Revenue", color = "Order Month") 
# 15) Create a bubble chart identify how the total cost and total profit relate considering the total revenue.
monthly_revenue <- aggregate(cbind(`Total Cost`, `Total Profit`, `Total Revenue`) ~ `Order Month`, data = data, sum)
ggplot(monthly_revenue, aes(x = `Total Cost`, y = `Total Profit`, size = `Total Revenue`, color = `Order Month`)) +
  geom_point(alpha = 0.6) +
  labs(title = "Relationship Between Total Cost and Total Profit", x = "Total Cost", y = "Total Profit", size = "Total Revenue", color = "Order Month")
# 16) Illustrate total profit variation over year with an area chart?
library(dplyr)
data_yearly <-data %>%
  mutate(Year=as.numeric(format(`Ship Date`,"%Y"))) %>%
  group_by(Year) %>%
  summarise(Total_Profit=sum(`Total Profit`,na.rm=TRUE))
data_yearly 
ggplot(data_yearly,aes(x=Year,y=Total_Profit))+
  geom_area(fill="pink",alpha=0.5)+ 
  geom_line(color="purple")+ 
  labs(title="Total Profit by Year",x="Year",y="Total Profit")
# 17) How do units sold vary across different item type and order priority in a hierarchical manner using tree map?
library(treemap)
aggregate_data <- aggregate(`Units Sold` ~ `Item Type` + `Order Priority`, data = data, sum)
treemap(aggregate_data,
        index = c("Item Type", "Order Priority"),  
        vSize = "Units Sold",  vColor = "Order Priority", title = "Units Sold by Item Type and Order Priority")
