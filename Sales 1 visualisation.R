data=Sales_1
data
# 1)Find the total orders.
unique_order_count <- data %>%
  summarise(total_orders = n_distinct(OrderID))
unique_order_count
# 2)Average sales 
data$Sales<-(data$UnitPrice)*(data$Quantity)
data$Sales
names(data)
avg_sales<-mean(data$Sales)
avg_sales
# 3)find the year 
data$Year <- format(data$OrderDate, "%Y")
head(data$Year)
# 4)Calculate the yearly sales and plot a bar chart.
library(dplyr) 
data_yearly <- data %>%
  mutate(Year = as.numeric(format(ShipDate, "%Y"))) %>%
  group_by(Year) %>%
  summarise(Total_Sales = sum(`Total Amount`, na.rm = TRUE))
data_yearly
library(ggplot2)
ggplot(data_yearly,aes(x=Year,y=Total_Sales)) +
  geom_bar(stat="identity",fill="red")+
  labs(title="Yearly Sales",x="Year",y="Sales")
# 5)Find the total quantity sold through each category. Create a pie chart for the above.
library(plotly)
plot_ly(data,labels= ~Category,values= ~Quantity,type="pie")%>%
  layout(title="Total Quantity sold through each Category")
# 6)Find the region wise category sold and plot column chart.
region_category_sales <- aggregate(`Quantity` ~ `Region` + `Category`, data = data, sum)
ggplot(region_category_sales, aes(x = Region, y = `Quantity`, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(title = "Total Units Sold by Category and Region",
       x = "Region",y = "Total Units Sold")
#7)Display the orders by region and create a donut chart
region_orders <- table(data$Region)
library(ggplot2)
region_df <- data.frame(Region = names(region_orders), Orders = as.numeric(region_orders))
ggplot(region_df, aes(x = 2, y = Orders, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  xlim(0.5, 2.5) +
  theme_void() +
  labs(title = "Orders by Region") +
  geom_text(aes(label = Orders), position = position_stack(vjust = 0.5))
#8)Display the total sales by each State. Create a horizontal bar chart using the above data. 
state_sales <- data %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(`Total Amount`, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))
ggplot(state_sales, aes(x = reorder(State, Total_Sales), y = Total_Sales, fill = State)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  coord_flip() + 
  labs(title = "Total Sales by State", x = "State", y = "Total Sales")
# 9)Find the total number of customers who paid in online mode. Create a horizontal bar chart.
online_customers <- data %>%
  filter(PaymentMethod == "UPI") %>%
  distinct(CustomerID) %>%
  summarise(Online_Customers = n())
#DATAFRAME
customer_payment_method <- data %>%
  filter(PaymentMethod %in% c("UPI", "Cash")) %>%  # Filter for Online and Offline payments
  distinct(CustomerID, PaymentMethod) %>%  # Count unique customers for each payment method
  group_by(PaymentMethod) %>%  # Group by PaymentMethod
  summarise(Total_Customers = n())
ggplot(customer_payment_method, aes(x = reorder(PaymentMethod, Total_Customers), y = Total_Customers, fill = PaymentMethod)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar chart
  coord_flip() +  # Flip coordinates to make it horizontal
  labs(title = "Total Number of Customers by Payment Method", x = "Payment Method", y = "Number of Customers")
# 10)calculate the total sales by region. Without using ggplot , plot a bar chart.
region_sales=tapply(data$`Total Amount`,data$Region,sum)#instead of tappply we can use by too
region_sales
barplot(region_sales,col = "cyan",xlab = "Region",ylab = "Total Sales",main="Total Sales by Region")