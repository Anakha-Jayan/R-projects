df=read.csv('C:/Users/lenovo/Downloads/R files/E-commerce.csv',skip=5)
head(df)
tail(df)
dim(df)
ncol(df)
nrow(df)
str(df)
names(df)
colSums(is.na(df))

#drop empty clms 
data=subset(df,select = -c(Column,Column2))
head(data,10)
names(data)
library(tidyr)
data<- drop_na(data)
head(data)
colSums(is.na(data))
library(dplyr)

data <- drop_na(data)


# Ensuring 'InvoiceDate' is in date format
data$InvoiceDate <- as.Date(data$InvoiceDate, format="%d-%m-%Y")

# Calculate total sales 
data$sales <- data$UnitPrice * data$Quantity
total_sales <- sum(data$sales, na.rm = TRUE)
print(total_sales)

# Total number of customers
total_customers <- n_distinct(data$CustomerID)
print(total_customers)

# Total Tax (5%)
tax_rate <- 0.05
data <- data %>%
  mutate(TotalTax = sales * tax_rate)
total_tax <- sum(data$TotalTax, na.rm = TRUE)
print(total_tax)

# Net total sales= Total sales - total tax
data <- data %>%
  mutate(Net_Sales = sales - TotalTax)

# Calculate net total sales (sum of net sales for each transaction)
Net_totalSales <- sum(data$Net_Sales, na.rm = TRUE)
print(Net_totalSales)

# Country-wise Net Sales
country_wise_sales <- aggregate(sales ~ Country, data = data, sum)
library(ggplot2)
ggplot(country_wise_sales, aes(x = reorder(Country, sales), y = sales)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Country-wise Net Sales",
    x = "Country",
    y = "Net Sales")


# Date-wise Net Sales
datewise_net_sales <- data %>%
  group_by(InvoiceDate) %>%
  summarise(Net_totalSales = sum(Net_Sales))
ggplot(datewise_net_sales, aes(x = InvoiceDate, y = Net_totalSales)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Date-wise Net Sales",
    x = "Date",
    y = "Net Sales")


# Average net sales by country
average_net_sales_by_country <- data %>%
  group_by(Country) %>%
  summarise(average_net_sales = mean(Net_Sales, na.rm = TRUE))
print(average_net_sales_by_country)

# Maximum net sales by country
max_net_sales_by_country <- data %>%
  group_by(Country) %>%
  summarise(max_net_sales = max(Net_Sales, na.rm = TRUE))
print(max_net_sales_by_country)

# Total Quantity Sold
total_quantity_sold <- sum(data$Quantity, na.rm = TRUE)
print(total_quantity_sold)

#  which day has the most net sales
data$Weekday <- weekdays(data$InvoiceDate)
# Summarize net sales by weekday
daywise_net_sales <- data %>%
  group_by(Weekday) %>%
  summarise(Net_Sales = sum(Net_Sales, na.rm = TRUE))
# day with the maximum net sales
day_with_max_sales <- daywise_net_sales %>%
  filter(Net_Sales == max(Net_Sales))
print(day_with_max_sales)

# Extract month name from InvoiceDate
data$Month <- format(data$InvoiceDate, "%B")
# Summarize net sales by month
monthwise_net_sales <- data %>%
  group_by(Month) %>%
  summarise(Net_Sales = sum(Net_Sales, na.rm = TRUE))
# month with the maximum net sales
month_with_max_sales <- monthwise_net_sales %>%
  filter(Net_Sales == max(Net_Sales))
print(month_with_max_sales)

# day-wise quantity sold
daywise_quantity_sold <- data %>%
  group_by(Weekday) %>%
  summarise(Total_Quantity_Sold = sum(Quantity, na.rm = TRUE))
print(daywise_quantity_sold)

# month-wise quantity sold
month_qty <- data %>%
  mutate(Month = format(as.Date(InvoiceDate), "%B")) %>%  
  group_by(Month) %>%                                      
  summarise(Month_Wise_Qty = sum(Quantity, na.rm = TRUE)) %>%
  arrange(desc(Month_Wise_Qty))
head(month_qty)
month_qty[1,]
ggplot(month_qty, aes(x = reorder(Month, Month_Wise_Qty), y = Month_Wise_Qty)) +
  geom_bar(stat = "identity", fill = "red") + 
  labs(title = "Total MonthWise Quantity Sold", 
       x = "Day", 
       y = "Net_totalSales")