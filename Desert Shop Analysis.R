library(readxl)
data <- read_excel("C:/Users/lenovo/Downloads/R files/Dessert Shop.xlsx", skip = 2)
head(data)
colSums(is.na(data))
data=na.omit(data)
colSums(is.na(data))

# 1. Find the total number of customers.
library(dplyr)
unique_customer_count <- data %>%
  summarise(total_customers = n_distinct(`Customer id`))
unique_customer_count

# 2. Find the total sales.
data$Sales=data$`Unit sold(box)`*data$`Unit price(USD)`
head(data$Sales)
total_sales <- sum(data$Sales, na.rm = TRUE)
total_sales

# Apply a 10% discount if the quantity is greater than or equal to 3,otherwise 0.
data <- data %>%
  mutate(Discounted_Sales = ifelse(`Unit sold(box)` >= 3, Sales * 0.9, 0))
head(data$Discounted_Sales)

# 4. Find the total discount applied.
total_discount=sum(data$Discounted_Sales)
total_discount

# 5. Find the region-wise sales and visualize it.
library(ggplot2)
region_wise_sales <- aggregate(`Sales` ~ `Region`,data = data, sum)
ggplot(region_wise_sales, aes(x = Region, y = `Sales`)) +
  geom_bar(stat = "identity", position = "dodge",fill="green") +  
  labs(title = "Region Wise Sales",
       x = "Region",y = "Sales")

# 6. Find the month-wise sales.
data_monthly <- data %>%
  mutate(Month = as.numeric(format(Date, "%m"))) %>%
  group_by(Month) %>%
  summarise(Total_Sales = sum(`Sales`, na.rm = TRUE))
data_monthly

# 7. Find the total sales by order platform.
platform_wise_sales <- aggregate(`Sales` ~ `Order platform`,data = data, sum)
ggplot(platform_wise_sales, aes(x = `Order platform`, y = Sales)) +
  geom_bar(stat = "identity", position = "dodge",fill="purple") +  
  labs(title = "Order Platform Wise Sales",
       x = "Order Platform",y = "Sales")

# 8. Find region-wise feedback.
region_feedback <- data %>%
  group_by(Region, Feedback) %>%
  summarise(Feedback_Count = n(), .groups = "drop")
region_feedback
ggplot(region_feedback, aes(x = Region, y = Feedback_Count, fill = Feedback)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Region-wise Feedback Counts", 
       x = "Region", 
       y = "Feedback Count", 
       fill = "Feedback Type")

# 9. Product-wise feedback.
product_feedback <- data %>%
  group_by(Product, Feedback) %>%
  summarise(Feedback_Count = n(), .groups = "drop")
product_feedback
ggplot(product_feedback, aes(x = Product, y = Feedback_Count, fill = Feedback)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Product-wise Feedback Counts", 
       x = "Product", 
       y = "Feedback Count", 
       fill = "Feedback Type")

# 10. Find the region-wise order platform.
region_wise_platform <- data %>%
  group_by(Region,`Order platform`) %>%
  summarise(Orderplatform_Count = n(), .groups = "drop")
region_wise_platform
ggplot(region_wise_platform, aes(x = Region, y = Orderplatform_Count, fill = `Order platform`)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Region Wise Order Platform", 
       x = "Region", 
       y = "Platform Count", 
       fill = "Order platform")










