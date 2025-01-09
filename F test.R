df=Speedy_Logistics_Solutions_1
head(df)
sum(is.na(df))
dim(df)

#1 #delivery day wise circulation for van and trucks
van_data<-df$Delivery_Day[df$VehicleType=='Van']
truck_data<-df$Delivery_Day[df$VehicleType=='Truck']
van_data
truck_data

#here if we take t test, we get avg delivery dates for each which shows no much difference
#but if we take f test, we get its variation in days for delivery from mean

f_test=var.test(van_data,truck_data)

## Display F test results
print(f_test)
if (f_test$p.value < 0.05) {
  print("The variances are significantly different")
} else{
  print("There is no significant difference in variances")
}


#2 Is there a significant difference in the delivery distance between Vans and Trucks used
#for shipments in Speedy Logistics Solutions?

van_dis=df$Distance[df$VehicleType=='Van']
truck_dis=df$Distance[df$VehicleType=='Truck']
van_dis
truck_dis
f_test1=var.test(van_dis,truck_dis)
f_test1
if(f_test1$p.value<0.05){
  print('There is significant difference in variances')
} else{
  print('There is no significant difference in variances')
}


#3 Is there a significant difference in the delivery cost between Vans and Trucks
#used for shipments in Speedy Logistics Solutions?
van_cost=df$Cost[df$VehicleType=='Van']
van_cost
truck_cost=df$Cost[df$VehicleType=='Truck']
truck_cost
f_test2=var.test(van_cost,truck_cost)
f_test2

if(f_test2$p.value<0.05){
  print('There is significant difference in variances')
} else{
  print('There is no significant difference in variances')
}