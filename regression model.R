setwd("/Users/jackduan/Desktop/ESDA/BENG0093_Spatial/2nd_Assignment/data_codes")




# regression model
Indonesia_data<-read.csv("Indonesia_GDP_Pupulation_Electricity.csv")
View(Indonesia_data)
lm(Electricty.used~GDP+Population,data =Indonesia_data )
Indonesia_data$X = c(Indonesia_data$X,seq(2023,2030,1))

# Make the col names consistent add year
new_rows <- data.frame(matrix(NA, ncol = ncol(Indonesia_data), nrow = 8))
names(new_rows)<-colnames(Indonesia_data)
Indonesia_data <- rbind(Indonesia_data,new_rows )


Indonesia_data$X <- c(na.omit(Indonesia_data$X),seq(2023,2030,1))
#  Add  predicted gdp
Indonesia_data$GDP <- c(na.omit(Indonesia_data$GDP),gdp_prd)
#  Add  predicted pop
Indonesia_data$Population <- c(na.omit(Indonesia_data$Population),pop_prd)




# 可视化
a<-data.frame( X = c(2023,2024,2025,2026,2027,2028,2029,2030) )
Indonesia_data$log_gdp = log(Indonesia_data$GDP)

model_1<-lm(log_gdp~X ,data = Indonesia_data)
summary(model_1)

model_2<-lm(Population~X ,data = Indonesia_data)
summary(model_2)
 
gdp_prd<-exp(predict(model_1, a))
pop_prd <-predict(model_2, c(a))


plot( x= seq(1990,2022,1) , y= Indonesia_data$GDP , xlab = " Years", ylab = "GDP (USD Billion)",
      col= "blue",ylim = c(0,3000),xlim = c(1990,2030), main = " Projection of Indonesia GDP by \n using Exponential Regression Model")
lines(x= a$X , y = gdp_prd, col = "red",type = "p")


plot( x= seq(1990,2022,1) , y= Indonesia_data$Population , xlab = " Years", ylab = "Population",
      col= "blue",ylim = c(0,320000000),xlim = c(1990,2030), main = " Projection of Indonesia Population by \n using Linear Regression Model")
lines(x= a$X , y = pop_prd, col = "red",type = "p")

