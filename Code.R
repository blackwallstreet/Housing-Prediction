library(tidyverse)
library(caret)
#training dataset
training_data <- read_csv('housingtraining.csv')
training_sales <-data.frame("Sales" =training_data$SalePrice)
training_std <- sd(training_sales$Sales, na.rm = TRUE) 
sum(is.na(training_data))
summary(training_sales) 
paste("Training Standard Deviation:", training_std, sep= "")
hist(training_sales$Sales, main = "Training Sale Price Distribution", xlab = "Sales Price", ylab = "# of Houses Sold", xlim= c(5000,800000), breaks =  10, labels = 100000,col="red")
options(scipen=999)

#testing dataset
testing_data <- read_csv('housingtesting.csv')
testing_sales <-data.frame("Sales" =testing_data$SalePrice)
testing_std <- round(sd(testing_sales$Sales, na.rm = TRUE), digits = 2)
summary(testing_sales) 
paste("Testing Standard Deviation:", testing_std, sep= "")
hist(testing_sales$Sales, main = "Testing Sale Price Distribution", xlab = "Sales Price", ylab = "# of Houses Sold", xlim= c(5000,800000), breaks =  10, labels = 100000,col="green")
options(scipen=999)


#combine dataset 
all_sales <-rbind(testing_sales, training_sales)
all_sales <-data.frame("All_Sales" = all_sales$Sales)
hist(all_sales$Sales, main = "All Sales Price Distribution", xlab = "Sales Price", ylab = "# of Houses Sold", xlim= c(5000,800000),ylim = c(0,1000), breaks =  10, labels = 1000000,col="purple")
options(scipen=999)
summary(all_sales)
all_sales_std <- sd(all_sales$All_Sales, na.rm = TRUE)
paste("Combined Standard Deviation:", all_sales_std, sep= "")
summary(all_sales)
#create Linear Model 
training_data<-training_data[-1]
model <-lm(SalePrice~.-SalePrice, data= training_data)
summary(model)

#remove all NA values from testing set 

testing_complete <- testing_data[complete.cases(testing_data), ] # Store the complete cases subset in a new data frame
testing_complete <- testing_complete[,-1]
testing_complete <- testing_complete[1:20,]
sum_of_na <-sum(is.na(testing_complete))
paste("Sum of NA values in Testing Data Set: ", sum_of_na, sep = "")
predict_values <- predict(model, newdata = testing_complete)
predict_values <- data.frame(predict_values)
predict_values
test_values <- data.frame(testing_complete$SalePrice)
testing_complete$SalePrice
results <- cbind(test_values, round(predict_values), round(test_values-predict_values))
colnames(results) <- c("Testing Sales Price", "Predicted Sales Price", "Difference")
results
summary(results)
R2(predict_values, test_values) 
coef(model)

