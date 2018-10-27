## Initialization and Data Reads
library(lubridate)
library(GGally)
library(ggplot2)
library(hydroGOF)
library(mvtnorm)

House <- read.csv("../input/kc_house_data.csv")

# Changing the Date Format for Regression
House$date<-(substr(House$date, 1, 8))
House$date<- ymd(House$date)
House$date<-as.numeric(as.Date(House$date, origin = "1900-01-01"))

# Splitting the Data Set 
ratio = sample(1:nrow(House), size = 0.25*nrow(House))
Test = House[ratio,] #Test dataset 25% of total
Training = House[-ratio,] #Train dataset 75% of total

## Checking Relationship between price, bedrooms, bathrooms, sqft_living and sqft lot
plot1<-ggpairs(data=Training, columns=3:7,
    mapping = aes(color = "dark green"),
    axisLabels="show")
plot1

## Checking Relationship between price, floors, waterfront, view, condition and grade
plot2<-ggpairs(data=Training, columns=c(3,8:12),
    mapping = aes(color = "dark green"),
    axisLabels="show")
plot2

## Checking Relationship between price, yr built, lat and long
plot3=ggpairs(data=Training, columns=c(3,15,18,19),
    mapping = aes(color = "dark green"),
    axisLabels="show")
plot3

## My final 5 variables are: sqft_living, bathrooms, grade, view and lat. Lets verify this using box plots:

## Price vs. Sqft_living ->> Nice correlation, as sqft increases, price increases as well.
boxplot1=boxplot(price~sqft_living, data=Training, 
  col=(c("gold","darkgreen")),
  main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price")

## Price vs. Bathrooms ->> Nice correlation, as # of bahtrooms increases [median of bar plot], price increases as well, with one expection in when bathroom=7
boxplot2=boxplot(price~bathrooms, data=Training, 
  col=(c("gold","darkgreen")),
  main="Price vs. Bathrooms", xlab="Bathrooms", ylab="Price")

## Price vs. Grade ->> Nice correlation, grade increases [median of bar plot], price increases as well
boxplot3=boxplot(price~grade, data=Training, 
  col=(c("gold","darkgreen")),
  main="Price vs. Grade", xlab="Grade", ylab="Price")

## Price vs. View ->> Nice correlation, view increases [median of bar plot], price increases as well
boxplot4=boxplot(price~view, data=Training, 
  col=(c("gold","darkgreen")),
  main="Price vs. View", xlab="View", ylab="Price")

## Price vs. Lat ->> This is more like a normal dist relationship, price peaks around when lat= 47.64 and declines afterwards, but this can be modeled easily. I would say Lat explains the price as well.
boxplot5=boxplot(price~lat, data=Training, 
  col=(c("gold","darkgreen")),
  main="Price vs. Lat", xlab="Lat", ylab="Price")

## Each of those box plots shows that those variables might be directly related in predicting house prices.

## To strengthen my hypothesis I also computed correlation between prices and variables, and my top 5 picks are supported with correlation coefficients as well [see below]

## Plots 1,2 and 3 shows the correlation between each variables and they are:
# corr between price vs sqft_living: 0.701
# corr between price vs bathrooms: 0.524
# corr between price vs bedrooms: 0.303
# corr between price vs sqft_lot: 0.0972
# corr between price vs floors: 0.282
# corr between price vs waterfront: 0.324
# corr between price vs view: 0.406
# corr between price vs condition: 0.0466
# corr between price vs grade: 0.654
# corr between price vs yr_built: 0.0366
# corr between price vs lat: 0.304
# corr between price vs long: 0.0161

############################################################################################################
## I want to use the predictor sqft living for predicting house prices.

plot(Training$sqft_living,Training$price, main="Sqft_Living vs. Price of House", xlab="Sqft_Living", ylab="Price of House", pch=19)

## Since this scatterplot is too crowded - I will plot aggregated vectors to see the relationship between 2 variables. 

vec_price_sqftliving <-aggregate(price~sqft_living, FUN=mean, data=Training)
plot(vec_price_sqftliving)
scatterplot1<-recordPlot()

## Plot does not show that price and sqft_living are linearly related. It more looks like an exponential relationship. 
linear_model<-lm(vec_price_sqftliving$price~vec_price_sqftliving$sqft_living)
expo_model<-lm(log(vec_price_sqftliving$price)~vec_price_sqftliving$sqft_living)

cat("Exponential model has ", 100*(summary(expo_model)$r.squared/summary(linear_model)$r.squared-1),"% better R-squared than Linear model, thus relationship between price and sqft_living can be said to be exponential rather than linear.[expo_model R-Squared:",summary(expo_model)$r.squared," linear model R-Squared:",summary(linear_model)$r.squared,"]")


############################################################################################################
## Ploting average prices in terms of the number of bathrooms and fit a linear model to this graph:

average_price_byBathrooms <-aggregate(price~bathrooms, FUN=mean, data=Training)
plot(average_price_byBathrooms,main="Avg. Price by # Bathroom")
lin_model_bathroom<-lm(price~bathrooms,data=average_price_byBathrooms)
summary(lin_model_bathroom)
abline(lin_model_bathroom)

############################################################################################################

## I am using aggregated data as opposed to using the raw data. By "aggragated" data, I mean I take the mean for all the same sqft_living. It makes the graph cleaner.
plot(log(vec_price_sqftliving$sqft_living),log(vec_price_sqftliving$price), main="Log of Sqft_Living vs. Log of Price of House", xlab="Log Sqft_Living", ylab="Log Price of House", pch=19)
scatterplot2<-recordPlot()


scatterplot1 ## Regular Sqft Living vs Price Graph
scatterplot2 ## Log of Sqft living vs Log of Price Graph

## Major differences between two scatterplots are: 1) 2nd plot which has log transformation seems to have squeezed the data - x axis ranges from 6 to 10 and y axis ranges from 10 to 17 as opposed to ranging from 100 to 10000 in x axis and  75000 to 7060000 in y axis in scatterplot1. So the difference between different data points is smaller. 2) Relationship between variables in plot 1 seems to be exponential and in plot 2 it seems to be linear. 3) Plot 2 clustered the data points in mid range of both axis and plot 1's data points are clustered around lower range of both axis 

## OLS performs poorly when there are big outliers and in scatterplot 1 there are many because of exponential behavior of the data. I would pick Model2 (can be seen in scatterplot2) because a linear line can explain the data more accurately since data looks linear.


Model1 <- lm(data=Training,price~sqft_living)
Model2 <- lm(data=Training,log(price)~log(sqft_living))

Beta0_Model1<-coef(Model1)[1]
Beta1_Model1<-coef(Model1)[2]
Beta0_Model2<-coef(Model2)[1]
Beta1_Model2<-coef(Model2)[2]
R_Squared_Model1<-summary(Model1)$r.squared
R_Squared_Model2<-summary(Model2)$r.squared

cat("Model1 coefficients and R-Squared:\nBeta0:",Beta0_Model1,"\nBeta1:",Beta1_Model1,"\nR-squared:",R_Squared_Model1)
cat("Model2 coefficients and R-Squared:\nBeta0:",Beta0_Model2,"\nBeta1:",Beta1_Model2,"\nR-squared:",R_Squared_Model2)


## I would compute MSE (Mean Squared Errors) to compare two different models.First I need to compute price_hats for my test data using the Model 1 and Model 2 coefficients. Then using mse function of package hydroGOF, I compute MSEs for my models.
price_hat_Model1<-predict(Model1,newdata=Test) ##Prediction using Model1
price_hat_Model2<-exp(predict(Model2,newdata=Test)) ##Prediction using Model2- notice that I had to take exponent of predict function because MODEL 2 returns log of predicted value.

MSE_Model1=mse(price_hat_Model1,Test$price) ## computing MSE for Model 1
MSE_Model2=mse(price_hat_Model2,Test$price) ## computing MSE for Model 2

cat("MSE for Model1:",MSE_Model1,"\nMSE for Model2:",MSE_Model2)

cat("MSE for Model 2 is ",round(100*(MSE_Model2/MSE_Model1-1),2),"% more than Model 1. Therefore I can safely suggest that Model 1 is better than Model 2.")

############################################################################################################

## Best Subset Selection Method: We have suggested 12 variables. For each of P choose 12 subsets, I will calculate SSE and see which one gives me a smaller SSE and I pick that variable. To ease our computation, it is suggested to use subset size 1.

## Suggested variables:  bedrooms, bathrooms, log(sqft living), log(sqft lot), floors, waterfront, view, condition, grade, yr built, lat, long

## Creating Models using 1 variables each so total 12 Models. 
Model_PartE_1<-lm(log(price)~bedrooms,data=Training)
Model_PartE_2<-lm(log(price)~bathrooms,data=Training)
Model_PartE_3<-lm(log(price)~log(sqft_living),data=Training)
Model_PartE_4<-lm(log(price)~log(sqft_lot),data=Training)
Model_PartE_5<-lm(log(price)~floors,data=Training)
Model_PartE_6<-lm(log(price)~waterfront,data=Training)
Model_PartE_7<-lm(log(price)~view,data=Training)
Model_PartE_8<-lm(log(price)~condition,data=Training)
Model_PartE_9<-lm(log(price)~grade,data=Training)
Model_PartE_10<-lm(log(price)~yr_built,data=Training)
Model_PartE_11<-lm(log(price)~lat,data=Training)
Model_PartE_12<-lm(log(price)~long,data=Training)

## Predicting prices using each Model. Please note that I have to take exponent of predict function since it returns log of price.
price_hat_PartE_1<-exp(predict(Model_PartE_1,newdata=Test))
price_hat_PartE_2<-exp(predict(Model_PartE_2,newdata=Test))
price_hat_PartE_3<-exp(predict(Model_PartE_3,newdata=Test))
price_hat_PartE_4<-exp(predict(Model_PartE_4,newdata=Test))
price_hat_PartE_5<-exp(predict(Model_PartE_5,newdata=Test))
price_hat_PartE_6<-exp(predict(Model_PartE_6,newdata=Test))
price_hat_PartE_7<-exp(predict(Model_PartE_7,newdata=Test))
price_hat_PartE_8<-exp(predict(Model_PartE_8,newdata=Test))
price_hat_PartE_9<-exp(predict(Model_PartE_9,newdata=Test))
price_hat_PartE_10<-exp(predict(Model_PartE_10,newdata=Test))
price_hat_PartE_11<-exp(predict(Model_PartE_11,newdata=Test))
price_hat_PartE_12<-exp(predict(Model_PartE_12,newdata=Test))

## Computing SSE for each variable Models.
SSE_PartE_1<-sum((Test$price-price_hat_PartE_1)^2)
SSE_PartE_2<-sum((Test$price-price_hat_PartE_2)^2)
SSE_PartE_3<-sum((Test$price-price_hat_PartE_3)^2)
SSE_PartE_4<-sum((Test$price-price_hat_PartE_4)^2)
SSE_PartE_5<-sum((Test$price-price_hat_PartE_5)^2)
SSE_PartE_6<-sum((Test$price-price_hat_PartE_6)^2)
SSE_PartE_7<-sum((Test$price-price_hat_PartE_7)^2)
SSE_PartE_8<-sum((Test$price-price_hat_PartE_8)^2)
SSE_PartE_9<-sum((Test$price-price_hat_PartE_9)^2)
SSE_PartE_10<-sum((Test$price-price_hat_PartE_10)^2)
SSE_PartE_11<-sum((Test$price-price_hat_PartE_11)^2)
SSE_PartE_12<-sum((Test$price-price_hat_PartE_12)^2)

## Finding variable with min SSE
SSE<-c(SSE_PartE_1,SSE_PartE_2,SSE_PartE_3,SSE_PartE_4,SSE_PartE_5,SSE_PartE_6,SSE_PartE_7,SSE_PartE_8,SSE_PartE_9,SSE_PartE_10,SSE_PartE_11,SSE_PartE_12)
which(SSE==min(SSE)) ## SSE number 9 is the minimim SSE which is variable grade so it is the best predictor.
SSE_PartE_9


############################################################################################################

# Using Scatter Plots for Bedrooms vs Price:

plot(Training$bedroom,log(Training$price), main="Bedrooms vs. Log Price of House", xlab="Bedrooms", ylab="Log Price of House", pch=19)
plot(log(Training$bedroom),log(Training$price), main="Log Bedrooms vs. Log Price of House", xlab="Log Bedrooms", ylab="Log Price of House", pch=19)

## I think we could take log of bedroom and get better performance.

# Using Scatter Plots for Bathrooms vs Price:

plot(Training$bathrooms,log(Training$price), main="Bathrooms vs. Log Price of House", xlab="Bathrooms", ylab="Log Price of House", pch=19)
plot(log(Training$bathrooms),log(Training$price), main="Log Bathrooms vs. Log Price of House", xlab="Log Bathrooms", ylab="Log Price of House", pch=19)

## I think we could take log of bahtroom and get better performance.


# Using Scatter Plots for Grade vs Price:

plot(Training$grade,log(Training$price), main="Grade vs. Log Price of House", xlab="Grade", ylab="Log Price of House", pch=19)
plot(log(Training$grade),log(Training$price), main="Log Grade vs. Log Price of House", xlab="Log Grade", ylab="Log Price of House", pch=19)

## I think grade is good as it is, we should keep it in the current form.


Model3<-lm(log(price)~log(sqft_living)+bedrooms+bathrooms+grade+waterfront,data=Training)
summary(Model3)

## Assigning coefficients and R-squared
Beta0_Model3<-coef(Model3)[1]
Beta1_Model3<-coef(Model3)[2]
Beta2_Model3<-coef(Model3)[3]
Beta3_Model3<-coef(Model3)[4]
Beta4_Model3<-coef(Model3)[5]
Beta5_Model3<-coef(Model3)[6]
R_Squared_Model3<-summary(Model3)$r.squared

cat("Model3 coefficients and R-Squared:\nBeta0:",Beta0_Model3,"\nBeta1:",Beta1_Model3,"\nBeta2:",Beta2_Model3,"\nBeta3:",Beta3_Model3,"\nBeta4:",Beta4_Model3,"\nBeta5:",Beta5_Model3,"\nR-squared:",R_Squared_Model3)

cat("R-Squared for Model 3 is ",100*(R_Squared_Model3/R_Squared_Model2-1),"% better than Model 2.\nR-squared for Model 3 and Model 2 are:",R_Squared_Model3,"and",R_Squared_Model2,"respectively.")


## Model 2 has higher bias and Model 3 has higher variance. As we increase the number of independent variables in our models, we increase the accuracy therefore we decrease the bias. However, at the same time, we increase the variance. To get the optimal model, we need to take a look at bias-variance trade off and select the model with minimum residual errors. Model 2 is too simple and it is likely to produce more errors, R-squared also supports this claim, Model 2 explains less % of data than Model 3 [comparing R-squareds]. We need to make sure not to overfit the data also.



## MSE is a good method to compare different models. I already compute MSE for Model 2 which is:
MSE_Model2

## Now I need to compute MSE for Model 3

price_hat_Model3<-exp(predict(Model3,newdata=Test)) ##Prediction using Model3- notice that I had to take exponent of predict function because MODEL 3 returns log of predicted value

MSE_Model3=mse(price_hat_Model3,Test$price) ## computing MSE for Model 3

cat("MSE for Model2:",MSE_Model2,"\nMSE for Model3:",MSE_Model3)

cat("MSE for Model 2 is ",round(100*(MSE_Model2/MSE_Model3-1),2),"% more than Model 3. Therefore I can safely suggest that Model 3 is better than Model 2.")


############################################################################################################
## If we plot residual vs. a variable that is not used in the prediction and if we see any recognizable patterns, then we can say that some of the variation in residual can be actually explained by the non-used variable therefore we should include it in our model to reduce the residual errors. Therefore we can say that this approach is a very good idea.


############################################################################################################
## To calculate residuals, we simply need to substract price_hat_Model3 from the actual price.
residual_Model3=Test$price-price_hat_Model3

plot(Test$sqft_lot,residual_Model3) ## Residual vs. sqft_lot

plot(Test$floors,residual_Model3) ## Residual vs. floors

plot(Test$view,residual_Model3) ## Residual vs. view

plot(Test$condition,residual_Model3) ## Residual vs. condition

plot(Test$sqft_above,residual_Model3) ## Residual vs. sqft_above

plot(Test$sqft_basement,residual_Model3) ## Residual vs. sqft_basement

plot(Test$yr_built,residual_Model3) ## Residual vs. yr_built

plot(Test$yr_renovated,residual_Model3) ## Residual vs. yr_renovated

plot(Test$zipcode,residual_Model3) ## Residual vs. zipcode

plot(Test$lat,residual_Model3) ## Residual vs. lat

plot(Test$long,residual_Model3) ## Residual vs. long

plot(Test$sqft_living15,residual_Model3) ## Residual vs. sqft_living15

plot(Test$sqft_lot15,residual_Model3) ## Residual vs. sqft_lot15
############################################################################################################



plot(Test$yr_built,residual_Model3) ## Residual vs. yr_built
plot(Test$lat,residual_Model3) ## Residual vs. lat

## I would include "year built" by indexing. So yr_built starts from 1900 and ends at 2015. I would use the following equation: 

## Age = 2015-yr_built +1 

#I want to start the age from 1 because in case I take the log function, I dont want to have log(0) since it is -infinity. I would do the same for lat also: 

## LatOpt= lat- min(lat) +1  

##this way I would start my lat from 1. Then I may perform a log transformation. It is hard to tell which one is more effective without actually computing this.


Model4<-lm(log(price)~log(sqft_living)+bedrooms+bathrooms+grade+waterfront+yr_built+lat,data=Training)
summary(Model4)

## Assigning coefficients and R-squared
Beta0_Model4<-coef(Model4)[1]
Beta1_Model4<-coef(Model4)[2]
Beta2_Model4<-coef(Model4)[3]
Beta3_Model4<-coef(Model4)[4]
Beta4_Model4<-coef(Model4)[5]
Beta5_Model4<-coef(Model4)[6]
Beta6_Model4<-coef(Model4)[7]
Beta7_Model4<-coef(Model4)[8]
R_Squared_Model4<-summary(Model4)$r.squared

cat("Model4 coefficients and R-Squared:\nBeta0:",Beta0_Model4,"\nBeta1:",Beta1_Model4,"\nBeta2:",Beta2_Model4,"\nBeta3:",Beta3_Model4,"\nBeta4:",Beta4_Model4,"\nBeta5:",Beta5_Model4,"\nBeta6:",Beta6_Model4,"\nBeta7:",Beta7_Model4,"\nR-squared:",R_Squared_Model4)

cat("R-Squared for Model 4 is ",100*(R_Squared_Model4/R_Squared_Model3-1),"% better than Model 3.\nR-squared for Model 4 and Model 3 are:",R_Squared_Model4,"and",R_Squared_Model3,"respectively.")


## We already computed MSE for Model 3:
MSE_Model3

## Let's compute MSE for Model 4:
price_hat_Model4<-exp(predict(Model4,newdata=Test)) ##Prediction using Model4- notice that I had to take exponent of predict function because MODEL 4 returns log of predicted value.

MSE_Model4=mse(price_hat_Model4,Test$price) ## computing MSE for Model 4

cat("MSE for Model3:",MSE_Model3,"\nMSE for Model4:",MSE_Model4)

cat("MSE for Model 3 is ",round(100*(MSE_Model3/MSE_Model4-1),2),"% more than Model 4. Therefore I can safely suggest that Model 4 is better than Model 3. So Model 4 predicts the prices better.")

############################################################################################################
## Residual vs. Zipcode 
boxplot_PartI=boxplot(residual_Model3~(Test$zipcode), 
  col=(c("gold","darkgreen")),
  main="Residual vs. Zipcode", xlab="Zipcode", ylab="Residual")

logboxplot_PartI=boxplot(residual_Model3~log(Test$zipcode), 
  col=(c("gold","darkgreen")),
  main="Residual vs. Log Zipcode", xlab="Log Zipcode", ylab="Residual")

## I tried both regular and log zip codes and they both seems to have same patterns so I will just include zip code directly without transformation.

Model5<-lm(log(price)~log(sqft_living)+bedrooms+bathrooms+grade+waterfront+yr_built+lat+zipcode,data=Training)
summary(Model5)

cat("R-squared for Model5 is:",summary(Model5)$r.squared)

## Let's compute MSE for Model 5:
price_hat_Model5<-exp(predict(Model5,newdata=Test)) ##Prediction using Model5- notice that I had to take exponent of predict function because MODEL 5 returns log of predicted value.

MSE_Model5=mse(price_hat_Model5,Test$price) ## computing MSE for Model 5

cat("MSE for Model4:",MSE_Model4,"\nMSE for Model5:",MSE_Model5)

cat("MSE for Model 4 is ",round(100*(MSE_Model4/MSE_Model5-1),2),"% more than Model 5. Therefore I can safely suggest that Model 5 is better than Model 4.")

############################################################################################################
## So my new Model X should be 15% or 25% better than Model 5.

ModelX<-lm(log(price)~log(sqft_living)+log(bedrooms+0.5)+exp(bathrooms)+grade+waterfront+log(lat-min(Training$lat)+0.2)+log(zipcode-min(Training$zipcode)+4)+log(abs(long-min(Training$long))+0.01)+log(view+0.5)+condition+sqft_above+log(sqft_basement+0.001)+log(sqft_lot15)+log(2015-yr_renovated+1)+date+(bedrooms*bathrooms)+(view*log(condition))+sqft_basement+(log(grade)*exp(condition))+(bathrooms*log(sqft_living))+log(condition)+(view*bedrooms)+(zipcode*lat),data=Training)

summary(ModelX)

## Let's compute MSE for Model X:
price_hat_ModelX<-round(exp(predict(ModelX,newdata=Test)),0) ##Prediction using ModelX- notice that I had to take exponent of predict function because MODEL X returns log of predicted value.

MSE_ModelX=mse(price_hat_ModelX,Test$price) ## computing MSE for Model X

cat("MSE for Model5:",MSE_Model5,"\nMSE for ModelX:",MSE_ModelX)

cat("MSE for Model 5 is ",round(100*(MSE_Model5/MSE_ModelX-1),2),"% more than Model X. Therefore I can safely suggest that Model X is better than Model 5.")

cat("R-Squared for Model X is:",summary(ModelX)$r.squared)

############################################################################################################
output <- (cbind("ID"=Test$id,"Orginal Price"=Test$price,"Predicted Price"=price_hat_ModelX))
write.csv(output, file = "test_data_originalPriceVsPredicted.csv", row.names=FALSE)
