### Data Understanding

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("randomForest")

library(randomForest)  
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(stringr)
library(tidyr)
library(plyr)
library(dplyr)

# Loading file
diabetes_data<- read.csv("../input/diabetes.csv", stringsAsFactors = F)
#diabetes_data<- read.csv("diabetes.csv", stringsAsFactors = F)

str(diabetes_data)    # 768 obs. of  9 variables including the target variable
nrow(diabetes_data) #768

#View(diabetes_data)

################################################################

### Data Preparation & Exploratory Data Analysis
# Understanding the structure of the collated file

diabetes_data <- mutate(diabetes_data, age_grp = ifelse(Age <=30, 'Young',
                                                               if_else(Age > 30 & Age <= 60, 'MiddleAge'
                                                                       ,'SeniorCitizen')))	  
str(diabetes_data)    # 768 obs. of  10 variables																	   
	  
#Summary of dataset
summary(diabetes_data)

sum(is.na(diabetes_data)) #No NA values
sum(duplicated(diabetes_data)) # No duplicate record



#######Outlier treatment of numeric variables

str(diabetes_data) #768 obs. of  10 variables:

# Plotted Histogram and Boxplots for numeric variables 
  box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                    axis.ticks=element_blank(), axis.text=element_blank())
  
  box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                      axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                      legend.position="none")
  
#Pregnancies Variable
  plot_grid(ggplot(diabetes_data, aes(Pregnancies))+ geom_histogram(binwidth = 12.5) + ggtitle("Histogram for Pregnancies"),
            ggplot(diabetes_data, aes(x="",y=Pregnancies))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for Pregnancies"), 
            align = "v",ncol = 1)
  #outlier observed for Pregnancies
 
 #Glucose Variable
  plot_grid(ggplot(diabetes_data, aes(Glucose))+ geom_histogram(binwidth = 12.5) + ggtitle("Histogram for Glucose"),
            ggplot(diabetes_data, aes(x="",y=Glucose))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for Glucose"), 
            align = "v",ncol = 1)
  #outlier observed for Glucose
  
   #BloodPressure Variable
  plot_grid(ggplot(diabetes_data, aes(BloodPressure))+ geom_histogram(binwidth = 12.5) + ggtitle("Histogram for BloodPressure"),
            ggplot(diabetes_data, aes(x="",y=BloodPressure))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for BloodPressure"), 
            align = "v",ncol = 1)
  #outlier observed for BloodPressure
  
     #SkinThickness Variable
  plot_grid(ggplot(diabetes_data, aes(SkinThickness))+ geom_histogram(binwidth = 12.5) + ggtitle("Histogram for SkinThickness"),
            ggplot(diabetes_data, aes(x="",y=SkinThickness))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for SkinThickness"), 
            align = "v",ncol = 1)
  #outlier observed for SkinThickness
  
   
     #Insulin Variable
  plot_grid(ggplot(diabetes_data, aes(Insulin))+ geom_histogram(binwidth = 12.5) + ggtitle("Histogram for Insulin"),
            ggplot(diabetes_data, aes(x="",y=Insulin))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for Insulin"), 
            align = "v",ncol = 1)
  #outlier observed for Insulin
  
       #BMI Variable
  plot_grid(ggplot(diabetes_data, aes(BMI))+ geom_histogram(binwidth = 12.5) + ggtitle("Histogram for BMI"),
            ggplot(diabetes_data, aes(x="",y=BMI))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for BMI"), 
            align = "v",ncol = 1)
  #outlier observed for BMI
  
       #DiabetesPedigreeFunction Variable
  plot_grid(ggplot(diabetes_data, aes(DiabetesPedigreeFunction))+ geom_histogram(binwidth = 12.5) + ggtitle("Histogram for DiabetesPedigreeFunction"),
            ggplot(diabetes_data, aes(x="",y=DiabetesPedigreeFunction))+ geom_boxplot(width=0.1)+coord_flip()+box_theme + ggtitle("Boxplot for DiabetesPedigreeFunction"), 
            align = "v",ncol = 1)
  #outlier observed for DiabetesPedigreeFunction
  
  #Here confirming it with percentiles for all the continuous variables
  sapply(diabetes_data[,c("Pregnancies", "Glucose", 
                             "BloodPressure", "SkinThickness",
                             "Insulin","BMI","DiabetesPedigreeFunction")], 
         function(x) quantile(x,seq(0,1,.01),na.rm = T))
		 
  diabetes_data$BloodPressure[which(diabetes_data$BloodPressure>106.00)] <- 106.00
  
  diabetes_data$SkinThickness[which(diabetes_data$SkinThickness> 51.33)] <-  51.33
  
  diabetes_data$Insulin[which(diabetes_data$Insulin> 519.90)] <-  519.90
  
  diabetes_data$BMI[which(diabetes_data$BMI> 50.759) ] <-  50.759 
  
  diabetes_data$DiabetesPedigreeFunction[which(diabetes_data$DiabetesPedigreeFunction> 1.69833)] <-  1.69833

   #age_grp vs Outcome
  Plot1<- ggplot(diabetes_data, aes(factor(Outcome), group = age_grp)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
    scale_y_continuous(labels=scales::percent) +
    geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(title='Percentage of Outcome w.r.t age_grp', x='Outcome',y='Relative Percentage') + 
    facet_grid(~age_grp) +
    scale_fill_discrete(name = "Outcome")

   Plot1
   
##########################################################################################################
############## MODEL 1 : RANDOM FOREST ############################


#Removing the age_grp column
diabetes_data_RF <- diabetes_data[ , -which(names(diabetes_data) %in% c("age_grp"))]

diabetes_data_RF$Outcome <- as.factor(diabetes_data_RF$Outcome)   

str(diabetes_data_RF)

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(diabetes_data_RF$Outcome, SplitRatio = 0.70)

train<- diabetes_data_RF[split_indices, ]

test<- diabetes_data_RF[!split_indices, ]

nrow(train)/nrow(diabetes_data_RF)

nrow(test)/nrow(diabetes_data_RF)

# Model Building: 

#Fit Random Forest Model

model_RF <- randomForest(Outcome~ . , data = train,proximity = F, do.trace = T, mtry = 5,ntree=500)

summary(model_RF)

print(model_RF)

# plot the Error by trees:
 plot(model_RF)
 
which.min(model_RF$err.rate[, 1])
# 70
# 70 trees are needed to optimize the model accuracy.
model_RF_new <- randomForest(Outcome ~ ., data = train, ntree = 70)
print(model_RF_new)

pred_test_RF<- predict(model_RF_new, newdata = test, type ="response")

table(pred_test_RF,test$Outcome)

confusionMatrix(pred_test_RF,test$Outcome)

# variable importance plot

varImpPlot(model_RF_new)	

##########################################  Model Building and Evaluation using Decison Tree #######################################

library(rpart)
library(rpart.plot)
library(kernlab)
library(readr)


#Removing the age_grp column
diabetes_data_DCT <- diabetes_data[ , -which(names(diabetes_data) %in% c("age_grp"))]

diabetes_data_DCT$Outcome <- as.factor(diabetes_data_DCT$Outcome)   

str(diabetes_data_DCT)

# splitting into train and test data

set.seed(1)

split_indices_1 <- sample.split(diabetes_data_DCT$Outcome , SplitRatio = 0.70)

train_DCT<- diabetes_data_DCT[split_indices_1, ]

test_DCT<- diabetes_data_DCT[!split_indices_1, ]

nrow(train_DCT)/nrow(diabetes_data_DCT)

nrow(test_DCT)/nrow(diabetes_data_DCT)

table(train_DCT$Outcome)

#check classes distribution
prop.table(table(train_DCT$Outcome))

#1 build tree model- default hyperparameters
tree_1 <- rpart(Outcome ~ .,  data = train_DCT, method = "class")       

# display decision tree
prp(tree_1)

# make predictions on the test set
tree_1_predict <- predict(tree_1, test_DCT, type = "class")

table(tree_1_predict, test_DCT$Outcome)

# evaluate the results
confusionMatrix(tree_1_predict, test_DCT$Outcome, positive = '1') 


#2 Change the algorithm to "information gain" instead of default "gini" ----------------------
tree_2 <- rpart(Outcome ~.,data = train_DCT, method = "class", parms = list(split = "information"))

# display decision tree
prp(tree_2)

# make predictions on the test set
tree_2_predict <- predict(tree_2, test_DCT, type = "class")

# evaluate the results
confusionMatrix(tree_2_predict, test_DCT$Outcome, positive = '1') 


#3 Tune the hyperparameters ----------------------------------------------------------
tree_3 <- rpart( Outcome ~ .,
                    data = train_DCT,  
                    method = "class",  
                    control = rpart.control(minsplit = 1000, 
                                            minbucket = 1000,
                                            cp = 0.05)) 

# display decision tree
prp(tree_3)

# make predictions on the test set
tree_3_predict <- predict(tree_3, test_DCT, type = "class")

# evaluate the results
confusionMatrix(tree_3_predict, test_DCT$Outcome, positive = "1")

#4 A more complex tree -----------------------------------------------------------------
tree_4 <- rpart(Outcome ~ .,   
                    data = train_DCT,    
                    method = "class", 
                    control = rpart.control(minsplit = 1, 
                                            minbucket = 1, 
                                            cp = 0.001)) 

# display decision tree
prp(tree_4)

# make predictions on the test set
tree_4_predict <- predict(tree_4, test_DCT, type = "class")

# evaluate the results
confusionMatrix(tree_4_predict, test_DCT$Outcome, positive = "1") 

#5 Cross test to choose CP ------------------------------------------------------------
library(caret)

# set the number of folds in cross test to 5
dtree.control = trainControl(method = "cv", number = 5)

# set the search space for CP
dtree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))


# train model
tree_5 <- train(Outcome ~ .,
                    data = train_DCT,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = dtree.control,
                    tuneGrid = dtree.grid,
                    control = rpart.control(minsplit = 50,
                                            minbucket = 20))

# look at cross validated model results
tree_5

# look at best value of hyperparameter
tree_5$bestTune  #0.0025

# make predictions on test set
tree_5_predict <- predict.train(tree_5, test_DCT)

# accuracy
confusionMatrix(tree_5_predict, test_DCT$Outcome) 

# plot CP vs Accuracy
library(ggplot2)
accuracy_graph <- data.frame(tree_5$results)
ggplot(data = accuracy_graph, aes(x = cp, y = Accuracy*100)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity Parameter (CP)", y = "Accuracy", title = "CP vs Accuracy")
  
  ############# Conclusion ####################			
  
 #Based on Random Forest final model, accuracy is  77.83%
 #Based on Decison Tree final model, accuracy is 73.04%
 
 # So we can consider the result 77.83%