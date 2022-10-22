# Definition of Libraries----
rm(list = ls()) # Cleaning environment
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(magrittr) # needs to be run every time you start R and want to use %>%
# Calling library dplyr for the use of filter
library(dplyr)
#install.packages("moments"); #De-comment to install the package
library(moments) #Load the package
#install.packages("scales"); #De-comment to install the package
library(scales) #For more graphic scales options
library(factoextra) # Library used to graph PCA Biplot
#install.packages('forcats') #De-comment to install the package
library(forcats) # Library to collapse factor levels into manually defined groups
library(caret) #Classification and Regression Training package
library(glmnet) #For visualisng correlation matrix
library(ranger) #For random forest
#install.packages("randomForest")#De-comment to install the package
library(randomForest)
#install.packages("rpart")#De-comment to install the package
#install.packages("rpart.plot")#De-comment to install the package
library(rpart) #For CART modelling
library(rpart.plot) #For plotting CART
library(broom) # For turns outputs into tidy tibbles



# Part 1 - General data preparation and cleaning----

# i. Clean the whole dataset----
## Import the ML_dataset2.csv into R Studio----
dat <- read.csv("path here", na.strings=NA, stringsAsFactors=TRUE)

## Replace NA as missing values on 99999 observations----
dat$Average.ping.to.attacking.IP.milliseconds[dat$Average.ping.to.attacking.IP.milliseconds == 99999] <- NA

## Replace NA as missing values on negative observations----
dat$Attack.Source.IP.Address.Count[dat$Attack.Source.IP.Address.Count < 0] <- NA


## Replace NA as missing values on '???' observations----
dat$Source.OS.Detected[dat$Source.OS.Detected == '???'] <- NA

## Delete Range.Trust.Score Feature----
dat$IP.Range.Trust.Score <- NULL 

## Delete Source.Port.Range----
dat$Source.Port.Range <- NULL 

# ii. Merge Categories of ML_dataset2 dataset----
## Merging Windows 10, Windows Server 2008 in only Windows_All category----
dat$Source.OS.Detected <- fct_collapse(dat$Source.OS.Detected, Windows_All = c("Windows 10", "Windows Server 2008"))
##Checking categories in Source.OS.Detected
fct_count(dat$Source.OS.Detected)

## Merging Windows (Desktops), Windows (Servers) in only Windows_DeskServ category----
dat$Target.Honeypot.Server.OS <- fct_collapse(dat$Target.Honeypot.Server.OS, Windows_DeskServ = c("Windows (Desktops)", "Windows (Servers)"))
## Merging Linux, MacOS (All) in only MacOS_Linus category----
dat$Target.Honeypot.Server.OS <- fct_collapse(dat$Target.Honeypot.Server.OS, MacOS_Linus = c("Linux", "MacOS (All)"))
#Checking categories in Target.Honeypot.Server.OS
fct_count(dat$Target.Honeypot.Server.OS)

# iii. Log transformation----

## Overwritten Average.ping.variability with the log-transformed data----
dat$Average.ping.variability <- log(dat$Average.ping.variability)
## Overwritten Hits with the log-transformed data
dat$Hits <- sqrt(dat$Hits)
## Overwritten Attack.Source.IP.Address.Count with the log-transformed data----
dat$Attack.Source.IP.Address.Count <- sqrt(dat$Attack.Source.IP.Address.Count)
## Overwritten Average.ping.to.attacking.IP.milliseconds with the log-transformed data----
dat$Average.ping.to.attacking.IP.milliseconds <- sqrt(dat$Average.ping.to.attacking.IP.milliseconds)
## Overwritten Individual.URLs.requested with the log-transformed data----
dat$Individual.URLs.requested <- sqrt(dat$Individual.URLs.requested)


# iv.Removing incomplete cases----
## Omit ??? cases
dat$Source.OS.Detected <- droplevels(dat$Source.OS.Detected)
## Removing NA cases-----
dat <- na.omit(dat)
## Saving Data after the cleaning-----
write.csv(dat, #Name of the data frame/tibble to be exported
          "C:/Users/juans/OneDrive - Edith Cowan University/MAT6206 - Data Analysis and Visualisation/Assessments/Assignment 2/ML_dataset_cleaned.csv" #Name of the new CSV file to write the data to.
)
## Summary of clean dataset
summary(dat)

# iv. Creating training and test sets using an 30/70 split using student id----

## Set the random seed.----
set.seed(IDNumberhere) 
## Get row numbers for the training data----
trainRowNumbers <- createDataPartition(dat$APT, #The outcome variable
                                       p=0.30, #proportion of data to form the training set
                                       list=FALSE #Don't store the result in a list
);
## Create the training dataset----
trainData <- dat[trainRowNumbers,]
## Create the test dataset----
testData <- dat[-trainRowNumbers,]
## Saving Training Data in CSV-----
write.csv(trainData, #Name of the data frame/tibble to be exported
          "C:/Users/juans/OneDrive - Edith Cowan University/MAT6206 - Data Analysis and Visualisation/Assessments/Assignment 2/training.csv" #Name of the new CSV file to write the data to.
)
## Saving Test Data in CSV-----
write.csv(testData, #Name of the data frame/tibble to be exported
          "C:/Users/juans/OneDrive - Edith Cowan University/MAT6206 - Data Analysis and Visualisation/Assessments/Assignment 2/test.csv" #Name of the new CSV file to write the data to.
)






# Part 2 - Compare the performances of different ML algorithms----


# i. Select THREE supervised learning modelling algorithms----
set.seed(10551059) 
models.list1 <- c("Logistic Ridge Regression", "Logistic LASSO Regression", "Logistic Elastic-Net Regression") 

models.list2 <- c("Classification Tree", "Bagging Tree", "Random Forest") 

myModels <- c("Binary Logistic Regression", 
              sample(models.list1,size=1), 
              sample(models.list2,size=1)) 

myModels %>% data.frame


## 1b. Run Binary Logistic Regression----
# Excluding Sample.ID and Initial.Modelling.Result and execution Binary Logistic Model
mod.dat.lg <- glm(APT~. - Sample.ID - Initial.Modelling.Result, family="binomial", data=trainData);
#Summaries the model
summary(mod.dat.lg) 

### Evaluate the predictive power of the logistic regression model on the test set----

# predicted probability of APT on the test data
pred.prob <- predict(mod.dat.lg,new=testData,type="response")
pred.class <- ifelse(pred.prob>0.5,"Yes","No") #Assign to the respective class

#Confusion matrix with re-ordering of "Yes" and "No" responses
cf.lg <- table(pred.class %>% as.factor %>% relevel(ref="Yes"),
               testData$APT %>% relevel(ref="Yes"));

prop <- prop.table(cf.lg,2); prop %>% round(digit=3) #Proportions by columns

#Summary of confusion matrix
confusionMatrix(cf.lg);

## 2b. Run Logistic Elastic-Net Regression----

#Getting current Time
currentTime <- Sys.time()
#Output time + iteration
out <- paste0(currentTime)
print(out)

lambdas <- 10^seq(-3,3,length=100) #A sequence 100 lambda values
alphas <- seq(0.1,0.9,by=0.1); alphas #A sequence of alpha values to test
set.seed(10551059)
mod.dat.elnet <- train(APT~. - Sample.ID - Initial.Modelling.Result, #Formula
           data = trainData, #Training data
           method = "glmnet", #Penalised regression modelling
           #Set preProcess to c("center", "scale") to standardise data
           preProcess = NULL,
           #Perform 10-fold CV, 5 times over.
           trControl = trainControl("repeatedcv",
                                    number = 10,repeats = 5),
           tuneGrid = expand.grid(alpha = alphas,
                                  lambda = lambdas)
)

#Getting current Time
currentTime <- Sys.time()
#Output time + iteration
out <- paste0(currentTime)
print(out)

#Optimal lambda value
mod.dat.elnet$bestTune

# Model coefficients
coef(mod.dat.elnet$finalModel, mod.dat.elnet$bestTune$lambda)

#predicted classes of APT on the test data
pred.class.elnet <- predict(mod.dat.elnet,new=testData)

#Confusion matrix with re-ordering of "Yes" and "No" responses
cf.elnet <- table(pred.class.elnet %>% relevel(ref="Yes"),
                  testData$APT %>% relevel(ref="Yes"));

#Proportions by columns
prop <- prop.table(cf.elnet,2); prop %>% round(digit=3) 

#Summary of confusion matrix
confusionMatrix(cf.elnet)



## 3b. Run Random Forest----
#Run the random forest model with 500 trees and 3 randomly selected features at every split

rf.dat <- randomForest(APT~. - Sample.ID - Initial.Modelling.Result,
                      data = trainData, ntree = 500,
                      seed = 10551059, 
                      importance = TRUE)
rf.dat

#Create a search grid for the tuning parameters
grid.rf.dat <- expand.grid(num.trees = c(400,500,600), #Number of trees
                          mtry = c(2,4,6), # split rule
                          min.node.size = seq(2,6,2),
                          sample.fraction = c(0.5,0.7,0.8),
                          #Initialise columns to store the OOB misclassification rate
                          OOB.misclass=NA,
                          test.sens = NA, #Column to store the test Sensitivity
                          test.spec = NA, #Column to store the test Specificity
                          test.acc = NA) #Column to store the test Accuracy)

dim(grid.rf.dat) #Check the dimension

View(grid.rf.dat) #View the search grid

for(I in 1:nrow(grid.rf.dat))
{
  
  rf.dat <- randomForest(APT~. - Sample.ID - Initial.Modelling.Result,
               data=trainData,
               num.trees=grid.rf.dat$num.trees[I],
               mtry=grid.rf.dat$mtry[I],
               min.node.size=grid.rf.dat$min.node.size[I],
               sample.fraction=grid.rf.dat$sample.fraction[I],
               seed=10551059,
               respect.unordered.factors="order")

  #OOB misclassification rate
  grid.rf.dat$OOB.misclass[I] <- rf.dat$err*100
  
  #Test classification
  #rf.pred.test <- predict(rf.dat,data=testData)$predictions; #Predicted classes
  rf.pred.test <- predict(rf.dat, testData, type = "class")
  #Summary of confusion matrix
  test.cf.dat <- confusionMatrix(rf.pred.test %>% relevel(ref="No"),
                                testData$APT %>% relevel(ref="No"));
  prop.cf <- test.cf.dat$table %>% prop.table(2)
  grid.rf.dat$test.sens[I] <- prop.cf[1,1] %>% round(5)*100 #Sensitivity
  grid.rf.dat$test.spec[I] <- prop.cf[2,2] %>% round(5)*100 #Specificity
  grid.rf.dat$test.acc[I] <- test.cf.dat$overall[1] %>% round(5)*100 #Accuracy
  
  #Getting current Time
  currentTime <- Sys.time()
  #Output time + iteration
  out <- paste0(currentTime, ": Combination #", I, ".")
  print(out)
  
}


#Sort the results by the OOB misclassification rate and display them.
grid.rf.dat[order(grid.rf.dat$OOB.misclass,decreasing=FALSE)[1:5],] %>% round(2)

#Run the random forest model with the best hyperparamaters for an optimal combination

rf.dat <- randomForest(APT~. - Sample.ID - Initial.Modelling.Result,
                       data = trainData, 
                       ntree = 400,
                       mtry=6,
                       min.node.size=6,
                       sample.fraction=0.8,
                       seed=10551059,
                       importance = TRUE,
                       respect.unordered.factors="order")
rf.dat

#predicted classes of APT on the test data
pred.class.rf <- predict(rf.dat,new=testData)

#Confusion matrix with re-ordering of "Yes" and "No" responses
cf.rf <- table(pred.class.rf %>% relevel(ref="Yes"),
               testData$APT %>% relevel(ref="Yes"));

#Proportions by columns
prop <- prop.table(cf.rf,2); prop %>% round(digit=3) 

#Summary of confusion matrix
confusionMatrix(cf.rf)


#Creation of a confusion matrix for the variable Initial.Modelling.Result in the test set.----

#Generation of table for identification of labels in Confusion Matrix

testData$Initial.Modelling.Result %>% table






