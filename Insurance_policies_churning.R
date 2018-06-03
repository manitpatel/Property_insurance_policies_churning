#Summary and Structure of the dataset
summary(Travelers_data)
str(Travelers_data) #Check the data types

#Checking the missing values in each column
colSums(is.na(Travelers_data))


##Exploring MISSING VALUES
library(Rcpp)
library(mice)

md.pattern(Travelers_data)

install.packages("VIM")

library(VIM)

mice_plot = aggr(Travelers_data, sortVars=TRUE,
                 labels=names(Travelers_data), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))


#IMPUTING THE MISSING VALUES IN NUMERICAL COLUMNS
library(Rcpp)
library(mice)

attach(Travelers_data)

#Adding all the variables that will predict the missing values. #Also we can add categorical variables

simple=Travelers_data[c("tenure","n.adults", "n.children", "premium","len.at.res", "ni.age","claim.ind")]  
summary(simple)

set.seed(144)

#This is the main command of imputing. 

imputed=complete(mice(simple))
summary(imputed)

Travelers_data$tenure=imputed$tenure
Travelers_data$n.adults=imputed$n.adults
Travelers_data$n.children=imputed$n.children
Travelers_data$premium=imputed$premium
Travelers_data$len.at.res=imputed$len.at.res
Travelers_data$ni.age=imputed$ni.age
Travelers_data$claim.ind=imputed$claim.ind


colSums(is.na(Travelers_data))


### impute with random value in the Categorical Column 

colSums(is.na(Travelers_data))
install.packages("Hmisc")
library(Hmisc)

Travelers_data$sales.channel = with(Travelers_data, impute(sales.channel, 'random'))
Travelers_data$ni.gender = with(Travelers_data, impute(ni.gender, 'random'))
Travelers_data$coverage.type = with(Travelers_data, impute(coverage.type, 'random'))
Travelers_data$dwelling.type = with(Travelers_data, impute(dwelling.type, 'random'))
Travelers_data$credit = with(Travelers_data, impute(credit, 'random'))
Travelers_data$house.color = with(Travelers_data, impute(house.color, 'random'))
Travelers_data$zip.code = with(Travelers_data, impute(zip.code, 'random'))
Travelers_data$ni.marital.status = with(Travelers_data, impute(ni.marital.status, 'random'))


colSums(is.na(Travelers_data))


### NOW, DATA HAS NO MISSING VALUES ###


#finding the number of rows having missing data in the Dataset
tot=sum(!complete.cases(Travelers_data)) 
tot

#removing the rows that have NA's in Data set

#Removing the observations having missing values in sales.channel column
Travelers_data = Travelers_data[complete.cases(Travelers_data$sales.channel), ]

#REmoving observations based on certain conditions
#D= subset(Assignment_data, !(is.na(Assignment_data$`US Box Office`) & Assignment_data$`Product Type`== "Feature Film"))

#set all missing value as "Missing" 
#train[is.na(train)] <- "Missing" 
# test[is.na(test)] <- "Missing"

#Removing the observations having missing values in the data
#Travelers_data=na.omit(Travelers_data)




## PRE PROCESSING ##

#Encoding male as 1 and Female as 0

Travelers_data$ni.gender[Travelers_data$ni.gender== "M"] = 1
Travelers_data$ni.gender[Travelers_data$ni.gender== "F"] = 0


#dplyr
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

# TO handle date and time 
#Travelers_data = mutate(Travelers_data, Day = day(Travelers_data$year))
#Travelers_data = mutate(Travelers_data, diff_between = difftime(Stoptime_variable , Starttime_variable, units = "secs"))


# Filter based on a certain variable and missing values ####################################

#View(filter(Travelers_data, Travelers_data$sales.channel == "Broker"))
#x= filter(Travelers_data, is.na(Travelers_data$claim.ind))

#table(Travelers_data$sales.channel)

#Travelers_data %>%
  #group_by(sales.channel) %>%
  #summarise(count = n())


####      FEATURE ENGINEERING       ####
Travelers_data$total_burden=Travelers_data$n.adults+Travelers_data$n.children
Travelers_data$age_lenatres=Travelers_data$ni.age- Travelers_data$len.at.res


#sorting the data in ascending order of Age_lenatres, we see that first 39 rows are wrong enteries. SO deleting them
Travelers_data$age_lenatres= sort(Travelers_data$age_lenatres)
Travelers_data$age_lenatres=as.integer(as.numeric(Travelers_data$age_lenatres))

Travelers_data= subset(Travelers_data, Travelers_data$age_lenatres >=0)    #Subsetting Based on a condition#

#Travelers_data= Travelers_data[-c(1:39),]            # REMOVING THE ROWS #

#Now deleting age_lenatres and id as it is of no use now    # REMOVING THE COLUMN #
Travelers_data=Travelers_data[, -c(20,1)]






#####      Focus on  DEPENDENT VARIABLE       #####


barchart(Travelers_data$cancel)

#Removing -1's in the dependent Variable (Dependent variable should be binary for Logistic)

Travelers_data= subset(Travelers_data, Travelers_data$cancel >=0)


### Making the dependent variable factor FOR LOGISTIC REGRESSION ###########################

Travelers_data$cancel=as.factor(as.character(Travelers_data$cancel))


#######Correlations among variables#######
D=data.frame(Travelers_data[,c("tenure","n.adults","n.children","ni.marital.status","premium","len.at.res","ni.age","year")])
cor(D)



######## Splitting the Data Set ######
ratio = sample(1:nrow(Travelers_data), size = 0.25*nrow(Travelers_data))
Test = Travelers_data[ratio,] #Test dataset 25% of total
Training = Travelers_data[-ratio,] #Train dataset 75% of total




###### MODEL 1 : LOGISTIC REGRESSION ############
detach(Travelers_data)
attach(Training)

Log_model=glm(cancel~tenure+claim.ind+ni.marital.status+sales.channel+len.at.res+credit+ni.age+total_burden, data = Training, family = "binomial")
summary(Log_model)

#Accuracy of log model on test data
predict_Log_test=predict(Log_model, type="response", newdata=Test)
table(Test$cancel,predict_Log_test>0.5)
(1337+85)/nrow(Test)

#Calculating c-stat on Test data
library(ROCR)
pred_input_test=prediction(predict_Log_test,Test$cancel)
AUC= performance(pred_input_test,"auc")
print(AUC@y.values)
#c-stat = 0.6889






###### MODEL 2 : DECISION TREES #########


library(rpart)
library(rpart.plot)

CART_model=rpart(Training$cancel~. , data=Training, method="class")
prp(CART_model)
summary(CART_model)


#Accuracy of CART model on test data

predict_CART_test=predict(CART_model,newdata=Test, type="class")
table(Test$cancel,predict_CART_test)
(1346+66)/nrow(Test)

#AUC: As the dependent variable is imbalanced, AUC should be evaluated instead of Accuracy   Calculating c-stat on Test data
pred_CART_test=predict(CART_model, newdata=Test)
pred_prob_Test_CART=pred_CART_test[, 2]

library(ROCR)

pred_input_test_CART=prediction(pred_prob_Test_CART,Test$cancel)
AUC= performance(pred_input_test_CART,"auc")
print(AUC@y.values)
# c-stat = 0.623




####### MODEL 3: XG Boost####

#Packages
install.packages("xgboost")
library(xgboost)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("Matrix")
library(Matrix)


install.packages("BBmisc")
library(BBmisc)
install.packages("mlr")

library(ParamHelpers)
library(ggplot2)
library(mlr)



#convert data frame to data table
library(data.table)

setDT(Training) 
setDT(Test)

class(Training)

#Up to this point, we dealt with basic data cleaning and data inconsistencies. To use xgboost package, keep these things in mind:
# 1.Convert the categorical variables into numeric using one hot encoding
# 2.For classification, if the dependent variable belongs to class factor, convert it to numeric


#using one hot encoding to convert the categorical variables into numeric 
labels = Training$cancel
ts_label = Test$cancel

new_tr = model.matrix(~.+0,data = Training[,-c("cancel"),with=F]) 
new_ts = model.matrix(~.+0,data = Test[,-c("cancel"),with=F])


#convert dependent factor to numeric 
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1


library(Matrix)
library(xgboost)
#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)



#default parameters
nc <- length(unique(labels))
xgb_params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

#Using the inbuilt xgb.cv function, let's calculate the best nround for this model. In addition, this function also returns CV error, which is an estimate of test error.
xgbcv <- xgb.cv( params = xgb_params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

#best_iteration=7

#we'll see our CV error:
min(xgbcv$test.error.mean)
#0.238

library(xgboost)
#first default - model training
xgb1 <- xgb.train (params = xgb_params, data = dtrain, nrounds = 7, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")

#model prediction
xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)


#confusion matrix
library(ggplot2)
library(caret)

#To find the Accuracy of XGBOOST
confusionMatrix (xgbpred, ts_label)



#Accuracy is 75.4%




#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)

install.packages("Ckmeans.1d.dp")

library(Ckmeans.1d.dp)
xgb.plot.importance (importance_matrix = mat[1:20]) 




########### MODEL 4: Bagging  ########


#load libraries 
library(data.table)
library(BBmisc)
library(mlr)

install.packages("h2o")
library(h2o)


setDT(Training)
setDT(Test)


#Being a binary classification problem, you are always advised to check if the data is imbalanced or not.
setDT(Training)[,.N/nrow(Training),cancel]
setDT(Test)[,.N/nrow(Test),cancel]

#Before we start model training, we should convert all character variables to factor
fact_col <- colnames(Training)[sapply(Training,is.character)]
for(i in fact_col)
  set(Training,j=i,value = factor(Training[[i]]))

for(i in fact_col)
  set(Test,j=i,value = factor(Test[[i]]))


#Let's start with modeling now. MLR package has its own function to convert data into a task, build learners, and optimize learning algorithms.

#create a task
traintask <- makeClassifTask(data = Training,target = "cancel") 
testtask <- makeClassifTask(data = Test,target = "cancel")

#create learner
bag <- makeLearner("classif.rpart",predict.type = "response")
bag.lrn <- makeBaggingWrapper(learner = bag,bw.iters = 100,bw.replace = TRUE)


#I've set up the bagging algorithm which will grow 100 trees on randomized samples of data with replacement. To check the performance, let's set up a validation strategy 

#set 5 fold cross validation
rdesc <- makeResampleDesc("CV",iters=5L)

#For faster computation, we'll use parallel computation backend.
library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())

r <- resample(learner = bag.lrn , task = traintask, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc) ,show.info = T)

#With 100 trees, bagging has returned an accuracy of 77%





##########  MODEL 5: RANDOM FOREST########

#make randomForest learner
rf.lrn <- makeLearner("classif.randomForest")
rf.lrn$par.vals <- list(ntree = 100L, importance=TRUE)
r <- resample(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)

#On this data set, random forest performs worse than bagging. Both used 100 trees and random forest returns an overall accuracy of 75.9 %



########### Tuning the RF model to increase the accuracy###############

getParamSet(rf.lrn)

#set parameter space
params <- makeParamSet(makeIntegerParam("mtry",lower = 2,upper = 10),makeIntegerParam("nodesize",lower = 10,upper = 50))

#set validation strategy
rdesc <- makeResampleDesc("CV",iters=5L)

#set optimization technique
ctrl <- makeTuneControlRandom(maxit = 5L)

#start tuning
tune <- tuneParams(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(acc), par.set = params, control = ctrl, show.info = T)

#We improved the accuracy of the model to 77.1%




