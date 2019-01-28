#Here comes my first data science project on R.
# Set the working directory.
setwd("C:/Users/shubham/Desktop/csv/ustrain")
library(data.table)   #loading data.table
# We should have data.frame also but data.table inherits from 
#data.frame. It offers fast and memory efficient: file reader and writer,
#aggregations, updates, equi, non-equi, rolling, range and interval joins, in a short and flexible syntax,
#for faster development.

train = fread("train.csv",na.strings = c(""," ","?","NA",NA))   # loading train data
setwd("C:/Users/shubham/Desktop/csv/ustest")
test =  fread("test.csv",na.strings = c(""," ","?","NA",NA))    # loading test data

#Have a look at the by using dim(gives dimensions that is no of and no of column),
#str(pop out results that will tell you about nature of variables),
# view(will open data)
dim(train); str (train); View(train) # 199523 rows and 41 columns
dim(test); str (test); View(test)    #99762 rows and 41 columns.

#Have a look at few rows of train & test
train[1:5]
test [1:5]

#check target variables
unique(train$income_level)
# [1] "-50000" "+50000"
unique(test$income_level)
# [1] "-50000"  "50000+."

# We can see that the values for income_level is different in train and test data so we will encode these variable as 0 and 1.
#encode target variables
train[,income_level := ifelse(income_level == "-50000",0,1)]
test[,income_level := ifelse(income_level == "-50000",0,1)]

round(prop.table(table(train$income_level))*100)
# From the result of table we can see the basic model(keep income_level=0) will give us 94% accuracy but as it is imbalanced data sheet we have focus on minority classes


#set column classes
factcols <- c(2:5,7,8:16,20:29,31:38,40,41)  #Specified columns in factcols are in character in data so will convert it into factor for the ease of modelling
numcols <- setdiff(1:40,factcols)  #setdiff select the columns which are in 1-40 but not in factcols.
train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]  # changing classes from character to factor.
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]

# For ease in analysis lets separate the categorical variables.
# Sebset categorial variables
cattrain = train[,factcols, with=FALSE]
cattest = test[,factcols,with=FALSE]

#subset numerical variables
numtrain = train[,numcols,with=FALSE]
numtest = test[,numcols,with=FALSE]

# We have formed subset train and test so to save memory we can delete train and test data.
rm(train,test) 

# Load libraries
library(ggplot2)
library(plotly)

#We will write empirical function so that we do not need to write the full code again and again.
empfr=  function(a){ggplot(data = numtrain, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
+  ggplotly()}
empfr(numtrain$age)  
# We can see that, the data set consist of people varying their age from 0 to 90 with declining frequency 

empfr(numtrain$wage_per_hour) 
# Right skewed graph

empfr(numtrain$num_person_Worked_employer)
numtrain[,income_level := cattrain$income_level]
# Similarly we can plot for other numerical variable.
# By above graph we get to know the the frequency of variable.Now we will plot these variable against dependent variable.
ggplot(data=numtrain,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))
# From the plot we can see that green dot falls mostly under the age group of 25-65.

#dodged empirical bar chart function.
ampbarfn = function(i){ggplot(cattrain,aes(x=i,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))}

ampbarfn(cattrain$class_of_worker)
# We can see from graph that private section have majority of people having income greater than 50000.
ampbarfn(cattrain$education)
ampbarfn(cattrain$tax_filer_status)
# Similarly we can plot other categorical variable also.Alternative way is to use two way table.
prop.table(table(cattrain$marital_status,cattrain$income_level),1)

# DATA MINING

#Lets check missing values first
table(is.na(numtrain))
table(is.na(numtest))
# We can see that numerical variable has no missing values. But if we had found missing values and we have to deal with missing values(if variable is important)else we can that variable.
# there are many ways of dealing with missng values 1. Filling with mean. 2.Filling with median 3.Using multiple imputation. 4 Using tree to predict missing values.

#now we will check the correlation between numerical variables using caret package, it provide us convenient way to filter out the variable with higher correlation.
library(caret)
relation =findCorrelation(x = cor(numtrain), cutoff = 0.7)  # setting threshold cutoff as 0.7
numtrain <- numtrain[,-relation,with=FALSE] #Removing varibale with correlation less than 0.7.
numtest[,weeks_worked_in_year := NULL] # weeks_worked_in_yearhas been removed from so we will remove it from test also.

#Now we will check missng values in categorical variables.
crelation = sapply(cattrain, function(x){sum(is.na(x))/length(x)})*100
crelation
# 4 variable have missing value close to 50%. High proportion of missing value will create problem so as of now we will delete variable which have more than 5% missing values.
# Lets check missng values in test data first.
ctrelation = sapply(cattest, function(x){sum(is.na(x)/length(x))}*100)
ctrelation
cattrain = subset(cattrain, select = crelation < 5 )
cattest = subset(cattest, select = ctrelation < 5)
# If we had one variable with missing values we must have dealt with it but imputing missing values on large data sets can be painstaking. So we will impute issing values with factor unknown.

#set NA as unknown - train data
#convert to characters
cattrain = cattrain[,names(cattrain) := lapply(.SD, as.character),.SDcols = names(cattrain)]
for (i in seq_along(cattrain)) set(cattrain, i=which(is.na(cattrain[[i]])), j=i, value="Unknown")
#convert back to factors
cattrain = cattrain[, names(cattrain) := lapply(.SD,factor), .SDcols = names(cattrain)]

#set NA as Unknown - test data
cattest = cattest[, (names(cattest)) := lapply(.SD, as.character), .SDcols = names(cattest)]
for (i in seq_along(cattest)) set(cattest, i=which(is.na(cattest[[i]])), j=i, value="Unknown")
#convert back to factors
cattest = cattest[, (names(cattest)) := lapply(.SD, factor), .SDcols = names(cattest)]

# DATA MANIPULATION

#We have seen that train data have many variable in which values have low frequency.Such values doesnt help or it may not ne in test data.So we will replace all such values with others.
#combine factor levels with less than 5% values
#train
 for(i in names(cattrain)){
  p = 5/100
  ld = names(which(prop.table(table(cattrain[[i]])) < p))
  levels(cattrain[[i]])[levels(cattrain[[i]]) %in% ld] = "Other"
 }

#test
 for(i in names(cattest)){
  p = 5/100
  ld = names(which(prop.table(table(cattest[[i]])) < p))
  levels(cattest[[i]])[levels(cattest[[i]]) %in% ld] = "Other"
}
#Let's check if there exists a mismatch between categorical levels in train and test data. 
library(mlr)
summarizeColumns(cattrain)[,"nlevs"]
summarizeColumns(cattest)[,"nlevs"]
#The parameter "nlevs" returns the unique number of factor from the given set of variables. We can see from the result, their is no mismatch in level in train and test.
numtrain[,.N,age][order(age)]
numtrain[,.N,wage_per_hour][order(-N)]
table(numtrain$income_level, numtrain$age)
# We can see from the values of table that binning age into three group 0-25, 26-65, 66-90 would be very efficient for further analysis.
#bin age variable 0-25, 26-65, 66-90
#train
numtrain[,age:= cut(x = age,breaks = c(0,25,65,90),include.lowest = TRUE,labels = c("young","adult","old"))]
#convert age into factor
numtrain[,age := factor(age)]
#test
numtest[,age:= cut(x = age,breaks = c(0,25,65,90),include.lowest = TRUE,labels = c("young","adult","old"))]
#convert age into factor
numtest[,age := factor(age)]

table(numtrain$wage_per_hour) # We can see that 188219 row have 0 wage per hour and rest 1.
#train
#so wq will bin this numeric variables with Zero and MoreThanZero
numtrain[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
# Similarly for capital_gains, capital_losses, dividend_from_stocks.
numtrain[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
numtrain[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
numtrain[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

#test
numtest[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
# Similarly for capital_gains, capital_losses, dividend_from_stocks.
numtest[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
numtest[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
numtest[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

# now we will remove dependent variable from numtrain, we had added it for visualization purpose.
numtrain[,income_level := NULL]

# MACHINE LEARNING

#combine data and make test & train files
ctrain = cbind(numtrain,cattrain)
ctest = cbind(numtest,cattest)

#remove unwanted files
rm(numtrain,numtest,cattrain,cattest) #save memory
library(mlr)

#create task for modeling and predicting
setkey(ctrain, verbose=getOption("datatable.verbose"), physical = TRUE)
train.task = makeClassifTask(data = ctrain,target = "income_level")
setkey(ctest, verbose=getOption("datatable.verbose"), physical = TRUE)
test.task = makeClassifTask(data=ctest,target = "income_level")


#remove zero variance features
train.task = removeConstantFeatures(train.task)
test.task = removeConstantFeatures(test.task)

#get variable importance chart
varimp = generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(varimp,feat.type.cols = TRUE)
# This chart provide information that major_occupation_mode would provide highest information to the model followed by other in decreasing order.
# Only one numerical variable is in this list.

# As this dataset is imbalanced so will use tenhniques such as oversampling, undersampling, smote.
#undersampling 
train.under = undersample(train.task,rate = 0.1) #keep only 10% of majority class
table(getTaskTargets(train.under))
# 0     1 
# 18714 12382

#oversampling
train.over = oversample(train.task,rate=15) #make minority class 15 times
table(getTaskTargets(train.over))
# 0      1 
# 187141 185730

# Smote( before using smote close all application, as our dataset is large so it require lot of memory and cpu)
train.smote = smote(train.task,rate = 10,nn = 3)

#we will start with naive-bayes algorithm
#naive Bayes
naivelearner = makeLearner("classif.naiveBayes",predict.type = "response")
naivelearner$par.vals = list(laplace = 1)

#10fold CV - stratified
folds = makeResampleDesc("CV",iters=10,stratify = TRUE)

#cross validation function, so that we dont need to write the code for all three
fun_cv = function(a){
  crv_val = resample(naivelearner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}
#Run cross-validation on all the four data 
fun_cv (train.task) 
fun_cv(train.under)
fun_cv(train.over)
fun_cv(train.smote)
# Now build model on smote data.
#train and predict using smote data
nBmodel = train(naivelearner, train.smote)
nBpredict = predict(nBmodel,test.task)

#evaluate
nBprediction = nBpredict$data$response
dCM = confusionMatrix(ctest$income_level,nBprediction)
dCM
# we will get the result of model (accuracy, sensitivity, specificity)
#train and predict using train.task
nBmodel1 = train(naivelearner, train.task)
nBpredict1 = predict(nBmodel1,test.task)

#evaluate
nBprediction1 = nBpredict1$data$response
dCM1 = confusionMatrix(ctest$income_level,nBprediction)
dCM1
 
# from the above result we can conclude that the model is doing better in predicting the majority class but disappoints at minority class.

#Let's use xgboost algorithm and try to improve our model.
# We'll do 5 fold cross validation and fnally, we'll build the model using the best tuned parameters.

#xgboost
set.seed(100)
xgblearner = makeLearner("classif.xgboost",predict.type = "response")
xgblearner$par.vals = list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 150,
  print.every.n = 50
)

#defining hyperparameters for tuning
xgps = makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)
#define search function
rancontrol = makeTuneControlRandom(maxit = 5L) #do 5 iterations
#5 fold cross validation
setcv = makeResampleDesc("CV",iters = 5L,stratify = TRUE)
#tune parameters
train.task = createDummyFeatures(train.task)
xgbtune = tuneParams(learner = xgblearner, task = train.task, resampling = setcv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xgps, control = rancontrol)
xgbnew = setHyperPars(learner = xgblearner, par.vals = xgbtune$x)
#train model
xgmodel = train(xgbnew, train.task)
# Predicting model on test data
predict.xg = predict(xgmodel, test.task)

#make prediction
xg_prediction = predict.xg$data$response

#make confusion matrix
xg_confused = confusionMatrix(ctest$income_level,xg_prediction)
Accuracy : 0.948
Sensitivity : 0.9574
Specificity : 0.6585

precision <- xg_confused$byClass['Pos Pred Value']
recall <- xg_confused$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure
#0.9726374 

# we can see from the result that xgboost has outperformed the naive bayes model.We have to think more to increase the further accuracy.

predict.xg$threshold
# [[1]] 0.5

# Due to imbalanced dataset the threshold dataset will always favour the majority class since the probabilities of minor class is very less.
# lets decrease the threshold value to 0.4 and see what happens.

#set threshold as 0.4
pred2 = setThreshold(predict.xgprob,0.4)
confusionMatrix(ctest$income_level,pred2$data$response)
# Sensitivity : 0.9315 
# Specificity : 0.7245

# With 0.4 threshold value model has done better job, lets further decrease this to 0.3.
pred3 = setThreshold(predict.xgprob,0.30)
confusionMatrix(ctest$income_level,pred3$data$response)
#Accuracy : 0.942 
# Sensitivity : 0.9512 
# Specificity : 0.7641
# it further increases the accuracy, on further decreasing it will decrease the accuracy. So upto now it is best model.