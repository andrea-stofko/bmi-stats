####January 15#####
####R Session Probability####
install.packages("Amelia")
install.packages("psych")
library("tidyverse")
library("Amelia")
library("mice")
library("caret")
library("psych")
library("star")
install.packages("gtsummary")
library("gtsummary")

###1. We are going to use our use of R and probabilities to be able to determine which features help 
##to separate diabetes patients

###First load the dataset

###Where is your dataset located? you can either import it from where it is located or set your working directory.

setwd("/Users/andreastofko/Desktop/bmi-stats/BMI Stats/Exercises and Datasets")

#check working directory
getwd() #looks good
#/Users/andreastofko/Desktop/bmi-stats/BMI Stats/Exercises and Datasets

##load csv (can call by document name once working directory is set)
diabetes = read.csv("diabetes.csv", header = TRUE)

head(diabetes)head(diabetes)head(diabetes)

#check datatype
class(diabetes) #data.frame

gtsummary::all_stat_cols(diabetes)

#Pregnancies: To express the Number of pregnancies
sum(diabetes$Pregnancies)
#7407

#Glucose: To express the Glucose level in blood

#BloodPressure: To express the Blood pressure measurement

#SkinThickness: To express the thickness of the skin

#Insulin: To express the Insulin level in blood

#BMI: To express the Body mass index

#DiabetesPedigreeFunction: To express the Diabetes percentage

#Age: To express the age

#Outcome: To express the final result 1 is Yes and 0 is No

###
###How many observations are there?
# There are 2,000 observations

###How many features??


##Which are your dependent and independent variables?


##How many patients have diabetes?


##How many diabetes patients have a high BMI > 40



#install.packages("Amelia",dependencies = T)

library(Amelia)
library(tidyverse)
library(psych)


###Let's look at the different features and what data type they belong to
str(diabetes)

#pregnancy is a discrete variable
table(diabetes$Pregnancies)

#plot distribution of glucose in sample
plot(density(diabetes$Glucose))

###Many packages give nice data summaries that are very useful while evaluating your data
psych::describe(diabetes)

?describe

##Frequency Distribution of the outcome
table(diabetes$Outcome)
table(diabetes$Glucose) #glucose is also a cont. variable

##
##Let's look at some of our features  distributions

hist(diabetes$Pregnancies) #hist is useful for discrete variables

?gather

##Facet plot of densities for features 
diabetes %>% #call dataframe you want to use
  select(Pregnancies, Glucose, BloodPressure, SkinThickness, Insulin, BMI,Age) %>% #select variables
  gather(metric, value) %>% #gather metric
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

##Let's compare the distribution of Insulin on the different outcomes
diabetes %>%
  ggplot(aes(x=Outcome,y=Insulin, fill = Outcome)) + #expecting two outcomes
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

##Convert the outcome variable to a factor variable to split the variable into two discrete values.
diabetes$Outcome = as.factor(diabetes$Outcome)

#rerun with outcome as factor
diabetes %>%
  ggplot(aes(x=Outcome,y=Insulin, fill = Outcome)) + #expecting two outcomes
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

#find range
range(diabetes$Age)

#visualize the missing data using a function from the Amelia package
missmap(diabetes)

#Convert '0' values into NA
diabetes[,2:7][diabetes[, 2:7] == 0] <- NA
head(diabetes,20)##Check the first 20 lines of the dataframe

##Get means of a column
mean(diabetes$Glucose)
mean(diabetes$Glucose,na.rm=TRUE)
sd = sd(diabetes$Glucose,na.rm=TRUE) #save this to an object called SD

#define standard error of mean function
std.error <- function(x) {
  sd = sd(x,na.rm=TRUE) #calc sd of object, remove NAs
  size = length(x[!is.na(x)]) #calc size of object
  serr = sd/sqrt(size) #standard error calc
  return(serr) #only return standard error
}


#calculate standard error of the mean 
std.error(diabetes$Glucose)

#run function on each column 
apply(diabetes, 2, std.error) #2 denotes apply to columns
?apply

#visualize the missing data
missmap(diabetes)

##How about the density plots 

diabetes %>% 
  select(Pregnancies, Glucose, BloodPressure, SkinThickness, Insulin, BMI,Age) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")


#BMI>=40 and diabetes
nrow(diabetes[diabetes$Outcome==1 & diabetes$BMI>=40,]) ##146
###No diabetes and age < 30
##
nrow(diabetes[diabetes$Outcome==0 & diabetes$Age<30,]) ##834

###############The code below will guide you through the steps to run and visualize a naive bayes classifier
#machine learning classifier

library(mice) #used to impute missing data - use the mean of distribution for NA
?mice
library(caret)

###We need to impute missing values to run naive bayes
##can use multiple approaches (k-mers, nearest neighbors)
mice_mod <- mice(diabetes[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf') #these are the variables with missing data
?mice
mice_complete <- complete(mice_mod)
?complete
##Let's transfer the imputed columns to the dataset
diabetes$Glucose = mice_complete$Glucose
diabetes$BloodPressure = mice_complete$BloodPressure
diabetes$SkinThickness = mice_complete$SkinThickness
diabetes$BMI = mice_complete$BMI
diabetes$Insulin = mice_complete$Insulin

##Any missing data
missmap(diabetes)

##Let's Build the naive bayes Classifier
#Building a model
#split data into training and test data sets you set the partition in this case we will do 75-25

set.seed(998)
indxTrain <- createDataPartition(y = diabetes$Outcome,p = 0.75,list = FALSE) #create a vector of the indeces of pts who are used for training data (75%)
training <- diabetes[indxTrain,] #subset on those = 1500
testing <- diabetes[-indxTrain,] #subset on the rest = 500

#Check dimensions of the split
prop.table(table(diabetes$Outcome)) * 100
prop.table(table(training$Outcome)) * 100
prop.table(table(testing$Outcome)) * 100
#proportions remain the same in the training and test data sets

#create objects x which holds the predictor variables and y 
#which holds the response variables
x = training[,-9] #x is all variables except outcome (right of comma includes all columns except 9)
y = training$Outcome #Y is a factor with two levels

#x has the 8 variables but not the outcome variable,
#y is the dependent variable

###Run the naive bayes algorithm on the training dataset using a resampling method, remember the goal is to maximize
##The class prediction.
model = train(x,y,'naive_bayes',trControl=trainControl(method='cv',number=10)) #10-fold cross validation

#Model Evaluation
#Predict using the testing set
Predict <- predict(model,newdata = testing )


#Get the confusion matrix to see accuracy value and other parameter values

confusionMatrix(Predict, testing$Outcome )


#Plot Variable performance
X <- varImp(model) #RF Variable Importance for Arbitrary Measures
plot(X)

###It appears that glucose concentration and age are the top two variables. Lets see a scatterplot of these two

testing$prediction = as.character(Predict)
testing$index = 1:nrow(testing)
testing$logic = ifelse((testing$Outcome==testing$prediction) & (testing$Outcome==0),0,
                       ifelse(testing$Outcome==testing$prediction & testing$Outcome==1,1,
                              ifelse(!(testing$Outcome==testing$prediction) & testing$Outcome==0,2,3)))

table(testing$logic)

ggplot(testing, aes(x=Age, y=Glucose,color = factor(logic))) + geom_point(size = 3)

##Check the mean of the factors for each classification 
##Remember 0 = TN no diabetes; 1 = TP diabetes; 2 = FP diabetes 3 = FN No diabetes

ggplot(testing, aes(x=factor(logic),y = Glucose,fill = factor(logic)))+
  geom_bar(stat = "summary",  fun = mean)

###Did the classifier struggle with these patients?

predict(model,newdata = testing,type = "prob")

##Plot ROC curve
library(pROC)
roc_ = roc(testing$Outcome,predict(model, newdata = testing, type ="prob")[,2])

plot(roc_,print.auc=T)

#diagnostic plot to evaluate how a classifier is working,
#area under the curve is the accuracy - plots sensitivity vs specificity(relates to true positives and false negatives)




