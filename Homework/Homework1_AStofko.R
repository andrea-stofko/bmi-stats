#####################################
#
#
#.  Problem Set 1
#.  Name: Andrea Stofko
#.  UNID: u6040357
#
#####################################



#Set Working Directory
setwd("/Users/andreastofko/Desktop/bmi-stats/BMI Stats")

#### just to make sure that the file is there let's see which files are in this directory
setwd("/Users/andreastofko/Desktop/bmi-stats/BMI Stats")
list.files() #Yes sample is in there


#### Let's import that data set to R
#### And check the first few lines with the function head()
KG_Samples = read.table("igsr_samples.tsv",sep = "\t", header = T)
head(KG_Samples)


#### there is a column in this dataframe that is comma separated, we can split this column<br>
#### into 5 different sections named from A to E using the separate function from the tidyverse package<br>
library(tidyverse)
head(KG_Samples$Data.collections,2)
KG_Samples = KG_Samples %>%
  separate(Data.collections,c("A","B","C","D","E"),",")

#### how many samples are there in total
nrow(KG_Samples)
# returns 2607


#### how is gender split, to see this we can use the table function, 
#alternately we can use the group_by function from the tidyverse package
table(KG_Samples$Sex)
KG_Samples %>% group_by(Sex) %>% tally()

#### We can also make our queries a little more complex, for example how many males are in the GBR population?
table(KG_Samples$Population.code,KG_Samples$Sex)

#### Or with the tidyverse package
KG_Samples %>%
  filter(Population.code == "GBR") %>%
  group_by(Sex) %>%
  tally()


#### Lets randomly add ages to each sample, there is an important function sample() that randmly draws numbers from the uniform 
#distribution, the first argument is the range to sample (in this case numbers from 0 to 100), the second is how many times we 
#want to sample, there is a third argument which is replace which by default is false, in our example we want to change that to TRUE so we can have duplicated ages<br>
sample(c((0:100),1))

#### Now we can populate the new Ages column in our dataframe, to do this we create a new column Age, and do the sample 
#command, see how the size argument is going to be as long as the number of rows that we have in the dataset, this way 
#we can fill the Age column with different Ages.<br>
KG_Samples$Age = sample(c(0:100),size = nrow(KG_Samples),replace = TRUE)
head(KG_Samples,2)

#### Hmmmm we want to have the Age column right next to Sex, so let's ret's reorganize this table by changing the order of the columns using their indeces or the column names and at the same time let's get rid of those columns that have duplicated data (columns A,B,C,D,E, which are 9 to 13)<br>
KG_Samples = KG_Samples[,c(1,2,14,3:8)]
head(KG_Samples,2)


#### Let's remove the Biosample.ID columnm we can use the colnames() function to check the column names.
#### To remove the column we can use the sample form as in the code before with the indices, or we can use the subset() function and select the column we want to remove<br>
colnames(KG_Samples)

KG_Samples = subset(KG_Samples, select = -c(Biosample.ID))

colnames(KG_Samples)


#### We can create summary statistics using the summarize() function. Let's calculate mean and standard deviation for the column Age<br>
summary_Age <- KG_Samples %>% 
  summarize(mean = mean(Age), std_dev = sd(Age))
summary_Age


#### How about a more complex summary how about summary of Age by Sex, we need to be carefull with the missing data or Age is not
#going to work the argument na.rm=TRUE will exclude the missing data (NAs) from the calculations, see that I am creating a new 
#Dataframe called summary_sex_Age with the summary data<br>
summary_sex_Age <- KG_Samples %>%
  group_by(Sex) %>%
  summarize(mean = mean(Age, na.rm = TRUE))
summary_sex_Age

#### I also want to count how many individuals are on each population (function summarize()) and sort the values from the largest to smallest (function arrange() with the argument desc() to sort from largest to smallest)<br>

KG_Samples %>% 
  group_by(Population.name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))


#### Another way to create a new variable is with the mutate() function from the tidyverse package in this case we are going to convert the Age in years to Age in months 
KG_Samples <- KG_Samples %>%
  mutate(Age_Months = Age * 52)

##ggplot2 is a great package included in the tidyverse group, let's use it to create a histogram of the Age distribution

ggplot(data = KG_Samples, mapping = aes(x=Age)) +
  geom_histogram(color = "white", bins = 25) +
  theme_classic()

  #### The last command for this guide is an important function if_else() that allows us to create variables baased on conditions<br> In this case let's create a new variable called Age_EUR that will give a 1 if and individual is less than 50 years old <strong>AND</strong> of European Ancestry and a 0 if it is not

KG_Samples = KG_Samples %>% 
  mutate(Age_EUR = if_else(condition = Age <50 & Superpopulation.code=="EUR",true = 1,false = 0))
head(KG_Samples,2)


# 2. Problem Set R

### Now is your turn to practice!!

#### We are using a new Dataset, This is a frequently used dataset for multiple applications in statistics and machine learning. This dataset is deposited in a website that is pretty useful in bioinformatics to look for tutorials, datasets and advice Kaggle (https://www.kaggle.com)

#### Go to https://www.kaggle.com/saurabh00007/diabetescsv and download the csv file
setwd("/Users/andreastofko/Desktop/bmi-stats/BMI Stats")

#### This dataset consists on clinical variables for 768 patients to evaluate a few variables to predict whether a patient has diabetes.

#### Please write the R code necessary to run the next items:

#### 1. Load the dataset and show the first 5 lines<br>

diabetes <-read.csv("diabetes.csv", header = TRUE)
head(diabetes,5)

#### 2. How many patients have diabetes?<br>

table(diabetes$Outcome) #using table

diabetes %>%
  filter(Outcome ==1) %>% #using filter
  tally()
#268 patients have diabetes


#### 3. How many patients have diabetes that are older than 45?
diabetes %>%
  filter(Outcome ==1) %>%
  filter(Age > 45) %>%
  tally()
#58 patients are over 45 and have diabetes

#### 4. What is the mean and variance of glucose levels for individuals without diabetes<br>

summary_glucose_0 <- diabetes %>%
  filter(Outcome ==0) %>%
  summarize(mean = mean(Glucose, na.rm = TRUE), var = var(Glucose, na.rm = TRUE))
summary_glucose_0

#    mean      var
#1 109.98 683.3623

#### 5. Create a new discrete variable that has 1 if the individual has diabetes and high blood pressure (above 100), 
#2 if an indivual has diabetes and low blood pressure and 3 if the individual does not have diabetes.<br>

diabetes = diabetes %>%
  mutate(dmHT = if_else(condition = Outcome ==1 & BloodPressure > 100, 1,
         if_else(condition = Outcome ==1 & BloodPressure <= 100, 2,3)))

head(diabetes, 2)

# Pregnancies Glucose BloodPressure SkinThickness Insulin  BMI DiabetesPedigreeFunction Age Outcome dmHT
#1           6     148            72            35       0 33.6                    0.627  50       1    2
#2           1      85            66            29       0 26.6                    0.351  31       0    3


#### 6. Construct two plots of the distribution of BMI for individuals with diabetes and without diabetes<br><br>
diabetes$Outcome = as.factor(diabetes$Outcome)

#density plot with BMI distributions
diabetes %>%
  ggplot(aes(x=BMI, fill = Outcome)) + #expecting two outcomes
  geom_density() + 
  theme_bw()+
  ggtitle("BMI Distributions") +
  facet_wrap(~Outcome)

#boxplot alternative
diabetes %>%
  ggplot(aes(x=Outcome, y = BMI, fill = Outcome)) + #expecting two outcomes
  geom_boxplot() + 
  ggtitle("BMI Distributions - Boxplot") 


