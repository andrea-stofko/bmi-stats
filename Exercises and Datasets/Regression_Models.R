##Regression Models
library(MASS)
library(corrplot)
library(RColorBrewer)
library(faraway)

############################
##1. Correlation


##Correlation does not imply causation

##Let's use a dataset created by Henderson that tries to demonstrate the 
##argument that global warming is a direct effect of the shrinking number of pirates since the 1800s

Pirates = data.frame(
                   check.names = FALSE,
                          Year = c(1820L,
                                   1860L,1880L,1920L,1940L,1980L,2000L),
                   global_average_temperature = c(14.25,
                                   14.35,14.75,14.85,15.25,15.65,15.95),
                   number_of_pirates = c(45000L,
                                   35000L,20000L,15000L,5000L,400L,17L)
)

##Scatterplot of the relationship
plot(Pirates$global_average_temperature,Pirates$number_of_pirates)

##To calculate the correlation we can use the function cor
cor(Pirates$global_average_temperature,Pirates$number_of_pirates)

##To evaluate the significance of the correlation we use the t-test
##What is the Null hypoyhesis
cor.test(Pirates$global_average_temperature,Pirates$number_of_pirates)

##Clearly, the answer to lowering the global temperature is to promote piracy, right?
  

############################
##Simple Linear Regression

##set your directory to import the dataset
setwd("/Users/javier/Documents/Jupyter/MBIO_6490_2023/R_Sessions/Week_4")

##In this Dataset, Data collectors set up a laser sensor, 
##with breaks in the laser beam recording when a rail-trail user passed the data collection station.

?RailTrail
RailTrail = read.csv("RailTrail.csv")


##Let's look at the relationship between high temperature and Volume
plot(RailTrail$hightemp,RailTrail$volume)

##Let's run a linear model to see the effect of change in temperature with the amount of people that
##crossed the sensor.

mod <- lm(volume ~ hightemp, data = RailTrail) 
coef(mod) ##Extract the coefficients
summary(mod)

###Conclusions
##1. In the coldest days, when the high temperature was 0 Fahrenheit, 
##the mean rider frequency is -17 (this is not possible). 
##Probably at these extreme temperatures the monitoring equipment was not working very well.

##2. This is the most important extrapolation from the linear model which is that 
##for each increase in 1 degree Fahrenheit, we increase about 5.7 riders a day.

##Add the line model to the scatterplot
plot(RailTrail$hightemp,RailTrail$volume)
abline(mod, col = "red", lty = 4, lwd = 3)

##Let's look at the residual plot, using the linear model (mod)
plot(mod)

##How about categorical variables
table(RailTrail$weekday)

mod2 = lm(volume ~ weekday, data = RailTrail)
summary(mod2)
##The above result can be interpreted as we expect 80 less riders on weekdays 
#(which was 1 in our variable) than on the weekend.


##Predictions

dftoPred = data.frame(hightemp=c(62,75,84,92,101))
predict(mod, newdata=dftoPred)

############################
#### Multiple Linear Regression

?UScrime
#Criminologists are interested in the effect of punishment regimes on crime rates.

#M:percentage of males aged 14–24.
#So:indicator variable for a Southern state.
#Ed:mean years of schooling.
#Po1:police expenditure in 1960.
#Po2:police expenditure in 1959.
#LF:labour force participation rate.
#M.F:number of males per 1000 females.
#Pop:state population.
#NW:number of non-whites per 1000 people.
#U1:unemployment rate of urban males 14–24.
#U2:unemployment rate of urban males 35–39.
#GDP:gross domestic product per head.
#Ineq:income inequality.
#Prob:probability of imprisonment.
#Time:average time served in state prisons.
##y:rate of crimes in a particular category per head of population.

UScrime = read.csv("UScrime.csv")

round(cor(UScrime),2)

##Correlation plot -- from the library corrplot

M <-cor(UScrime)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


##Let's run a lm with all of the variables
crime_rate_lm = lm(y ~ .,data = UScrime)

summary(crime_rate_lm)

plot(crime_rate_lm)
###Colinearity
cor(UScrime$Po1,UScrime$Po2)
cor(UScrime$U1,UScrime$U1)

library(faraway)

uscrimewor = UScrime[,-16]##Let's use the dataset without the dependent variable

###A good metric to evaluate collinearity is the VIF, Variance Inflation Factors VIFj = 1/ 1-R2j
###This function comes from the library faraway

##A large VIF on an independent variable indicates a highly collinear relationship 
##to the other variables that should be considered or adjusted for in the structure of the 
##model and selection of independent variables.

vif(uscrimewor)

##We see that Po1 and 2 have high values (as a rule of thumb above 10 needs to be taking into consideration)
##Let's remove Po2, U2, and GPD in the new linear model
crime_rate_lm2 = lm(y ~ M+So+Ed+Po1+LF+M.F+Pop+NW+U1+Ineq+Prob+Time,data = UScrime)

summary(crime_rate_lm2)
##Compare adjusted R2
summary(crime_rate_lm)[9]
summary(crime_rate_lm2)[9]

##Compare both models
anova(crime_rate_lm,crime_rate_lm2)

##Evaluate Residuals
plot(crime_rate_lm2)
##residual vs. fitted plot: i'ts helpful for visually detecting heteroscedasticity 
##Q-Q plot: it's useful for determining if the residuals follow a normal distribution.

##Get the residuals
res = resid(crime_rate_lm2)
#Create density plot of residuals
plot(density(res))

vif(uscrimewor[,-c(5,11,12)]) ##No VIF above 10

############################
###Interactions

##We can evaluate the interactions between 

crime_rate_lm3 = lm(y ~ M+So+Ed+Po1+LF+M.F+Pop+NW+U1+Ineq+Prob+Time+Prob*Time,data = UScrime)
summary(crime_rate_lm3)


#β0 is the predicted crime rate when all of the independent the variables equal zero:
##β1 = M = 3.22If we set all of the variables except for percentage of males aged 14–24 = 0 
#then β1 will be the increase in crime rates per unit increase in percentage of males
#aged 14-24 when all of the other variables is 0

##BProbXtime will change the slope at each value of prob and time due to the interaction term



##How about the residuals?
plot(crime_rate_lm3)

############################
##Logistic Regression

##This is survey data collected by the US National Center for Health Statistics (NCHS)
##We want to evaluate variables related to diabetes
library(NHANES)
?NHANES
NHANES = read.csv("NHANES.csv")

##Frequency of the dependent variable
table(NHANES$Diabetes)

##We want to convert to a numeric variable
NHANES <- NHANES %>% mutate(has_diabetes = as.numeric(Diabetes == "Yes"))

##
table(NHANES$has_diabetes)

##Let's plot the data and add a smoothed curve for the logistic model using age as predictor
logistic_plot <- ggplot(data = NHANES, aes(x = Age, y = has_diabetes)) + 
  geom_jitter(alpha = 0.1, height = 0.05) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) + 
  ylab("Diabetes status")
logistic_plot = logistic_plot + xlab("Age (in years)")

logistic_plot

##Let's run the model with BMI and Age
logistic_reg2 <- glm(has_diabetes ~ BMI + Age, family = "binomial", data = NHANES) 
summary(logistic_reg2)

##The coefficient for AGE= 0.057278 which is interpreted as the expected change in log odds 
##for a one-unit increase in age. The odds ratio can be calculated by exponentiating this value 
##to get 1.05895 which means we expect to see about 6% increase in the odds of being diabetic, 
##for a one-unit increase in age score

odd = exp(0.057278)

##Which variable is more important?

ages <- range(~Age, data = NHANES) ##range of ages
bmis <- range(~BMI, data = NHANES, na.rm = TRUE) ##range of BMI
res <- 100 

##Expand the range to 100 rows
fake_grid <- expand.grid(
  Age = seq(from = ages[1], to = ages[2], length.out = res), 
  BMI = seq(from = bmis[1], to = bmis[2], length.out = res)
) 

##Get the probability of diabetes using the model logreg
y_hats <- fake_grid %>%
  mutate(y_hat = predict(logistic_reg2, newdata = ., type = "response"))

head(y_hats) ##The y_hat variable are log_odds

###Let's plot the grid

ggplot(data = NHANES, aes(x = Age, y = BMI)) +  ##Scatterplot Age v BMI
  geom_tile(data = y_hats, aes(fill = y_hat), color = NA) + ###Add predictions using tiles
  geom_count(aes(color = as.factor(has_diabetes)), alpha = 0.4) + ###Count the number of obs at each location and create two colors 0 or 1
  scale_fill_gradient(low = "white", high = "dodgerblue") + ##Change gradient color range
  scale_color_manual("Diabetes", values = c("gray", "gold")) +##Change the color if individual has diabetes
  scale_size(range = c(0, 4)) ##

############################
##Dummy Variables

##Two hundred observations were randomly sampled from the High School and Beyond survey, 
##a survey conducted on high school seniors by the National Center of Education Statistics.

# id:Student ID.
# gender:Student's gender, with levels female and male.
# race:Student's race, with levels african american, asian, hispanic, and white.
# ses:Socio economic status of student's family, with levels low, middle, and high.
# schtyp:Type of school, with levels public and private.
# prog:Type of program, with levels general, academic, and vocational.
# read:Standardized reading score.
# write:Standardized writing score.
# math:Standardized math score.
# science:Standardized science score.
# socst:Standardized social studies score.

##This dataset evaluate predictor that are believe to influence student scores
Student_scores = read.csv("Student_scores.csv")

##Let's looks at the effect of race on writing scores
fit <- lm(write ~ as.factor(race), data=Student_scores)
summary(fit)

##
fit2 <- lm(write ~ schtyp, data=Student_scores)
summary(fit2)

##
fit3 <- lm(write ~ as.factor(race)+as.factor(ses), data=Student_scores)
summary(fit3)
