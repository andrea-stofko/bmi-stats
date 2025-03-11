###Inferential Statistics

##The CLT states that the sampling distribution of a sample mean becomes approximately normal 
##if the sample size is large enough, 
##even if the population distribution is not normal.

##Example

###Suppose we’re studying the width of turtle shells. 
##The width follows a uniform distribution with a minimum of 2 inches and a maximum of 6 inches. 
##We want to explore how the sample means behave as we take repeated random samples 
##from this population.

# Set seed for reproducibility
set.seed(0)

# Create random variable with sample size of 1000 (uniformly distributed)
data <- runif(n = 1000, min = 2, max = 6)

# Create histogram to visualize the distribution of turtle shell widths
hist(data, col = 'steelblue', main = 'Histogram of Turtle Shell Widths')

data_mean <- mean(data)
data_sd <- sd(data)
##The resulting histogram shows that the distribution of turtle shell widths 
##is not normally distributed at all.


##Sampling Distribution of Sample Means: 
##Now, let’s take repeated random samples of 5 turtles 
##from this population and measure the sample mean over and over again:

# Create an empty vector to hold sample means
sample5 <- numeric()

# Take 1,000 random samples of size n = 5
for (i in 1:1000) {
  sample5[i] <- mean(sample(data, 5, replace = TRUE))
}

# Calculate mean and standard deviation of sample means
sample5_mean <- mean(sample5)
sample5_sd <- sd(sample5)

sample5_mean - data_mean
sample5_sd - data_sd

# Create histogram to visualize the sampling distribution of sample means
hist(sample5, col = 'steelblue', xlab = 'Turtle Shell Width', main = 'Sample Size = 5')

##Notice that the sampling distribution of sample means appears normally distributed, even though the original distribution of turtle shell widths was not normal. 
##The sample mean and sample standard deviation for this sampling distribution are approximately:

##Let's increase the sample size from 5 to 30

# Create an empty vector to hold sample means
sample30 <- numeric()

# Take 1,000 random samples of size n = 30
for (i in 1:1000) {
  sample30[i] <- mean(sample(data, 30, replace = TRUE))
}

# Calculate mean and standard deviation of sample means
sample30_mean <- mean(sample30)##3.99713
sample30_sd <- sd(sample30)##0.2146805

sample30_mean - data_mean
sample30_sd - data_sd

# Create histogram to visualize the sampling distribution of sample means
hist(sample30, col = 'steelblue', xlab = 'Turtle Shell Width', main = 'Sample Size = 30')

##the Central Limit Theorem allows us to rely on the normality of the sampling distribution of sample means, even when our original data is not normally distributed. 
##Increasing the sample size further improves the approximation to normality.


###Z-score

# We collected data from a sample, and we found that the age distribution had a mean of 49.3 
# and a variance of 260 (both sides of the curve), standard deviation is sqrt(260) = 12.1245. 
# What is the z-score (standard score of an individual of age 53?
# 
# X ~ N(49.3, 260)

AgeZ = (53-49.3) / (16.1245)

#= 0.2294

#An individual of 53 years of age is approximatelly 0.23 standard deviations above the mean

# Recall that the area under the curve up to the Z value represents the probability of obtaining 
# that Z value or less, thus for a z = 0.2294, what is the exact area under the curve? 
# looking at the curves below we can say that the probability is at least less than 0.6827, 
# but to get to the exact value we need to look at the z table.

pnorm(53,49.3,16.12)

# which means that about 59% of the ages are below this subject's.

##Properties of point estimators

# Bias: Measures how far, on average, the point estimate is from the true parameter.
# Variance: Reflects the variability of point estimates across different samples.
# Mean Square Error (MSE): Combines bias and variance to assess overall accuracy.


##We calculate point estimates to reduce bias and increase efficiency
##Bias and Efficiency

#Bias:
#Definition: Bias refers to the systematic deviation of a point estimate from the true population parameter.
#Example:
#Imagine we want to estimate the average blood pressure (BP) of patients in a clinical trial.
#We collect a random sample of patients and calculate the sample mean BP.
#If our sample is biased (e.g., we only include healthy patients), our estimate may be far from the true average BP.

actual <- c(120, 130, 140, 150, 160)  # True BP values
predicted <- c(125, 135, 145, 155, 165)  # Estimated BP values
bias_value <- mean(actual - predicted)
print(paste("Bias:", bias_value))

###Efficiency:
#Definition: Efficiency refers to how well an estimator utilizes the available information.
#Efficient Estimator: An estimator with minimal variance among unbiased estimators.
#Example:
#Consider estimating the variance of a drug’s effect on blood pressure.
#Two estimators are available: the sample variance and an alternative unbiased estimator.
#The sample variance is efficient because it minimizes variance while remaining unbiased.

# Simulated data
bp_effects <- rnorm(100, mean = 5, sd = 2)  # True effects
sample_var <- var(bp_effects)  # Sample variance
unbiased_var <- sum((bp_effects - mean(bp_effects))^2) / (length(bp_effects) - 1)

# Compare efficiency
if (sample_var < unbiased_var) {
  print("Sample variance is more efficient.")
} else {
  print("Unbiased estimator is more efficient.")
}

##Remember, bias and efficiency impact the quality of our estimates.

###Sampling Distribution

##Random Sampling
##Stratified Sampling

#Stratified sampling is a method where researchers divide a population into homogeneous subpopulations 
#called strata based on specific characteristics (such as race, gender identity, location, etc.). 
#Each member of the population belongs to exactly one stratum. 

#Researchers then sample from each stratum using another probability sampling method 
#(e.g., cluster sampling or simple random sampling). 
#This approach ensures that every characteristic is properly represented in the sample, 
#improving generalizability and validity while avoiding biases like undercoverage bias

##Here are the steps for implementing stratified sampling:
#1.Define Your Population and Subgroups:
#   Clearly define your population and identify mutually exclusive and exhaustive subgroups (strata).
# Each individual should belong to exactly one subgroup.

#2.Separate the Population into Strata:

# Create separate strata based on the chosen characteristics 
#(e.g., age groups, disease severity levels, etc.).

##3. Decide on Sample Size for Each Stratum:
#  Determine the desired sample size for each stratum.
#  Consider factors like the variability within each subgroup and the overall population.

#4. Randomly Sample from Each Stratum:
#  Use appropriate sampling methods (e.g., simple random sampling within each stratum) 
#to select participants.

##Example 1: Stratified Sampling Using Number of Rows
# Generate sample data frame of 400 students
set.seed(1)
df <- data.frame(
  grade = rep(c('Freshman', 'Sophomore', 'Junior', 'Senior'), each = 100),
  gpa = rnorm(400, mean = 85, sd = 3)
)

# Obtain stratified sample (10 students from each grade)
library(dplyr)
strat_sample <- df %>%
  group_by(grade) %>%
  sample_n(size = 10)

# Check frequency of students from each grade
table(strat_sample$grade)

#Example 2: Stratified Sampling Using Fraction of Rows

# Obtain stratified sample (15% of students from each grade)
strat_sample <- df %>%
  group_by(grade) %>%
  sample_frac(size = 0.15)

# Check frequency of students from each grade
table(strat_sample$grade)


##Clustering Sampling

##Cluster sampling is a probability-based sampling method where researchers divide a population 
##into clusters (smaller groups)
#and then randomly select some of these clusters as the sample.

#1. Define Your Population:
#2. Divide Your Sample into Clusters:
  #Ideally, each cluster should be a mini-representation of the entire population.
#3.Randomly Select Clusters:
#  Randomly choose clusters from your defined population.
#  Each selected cluster will represent a subset of the population.
#4. Sample Within Each Cluster:

##example
##Suppose we have a data frame df representing customer experiences on city tours. 
##Each row corresponds to a customer, 
##and the columns include the tour number (tour) and their experience rating (experience).

# Make this example reproducible
set.seed(1)

# Create data frame
df <- data.frame(
  tour = rep(1:10, each = 20),
  experience = rnorm(200, mean = 7, sd = 1)
)

# View the first six rows of the data frame
head(df)

##Randomly Select Clusters:

# Randomly choose 4 tour groups out of the 10 -  
# Let’s randomly choose four tour groups (clusters) out of the ten available tours:

clusters <- sample(unique(df$tour), size = 4, replace = FALSE)

#Create the Cluster Sample:

# Obtain the cluster sample
cluster_sample <- df[df$tour %in% clusters, ]

# Check how many customers came from each tour
table(cluster_sample$tour)

##Standard Error of the mean

##The standard error of the mean (SEM) quantifies how much the sample mean is likely to vary 
#from the true population mean. 
#It helps us understand the precision of our sample estimate.

# Generate some example data (turtle shell widths)
set.seed(123)
shell_widths <- runif(100, min = 2, max = 6)

# Calculate sample mean and standard deviation
sample_mean <- mean(shell_widths)
sample_sd <- sd(shell_widths)

# Calculate SEM
n <- length(shell_widths)
sem <- sample_sd / sqrt(n)

# Print results
cat("Sample Mean (x̄):", round(sample_mean, 3), "\n")
cat("Sample Standard Deviation (SD):", round(sample_sd, 3), "\n")
cat("Sample Size (n):", n, "\n")
cat("Standard Error of the Mean (SEM):", round(sem, 3), "\n")


##Example 2

library(mosaic)
time = c(190.5, 109, 95.5, 137)
resample(time)
mean(time)
sd(time)

##let's generate multiple resamplings of the whole data (bootstraps)

##Generate 10 resamples and get the mean for each
bootstrap = do(10) * mean(resample(time))
head(bootstrap)
sd_B = sd(bootstrap$mean)
Standard_Error_Mean = sd_B/sqrt(10)
Standard_Error_Mean

densityplot(~mean, data=bootstrap)

##How about 100

bootstrap = do(100) * mean(resample(time))
head(bootstrap)
sd_B = sd(bootstrap$mean)
Standard_Error_Mean = sd_B/sqrt(100)
Standard_Error_Mean

densityplot(~mean, data=bootstrap)

##10000

bootstrap = do(10000) * mean(resample(time))
head(bootstrap)
sd_B = sd(bootstrap$mean)
Standard_Error_Mean = sd_B/sqrt(10000)
Standard_Error_Mean

densityplot(~mean, data=bootstrap,bw = 5)


##Confidence Intervals

#install.packages("mosaic")
library(mosaic)

trellis.par.set(theme=col.mosaic())
options(digits=3)

###
mu = 500
sigma = 100
x = rnorm(500, mean=mu, sd=sigma)

plot(density(x))

##Estimating confidence intervals

meanconfint = function (x, sigma, level = 0.95, ...) {
  se = sigma / sqrt(length(x))
  mu = mean(x)
  z = qnorm(1 - (1 - level)/2)
  out = c(mu, mu - z * se, mu + z * se)
  names(out) = c("mean", "lower", "upper")
  return(out)
}

meanconfint(x, sigma = sigma)

z_critical <- qnorm(0.975)
z_critical

###do function from the mosaic package, repear something many times
randomx = do(50) * rnorm(500, mean=mu, sd=sigma)

ci = data.frame(t(apply(randomx, 1, meanconfint, sigma=sigma)))
head(ci)


###plot confidence intervals -- sometimes the 
### simulation it doesn't cover the actual mean
xyplot(1:nrow(ci) ~ mean, data=ci, xlim=range(ci), xlab="SAT score", ylab="Index")

# ladd(panel.abline(v=500, col="lightgray", lty=2))
# 
# ladd(with(ci, panel.arrows(x0 = lower, y0=1:nrow(ci), y1=1:nrow(ci), cex=0.5,
#                              x1=upper, code=3)))

##Hypothesis Testing

###Is the coin biased?? Hypothesis testing
library(mosaic)
n_tosses = 1000

lower_bound = qbinom(0.025, n_tosses, 0.5)
upper_bound = qbinom(0.975, n_tosses, 0.5)


lower_bound
upper_bound

coin_flip = function(){
  ifelse(runif(1)>0.51,1,0)
}

coin_flip_biased = function(){
  rbinom(1,1,0.7)
}

observed_head_count = sum(do(n_tosses) * coin_flip())


head(observed_head_count)
print(paste(lower_bound,upper_bound,sep = " "))
if (observed_head_count >= lower_bound & observed_head_count <= upper_bound){
  print("Failed to reject the null!")
}else{
  print("Null rejected!")
}

observed_head_count = sum(do(n_tosses) * coin_flip_biased())

head(observed_head_count)
print(paste(lower_bound,upper_bound,sep = " "))
if (observed_head_count >= lower_bound & observed_head_count <= upper_bound){
  print("Failed to reject the null!")
}else{
  print("Null rejected!")
}

###Please read all contents from chapter 6 from this book

##https://pressbooks.lib.vt.edu/introstatistics/chapter/introduction-19/