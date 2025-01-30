##############################
# Intro Vid
#Strings and Factors, etc
##############################


#strings
x = 'hello'
y = 'world'

#check class
class(x)

x1 = c("hello", "world")

#paste to combine vector of strings
paste(c("red","yellow"),"cat")

paste(c(1:4),"cat")

#use an argument to specify separater
paste(c(1:4), "cat", sep = ",")


## %s strings
## %f %e floating point numbers fixed of scientific format
## %d for integers

pow = 1:3
power_of_e = exp(pow)

sprintf("%s %d = %f", "Euler's constant to the power", pow, power_of_e)

#factors are used to store categorical variables and are stored as integers with levels
#use function factor

#categorical variable with two levels
x = c("male","female", "female")












