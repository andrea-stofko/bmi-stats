##########################################
#  Date: 1/8/24
#  Module 1: Intro Video Follow Along
#
##########################################

install.packages("abind")

#vectors, matrices, arrays

#vectors are the same date type and can be created using 'c' for combine
x=c(1,2,3,4,5,8,10)
y=c("a","b","c")
z=c(TRUE,FALSE, T,T,F)

#can also use the colon operator
x1 = 1:10

#how to extract values of vectors?
x[7] #10 is returned

#indexing can also take a subset
x2 = x[2:6]

#extract two elements, not in a range
x3 = x[c(2,6)] # (so need the function 'c')

#create a name vector using 'c' function
x = c(a=1, b=2, c=3)

#access first element:
x[1]
x['a']

#use names function on vector x
names(x)

#rename
names(x) = c("A","B","C") 

names(x)

#create numeric vector with built in constant
x = c(1,2,3,NA,5)

#create an empty vector with x = null
x = NULL
is.null(x)

#print all letters
print(LETTERS)

#use combine function when it's not a range
letters[c(4,7,10)] 

#month
month.abb
month.name  

#states
state.abb
?state.abb #more facts and figures


#matrix - 2D array of values
x = matrix(1:9,nrow = 3, ncol = 3) #create 3x3 matrix

x = matrix(1:9,nrow = 3, ncol = 3, byrow = TRUE) 

#use cbind to combine by columns
x = 1:3
y = 4:6
z= cbind(x,y)
z1 = rbind(x,y)

#subset matrix
z[1,2] #row 1, element 2

#find the dimension
dim(z) #3 rows, 2 columns

#create column names
colnames(z) = c("X","Y")
rownames(z) = c("a","b","c")

#subset with new names
rownames(z)[1] #a
colnames(z)[2] #Y
z[rownames(z)[1],colnames(z)[2]] #by position

#by name
z[rownames(z)=="a",colnames(z)[2]]
rownames(z)=="a"

#subset using just colnames and perform functions
mean(z[,colnames(z)=="X"])

#create matrix using range
let = matrix(letters[1:9], nrow = 3, ncol =3)


#arrays - multi-dimensional but one data type

#array function
x = array(1:8, dim = c(2,2,2))

#make an array by combining two matrices

#create matrix
x = matrix(1:4, nrow = 2, ncol = 2)
y = matrix(5:8, nrow = 2, ncol=2)
z = abind::abind(x,y,along=3)

z[1,1,2]

dim(z)

#lists
x = list(1, "a", TRUE, c(2,3,6))

#vectors have to be one data type
a = 1:3 
b= c("a", "b", "c", "d")

#create a list by combining vectors
y=list(a,b)

#extract the fourth element
x[[4]]
x[[4]][[2]]


#extract elements using $
x = list(a=1, b=2, c=c(1,2,3))
x$b

length(x) #list has three elements

#names
names(y) = c("first", "second")
y$first

#dataframes - 2D structure 
#similar to a matrix but can contain multiple data types

data.frame()

data1 = c("a", "b", "c")
data2 = c(T,T,F)
data3 = c(1,2,3)

data_Total = data.frame(data1, data2, data3)


#call columns in data set
BirdMalaria$bird


#class
class(BirdMalaria)
#data.frames

#load dataset
?mtcars
view(mtcars)

#how to index dataframes
mtcars[1,2] #first row of second column
mtcars[1,] #all values for that row

#sort from smallest to largest
sort(mtcars[,1], decreasing = T)

#create new variable
L = mtcars$am==0

mtcars[L,]$mpg

#create subset of dataframe
mtcars_mpg3 = subset(mtcars,mpg>20)

mtcars_mpg4 = subset(mtcars, mpg>20 & cyl ==6)





