library(markovchain)


#Lets define our states
weatherStates <- c("Rain", "Nice", "Snow")


byRow <- TRUE


##lets create the transitional matrix
weatherMatrix <- matrix(data = c(0.5, 0.25, 0.25,
                                 0.5, 0.0, 0.5,
                                 0.25, 0.25, 0.5), byrow = byRow, nrow = 3,
                        dimnames = list(weatherStates, weatherStates))


?new
##make the matrix a markovchain class
mcWeather <- new("markovchain", states = weatherStates, byrow = byRow,
                 transitionMatrix = weatherMatrix, name = "Weather")


##define the initial state
initialState <- c(0, 1, 0)

mcWeather^3

after3Days <- initialState * (mcWeather ^3)
after3Days

plot(mcWeather)


###

tran = matrix(c(0.85,0.15,0.65,0.35),ncol = 2,nrow = 2, byrow = T)
tran

s0 = c(0.05,0.95)
s0 * tran
s1 = colSums(s0 * tran)
s2 = colSums(s1 * tran)
s3 = colSums(s2 * tran)
s4 = colSums(s3 * tran)
s5 = colSums(s4 * tran)
s6 = colSums(s5 * tran)
s4
s5
s6
s7  = colSums(s6 * tran)
s7


###
P=matrix(0,5,5)
P[1,] = c(1,0,0,0,0)
P[2,] = c(0.5,0,0.5,0,0)
P[3,] = c(0,0.5,0,0.5,0)
P[4,] = c(0,0,0.5,0,0.5)
P[5,] = c(0,0,0,0,1)
#P

##
markov = function(N,Po,P){
  # P0 = c(0.0005,0.4,0.199,0.4,0.0005)
  # P = P*P0
  X=matrix(0,1,N)
  a = sample(c(1:5),1,prob = Po)
  X[1] = a
  for (i in 2:N){
    a=sample(c(1:5),1,replace=T,P[a,])
    X[i]=a
  }
  b = as.vector(X)
  return(b)
}


P0 = c(0.0005,0.4,0.199,0.4,0.0005)
markov(10,P0,P)






N =20
plot(NA, xlim=c(0,20), ylim=c(0,5))#empty plot
datas = matrix(ncol = 20, nrow = 100)
for (i in 1:100){
  datas[i,] = markov(N,P0,P)
  condir = datas[i,]
  col = (condir[10]==1 | condir[10]==5)
  lines(condir, lwd=2,col = ifelse(col, "coral","forestgreen"))
}
