###install.packages("fastDummies")

# Load the library
library(fastDummies)
library(corrplot)
library(RColorBrewer)
library(tidyverse)


setwd("/Users/javier/Documents/Jupyter/MBIO_6490_2023/R_Sessions/Week_4")
##This dataset evaluate predictor that are believe to influence student scores
Student_scores = read.csv("Student_scores.csv")

M <-cor(Student_scores)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

##Let's look at the dataset and the variables associated to it...

##1. we don't need the student ID, we can drop that variable
##2. the variables race, ses and prog have multiple levels, we have two options leave them that way, or create dummy variables
##If you have more levels >2 per node then the fold changes calculations need to address the multiple levels

# Create dummy variable
data <- dummy_cols(Student_scores, 
                   select_columns = c("race","ses","prog"))

#We can subset now to keep the variables we need only
data2 = data[,c(2,5,7:21)]

##For the analysis we are going to run we want to discretize the continuous variables
library(bnlearn)
##Split variables to discretize
data_ToDiscrete = data2[,c(3:7)]
##Convert these variables to numeric
data_ToDiscrete = data.frame(apply(data_ToDiscrete, 2, as.numeric))

##Let's discretize in two levels
data_disc = discretize(data_ToDiscrete, 
                                  method = "hartemink", breaks = 2,
                                  ibreaks = 3, idisc = "quantile")

##Combine binary and discretized variables
data_final = cbind(data2[,c(1,2,8:17)],data_disc)

data_final = data_final %>% mutate_if(is.factor,as.numeric)
M2 <-cor(data_final)
corrplot(M2, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

##Now we can create a network with all of the variables
library(bnlearn)
##Convert all variables to factors
data_final <- data_final %>% mutate_if(is.numeric,as.factor)

###Apply a bootstrapping approach with 500 runs, using the greedy HC algorithm
net_bnlearn = boot.strength(data = data_final, R = 500, algorithm = "hc",algorithm.args = list(score = "bde",iss=10))

##Query
net_bnlearn[net_bnlearn$strength>0.85 & (net_bnlearn$direction>=0.5),]

##What is the best threshold of strength to segment the network? 
averaged.network(net_bnlearn)

##Construct and averaged network
avg.boot = averaged.network(net_bnlearn, threshold = 0.5)


#net_bnlearn = hc(data_final, score = "bde",iss=10,)

##Attach probability tables to the structure we just learned
fitted = bn.fit(avg.boot, data_final, method = "bayes")

##One way to plot the network
library(graph)
library(igraph)
g <- igraph.from.graphNEL(as.graphNEL(fitted))

plot(g)


####We can use blacklists to remove those associations that we don't want evaluated in the network
bl = matrix(c("read", "write", 
              "write", "read",
              "write","math",
              "math","write",
              "read","math", 
            "math","read",
              "science","math",
"math","science",
"read","science",
"science","read",
"science","write",
"write","science",
"ses_1","ses_2",
"ses_2","ses_1",
"ses_1","ses_3",
"ses_3","ses_1",
"ses_2","ses_3",
"ses_3","ses_2",
"race_1","race_2",
"race_2","race_1",
"race_1","race_3",
"race_3","race_1",
"race_1","race_4",
"race_4","race_1",
"race_2","race_3",
"race_3","race_2",
"race_2","race_4",
"race_4","race_2",
"race_3","race_4",
"race_4","race_3",
"prog_1","prog_2",
"prog_2","prog_1",
"prog_1","prog_3",
"prog_3","prog_1",
"prog_2","prog_3",
"prog_3","prog_2"),
ncol = 2, byrow = TRUE,dimnames = list(NULL, c("from", "to")))

net_bnlearn_bl = boot.strength(data = data_final, R = 500, algorithm = "hc",algorithm.args = list(score = "bde",iss=10,blacklist = bl))

averaged.network(net_bnlearn_bl)

avg.boot_bl = averaged.network(net_bnlearn_bl, threshold = 0.5)


fitted_bl = bn.fit(avg.boot_bl, data_final, method = "bayes")


library(graph)
library(igraph)
g_bl <- igraph.from.graphNEL(as.graphNEL(fitted_bl))

plot(g_bl)




##Fit data to the structure
#net_fit <- bn.fit(net_bnlearn, data2015_ALA_BN_disc, method = "bayes")
##Convert the bn.fit object to a grain object to get propagated probability calculations
net_fit_junction = compile(as.grain(fitted_bl), propagate = T)

net_fit_junction$universe
#######----------Probabilities----------####
##Marginal
table(data_final$female)/nrow(data_final)
##We get values for both states
querygrain(net_fit_junction,nodes = c("female"), type = "marginal") 


##Joint
##We get values for both states
querygrain(net_fit_junction,nodes = c("female","write"), type = "joint")

##Conditional
##We get values for both states
##if we want to calculate P(write  | "female" (1))


querygrain(setEvidence(net_fit_junction, nodes = c("female"),
                       states = c("1"),
                       propagate = T), 
           nodes = "write")

plot(data_final$female~data_final$write)
##if we want to calculate P(write | "Adult.obesity.Value" (1))
querygrain(setEvidence(net_fit_junction, nodes = c("female","prog_2"),
                       states = c("1","0"),
                       propagate = T), 
           nodes = "write")



##Relative Risk
##We can calulate RR by dividing the conditional present by the conditional absent
cond = querygrain(setEvidence(net_fit_junction, nodes = c("race_1","socst"),
                              states = c("1","1"),
                              propagate = T), 
                  nodes = "science")[[1]][1]
cond2 = querygrain(setEvidence(net_fit_junction, nodes = c("race_1","socst"),
                               states = c("0","0"),
                               propagate = T), 
                   nodes = "science")[[1]][1]

RR = cond/cond2

##Absolute Risk
##We can calulate RR by dividing the conditional present by the conditional absent
cond = querygrain(setEvidence(net_fit_junction, nodes = c("race_1","socst"),
                              states = c("1","1"),
                              propagate = T), 
                  nodes = "science")[[1]][1]
baseline = querygrain(net_fit_junction, nodes = c("science"),
                               type = "marginal")[[1]][1]

AR = cond/baseline


##################-----Optional---######################
##################----------------######################
##################----------------######################
##################-----Another option is to learn the network using the bnstruct package and save the adjacent matrix
##################-----to fit the best network
##################-----but the data need to be converted to numeric
library(dplyr)
##################-----This will convert the factors to integers
data_final_Num = data_final %>% mutate_if(is.factor,as.integer)
##################-----Create an object BN from the data
detach("package:bnlearn", unload=TRUE)
library(bnstruct)
data2015_bnstruct <- BNDataset(data = data_final_Num,
                               discreteness = rep(T, ncol(data_final_Num)),
                               variables = colnames(data_final_Num),
                               starts.from = 1,
                               node.sizes = c(replicate(ncol(data_final_Num),2)))
##################-----Learn the structure of the object using the exact algorithm
Exactnet_bnstruct <- learn.network(data2015_bnstruct,algo = "sm")
plot(Exactnet_bnstruct)
##################-----Save the adjacent matrix and use it in bnlearn to fit the data to best structure
netdata2015_bnstruct_dag = Exactnet_bnstruct@dag
##################-----Convert numeric to factor
data2015_ALA_BN_fact <- data2015_ALA_BN_Num %>% mutate_if(is.integer,as.factor)
##################-----Create an empty graph with the variables from the dataset
detach("package:bnstruct", unload=TRUE)
library(bnlearn)
data2015_ALA_BN_fact.net2 = empty.graph(names(data_final))
##################-----Get the adjacent matrix from bnstruct
amat(data2015_ALA_BN_fact.net2) <- netdata2015_bnstruct_dag
##################-----Fit the data to the structure
net_fit <- bn.fit(data2015_ALA_BN_fact.net2, data_final, method = "bayes")
##################----------------######################
##################----------------######################
g_exact <- igraph.from.graphNEL(as.graphNEL(net_fit))

plot(g_exact)
