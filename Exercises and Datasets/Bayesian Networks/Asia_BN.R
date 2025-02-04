detach("package:bnstruct", unload=TRUE)
library(bnlearn)
data(asia)

class(asia)
###let's see some stats
apply(asia, 2, table)/5000

##Convert all variables to factors
asia <- asia %>% mutate_if(is.numeric,as.factor)

###Apply a bootstrapping approach with 500 runs, using the greedy HC algorithm
net_bnlearn = boot.strength(data = asia, R = 500, algorithm = "hc",
                            algorithm.args = list(score = "bde",iss=10))

##Query
net_bnlearn[net_bnlearn$strength>0.85 & (net_bnlearn$direction>=0.5),]

##What is the best threshold of strength to segment the network? 
averaged.network(net_bnlearn)

##Construct and averaged network
avg.boot = averaged.network(net_bnlearn, threshold = 0.50)

plot(avg.boot)

##Attach probability tables to the structure we just learned
fitted = bn.fit(avg.boot, asia, method = "bayes")

##One way to plot the network
library(graph)
library(igraph)
g <- igraph.from.graphNEL(as.graphNEL(fitted))

plot(g)

##Convert the bn.fit object to a grain object to get propagated probability calculations
net_fit_junction_asia = compile(as.grain(fitted), propagate = T)

net_fit_junction_asia$universe

###Run queries

##1. Change that one of the disease is present if the patient has dyspnoea

###Chances that tuberculosis is present if the patient has dyspnoea
querygrain(setEvidence(net_fit_junction_asia, nodes = "D",
                       states = "yes",
                       propagate = T), 
           nodes = "T")

##how about lung disease, bronchitis?


##2. if the patient does not have tuberculosis how about lung cancer?
##p(t | D yes , T no)
querygrain(setEvidence(net_fit_junction_asia, nodes = c("D","T"),
                       states = c("yes","no"),
                       propagate = T), 
           nodes = "B")

####
###Exact Network
data(asia)
library(dplyr)
##################-----This will convert the factors to integers
Asia_Num = asia %>% mutate_if(is.factor,as.integer)
#
detach("package:bnlearn", unload=TRUE)
library(bnstruct)
data2015_bnstruct <- BNDataset(data = Asia_Num,
                               discreteness = rep(T, ncol(Asia_Num)),
                               variables = colnames(Asia_Num),
                               starts.from = 1,
                               node.sizes = c(replicate(ncol(Asia_Num),2)))
##################-----Learn the structure of the object using the exact algorithm
Exactnet_bnstruct <- learn.network(data2015_bnstruct,algo = "sm", scoring.func = "BIC")
plot(Exactnet_bnstruct)
##################-----Save the adjacent matrix and use it in bnlearn to fit the data to best structure
netdata2015_bnstruct_dag = Exactnet_bnstruct@dag
##################-----Convert numeric to factor
data2015_ALA_BN_fact <- data2015_ALA_BN_Num %>% mutate_if(is.integer,as.factor)
##################-----Create an empty graph with the variables from the dataset
detach("package:bnstruct", unload=TRUE)
library(bnlearn)
data2015_ALA_BN_fact.net2 = empty.graph(names(Asia_Num))
##################-----Get the adjacent matrix from bnstruct
amat(data2015_ALA_BN_fact.net2) <- netdata2015_bnstruct_dag
##################-----Fit the data to the structure
net_fit <- bn.fit(data2015_ALA_BN_fact.net2, asia, method = "bayes")
##################----------------######################
##################----------------######################
g_exact <- igraph.from.graphNEL(as.graphNEL(net_fit))

plot(g_exact)
