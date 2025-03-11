###R Code Week 5 Descriptive statistics

#install.packages("ggthemes")
#install.packages("jmv")
#install.packages("dlookr")
library(dlookr)
library(jmv)
library(ggthemes)
library(psych)
library(car)
library(tidyverse)

setwd("/Users/javier/Documents/Jupyter/6106_Material/Week_5/")

# Read data_frame
Inequality = read.csv('HouseHold_Survey.csv')

#1. Let's look at what kind of variables we have in the dataset

##Types of variables

##Continuous variables

##Height in meters 

##Discrete Quantitative

##Age

table(Inequality$Age)
barplot(table(Inequality$Age))

##Income 

##We can create a new column that splits the Income variable 
#into discrete units like this

A ► above 15.760
B ► from 7.880 to 15.760
C ► from 3.152 to 7.880
D ► from 1.576 to 3.152
E ► Up until 1.576

# Income clases
Inequality$Rent_Discrete = cut(x=Inequality$Rent,
                               breaks<-c(0, 1576, 3152, 7880, 15760, 200000),
                               labels<-c('E', 'D', 'C', 'B', 'A'),
                               include.lowest=T)

freq_rent = table(cut(x=Inequality$Rent,
                       breaks<-c(0, 1576, 3152, 7880, 15760, 200000),
                       labels<-c('E', 'D', 'C', 'B', 'A'),
                       include.lowest=T))
table(Inequality$Rent_Discrete)
# Percentages for each of the classes of income
perct_rent = prop.table(table(cut(x=Inequality$Rent,
                                   breaks<-c(0, 1576, 3152, 7880, 15760, 200000),
                                   labels<-c('E', 'D', 'C', 'B', 'A'),
                                   include.lowest=T))) * 100
freq_perct_rent <- data.frame(cbind("Frequency"=freq_rent, "Percent"=perct_rent))

##Get final sorted values
freq_perct_rent[order(row.names(freq_perct_rent)),]

##Bar plot the inequality of income by social classes
options(repr.plot.width=14, repr.plot.height=6)
ggplot(freq_perct_rent, aes(x=row.names(freq_perct_rent), y=Frequency))+
  geom_bar(stat="identity", color="black", fill=c('cyan', 'red', 'green', 
                                                  'purple', 'blue'), alpha=.7)+
  theme_economist()+
  ggtitle("Distribution of classes by income")+
  xlab("")+
  ylab("Frequência")+
  geom_label(aes(label=Frequency), col = "black", fill="white", fontface = "bold", size=5)+
  theme(plot.background=element_rect(fill='#ffee6c', color='#FFA500'),
        plot.title = element_text(size=21, hjust=.5),
        axis.title.y = element_text(size=21, vjust=2),
        axis.title.x = element_text(size=21, vjust=-1),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

##Nominal

#qualitative nominal variables: where there is no order
#Location 

nrow(unique(Inequality["Location"],))
table(Inequality$Location)

##27 different sampling locations

#Sex another nominal variable

##Skin Color
counts = table(Inequality$Color)
0 = Indigenous
2 = White
4 = Black
6 = Yellow
8 = brown

barplot(counts,names.arg = c("Indigenous","White","Black","Yellow","brown"))

##Ordinal variables - They represent discrete bins or categories 
#that have some order (numerical, qualitative)

barplot(table(Inequality$Years.of.study))


##Central Tendencies measurements

#Median
#Mean



median(Inequality$Height)
mean(Inequality$Height)
exp(mean(log(Inequality$Height)))  ##Geometric mean

median(Inequality$Age)
mean(Inequality$Age)

median(Inequality$Rent)
mean(Inequality$Rent)
geometric.mean(Inequality$Rent)

geometric.mean(Inequality$Height)

#Mode
Mode_F = function(x){
  freq <- table(x)
  return(names(freq)[freq == max(freq)])
}


Mode_F(Inequality$Sex)
Mode_F(Inequality$Location)
table(Inequality$Location)

##Means within variables e.g. Age by location

Mean_Age = Inequality %>% 
  group_by(Location) %>%
  summarise(Mean = mean(Age, na.rm=TRUE),
            Median = median(Age, na.rm = TRUE),
            geom.mean = geometric.mean(Age, na.rm = TRUE))


Mean_Income_Location = Inequality %>% 
  group_by(Location) %>%
  summarise(Mean = mean(Rent, na.rm=TRUE),
            Median = median(Rent, na.rm = TRUE),
            geom.mean = geometric.mean(Rent, na.rm = TRUE))


##relative location
#Quantiles

quantile(Inequality$Rent, c(.25, .50 ,.75))

#quartile
###In descriptive statistics, a quartile is any of the three values that divide the ordered set of data into four equal parts, and so each part represents 1/4 of the sample or population.

quart<-c()
for(i in 1:3){
  quart<-c(quart, i/4)
}
df_quartis<-data.frame(quantile(Inequality$Rent, quart))
colnames(df_quartis) <- c("Quartiles")
df_quartis

##Deciles
decis<-c()
for(i in 1:9){
  
  decis<-c(decis, i/10)
}
df_decis<-data.frame(quantile(Inequality$Rent, decis))
colnames(df_decis) <- c("Deciles")
df_decis


##Percentiles

percentis <- c()
for(i in 1:99){
  percentis<-c(percentis, i/100)
}

df_percentis = data.frame(quantile(Inequality$Rent, percentis))
colnames(df_percentis)<- c("Percentis")
head(df_percentis, 99)

##How About Age??

##Libraries with summary statistics

# mean,median,25th and 75th quartiles,min,max
summary(Inequality$Age)

psych::describe(Inequality$Rent) ##From the psych package

describe(Inequality$Rent)

describeBy(Inequality$Rent, Inequality$Sex) ##Also from the psych package

#install.packages("jmv")
library(jmv)

descriptives(
  data = Inequality,
  vars = c(
    "Rent",
    "Age"),
  splitBy = "Sex")

descriptives(
  data = Inequality,
  vars = c(
    "Rent",
    "Age"),
  splitBy = "Sex",
  freq = TRUE,
  hist = TRUE,
  dens = TRUE,
  bar = TRUE,
  barCounts = TRUE,
  box = TRUE,
  sd = TRUE,
  variance = TRUE,
  range = TRUE,
  se = TRUE,
  skew = TRUE,
  kurt = TRUE,
  #quart = TRUE,
  pcEqGr = TRUE,
  pcNEqGr = 10
)


##Dispersion
#Range

apply(Inequality,2,range)

#Interquartile range
#IQR = upper quartile - lower quartile
IQR(Inequality$Age) 

#Standard Deviation
#Variance

Var_Income_Location = Inequality %>% 
  group_by(Location) %>%
  summarise(Var = var(Rent, na.rm=TRUE),
            sd = sd(Rent, na.rm = TRUE),
            range = range(Rent, na.rm = TRUE))

#Confidence Intervals

t.test(Inequality$Age)

###
Skewness
Kurtosis


df = data.frame(Rent=Inequality$Rent[Inequality$Rent < 10000])
options(repr.plot.width=12, repr.plot.height=6)
ggplot(df, aes(x=Rent, y=..density..))+
  geom_density(fill="green", color="black", linetype="dashed", alpha=.8)+
  theme_economist()+
  geom_vline(aes(xintercept=mean(Inequality$Rent)),
             color="black", linetype="dashed", size=1.5)+
  geom_vline(aes(xintercept=median(Inequality$Rent)), color="blue", size=1.5, linetype="dashed")+
  geom_vline(aes(xintercept=788), color="red", size=1.5, linetype="dashed")+
  
  annotate("text", label="Mean = 2000.38", x = 3400, y = 0.00085, color = "black", size=7)+
  annotate("text", label="Median = 1200", x=4000, y=0.00072, color="blue", size=7)+
  annotate("text", label="Mode = 788", x=5000, y=0.00054, color="red", size=7)+
  ggtitle("Density Rent < 10000")+
  theme(plot.background=element_rect(fill="#F0E68C", color="black"),
        plot.title=element_text(size=25, hjust=.5, vjust=2),
        axis.title.x=element_text(size=23, vjus=-1),
        axis.title.y=element_text(size=23, vjust=2),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20))


options(repr.plot.width=12, repr.plot.height=6)
ggplot(Inequality, aes(x=Height, y=..density..))+
  geom_density(fill="yellow", color="black", alpha=.8, linetype="dashed")+
  theme_economist()+
  geom_vline(aes(xintercept=mean(Height)), color="black", linetype="dashed", size=1.5)+
  geom_vline(aes(xintercept=median(Height)), color="blue", size=1.5, linetype="dashed")+
  
  annotate("text", label="Mean=1.7", x=1.8, y=4.5, size=7, color="black")+
  annotate("text", label="Median=1.7",  x=1.9, y=3.5, size=7, color="blue")+
  
  ggtitle("Density - Height")+
  theme(plot.background=element_rect(fill="#88CEEB", color="black"),
        plot.title=element_text(size=25, hjust=.5, vjust=2),
        axis.title.x=element_text(size=23, vjus=-1),
        axis.title.y=element_text(size=23, vjust=2),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20))

###Graphical Methods (Data Exploration)
##Boxplots

tema=theme(plot.background=element_rect(fill="#F0FFFF", color="black"),
           plot.title=element_text(hjust=.5, size=30),
           axis.title.x=element_text(size=30),
           axis.text.x=element_text(size=30),
           axis.text.y=element_text(size=30))
options(repr.plot.width=20, repr.plot.height=10)
a<-ggplot(data = Inequality, aes(x="Height", y = Height)) + 
  stat_boxplot(geom ='errorbar', width = 0.4, color="black", size=1) + 
  geom_boxplot(fill = '#FF7F50', color="black", size=1) + 
  theme_economist()+
  coord_flip() +
  ylab("Metros") + 
  xlab("")+
  ggtitle("Boxplot - Height") + 
  tema

b<-ggplot(data = Inequality, aes(x="Age", y = Age)) + 
  stat_boxplot(geom ='errorbar', width = 0.4, color="black", size=1) + 
  geom_boxplot(fill = '#32CD32', color="black", size=1) + 
  theme_economist()+
  coord_flip() +
  ylab("Anos") + 
  xlab("")+
  ggtitle("Boxplot - Age") + 
  tema
Inequality$Height
c<-ggplot(data = Inequality, aes(x="Years.of.study", y = Years.of.study)) + 
  stat_boxplot(geom ='errorbar', width = 0.4, color="black", size=1) + 
  geom_boxplot(fill = '#DAA520', color="black", size=1) +
  theme_economist()+
  coord_flip() +
  ylab("Anos") + 
  xlab("")+
  ggtitle("Boxplot - Years.of.study") + 
  tema

d<-ggplot(data = Inequality, aes(x="Rent", y = Rent)) + 
  stat_boxplot(geom ='errorbar', width = 0.4, color="black", size=1) + 
  geom_boxplot(fill = '#FF00FF', color="black", size=1) +
  theme_economist()+
  coord_flip() +
  ylab("Reais") + 
  xlab("")+
  ggtitle("Boxplot - Rent") + 
  tema

library(cowplot)
plot_grid(a, b, c, d, nrow=2, ncol=2)


Outliers	

Crayfish <- read.table(file = "Procambarus.txt",
                       header = TRUE)

head(Crayfish)

library(car)
Boxplot(CTL ~ Month,
        ylab = "Cephalothorax Length",
        xlab = "Month", 
        data= Crayfish,
        main = expression(italic("Procambarus clarkii")))

stripchart(CTL ~ Month,data = Crayfish, 
           vertical = TRUE, method = "jitter", 
           pch = 21, 
           add = TRUE,col=rgb(1, 0, 0,0.5))

ggplot(data = Crayfish, aes(x = Month, y = CTL)) +
  geom_boxplot(alpha = 0.2) +
  geom_violin(fill='red', color='red',  alpha=0.4) +
  geom_jitter(alpha = 0.6, color = "black") + 
  theme_bw()


##Histograms
##Histograms Vs Density plots


options(repr.plot.width=14, repr.plot.height=6)
a<-ggplot(Inequality, aes(x=Age))+
  geom_histogram(bins=50, col='black', fill='#00BFFF', alpha=.7, size=1.1)+
  theme_economist()+
  ggtitle("Age Histogram")+
  ylab("Frequency")+
  theme(plot.title = element_text(size=21, hjust=.5),
        axis.title.y = element_text(size=21, vjust=2),
        axis.title.x = element_text(size=21, vjust=-1),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20)
  )

b<-ggplot(Inequality, aes(x=Age, y=..density..))+
  geom_density(col='black', fill='#00BFFF', alpha=.7, size=1)+
  theme_economist()+
  ggtitle("Age Density")+
  ylab("Density")+
  theme(plot.title = element_text(size=21, hjust=.5),
        axis.title.y = element_text(size=21, vjust=2),
        axis.title.x = element_text(size=21, vjust=-1),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))
plot_grid(a, b, nrow=1, ncol=2)

##CMF
ggplot(Inequality, aes(x=Age))+
  theme_solarized(light=FALSE)+
  scale_colour_solarized('blue')+
  ylab("Percent (%)")+
  ggtitle("Cumulative Chart - x=Age")+
  geom_histogram(
    aes(y=cumsum(..count..)/sum(..count..)),
    bins=100,
    col="black",
    fill="#FFA500",
    size=1.1
  ) +
  geom_freqpoly(
    aes(y=cumsum(..count..)/sum(..count..)),
    bins=100,
    color="pink",
    size=1.2)



#Normality
#qqplot

qqnorm(Inequality$Age, pch = 1, frame = FALSE)
qqline(Inequality$Age, col = "steelblue", lwd = 2)

qqnorm(Inequality$Rent, pch = 1, frame = FALSE)
qqline(Inequality$Rent, col = "steelblue", lwd = 2)



###Transformations

#install.packages("dlookr")

library(dlookr)

Inequality %>%
  select(Rent) %>% 
  boxplot()

Inequality %>% 
  mutate(Income_log1 = transform(Inequality$Rent, method = "log+1")) %>% 
  select(Income_log1) %>% 
  boxplot()

Inequality = Inequality %>% 
  mutate(Income_log1 = transform(Inequality$Rent, method = "log+1"))
Inequality$Income_log1 = as.numeric(Inequality$Income_log1)

options(repr.plot.width=14, repr.plot.height=6)
a<-ggplot(Inequality, aes(x=Rent))+
  geom_histogram(bins=50, col='black', fill='green', alpha=.7, size=1.1)+
  theme_economist()+
  ggtitle("Rent Histogram")+
  ylab("Frequency")+
  theme(plot.title = element_text(size=21, hjust=.5),
        axis.title.y = element_text(size=21, vjust=2),
        axis.title.x = element_text(size=21, vjust=-1),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20)
  )

b<-ggplot(Inequality, aes(x=Income_log1, y=..density..))+
  geom_density(col='black', fill='green', alpha=.7, size=1)+
  theme_economist()+
  ggtitle("Income_log1 Histogram")+
  ylab("Density")+
  theme(plot.title = element_text(size=21, hjust=.5),
        axis.title.y = element_text(size=21, vjust=2),
        axis.title.x = element_text(size=21, vjust=-1),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))
plot_grid(a, b, nrow=1, ncol=2)

## 
library(car)  
qqPlot(Inequality$Income_log1, pch = 1)



