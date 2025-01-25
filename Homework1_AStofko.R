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





