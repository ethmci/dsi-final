library(tidyverse)
library(readxl)
library(Hmisc)
library(descr)
library(weights)


#Importing Pre-Tidied Version of GSS  downloaded from the GSS Data Explorer and selecting the specific variables 
#used in the final edition of this project. The complete GSS data set would consistently take way too long to read into R
#and could not be stored easily on github because it was a large (~400MB +) file.

##tgss is used to refrence this code because it is thename i stored a filtered version of the entire GSS dataset on that had
##the 18 variables used in our code on

tgss <- read_excel("~/GSS.xls")

#Sci Knowledge Variable
tgss <- mutate(tgss, 
               sciknowledge = (HOTCORE + RADIOACT + EVOLVED + CONDRIFT + 
                                 VIRUSES + ELECTRON + LASERS + BOYORGRL) / 8)
describe(tgss$sciknowledge, weights = tgss$WTSSALL)

#REGRESSION MODEL
summary(lm(tgss$ADVFRONT ~ tgss$RELIG + tgss$POLVIEWS + tgss$INCOME + tgss$SEX + tgss$DEGREE + tgss$AGE + tgss$RACE, weights = tgss$WTSSALL))

#REGRESSION MODEL for SCI KNOWLEDGE
summary(lm(tgss$sciknowledge ~ tgss$RELIG + tgss$POLVIEWS + tgss$INCOME + tgss$SEX + tgss$DEGREE + tgss$AGE + tgss$RACE, weights = tgss$WTSSALL))


##Transforming Variables from Default Data Type to Factors/appropriate data type
#Personal Demographics, Control Variables
describe(tgss$POLVIEWS, weights = tgss$WTSSALL)
tgss$POLVIEWS <- as.factor(tgss$POLVIEWS)
levels(tgss$POLVIEWS) = c("Extremely Liberal", "Liberal", "Slightly Liberal",
                          "Moderate", "Slightly Conservative", "Conservative", "Extremely Conservative")

describe(tgss$INCOME, weights = tgss$WTSSALL) #Get Mean and Other Summary Stats Before Changing Into Factor
tgss$INCOME <- as.factor(tgss$INCOME)
levels(tgss$INCOME) = c( "Under $1,000", "$1,000--2,999", '$3,000--3,999', '$4,000--4,999',
  '$5,000--5,999', '$6,000--6,999', '$7,000--7,999', '$8,000--9,999', '$10,000--14,999', 
  '$15,000--19,999', '$20,000--24,999', "$25,000 or over")

describe(tgss$SEX, weights = tgss$WTSSALL)

describe(tgss$DEGREE, weights = tgss$WTSSALL)
tgss$DEGREE <-  as.factor(tgss$DEGREE)
levels(tgss$DEGREE) = c("Did Not Complete High School", "High School","Associates","Bachelors","Graduate")
describe(tgss$DEGREE, weights = tgss$WTSSALL)

describe(tgss$AGE, weights = tgss$WTSSALL)

tgss$RACE <- as.factor(tgss$RACE)
levels(tgss$RACE) = c("White","Black","Other")
describe(tgss$RACE, weights = tgss$WTSSALL)

#Y/DEPENDENT VARIABLE: Even if it brings no immediate benefits, scientific research that advances the frontiers 
#of knowledge is necessary and should be supported by the federal government
describe(tgss$ADVFRONT, weights = tgss$WTSSALL)
tgss$ADVFRONT <- as.factor(tgss$ADVFRONT)
levels(tgss$ADVFRONT) = c("Strongly Agree","Agree","Disagree","Strongly Disagree")

#X/INDEPENDENT VARIABLE: Religion 
describe(tgss$RELIG, weights = tgss$WTSSALL)
tgss$RELIG <- as.factor(tgss$RELIG)
levels(tgss$RELIG) = c("Protestant","Catholic","Jewish","None","Other","Buddhism", "Hinduism",
                 "Other Eastern", "Islam","Orthodox Christian","Christian","Native American","Inter-NonDenominational")
describe(tgss$RELIG, weights = tgss$WTSSALL)

