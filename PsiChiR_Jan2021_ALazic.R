#This code was written for the PsiChiR January 2021 contest (https://osf.io/24653/)
#Code developed by Aleksandra Lazic
#Data available at https://osf.io/4fm32/

install.packages("readr") #install pckg readr
library(readr) #load pckg readr
dat <- read_csv("~/LeeOutton20.csv") #import dataset

#LEVEL 1: Remove participants outside of the United States.
install.packages("dplyr") #install pckg dplyr
library("dplyr") #load pckg dplyr
dat <- filter(dat, VGeoCountry == "United States") #keep only participants in the US

#LEVEL 2: For the remaining participants, find the mean and standard deviation for "Age".
mean(dat$Age)
sd(dat$Age)

#LEVEL 3: Create a histogram for "Age"
Age <- dat$Age
hist(Age,
     xlab="Years of age")

#LEVEL 4: Test whether "Age" differed significantly across experimental conditions ("Manipulation").
as.numeric(dat$Age)
as.factor(dat$Manipulation)

res.aov <- aov(Age~Manipulation, data = dat) #run a one-way ANOVA
summary(res.aov)