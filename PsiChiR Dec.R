#This code was written for the PsiChiR December 2020 contest (https://osf.io/jg74y/)
#Code developed by Aleksandra Lazic
#Data available at https://osf.io/qajr6/
#Codebook available at https://osf.io/bqh87/

#load csv file into RStudio
install.packages("readr") #install pckg readr
library(readr) #load pckg readr
PsiChiR_Dec <- read_csv("~/NICE CROWD DATA.csv") #import dataset


#LEVEL 1: Subset and filter your data so you have a dataset that excludes participants 
#with missing values for "Race" and "Gender", 
#and includes only these two variables plus the variable "COShcollect" (horrizontal collectivism)
dat1 <-PsiChiR_Dec[!(is.na(PsiChiR_Dec$Race)| PsiChiR_Dec$Race==""),] #remove NA from Race
dat2 <- dat1[!(is.na(dat1$Gender)| dat1$Gender==""),] #remove NA from Gender
myvars <- c("Race", "Gender", "COShcollect") #select the 3 vars
newdata <- dat2[myvars] #keep only the 3 vars in a new dataset

as.factor(newdata$Race) #convert to factor
as.factor(newdata$Gender) #convert to factor
as.numeric(newdata$COShcollect) #convert to numeric


#LEVEL 2: Calculate the means and standard deviations for COShcollect for all gender categories.
means = aggregate(newdata$COShcollect, #specify data column
          list(newdata$Gender), #specify group indicator
          mean, #specify function (mean)
          na.rm = TRUE) #aggregate with na.rm because COShcollect contains missing values

sd = aggregate(newdata$COShcollect, #specify data column
          list(newdata$Gender), #specify group indicator
          sd, #specify function (standard deviation)
          na.rm = TRUE) #aggregate with na.rm because COShcollect contains missing values


#LEVEL 3: Create a line graph that displays mean COShcollect scores for different race categories, 
#with separate lines for each gender.
df = aggregate(newdata$COShcollect, 
         list(newdata$Gender, newdata$Race), 
         mean, 
         na.rm = TRUE) 

install.packages("ggplot2") #install pckg ggplot2
library(ggplot2) #load pckg ggplot2

ggplot(df, aes(x = Group.2, y = x, group = Group.1, color=as.factor(Group.1))) +
  geom_line() +
  geom_point() +
  ylab("Horrizontal Collectivism") +
  xlab("Race") +
  labs(col = "Gender") +
  scale_x_discrete(limits=c("1","2","3", "4", "5", "6")) +
  scale_y_continuous(limits = c(0, 40))


#LEVEL 4: Conduct a factorial ANOVA to compare COShcollect scores across race and gender.
#Run a two-way ANOVA
res.aov2 <- aov(COShcollect ~ Race + Gender, data = newdata)
#Summary of the analysis
summary(res.aov2)