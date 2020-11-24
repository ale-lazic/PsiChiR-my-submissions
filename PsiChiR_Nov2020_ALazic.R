#This code was written for the PsiChiR November contest (https://osf.io/6brdt/)
#Code developed by Aleksandra Lazic
#Data available at https://osf.io/qajr6/
#Codebook available at https://osf.io/bqh87/

#load csv file into RStudio
install.packages("readr") #install pckg readr
library(readr) #load pckg readr
PsiChiR_Nov <- read_csv("~/NICE CROWD DATA.csv") #import dataset


#LEVEL 1: Recode the FACES satisfaction subscale 
#("FACEsatis") into three categories: Low (10 - 23), Medium (24 - 37), and High (38 - 50)
#Recode into same variable
PsiChiR_Nov$FACEsatis[PsiChiR_Nov$FACEsatis >= 10 & PsiChiR_Nov$FACEsatis <= 23] <- "Low"
PsiChiR_Nov$FACEsatis[PsiChiR_Nov$FACEsatis >= 24 & PsiChiR_Nov$FACEsatis <= 37] <- "Medium"
PsiChiR_Nov$FACEsatis[PsiChiR_Nov$FACEsatis >= 38 & PsiChiR_Nov$FACEsatis <= 50] <- "High"
PsiChiR_Nov$FACEsatis <- as.factor(PsiChiR_Nov$FACEsatis) #convert to factor

#LEVEL 2: Calculate the means and standard deviations for the following variables: 
#SSBQtotal, FACEchaotic, and SDQprosocial.
PsiChiR_Nov$SSBQtotal <- as.numeric(PsiChiR_Nov$SSBQtotal) #convert to numeric
PsiChiR_Nov$FACEchaotic <- as.numeric(PsiChiR_Nov$FACEchaotic)
PsiChiR_Nov$SDQprosocial <- as.numeric(PsiChiR_Nov$SDQprosocial)

mean(PsiChiR_Nov$SSBQtotal, na.rm=TRUE) #calculate mean, eclude missing values from analysis
sd(PsiChiR_Nov$SSBQtotal, na.rm=TRUE) #calculate standard deviation, eclude missing values from analysis

mean(PsiChiR_Nov$FACEchaotic, na.rm=TRUE) #calculate mean, eclude missing values from analysis
sd(PsiChiR_Nov$FACEchaotic, na.rm=TRUE) #calculate standard deviation, eclude missing values from analysis

mean(PsiChiR_Nov$SDQprosocial, na.rm=TRUE) #calculate mean, eclude missing values from analysis
sd(PsiChiR_Nov$SDQprosocial, na.rm=TRUE) #calculate standard deviation, eclude missing values from analysis


#LEVEL 3: Create a bar graph to compare the SSBQtotal means across FACEsatis groups. Include 95% confidence interval error bars.

PsiChiR_Nov<-PsiChiR_Nov[!(is.na(PsiChiR_Nov$FACEsatis)| PsiChiR_Nov$FACEsatis==""),] #remove missing values from the factor FACEsatis

PsiChiR_Nov$FACEsatis <- factor(PsiChiR_Nov$FACEsatis, levels = c("Low", "Medium", "High")) #order the factor

install.packages("Rmisc") #install pckg Rmisc
library(Rmisc) #load pckg Rmisc

#Summarizing the data (a default 95% confidence interval)
Sum <- summarySE(PsiChiR_Nov, measurevar="SSBQtotal", groupvars = "FACEsatis", na.rm=TRUE)
Sum

install.packages("ggplot2") #install pckg ggplot2
library(ggplot2) #load pckg ggplot2

#Pass data frame Sum to ggplot2 function
ggplot(Sum, aes(x=FACEsatis, y=SSBQtotal)) + 
  geom_bar(position=position_dodge(), stat="identity") + 
  geom_errorbar(aes(ymin=SSBQtotal-ci, ymax=SSBQtotal+ci), #Error bars represent 95% confidence intervals
                width=.2) #Width of the error bars                                    


#LEVEL 4: Test the hypothesis that SSBQtotal scores differ across the three FACEsatis groups.

#Run a one-way ANOVA
res.aov <- aov(SSBQtotal~FACEsatis, data = PsiChiR_Nov)
#Summary of the analysis
summary(res.aov)
