#This code was written for the PsiChiR October contest (https://osf.io/kgprd/)
#Code developed by Aleksandra Lazic
#Data available at https://osf.io/qajr6/
#Codebook available at https://osf.io/bqh87/

#load csv file into RStudio
install.packages("readr") #install pckg readr
library(readr) #load pckg readr
PsiChiR_Oct <- read_csv("~/NICE CROWD DATA.csv") #import dataset

#LEVEL 1: Create a new variable "Duration in minutes" by taking the variable "Duration__in_seconds_" and dividing it by 60.
PsiChiR_Oct$DurationInMinutes <- PsiChiR_Oct$Duration__in_seconds_ /60

#LEVEL 2: Find the median for the variable "Duration in minutes".
as.numeric(PsiChiR_Oct$DurationInMinutes)  #convert the variable as numeric
MedianValue <- median(PsiChiR_Oct$DurationInMinutes)
print(MedianValue)

#LEVEL 3: Create a bar graph for the variable "Duration in minutes" to display medians for men and women.
PsiChiR_Oct2<-PsiChiR_Oct[!(PsiChiR_Oct$Gender =="3"),] #remove level 3 from the factor Gender
PsiChiR_Oct_final<-PsiChiR_Oct2[!(is.na(PsiChiR_Oct2$Gender)| PsiChiR_Oct2$Gender==""),] #remove missing values from the factor Gender

as.factor(PsiChiR_Oct_final$Gender) #convert Gender into a factor

boxplot(PsiChiR_Oct_final$DurationInMinutes ~ PsiChiR_Oct_final$Gender, #note: boxplot has a median mark
        ylim=c(1,200),  #restrict y-axis range (set the limit to 200)
        names=c("Men", "Women"), #change labels of the factor levels
        xlab="Gender", #change label of the x axis
        ylab="Duration in minutes") #change label of the y axis

#LEVEL 4: Test whether men and women differ on the variable "Duration in minutes".
t.test(PsiChiR_Oct_final$DurationInMinutes~PsiChiR_Oct_final$Gender)
