#This code was written for the PsiChiR September contest (https://osf.io/ge5h3/)
#Code developed by Aleksandra Lazic
#Data available at https://osf.io/qajr6/
#Codebook available at https://osf.io/bqh87/

#load csv file into RStudio

install.packages("readr") #install pckg readr
library(readr) #load pckg readr
PsiChiR_Sept <- read_csv("~/NICE CROWD DATA.csv") #import dataset

# LEVEL 1: Remove cases that have missing values on Countryborn

install.packages("dplyr") #install pckg dplyr
library("dplyr") #load pckg dplyr
PsiChiR_Sept <- filter(PsiChiR_Sept, !is.na(Countryborn)) #remove the NA from Countryborn var

#LEVEL 2: Find frequencies for the following variables: UserLanguage, Gender, and Race

install.packages("plyr") #install pckg plyr
library("plyr") #load pck plyr
as.factor(PsiChiR_Sept$UserLanguage) #convert UserLanguage into a factor
as.factor(PsiChiR_Sept$Gender) #convert Gender into a factor
as.factor(PsiChiR_Sept$Race) #convert Race into a factor
count(PsiChiR_Sept, "UserLanguage") #find frequencies for UserLanguage
count(PsiChiR_Sept, "Gender") #find frequencies for Gender
count(PsiChiR_Sept, "Race") #find frequencies for Race

#LEVEL 3: Create a scatterplot to visualize the relationship between vertical collectivism (COSvcollect) and horizontal collectivism (COShcollect)
plot(PsiChiR_Sept$COSvcollect, PsiChiR_Sept$COShcollect) 

#LEVEL 4: Calculate Pearson's R correlation coefficient between vertical collectivism and horizontal collectivism
PearsonCor <- cor.test(PsiChiR_Sept$COSvcollect, PsiChiR_Sept$COShcollect, method = "pearson") #run the Pearson correlation test
PearsonCor #see the results of the Pearson correlation test
