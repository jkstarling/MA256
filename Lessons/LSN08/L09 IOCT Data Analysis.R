#Slide 3: Explore IOCT Data for Associations & Potential Confounders
#2 Things we do every R code session
library(tidyverse)
#Set Working Directory to L08 folder, because that's where my data file is.
setwd("C:/Users/nicholas.reisweber/OneDrive - West Point/MA256_AY23/Lesson 07 Generationation (2 of 2)")

#Read in the IOCT Data from Lesson 07
IOCT_Data=read_csv("L07_IOCT_Data_Cleaned.csv")

#Graph IOCT Data Height vs IOCT Time
IOCT_Data%>%
  ggplot(aes(x = height, y=IOCT_Time))+
  geom_point()+ #Scatter Plot when comparing two quantitative variables
  geom_smooth(method="lm")+ #Overlays the best-fitting line for the data 
  labs(x="All Heights", y="All IOCT Times")

#Graph IOCT Data Height vs IOCT Time but Color Based on Sex of CDT
IOCT_Data%>%
  ggplot(aes(x = height, y=IOCT_Time, color=as.factor(sex)))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="All Heights Colored by Sex", y="All IOCT Times Colored by Sex")
  
#Now that we recognize sex is a confounding, could CS be additional 
#confounding variable? We must separate out female & male data & check CS
Male_IOCT_Data=IOCT_Data%>%
  filter(sex=="M")
Female_IOCT_Data=IOCT_Data%>%
  filter(sex=="F")

#Male data colored based on CS Status
Male_IOCT_Data%>%  
  ggplot(aes(x = height, y=IOCT_Time, color=as.factor(CS)))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Male Heights", y="Male IOCT Times", color="CS Status")

#Female data colored based on CS Status
Female_IOCT_Data%>%  
  ggplot(aes(x = height, y=IOCT_Time, color=as.factor(CS)))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Female Heights", y="Female IOCT Times", color="CS Status")

#WORKSHEET R CODE INFORMATION#
#Question 9
Pi=0.6
n=500
Phat=320/500
SD=sqrt(Pi*(1-Pi)/n)
Z=(Phat-Pi)/SD
P_Value=1-pnorm(Z)

#Question 10
#library(tidyverse)
#setwd("C:/Users/marshall.titch/OneDrive - West Point/MA256 AY22-1/Lesson 07 Generationation (2 of 2)")
#data=read_csv("L07_IOCT_Data_Cleaned.csv")
data=IOCT_Data
data%>%
  ggplot(aes(x = su_score))+geom_histogram()+
  labs(x = "Situp Score (Points)", y = "Count", 
       title = "Situp Score in Points")
#Since the problem told us to assume the histogram was not strongly skewed
#and since we have at least 20 observations, then we meet validity conditions

#Question 11
#To get a 94% theory-based confidence interval, we must use example code from
#the course guide: qt(1-(Alpha/2), n-1). Given a 94% CI, Alpha = 0.06

data%>%
  summarise(n()) #n=384. We can also see this in the "Environment" section 
#of R Studio after reading in the data

Multiplier=qt(.97,383)

#Now calculate xbar, SD, and n. To use these values in the CI formula, I must
#complete my calculations inside the summarise function, or create new variables
#outside the summarise function to equal the values calculated inside summarise
data%>%
  summarise(SU_AVG=mean(su_score), SU_SD=sd(su_score), SU_Size=n(), 
            CI11Lower=SU_AVG - Multiplier*SU_SD/sqrt(SU_Size),
            CI11Upper=SU_AVG + Multiplier*SU_SD/sqrt(SU_Size))
#94% CI for su_score is from (89.1, 91.8) points.  

