 
#Clear the console: Ctrl+Alt+L

#Change home directory to the directory Data/DataAnalysis
setwd("~/Data/Logit")

#Change back to the home directory
#setwd("~")

#Install a package

#install.packages("foreign")
#install.packages("pastecs")

#load package
library(pastecs)

#Upload the file: Logit_Analysis_Kedar_Fall2016.csv
mydata<-read.csv(file="Logit_Analysis_combined.csv",head=TRUE,sep=",")

#Create an output file: Output_Logit_Analysis_Kedar_Fall2016.txt
#Write the result in the txt output file:
sink("Output_Logit_Analysis_Kedar_combined.txt",append=TRUE,split=TRUE)
#To write the file back to the console: sink()


#To Force R to give the output in decimal format instead of exponential
options("scipen"=100, "digits"=2)

#Descriptive Statistics
stat.desc(mydata)

#To paste the data in Excel or CSV file, copy and paste. Then #click on data and click on text To Columns. 
#Logistic Analysis: When the preditive/dependent variable is a binary variable (0,1), we can use it. We can make the behavior variable as over estimate and #under estimate with values 1 and 0 and analyze the dependence of it with other continuous or catarorical variables.
#Independent variables: actual grades, initial preparation level, gender, semester, and so on. In this case, the actual grade is a continuous variable, #others are categorical variables so that we have to use factor(variable) to change the variable into the categorical variable. 

#Behavior1: underestimate=0, overestimate=1
#Behavior2: understimate=1, overestimate=0 
Behavior1<-mydata$Behavior1;
Behavior2<-mydata$Behavior2;
#initialprep<-mydata$InitialPreparation;
Grade<-mydata$InstructorsGrade;
Gender<-factor(mydata$Gender);
#Semester<-factor(mydata$Semester);
#CourseLevel<-factor(mydata$CourseLevel);
Category<-factor(mydata$Category)
Required<-factor(mydata$Required)
FirstTime<-factor(mydata$FirstTime)
Preparation<-factor(mydata$Preparation)
PerceivedDiffLevel<-factor(mydata$PerceivedDiffLevel)
ExpGrade<-factor(mydata$ExpGrade)
TakeUpDiv<-factor(mydata$TakeUpDiv)
ActualCourseGrade<-factor(mydata$ActualCourseGrade)


mylogit1 <- glm(formula=Behavior1 ~ Grade+Gender+Category+Required+FirstTime+Preparation+PerceivedDiffLevel+ExpGrade+TakeUpDiv+ActualCourseGrade, family = "binomial");
summary(mylogit1)
#to find the odds ratio:
exp(coef(mylogit1))

mylogit2 <- glm(formula=Behavior2 ~ Grade+Gender+Category+Required+FirstTime+Preparation+PerceivedDiffLevel+ExpGrade+TakeUpDiv+ActualCourseGrade, family = "binomial");
summary(mylogit2)
#to find the odds ratio:
exp(coef(mylogit2))

#KK=1,NKK=2, KNK=3, NKNK=4, KBNKW=5, Others=6

#Next we only consider the variable with p value less than 0.05.
#Grade, Gender1, Category1, Catogery2, Category3, Category5, Required1,
mylogit3 <- glm(formula=Behavior2 ~ Grade+Gender+Category+Required, family = "binomial");
summary(mylogit2)
#to find the odds ratio:
exp(coef(mylogit2))
  