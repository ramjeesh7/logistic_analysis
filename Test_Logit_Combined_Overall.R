setwd("~/Data/Test")
library(aod)
library(ggplot2)
mydata<-read.csv(file="Test_Logit_Combined_Overall.csv",head=TRUE,sep=",");
#mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)
summary(mydata)
sapply(mydata,sd)
xtabs(~Estimate + CourseLevel, data = mydata)
fix(mydata)
mydata$Semester <- factor(mydata$Semester)
mydata$CourseLevel <- factor(mydata$CourseLevel)
mydata$Gender <- factor(mydata$Gender)
is.factor(mydata$Semester)
contrasts(mydata$Semester)


mylogit1 <- glm(Estimate ~ InstructorsGrade + Semester + CourseLevel+Gender, data = mydata, family = "binomial")
summary(mylogit1)
confint(mylogit1)
confint.default(mylogit1)
wald.test(b = coef(mylogit1), Sigma = vcov(mylogit1), Terms = 4:6)
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit1), Sigma = vcov(mylogit1), L = l)
exp(coef(mylogit1))
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
newdata1 <- with(mydata, data.frame(InstructorsGrade = mean(InstructorsGrade), Semester = factor(1:2), CourseLevel = factor(1:2), Gender=factor(0:1)))
newdata1
newdata1$CourseLevelP <- predict(mylogit1, newdata = newdata1, type = "response")
newdata1
newdata2 <- with(mydata, data.frame(InstructorsGrade = rep(seq(from = 200, to = 800, length.out = 100),
    4), Semester=factor(1:2),CourseLevel = factor(rep(1:1, each = 100), Gender=factor(0:1))))
newdata3 <- cbind(newdata2, predict(mylogit1, newdata = newdata2, type = "link",
    se = TRUE))
newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})
head(newdata3)
ggplot(newdata3, aes(x = InstructorsGrade, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
    ymax = UL, fill = CourseLevel), alpha = 0.2) + geom_line(aes(colour = CourseLevel),
    size = 1)
