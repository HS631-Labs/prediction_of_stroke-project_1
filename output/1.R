install.packages("caTools")
install.packages("ggpubr")
install.packages("ggcorrplot")
library(plotly)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(dplyr)
library(VennDiagram)
library(ggbeeswarm)
library(pwr)
library(psych)
library(pastecs)
library(ggm)
library(Hmisc)
library(pwr)
library(car)
library(caTools)
library("MASS")


#Project Description: 
#

# Aim to predict if the patient will have stroke
#Does the patient experience stroke
# Null Hypothesis: Patients will not have stroke
#Alternative hypothesis: Patients will have stroke 
#Methodology: Use Multiple logistic regression to predict
#Dependent variable: Stroke
#Independent variables: 10 independent variables
#outcome variable: Stroke
#Quantitative variables: age, avg glucose level, bmi, stroke, sus BP, diaBP, BMI, on admit heart rate
#Categorical: gender, hypertension, heart disease, ever married, work type, residence type, smoking status


#Chunk 1: Read the stroke_dataset file and check for NA's
Stroke <- read.csv("Stroke_dataset.csv",header = TRUE, sep = ",")
any(is.na(Stroke))
Stroke[is.na(Stroke)] <- 0
summary(Stroke)
str(Stroke)


#Chunk 2: Replace the NA values
Stroke <- Stroke[,-15]

Stroke$bmi[Stroke$bmi == 0] <- runif(sum(Stroke$bmi == 0), min = 22.9, max = 32.8)

#Chunk 3: 
Stroke[Stroke$gender == "other",]$gender <- NA
summary(Stroke$gender)
Stroke <- Stroke[!(is.na(Stroke$gender)),]
summary(Stroke$gender)

#Chunk 4: Converting stroke data to Binomial variables
Stroke$gender <- as.factor(Stroke$gender)
Stroke$ever_married <- as.factor(Stroke$ever_married)
Stroke$work_type <- as.factor(Stroke$work_type)
Stroke$Residence_type <- as.factor(Stroke$Residence_type)
Stroke$smoking_status <- as.factor(Stroke$smoking_status)
Stroke$hypertension <- as.factor(Stroke$hypertension)
Stroke$heart_disease <- as.factor(Stroke$heart_disease)
Stroke$stroke <- as.factor(Stroke$stroke)

summary(Stroke)

#Chunk 5: Plotting the variables
#Plot the axis
plot(Stroke$hypertension,Stroke$age)
plot(Stroke$heart_disease,Stroke$age)
plot(Stroke$stroke,Stroke$age)
plot(Stroke$stroke,Stroke$OnAdmit_heartRate)

#plotting for all continuous variables
ggplot(data=Stroke,aes(x=age)) + geom_density()
ggplot(data=Stroke,aes(x=bmi)) + geom_density() #####
ggplot(data=Stroke,aes(x=avg_glucose_level)) + geom_density()####
ggplot(data=Stroke,aes(x=sysBP)) + geom_density()
ggplot(data=Stroke,aes(x=diaBP)) + geom_density()
ggplot(data=Stroke,aes(x=OnAdmit_heartRate)) + geom_density()

#plotting for combination nominal variables (smoking staus, bmi)
ggplot(data=Stroke,aes(x=smoking_status,fill=stroke)) + geom_histogram(stat="count")
ggplot(data=Stroke,aes(x=bmi,fill=stroke)) + geom_density()




#Chunk 6: Log Transformation
Stroke$logbmi <- log(Stroke$bmi)
ggplot(data=Stroke,aes(x=logbmi)) + geom_density()

Stroke$logglucose <- log(Stroke$avg_glucose_level-45)####iterated to 45 for alteast normal distribution
ggplot(data=Stroke,aes(x=logglucose)) + geom_density()

#Chunk 7: Splitting the data to Train data and test data as the sample size is high

split <- sample.split(Stroke$stroke, SplitRatio = 0.8)
TrainStroke <- subset(Stroke,split==TRUE)
TestStroke <- subset(Stroke,split==FALSE)

stat.desc(TrainStroke, basic = FALSE, norm = TRUE)
stat.desc(TestStroke, basic = FALSE, norm = TRUE)

#Chunk 8: Identifying correlation
corr = round(cor(Stroke[,c(3,9,10,13,14,15,16,17)]),2) #rounding to 2 decimal
corr
#none correlated
ggcorrplot(corr,hc.order=TRUE)

#Chunk 9: Applying Logistic regression

strokepredict <- glm(stroke ~ gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi + smoking_status + sysBP + diaBP + OnAdmit_heartRate + logbmi + logglucose, data = Stroke, family="binomial")
summary(strokepredict)

exp(coef(strokepredict))

##
ll.null <- strokepredict$null.deviance/-2
ll.proposed <- strokepredict$deviance/-2
(ll.null-ll.proposed)/ll.null

1-pchisq(2*(ll.proposed-ll.null),df=(length(strokepredict$coefficients)-1))

predicted.data <- data.frame(probability.of.stroke=strokepredict$fitted.values,stroke = Stroke$stroke)
predicted.data <- predicted.data[order(predicted.data$probability.of.stroke,decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
ggplot(data=predicted.data,aes(x=rank,y=probability.of.stroke))+geom_point(aes(color=stroke),alpha=1,shape=4,stroke=2)

Newstrokepredict <- stepAIC (strokepredict)

ll.null <- Newstrokepredict$null.deviance/-2
ll.proposed <- Newstrokepredict$deviance/-2
(ll.null-ll.proposed)/ll.null

1-pchisq(2*(ll.proposed-ll.null),df=(length(Newstrokepredict$coefficients)-1))

predicted.data1 <- data.frame(probability.of.stroke=Newstrokepredict$fitted.values,stroke = Stroke$stroke)
predicted.data1 <- predicted.data1[order(predicted.data1$probability.of.stroke,decreasing=FALSE),]
predicted.data1$rank <- 1:nrow(predicted.data1)
ggplot(data=predicted.data1,aes(x=rank,y=probability.of.stroke))+geom_point(aes(color=stroke),alpha=1,shape=4,stroke=2)
summary(Newstrokepredict)

ResultStroke <- predict(strokepredict,TestStroke,type="response")

confmatrix <- table(Actualvalue=TestStroke$stroke, Predictedvalue = ResultStroke >0.5)
confmatrix

(confmatrix[[1,1]] + confmatrix[[2,2]])/sum(confmatrix)

ResultStroke <- predict(Newstrokepredict,TestStroke,type="response")

confmatrix <- table(Actualvalue=TestStroke$stroke, Predictedvalue = ResultStroke >0.5)
confmatrix

round (exp(coef(Newstrokepredict)),3)
