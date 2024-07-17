#2

#(a)	Display the data with a boxplot and answer the question by running an ANOVA. 
#Evaluate the validity of the assumptions based on the graphics, and report your conclusions.

install.packages("Sleuth3")
library(Sleuth3)
ex0523
data<-ex0523

colnames(data)
summary(data)

boxplot(data$Oxygen~data$Bone)

output <- aov(data$Oxygen~data$Bone, data=data)
output
summary(output)

plot(output$fitted.values, output$residuals)
abline(h=0)

  #We have the assumptions of independece (no reason to contradict), 
  #equal variance (the sample is samll the the different whisker lengths make sense), 
  #and normality (the residual vs. fitted value plot shows an equal spread) 
  #so the assumption holds. 
  #The p-value is really small, at a value of 9.73e-07, so the these is strong evidence 
  #that the bones' oxygen population means are likely to be significantly different. 

#(b)	Create a subset of the data that contains only bone 1 and bone 2. 
#How many rows does it have? Use this subset of the data for the rest of 
#this problem.

bone_12<-data[data$Bone=="Bone1"|data$Bone=="Bone2",]
bone_12

#(c)	Working with the bone 1 and 2 subset, calculate the total sum 
#of squares for all oxygen values (from the equal-means model).

y <- bone_12$Oxygen
ymean <- mean(y)
sst <- sum((y - ymean)^2) 
sst
  #sst = 0.2346

#(d)	Calculate the within-group sum of squares for oxygen,
#where the groups are bone 1 and bone 2 (from the separate means model).

mean_bone1 <- mean(bone_12$Oxygen[bone_12$Bone == "Bone1"])
mean_bone2 <- mean(bone_12$Oxygen[bone_12$Bone == "Bone2"])

ssw_bone1 <- sum((bone_12$Oxygen[bone_12$Bone == "Bone1"] - mean_bone1)^2)
ssw_bone2 <- sum((bone_12$Oxygen[bone_12$Bone == "Bone2"] - mean_bone2)^2)

ssw <- ssw_bone1 + ssw_bone2

ssw

  #ssw=0.165

#(e)	Calculate the mean oxygen for bone 1 and the mean oxygen for 
#bone 2, and use these numbers to calculate the between-group sum of 
#squares. Verify that the within-group sum of squares and the 
#between-group sum of squares add up to the total sum of squares.

mean_oxygen <- mean(bone_12$Oxygen)

n_bone1 <- length(bone_12$Oxygen[bone_12$Bone == "Bone1"])
n_bone2 <- length(bone_12$Oxygen[bone_12$Bone == "Bone2"])
ssb <- n_bone1 * (mean_bone1 - mean_oxygen)^2 + n_bone2 * (mean_bone2 - mean_oxygen)^2
ssb
  #ssb=0.0697

#(f)	Use R to run an ANOVA F-test for the null hypothesis that 
#these two bones have the same mean oxygen.

anova_result <- aov(bone_12$Oxygen ~ bone_12$Bone)
anova_result

output_12 <- aov(bone_12$Oxygen~bone_12$Bone)
output_12
summary(output_12)
  #The p-value is relatively large at 0.206. 

#3

#(a)	Return to the ANES data set. Consider the variables ftfem and gehappy. 
# For each of these variables, specify the number of missing values.

anes <- read.csv("anes2022.csv")
head(anes)
colnames(anes)
anes$ftfem
summary(anes$ftfem)
table(anes$ftfem)
anes$gehappy
table(anes$gehappy)
anes$gehappy
boxplot(anes$gehappy, anes$ftfem)

library(tidyverse)

anes1<-anes %>%
  select("gehappy", "ftfem")


anes$anesCleaned <- NA
anes$anesCleaned[anes$gehappy==-7]<-NA
anes$anesCleaned[anes$gehappy==1]<-"Not at all"
anes$anesCleaned[anes$gehappy==2]<-"A little"
anes$anesCleaned[anes$gehappy==3]<-"Somewhat"
anes$anesCleaned[anes$gehappy==4]<-"Very"
anes$anesCleaned[anes$gehappy==5]<-"Extremely"
print(anes$anesCleaned)

summary(anes$anesCleaned)
sum(is.na(anes$anesCleaned))
#6  

anes$ftfem
