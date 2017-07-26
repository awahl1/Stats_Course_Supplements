library(ggplot2)

###Covariance Examples


#                         Person 1    Person 2    Person 3
#AGE                      5           7           8
#HAGELSLAG CONSUMPTION    24          28          31

n <- 3

age <- c(5,7,8); mean(age)
hc <- c(24,28,31); mean(hc)

age_deviations <- age - mean(age); age_deviations
hc_deviations <- hc - mean(hc); hc_deviations

covariance <- sum((age_deviations*hc_deviations))/(n-1); covariance

cov(age,hc)


#                         Person 1    Person 2    Person 3
#AGE                      5           8           7
#HAGELSLAG CONSUMPTION    24          28          31

n <- 3

age <- c(5,8,7); mean(age)
hc <- c(24,28,31); mean(hc)

age_deviations <- age - mean(age); age_deviations
hc_deviations <- hc - mean(hc); hc_deviations

covariance <- sum((age_deviations*hc_deviations))/(n-1); covariance


#THE HIGHEST COVARIANCE YOU CAN GET FOR A GIVEN DATA SET X VALUES AND Y VALUES IS IF THE SORTED ORDER OF DATAPOINTS IS THE SAME FOR BOTH X AND Y



###Pearson's r correlation coefficient

covariance/(sd(age)*sd(hc))

cor(age,hc)

cor(age,hc)^2 * 100


#Load in Exam Anxiety data
exam = read.table("Exam Anxiety.dat", sep="\t", header=TRUE, comment.char="", quote="")
head(exam)
#103 students, each on 1 row
#Code: unique identifier for each student
#Exam: exam performance as percentage
#Revise: time spent reviewing for the exam
#Anxiety: anxiety score out of 100
#Gender

#subset for columns 2 through 4 (interval/ratio variables)
cor(exam[,2:4])

ggplot(exam, aes(Anxiety, Exam)) + geom_point()
ggplot(exam, aes(Exam, Revise)) + geom_point()
ggplot(exam, aes(Anxiety, Revise)) + geom_point()

#What if there's a missing value?
exam_with_NA <- exam
exam_with_NA$Anxiety[1:10] <- NA
head(exam_with_NA,10)

#correlation cannot be performed for certain rows between Anxiety and other variables
cor(exam_with_NA[,2:4])

cor(exam_with_NA[,2:4], use="complete.obs")            #Rows 1-10 not included at all
cor(exam_with_NA[,2:4], use="pairwise.complete.obs")   #Rows 1-10 not included in Anxiety correlations




#How to calculate whether a correlation is statistically significant
cor.test(exam$Anxiety, exam$Exam)      #This is for a two-tailed test

#Note that the p-value is less that .05, so the correlation is significant!
#Note the confidence intervals and r value

#If your alternative hypothesis is that the correlation will be positive, you can perform a one-tailed test in the positive direction
cor.test(exam$Exam, exam$Revise, alternative="greater")


#If your alternative hypothesis is that the correlation will be negative, you can perform a one-tailed test in the negative direction
cor.test(exam$Anxiety, exam$Revise, alternative="less")


#What happens if you hypothesize a positive correlation but the data reveals a negative correlation?
cor.test(exam$Anxiety, exam$Revise, alternative="greater")


#What happens if there is missing data?
cor.test(exam_with_NA$Anxiety, exam_with_NA$Revise)       #Automatically excluded missing data


#If you want to run multiple significance tests simultaneously, check out rcorr() from the Hmisc package


###Partial Correlations
library(ggm)

exam2 <- exam[,2:4]

pc <- pcor(c("Exam","Anxiety","Revise"), var(exam2)); pc

#Get the R2 value for the partial correlation
pc^2

#Run a significance test for the partial correlation
pcor.test(pc, 1, 103)
