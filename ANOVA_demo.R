library(ggplot2)
library(car)
library(pastecs)

#########Wilcoxon

spider_data_long <- read.table("SpiderLong.dat", sep="\t", comment.char="", quote="", header=T)
head(spider_data_long)

###Non-parametric tests for ordinal/non-normal data

#Summary statistics again: you'll need medians for reporting the results
by(spider_data_long$Anxiety, spider_data_long$Group, stat.desc, basic=FALSE, norm=TRUE)

#you'll also need the interquartile ranges of your two samples
by(spider_data_long$Anxiety, spider_data_long$Group, IQR)

#Wilcoxon rank-sum test for independent samples

model1 <- wilcox.test(Anxiety~Group, data=spider_data_long, correct=FALSE, exact=FALSE); model1

#Effect size
# z = qnorm(model$p.value/2)
# N = number of data points
# r = z / sqrt(N)

qnorm(model1$p.value/2)/sqrt(dim(spider_data_long)[1])
#-0.31


#Wilcoxon signed-rank test for dependent samples

model2 <- wilcox.test(Anxiety~Group, data=spider_data_long, paired=TRUE, correct=FALSE, exact=FALSE); model2


#Effect size (calculate the same way)

qnorm(model2$p.value/2)/sqrt(dim(spider_data_long)[1])





#########ANOVA

###Load in data

ViagraData <- read.table("ViagraData.csv",sep="\t",header=TRUE)
head(ViagraData)


###Graph the data

ggplot(ViagraData, aes(Dose, Libido)) + geom_boxplot(notch=TRUE) + stat_summary(fun.y=mean, geom="point", colour="red")

###If we want summary statistics
by(ViagraData$Libido, ViagraData$Dose, stat.desc, basic=FALSE, norm=TRUE)


###Check assumptions

#Data points must be independent!

#Normality can be a problem if sample sizes are quite different. In such a case, there is an approach known as the Kruskall-Wallis test that you can use. If sample sizes are equal, don't worry about normality.

#Check Homogeneity of variance

leveneTest(ViagraData$Libido, ViagraData$Dose, center = "median")


###When doing a simple one-way ANOVA, we use the aov() function instead of lm() because (1) we don't necessarily want the dummy contrasts, and (2) we need sum of squares info to calculate an effect size

ViagraModel <- aov(Libido ~ Dose, data=ViagraData); summary(ViagraModel)

#but see the overall model results are the same:

summary(lm(Libido ~ Dose, data=ViagraData))
#We don't want the (bad) dummy contrasts...why is High Dose the reference level?


#Calculate an effect size for overall model
#Omega^2 = (SSm - DFm * MSr) / (SSm + SSr + MSr)

Omega2 = (20.13 - 2 * 1.967) / (20.13 + 23.6 + 1.967); Omega2
#0.354


###Planned Comparisons

levels(ViagraData$Dose)
#High Dose as baseline level doesn't make much sense

#Change the reference level
ViagraData$Dose <- relevel(ViagraData$Dose, "Placebo")

levels(ViagraData$Dose)


#First, let's try dummy contrasts. This is how we assign contrast variables to a nominal variable. We use contr.treatment function (treatment contrasts are any kind of contrast where all other means are compared to some level; default level is reference level, which is the same as dummy contrasts).
contrasts(ViagraData$Dose) <- contr.treatment(3)

contrasts(ViagraData$Dose)

ViagraModelPlanned <- aov(Libido ~ Dose, data=ViagraData); summary.lm(ViagraModelPlanned)

#Note that this is the same as:
summary(lm(Libido ~ Dose, data=ViagraData))


#Now, let's try our own planned contrasts that we saw in the lecture slides

levels(ViagraData$Dose)

contrast1 <- c(-2,1,1)   #placebo to High and Low
contrast2 <- c(0,1,-1)   #High to Low

contrasts(ViagraData$Dose) <- cbind(contrast1, contrast2)

contrasts(ViagraData$Dose)

ViagraModelPlanned2 <- aov(Libido ~ Dose, data=ViagraData); summary.lm(ViagraModelPlanned2)


#calculating effect size for contrasts
#r = sqrt(t^2 / (t^2 + df))
#df = num of data points - num of predictors (i.e., contrast variables) - 1

#contrast1
sqrt((2.474)^2 / ((2.474)^2 + 12))
#0.58

#contrast2
sqrt((2.029)^2 / ((2.029)^2 + 12))
#0.51



###If we didn't have hypotheses about which means should be different from one another, we could run post-hoc testing, which looks at every possible pairwise comparison between means. Let's use the Benjamini-Hochberg correction ("BH") instead of the more conservative and conventional Bonferroni correction.

pairwise.t.test(ViagraData$Libido, ViagraData$Dose, p.adjust.method="BH")
#Note higher p-value for high/low comparison



###Let's report the version with planned comparison
#Reporting: All significant values are reported at p < .05. There was a significant effect of dose on libido, F(2, 12) = 5.119, omega^2 = .354. Planned contrasts revealed that high dose and low dose lead to significantly higher libido than placebo, t(12) = 2.474, r = .58; furthermore, high dose lead to higher libido than low dose (t(12) = 2.029, r = .51); however, the effect was not significant (p = .0652).
