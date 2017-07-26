library(ggplot2)
library(car)
library(pastecs)
library(effects)
library(gvlma)




###Interaction between two interval/ratio predictors (multiple regression)

#Outcome variable is how crazy someone is
#Predictor 1: how many beers per week that person drinks
#Predictor 2: how many years they've been working at the university
crazy_data <-read.table("crazy_data.csv",sep="\t",header=TRUE,comment.char="",quote="")
head(crazy_data)

#We have added an interaction between two predictors using the ":"
crazy_model <- lm(craziness ~ beers_per_week + years_working_at_uni + beers_per_week:years_working_at_uni, data=crazy_data)

summary(crazy_model)

#Relationship between coefficients and prediction; how crazy are you after 10 beers when you've worked for 15 years?
# y = b0 + b1*beers_per_week + b2*years_working_at_uni + b3*beers_per_week*years_working_at_uni

1997.56 + 30.14*10 + 17.42*15 + -3.03*10*15

#Must drop an interaction before the main effects that make it up
drop1(crazy_model, test="F")


###Check Assumptions!
durbinWatsonTest(crazy_model)

#homoscedasticity normality, and high influence points
par(mfrow=c(2,2))
par(mar=c(4,4,4,4))

par(mfrow=c(1,1))
plot(crazy_model)


#No severe multicollinearity
vif(crazy_model)                #Don't worry about multicollinearity for interaction; it's going to be high since the interaction is a product of 2 predictors which were already included in the model as separate terms


###Graphing

plot(allEffects(crazy_model))

#Create vectors of beers_per_week and years_working_at_uni values
xgrid <-  seq(min(crazy_data$beers_per_week), max(crazy_data$beers_per_week), 0.5)
ygrid <-  seq(min(crazy_data$years_working_at_uni), max(crazy_data$years_working_at_uni), 0.5)

#create dataframe of all possible combinations of the values in these two vectors
all_combos <- expand.grid(beers_per_week=xgrid, years_working_at_uni=ygrid)
head(all_combos)

predicted_outcomes <- predict(crazy_model, newdata=all_combos)


#Put the predicted outcomes into the data frame as a new column
all_combos$craziness <- predicted_outcomes
head(all_combos)

#Now we plot!
ggplot(all_combos, aes(x=beers_per_week, y=years_working_at_uni, z=craziness)) + geom_tile(aes(fill = craziness)) + scale_fill_gradient(low="white", high="black") + labs(x="Beers per Week", y="Years Working at University")

#Note that main effects can't be interpreted independent of the interaction


#Reporting: The final model’s formula was craziness ~ beers_per_week + years_working_at_uni + beers_per_week:years_working_at_uni. All main effects and the two-way interaction were very significant: p<0.001. When years_working_at_uni was at its baseline level (i.e., 0), there was a positive relationship between beers_per_week and craziness. However, this relationship eventually reversed as years_working_at_uni increased. The model was highly significant overall (F(3,96)=36060, p<0.001) and achieved a high variance explanation (mult. R2=0.9991, adj. R2=0.9991). All regression coefficients, as well as their standard errors, t scores, and p-values, are provided in the appendix, and checking of model assumptions revealed no problems.





###Numerical + categorical predictor example
RTs <- read.table("RTs.csv", header=T, sep="\t", row.names=1) # </_inputfiles/05-2_reactiontimes.csv>
head(RTs)

#check levels; hi is ref level since it is alphabetically first
levels(RTs$FAMILIARITY)

#change default level to lo
RTs$FAMILIARITY <- relevel(RTs$FAMILIARITY, "lo"); levels(RTs$FAMILIARITY)

contrast1 <- c(-2, 1, 1)
contrast2 <- c(0, -1, 1)

contrasts(RTs$FAMILIARITY) <- cbind(contrast1,contrast2)
contrasts(RTs$FAMILIARITY)

#        contrast1 contrast2
#lo         -2         0
#hi          1         1
#med         1        -1


rt_model_planned <- lm(RT ~ FREQUENCY + FAMILIARITY + FREQUENCY:FAMILIARITY, data=RTs)
summary(rt_model_planned)


#How do we use the coefficients to make predictions about the outcome variable?
#644.244 + -16.091*FREQUENCY + -26.859*contrast1 ... 8.320*FREQUENCY*contrast1


#What does the p-value of the coefficient (beta) of contrast 1 tell us?


#What does the p-value of the coefficient (beta) of FREQUENCY:FAMILIARITYcontrast1 tell us?

plot(allEffects(rt_model_planned))


#Model selection
drop1(rt_model_planned, test="F")

rt_model_planned_2 <-  lm(RT ~ FREQUENCY + FAMILIARITY, data=RTs)
summary(rt_model_planned_2)

plot(allEffects(rt_model_planned_2))

drop1(rt_model_planned_2, test="F")

#Remember that when we used dummy contrasts, we violated the multicollinearity assumption here, which could inflate our standard errors. Let's check it again.
vif(rt_model_planned_2)

#And we can get statistics for the whole FAMILIARITY predictor:
Anova(rt_model_planned_2, type="III")


#Reporting: The final model’s formula was RT ~ FREQUENCY + FAMILIARITY. The overall effect of FAMILIARITY was significant (p=0.015). However, the only planned contrast that was significant was between the low familiarity condition and the medium and high familiarity conditions (p=0.004). Specifically, reaction times in the medium and high familiarity conditions were lower than in the low condition. While the reaction times in the medium condition were higher than in the high condition, this difference was not significant (p=0.52). There was also a marginally significant effect of FREQUENCY (p=0.051); as Frequency increased, reaction times decreased. Finally, the model was significant overall (F(3,51)=6.959, p<0.001) but did not explain a large amount of variance (mult. R2=0.2904, adj. R2=0.2487). All regression coefficients, as well as their standard errors, t scores, and p-values, are provided in the appendix. In addition, the appendix includes the results of an F test to investigate the overall effect of the FAMILIARITY predictor. Checking of model assumptions revealed no problems.





###2 Binary predictors

Goggles <- read.table("goggles.csv", sep=",", header=T, comment.char="", quote="")
head(Goggles)

Goggles <- subset(Goggles, alcohol!="2 Pints")

levels(Goggles$alcohol) <- c("4 pints","4 pints","none")

Goggles$alcohol <- relevel(Goggles$alcohol, "none")

goggles_model <- lm(attractiveness ~ gender + alcohol + gender:alcohol, data=Goggles) 

summary(goggles_model)

#By now, we've talked about contrasts several times
#Contrasts are just numbers we assign to levels of our categorical predictors (female = 0, male = 1)
#They allow us to place our non-numerical categories in a numerical multi-dimensional space
#We want to do this so we can connect our categories' means with lines
#The slopes of these lines tell us about how different the means are
#
#Different kinds: dummy, custom
#R uses dummy contrasts by default--THIS IS FINE FOR FINAL ASSIGNMENT
#With dummy contrasts, REFERENCE CATEGORY is at position 0 and each non-reference category is at position 1 on its own axis
#Thus, every slope (coefficient) of a main effect represents the difference between the reference category mean and the mean of that category, when the other main effects are at their reference levels as well
#
#Intercept: all predictors at their reference level
#genderMale: slope (difference) from female to male WHEN ALCOHOL AT ITS REFERENCE LEVEL
#alcohol4 pints: slope (difference) from none to 4 pints when ALCOHOL AT ITS REFERENCE LEVEL

#Well what about difference from female to male when alcohol = 4 pints?
#Or, what about difference from none to 4 pints when gender = male?

#If there is not a significant interaction, then there is no difference in the slope
#If there is a significant interaction, then the slope must be adjusted by the value of the interaction coefficient


plot(allEffects(goggles_model))

#Note that our x-axis is gender and our grouping variable is alcohol
#In the left panel, the line represents the difference between males and females when alcohol=none (this corresponds to 6.250 in the table)
#In the right panel, the line represents the difference between females and males when alcohol=4 pints. We don't get a coefficient for this in our table. Rather, to get this line's slope, we subtract:
6.250-28.125
#-21.875


#This is a really easy plot to generate and gives you lots of information, such as interactions. 
#However, it arbitrarily chooses an x-axis variable and a grouping variable. 
#But the interaction coefficient is an adjustment on both main effects. 
#Thus, the difference between no alcohol and 4 pints when gender=male would be: 
-3.125-28.125
#-31.25


#It's easier to see this relationship if we put alcohol on the x-axis and use gender as the grouping variable

ggplot(Goggles, aes(alcohol, attractiveness)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line", aes(group=1)) + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, colour="Blue") + facet_wrap(~gender)


#means for each combination of factor levels
by(Goggles$attractiveness, list(Goggles$gender, Goggles$alcohol), stat.desc)
#female none: 60.625
#male none: 66.875
#female 4 pints: 57.5
#male 4 pints: 35.625

35.625-66.875

35.625-57.5



#Problem with dummy contrasts: Significance of coefficients can change depending on what categories you use as your reference categories.


drop1(goggles_model, test="F")

#Can't drop anything, but if we were going to, we would remove the interaction
goggles_model_2 <- lm(attractiveness ~ gender + alcohol, data=Goggles); summary(goggles_model_2)

drop1(goggles_model_2, test="F")


#The final model’s formula was attractiveness ~ gender + alcohol + gender:alcohol. The main effects were not sigificant (p>0.5), though the interaction between gender and alcohol was significant (p<0.001). Specifically, compared to females, males showed a significant decrease in the attractiveness of the people they talked to as they moved from no beers to 4 beers. Put another way, compared to no beers, attractiveness of the people talked to when 4 beers were consumed decreased from females to males. The model was highly significant overall (F(2,29)=11.26, p<0.001, mult. R2=0.4371, adj. R2=0.3983). All regression coefficients, as well as their standard errors, t scores, and p-values, are provided in the appendix. Checking of model assumptions revealed no problems.











###Nominal Predictor + Binary Predictor

Goggles <- read.table("goggles.csv", sep=",", header=T, comment.char="", quote="")
head(Goggles)

levels(Goggles$alcohol) #2 pints is the baseline; switch to None
Goggles$alcohol <- relevel(Goggles$alcohol, "None"); levels(Goggles$alcohol)

levels(Goggles$gender) #Female is the reference level; this is fine


###Dummy contrasts
goggles_model <- lm(attractiveness ~ gender + alcohol + gender:alcohol, data=Goggles)
summary(goggles_model)

plot(allEffects(goggles_model))



#None versus 2 pints interaction plots
Goggles_2p <- subset(Goggles, alcohol!="4 Pints")

line_plot <- ggplot(Goggles_2p, aes(alcohol, attractiveness, colour=gender)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line", aes(group=gender)) + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, aes(group=gender)); line_plot

#Can also place gender on the x-axis and group by amount of alcohol
line_plot <- ggplot(Goggles_2p, aes(gender, attractiveness, colour=alcohol)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line", aes(group=alcohol)) + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, aes(group=alcohol)); line_plot

#None versus 4 pints interaction plots
Goggles_4p <- subset(Goggles, alcohol!="2 Pints")

line_plot <- ggplot(Goggles_4p, aes(alcohol, attractiveness, colour=gender)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line", aes(group=gender)) + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, aes(group=gender)); line_plot


###Homogeneity of variance assumption
leveneTest(Goggles$attractiveness, interaction(Goggles$alcohol, Goggles$gender), center=median)

###Model selection
drop1(goggles_model, test="F")

###Overall p-values for the predictors
Anova(goggles_model, type="III")

###Reporting for dummy contrasts
#The final model’s formula was attractiveness ~ gender + alcohol + gender:alcohol. The main effects were not sigificant (p>0.5), though the interaction between gender and alcohol was significant (p<0.001). Planned contrasts revealed no significant differences between consuming no beer and 2 pints, and between consuming no beer and 4 pints. There was also no significant interaction between gender and consuming no beer versus 2 beers. However, there was a significant interaction between gender and consuming no beers versus 4 beers; specifically, compared to females, males showed a significant decrease in the attractiveness of the people they talked to as they moved from no beers to 4 beers. The model was highly significant overall (F(5,42)=13.2, p<0.001) and achieved a high variance explanation (mult. R2=0.6111, adj. R2=0.5648). All regression coefficients, as well as their standard errors, t scores, and p-values, are provided in the appendix. In addition, the appendix includes the results of F tests to investigate the overall effects of the 2 predictors. Checking of model assumptions revealed no problems.

###Planned contrasts
F_vs_M <- c(-1, 1)
contrasts(Goggles$gender) <- cbind(F_vs_M); contrasts(Goggles$gender)

None_vs_two_and_four <- c(-2, 1, 1)
two_vs_four <- c(0, -1, 1)
contrasts(Goggles$alcohol) <- cbind(None_vs_two_and_four, two_vs_four); contrasts(Goggles$alcohol)

goggles_model_planned <- lm(attractiveness ~ gender + alcohol + gender:alcohol, data=Goggles)
summary(goggles_model_planned)

                      


#Linear model equation
#58.33 + -1.875*F_vs_M + -2.708*None_vs_two_and_four + -9.062*two_vs_four + -2.5*F_vs_M*None_vs_two_and_four + -6.562*F_vs_M*two_vs_four

#How do we actually interpret the coefficients of interactions?
#-2.5*F_vs_M*None_vs_two_and_four

#Female None
-2.5*-1*-2

#Male None
-2.5*1*-2

#Female 2 pints
-2.5*-1*1

#Male 2 pints
-2.5*1*1

###In sum, interaction coefficients can be very hard to interpret just by looking at them! They are NOT slopes! Also helpful to look at plots and means!


###Plots

#Create a new factor corresponding to "None" vs "Alcohol"
head(Goggles)
Goggles$None_vs_alc <- rep("None",dim(Goggles)[1])
indexes <- which(Goggles$alcohol!="None")
Goggles$None_vs_alc[indexes] <- "Alcohol"
Goggles$None_vs_alc <- factor(Goggles$None_vs_alc, levels=c("None", "Alcohol"))

#F_vs_M:None_vs_two_and_four
line_plot <- ggplot(Goggles, aes(None_vs_alc, attractiveness)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line", aes(group=1)) + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2); line_plot

line_plot + stat_summary(fun.y=mean, geom="point", aes(group=gender, colour=gender)) + stat_summary(fun.y=mean, geom="line", aes(group=gender, colour=gender)) + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, aes(group=gender, colour=gender))


###F_vs_M:two_vs_four

Goggles_alc <- subset(Goggles, alcohol!="None")
head(Goggles_alc)

line_plot_2 <- ggplot(Goggles_alc, aes(alcohol, attractiveness)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line", aes(group=1)) + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2); line_plot_2

line_plot_2 + stat_summary(fun.y=mean, geom="point", aes(group=gender, colour=gender)) + stat_summary(fun.y=mean, geom="line", aes(group=gender, colour=gender)) + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2, aes(group=gender, colour=gender))

by(Goggles$attractiveness, Goggles$gender, stat.desc)
#Female mean: 60.2083
#Male mean: 56.4583

by(Goggles$attractiveness, Goggles$alcohol, stat.desc)
#None mean: 63.75
#2 pints mean: 64.6875
#4 pints mean: 46.5625

#Helpful to have the mean of 2 and 4 pint conditions since the None_vs_two_and_four contrast collapses these two levels
(64.6875 + 46.5625)/2
#55.625

by(Goggles$attractiveness, list(Goggles$gender, Goggles$alcohol), stat.desc)
#Female 2 pints: 62.5
#Female 4 pints: 57.5
#Female 2 and 4 pints:
(62.5+57.5)/2
#60

#Male 2 pints: 66.875
#Male 4 pints: 35.625
#Male 2 and 4 pints:
(66.875+35.625)/2
#51.25

#Female None: 60.625
#Male None: 66.875

#Get p-values for whole predictors; note t test results are same as f test results for a single degree of freedom
Anova(goggles_model_planned, type="III")

#Reporting
#The final model’s formula was attractiveness ~ gender + alcohol + gender:alcohol. The main effect of gender was not sigificant (p>0.05) while the main effect of alcohol was significant (p<0.001). Also, the interaction between gender and alcohol was significant (p<0.001). Planned contrasts revealed significant differences between consuming no beer versus consuming 2 or 4 pints, as well as between consuming 2 versus 4 pints. In both cases, drinking more beer led to a decrease in attractiveness of the person being talked to. 

#However, these main effects must be interpreted in the context of significant interactions between both of these effects and the effect of gender. Specifically, for the contrast between no alcohol and 2 or 4 pints, the negative trend becomes more severe for men, but less severe for women. In fact, examining the interaction plots and confidence intervals suggests that, for females, the negative trend disappears altogether.

#For the contrast between 2 a 4 pints, again the negative trend becomes more severe for men and less severe for females. And again, the overlap between the confidence intervals for females in the 2 pint and 4 pint conditions suggests that the negative trend is not even reliable.

#Finally, the model overall was significant (F(5,42)=13.2, p<.001) and achieved a high level of variance explanation (multiple r2=0.6111, adjusted r2=0.5648). All regression coefficients, as well as their standard errors, t scores, and p-values, are provided in the appendix. In addition, the appendix includes the results of F tests to investigate the overall effects of the 2 predictors, as well as all the means under comparison. Checking of model assumptions revealed no problems.








###Random Intercepts

PressureData <- read.table("BloodPressure.csv", sep="\t", header=T, comment.char="", quote="")
head(PressureData)
#Subjects measured their blood pressure during the week leading up to an election.


ggplot(PressureData, aes(Hour, BloodPressure)) + geom_point() + geom_smooth(method="lm")

#What is the slope and intercept of the line of best fit?
summary(lm(BloodPressure~Hour, data=PressureData))


#Problem: violation of independence assumption! (each subject provides multiple data points)
#Therefore, variance is shared across datapoints; some people may just have higher/lower baseline blood pressure.
#To account for this, we can give each subject their own intercept (and thus their own regression line).


#Run a mixed model with random intercepts
library(lme4)
library(lmerTest)

m2 <- lmer(BloodPressure ~ Hour + (1|Subject), data=PressureData)
summary(m2)

#Calculate r2
library(MuMIn)
r.squaredGLMM(m2)

#you can get the random intercepts like this
coef(m2)

ggplot(PressureData, aes(Hour, BloodPressure, color=Subject)) + geom_point() + geom_smooth(method="lm") + geom_abline(intercept=65.72, slope=.59, color="red") + geom_abline(intercept=89.86, slope=.59, color="red") + geom_abline(intercept=101.13, slope=.59, color="red") + geom_abline(intercept=156.29, slope=.59, color="red") + geom_abline(intercept=128.95, slope=.59, color="red") + geom_abline(intercept=108.39, slope=.59, color="green")


#Can you only use random intercepts for subjects? NO!
#items are often also treated as a random effect
#e.g., 20 pictures that participants must look at in an MRI experiment

