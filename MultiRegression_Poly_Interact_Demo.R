library(ggplot2)
library(car)
library(stats)
library(car)

###Fitting a polynomial curve

sales_data <- read.table("sales_over_times.csv",sep="\t",header=T,comment.char="",quote="")
head(sales_data)

#Note how the smoother, which is a linear regression, doesn't fit the curvature in the data
ggplot(sales_data, aes(TIME,SALES)) + geom_point() + geom_smooth(method="lm")

#Linear regression shows that 92.6% of the variance is explained
model_1 <- lm(SALES~TIME, data=sales_data)
summary(model_1)

#We can wrap our predictor in the poly() function and specify a power. With a power of 2, the lm() function will fit a parabola (curve) to the data instead of a straight line.
model_2 <- lm(SALES~poly(TIME,2), data=sales_data)


#Note that explained variance has increased to 96.55%
summary(model_2)



#We can use the predict() function to predict new values of y (SALES) for particular values of x (TIME), using our model
predict(model_2, newdata=data.frame(TIME=c(10,20,30)))

#We can also use it to get all predicted SALES values for every value of TIME in our data frame.
sales_data$PREDICTED_SALES <- predict(model_2)
head(sales_data)

#Then, we can use these predicted y values to plot a curved regression line that represents the model
ggplot(sales_data, aes(TIME, SALES)) + geom_point() + geom_smooth(method="lm") + geom_line(aes(TIME,PREDICTED_SALES), colour="red")

#Note that you can choose other powers for your polynomial besides just 2! You can also fit a cubic function (3rd power) to your data. Generally, the power you want is 1 + the number of curves in your data.



###Multiple Regression

#How many albums are sold as a function of various predictors
album_data <- read.table("Album Sales 2.dat", sep="\t", header=TRUE, comment.char="", quote="")
head(album_data)

album_model_1 <- lm(sales~adverts + airplay, data=album_data)
summary(album_model_1)

#betas still tell us slopes of our predictors (how much each predictor affects outcome...if other predictor[s] are held constant)
#all betas are significant, meaning they are significantly different from 0
#the overall model is significant
#only a slight difference between multiple and adjusted R^2

#we can verify that betas are significantly different from 0 by looking at their confidence intervals:
confint(album_model_1)                     #95% confidence intervals do not include 0 for b0, b1, or b2

#Standard testing of assumptions
durbinWatsonTest(album_model_1)
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
plot(album_model_1)
par(mfrow=c(1,1))

#Now test for multicollinearity too!
vif(album_model_1)                    #Values not greater than 10; average not substantially greater than 1
#if we did see problems, we could use cor() to figure out which combination of predictors was collinear


#Visualizing the different "effects" (the regression with respect to different predictors)
library(effects)
plot(allEffects(album_model_1))


#If you have only 2 predictors, you can visualize the regression surface from a "bird's eye view" (sort of like looking at an elevation map). First, we need a bunch of outcome values predicted by the model as a function of different combinations of values along the two predictors. Let's create a bunch of values along the adverts axis and along the airplay axis. Choose increments that will give you around 40 to 60 points.
xgrid <-  seq(min(album_data$adverts), max(album_data$adverts), 50)
ygrid <-  seq(min(album_data$airplay), max(album_data$airplay), 1)

#Now we create a data frame that enumerates every possible combination of the two ranges we just created.
all_combos <- expand.grid(adverts=xgrid, airplay=ygrid)
head(all_combos)

#Next, using the model, we predict outcomes (sales) based on the values of adverts and airplay that we just created
predicted_outcomes <- predict(album_model_1, newdata=all_combos)

#Put the predicted outcomes into the data frame as a new column
all_combos$sales <- predicted_outcomes
head(all_combos)

#Now we plot!
ggplot(all_combos, aes(x=adverts, y=airplay, z=sales)) + geom_tile(aes(fill = sales)) 


#Grayscale better for publications
ggplot(all_combos, aes(x=adverts, y=airplay, z=sales)) + geom_tile(aes(fill = sales)) + scale_fill_gradient(low="white", high="black")



###Model selection

#Create some "stupid" predictors that don't correlate with our outcome: temperature on the day the album was released and the angle of Venus above the horizon (obviously, these shouldn't predict album sales. We're just using them to demonstrate the model selection process.)
temp_day_release <- sample(seq(5,40), dim(album_data)[1], replace=TRUE)
album_data$temp_day_release <- temp_day_release
plot(temp_day_release,album_data$sales)

angle_venus_above_horiz <- sample(seq(1,90), dim(album_data)[1], replace=TRUE)
album_data$angle_venus_above_horiz <- angle_venus_above_horiz
plot(angle_venus_above_horiz,album_data$sales)

head(album_data)

#Build the full model with all predictors.
album_model_10 <- lm(sales~adverts + airplay + attract + temp_day_release + angle_venus_above_horiz, data=album_data)
summary(album_model_10)

#Use drop1 to check for which predictors, if any, we can drop.
drop1(album_model_10, test="F")
#first row that says <none> provides AIC of current model
#subsequent rows indicate the AIC of different models if you were to drop different predictors
#We want to look for the predictor that (1) shows the largest drop in AIC, and (2) is the most non-significant.

#We create a new model, removing the worst predictor (which was the Venus one).
album_model_11 <- lm(sales~adverts + airplay + attract + temp_day_release, data=album_data)
summary(album_model_11)

#We run drop1 again, looking for another predictor we can drop.
drop1(album_model_11, test="F")
#Note that here, the AIC doesn't drop for temp_day_release. However, the p-value is non-significant. What should we do? Well, we know that the temperature on the day that the album was released doesn't make a lot of theoretical sense as a predictor of album sales, so we're going to drop it despite the fact that it will increase the AIC value.

#Create our new model with temp_day_release removed
album_model_12 <- lm(sales~adverts + airplay + attract, data=album_data)
summary(album_model_12)

#Check drop1; no more predictors that make sense to remove! This is our final model.
drop1(album_model_12, test="F")





###Interaction

crazy_data <-read.table("crazy_data.csv",sep="\t",header=TRUE,comment.char="",quote="")
head(crazy_data)

crazy_model <- lm(craziness ~ beers_per_week * years_working_at_uni, data=crazy_data)
summary(crazy_model)
drop1(crazy_model, test="F")

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

