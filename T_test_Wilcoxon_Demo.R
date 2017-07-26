library(ggplot2)
library(pastecs)



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




###check linearity when you have multiple predictors

d <- read.table("Album Sales 2.dat", comment.char="", quote="", header=T, sep="\t")
head(d)

album_model <- lm(sales~adverts + airplay + attract, data=d)
summary(album_model)

p1 <- ggplot(d, aes(adverts, sales)) + geom_point()
p2 <- ggplot(d, aes(airplay, sales)) + geom_point()
p3 <- ggplot(d, aes(attract, sales)) + geom_point()

#Now we use multiplot function we defined above
multiplot(p1,p2,p3, cols=2)



###Load in spider data
spider_data_long <- read.table("SpiderLong.dat", sep="\t", comment.char="", quote="", header=T)
head(spider_data_long)
#Each variable (predictor and outcome) gets a column
#also called "molten" format

spider_data_wide <- read.table("SpiderWide.dat", sep="\t", comment.char="", quote="", header=T)
head(spider_data_wide)
#Each category of predictor (picture and real) gets a column

###Boxplots (I prefer these to barplots for categorical data, since they provide a lot of information)

ggplot(spider_data_long, aes(Group, Anxiety)) + geom_boxplot(notch=TRUE)

###Boxplots include medians by default, but we'd also like to see means

ggplot(spider_data_long, aes(Group, Anxiety)) + geom_boxplot(notch=TRUE) + stat_summary(fun.y=mean, geom="point", colour="red")



###Checking assumptions

#descriptive statistics using stat.desc() from pastecs package; you'll need the means and the standard errors for when you report results
stat.desc(spider_data_wide, basic=FALSE, norm=TRUE)

#for long format data
by(spider_data_long$Anxiety, spider_data_long$Group, stat.desc, basic=FALSE, norm=TRUE)


shapiro.test(spider_data_wide$picture)

shapiro.test(spider_data_wide$real)

by(spider_data_long$Anxiety, spider_data_long$Group, shapiro.test)




###Running the independent t-test

#For wide format data
t.test(spider_data_wide$picture, spider_data_wide$real)

#For long format data
t.test(Anxiety ~ Group, data=spider_data_long)


#optional parameters:
#alternative = "less" or "greater"; use if you have a directional hypothesis
#na.action = na.exclude; use this option if you have missing data points


###Calculating an effect size

#r = sqrt(t^2 / (t^2 + df))

sqrt((-1.681)^2/((-1.681)^2 + 21.385))

#0.3416; remember that 0.3 is considered a medium effect




###Running the dependent t-test

#Let's use the same data, but let's now say that the data points are paired: the first data point in the picture condition and the first data point in the real condition belong to the same person, and so forth.

#We don't need to reload data or redo the boxplots, but we do have a slightly different assumption; now, we need to make sure that the differences between each pair of data points are normally distributed

differences <- spider_data_wide$picture - spider_data_wide$real

differences <- subset(spider_data_long, Group=="Picture")[,"Anxiety"] - subset(spider_data_long, Group=="Real Spider")[,"Anxiety"]

shapiro.test(differences)

#We use the same function to run dependent t-test, but we set paired=TRUE

#For wide format data
t.test(spider_data_wide$picture, spider_data_wide$real, paired=TRUE)

#For long format data
t.test(Anxiety~Group, data=spider_data_long, paired=TRUE)

#Now the result is significant!

#Calculate an effect size (same equations as before)

#r = sqrt(t^2 / (t^2 + df))

sqrt((-2.473)^2/((-2.473)^2 + 11))

#0.5978; remember that 0.5 is considered a strong effect



###Non-parametric tests for ordinal/non-normal data

#Summary statistics again: you'll need medians for reporting the results
stat.desc(spider_data_wide, basic=FALSE, norm=TRUE)

#you'll also need the interquartile ranges of your two samples
IQR(spider_data_wide$picture)
IQR(spider_data_wide$real)

#Wilcoxon rank-sum test for independent samples

wilcox.test(spider_data_wide$picture, spider_data_wide$real, correct=FALSE, exact=FALSE)
model1 <- wilcox.test(Anxiety~Group, data=spider_data_long, correct=FALSE, exact=FALSE); model1

#Effect size
# z = qnorm(model$p.value/2)
# N = number of data points
# r = z / sqrt(N)

qnorm(model1$p.value/2)/sqrt(dim(spider_data_long)[1])



#Wilcoxon signed-rank test for dependent samples

wilcox.test(spider_data_wide$picture, spider_data_wide$real, paired=TRUE, correct=FALSE, exact=FALSE)
model2 <- wilcox.test(Anxiety~Group, data=spider_data_long, paired=TRUE, correct=FALSE, exact=FALSE); model2


#Effect size (calculate the same way)

qnorm(model2$p.value/2)/sqrt(dim(spider_data_long)[1])
