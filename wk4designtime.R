
designtime = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/designtime.csv")
View(designtime)

# convert to nominal factor
designtime$Subject = factor(designtime$Subject)
summary(designtime)

library(plyr)
ddply(designtime, ~ Tool, function(data) summary(data$Time))
ddply(designtime, ~ Tool, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

# Boxplot
hist(pgviews[pgviews$Site == "A",]$Pages)
hist(pgviews[pgviews$Site == "B",]$Pages)
plot(Pages ~ Site, data=pgviews)

hist(designtime[designtime$Tool == "InDesign", ]$Time)
hist(designtime[designtime$Tool == "Illustrator", ]$Time)
plot(Time ~ Tool, data=designtime)

# Shapiro-Wilk normality test on response
shapiro.test(designtime[designtime$Tool == "InDesign", ]$Time)
shapiro.test(designtime[designtime$Tool == "Illustrator", ]$Time)


# but really what matters most is the residuals
m = aov(Time ~ Tool, data=designtime)
shapiro.test(residuals(m))
qqnorm(residuals(m)); qqline(residuals(m))

library(car)
leveneTest(Time ~ Tool, data=designtime, center=mean) # Levene's test
leveneTest(Time ~ Tool, data=designtime, center=median) # Brown-Forsythe test

# Kolmogorov-Smirnov test for log-normality
# fit the distribution to a lognormal to estimate fit parameters
# then supply those to a K-S test with the lognormal distribution fn (see ?plnorm)
# see ?Distributions for many other named probability distributions
library(MASS)
fit = fitdistr(designtime[designtime$Tool == "InDesign",]$Time, "lognormal")$estimate
ks.test(designtime[designtime$Tool == "InDesign",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)
fit = fitdistr(designtime[designtime$Tool == "Illustrator",]$Time, "lognormal")$estimate
ks.test(designtime[designtime$Tool == "Illustrator",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)



# create a new column in ide2 defined as log(Time)
designtime$logTime = log(designtime$Time) # log transform

View(designtime) # verify
library(plyr)
ddply(designtime, ~ Tool, function(data) summary(data$logTime))
ddply(designtime, ~ Tool, summarise, logTime.mean=mean(logTime), logTime.sd=sd(logTime))



summary(designtime)    

# Welch t-test for unequal variances handles
# the violation of homoscedasticity. but not
# the violation of normality.
t.test(logTime ~ Tool, data=designtime, var.equal=FALSE) # Welch t-test

# Mann-Whitney U test
library(coin)
wilcox_test(Time ~ Tool, data=designtime, distribution="exact")
wilcox_test(logTime ~ IDE, data=ide2, distribution="exact") # note: same result


