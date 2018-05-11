alphabets = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/alphabets.csv")
View(alphabets)
alphabets$Subject = factor(alphabets$Subject) # convert to nominal factor
summary(alphabets)

library(plyr)
ddply(alphabets, ~ Alphabet, function(data) summary(data$WPM))
ddply(alphabets, ~ Alphabet, summarise, WPM.mean=mean(WPM), WPM.sd=sd(WPM))

# explore new response distribution
hist(alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM)
hist(alphabets[alphabets$Alphabet == "Graffiti",]$WPM)
hist(alphabets[alphabets$Alphabet == "Unistrokes",]$WPM) # new one
plot(WPM ~ Alphabet, data=alphabets) # boxplot

# test normality for new Edgewrite
shapiro.test(alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM)
shapiro.test(alphabets[alphabets$Alphabet == "Graffiti",]$WPM)
shapiro.test(alphabets[alphabets$Alphabet == "Unistrokes",]$WPM)
m = aov(WPM ~ Alphabet, data=alphabets) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# test log-normality of new IDE
library(MASS)
fit = fitdistr(alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM, "lognormal")$estimate
fit = fitdistr(alphabets[alphabets$Alphabet == "Graffiti",]$WPM, "lognormal")$estimate
fit = fitdistr(alphabets[alphabets$Alphabet == "Unistrokes",]$WPM, "lognormal")$estimate
ks.test(alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE) # lognormality
ks.test(alphabets[alphabets$Alphabet == "Graffiti",]$WPM, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE) # lognormality
ks.test(alphabets[alphabets$Alphabet == "Unistrokes",]$WPM, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE) # lognormality


# compute new log(Time) column and re-test
alphabets$logTime = log(alphabets$WPM) # add new column
View(alphabets) # verify
shapiro.test(alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM)
shapiro.test(alphabets[alphabets$Alphabet == "Graffiti",]$WPM)
shapiro.test(alphabets[alphabets$Alphabet == "Unistrokes",]$WPM)
m = aov(WPM ~ Alphabet, data=alphabets) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# test homoscedasticity
library(car)
leveneTest(WPM ~ Alphabet, data=alphabets, center=median) # Brown-Forsythe test


# one-way ANOVA, suitable now to logTime
m = aov(WPM ~ Alphabet, data=alphabets) # fit model
anova(m) # report anova


# post hoc independent-samples t-tests
plot(WPM ~ Alphabet, data=alphabets) # for convenience
library(multcomp)
summary(glht(m, mcp(Alphabet ="Tukey")), test=adjusted(type="holm")) # Tukey means compare all pairs

library(lsmeans)
summary(glht(m, lsm(pairwise ~ Alphabet)), test=adjusted(type="holm"))

summary(as.glht(pairs(lsmeans(m, pairwise ~ Alphabet))), test=adjusted(type="holm"))

# Kruskal-Wallis test
library(coin)
kruskal_test(WPM ~ Alphabet, data=alphabets, distribution="asymptotic") # can't do exact with 3 levels
kruskal_test(logTime ~ Alphabet, data=alphabets, distribution="asymptotic") # note: same result
# for reporting Kruskal-Wallis as chi-square, we can get N with nrow(ide3)


# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
vs.ec = wilcox.test(alphabets[alphabets$Alphabet == "EdgeWrite",]$WPM, alphabets[alphabets$Alphabet == "Unistrokes",]$WPM, exact=FALSE)
vs.py = wilcox.test(alphabets[alphabets$Alphabet == "Graffiti",]$WPM, alphabets[alphabets$Alphabet == "Unistrokes",]$WPM, exact=FALSE)
ec.py = wilcox.test(alphabets[alphabets$Alphabet == "Unistrokes",]$WPM, alphabets[alphabets$Alphabet == "Graffiti",]$WPM, exact=FALSE)
p.adjust(c(vs.ec$p.value, vs.py$p.value, ec.py$p.value), method="holm")

