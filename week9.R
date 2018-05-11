websearch = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/websearch3.csv")

View(websearch)
websearch$Subject

websearch$Subject = factor(websearch$Subject) # convert to nominal factor
websearch$Order = factor(websearch$Order) # convert to nominal factor
websearch$Effort = factor(websearch$Effort) # convert to nominal factor
summary(websearch)

# explore the Searches data
library(plyr)
ddply(websearch, ~ Engine, function(data) summary(data$Searches))
ddply(websearch, ~ Engine, summarise, Searches.mean=mean(Searches), Searches.sd=sd(Searches))

# histograms, boxplot  
hist(websearch[websearch$Engine == "Google",]$Searches)
hist(websearch[websearch$Engine == "Yahoo",]$Searches)
hist(websearch[websearch$Engine == "Bing" ,]$Searches)
boxplot(Searches ~ Engine, data=websearch, xlab="Engine", ylab="Searches") # boxplots


# libraries for LMMs we'll use on Searches
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova


# set sum-to-zero contrasts for the Anova calls
contrasts(websearch$Engine) <- "contr.sum"
contrasts(websearch$Order) <- "contr.sum"

# Linear mixed model (LMM) on Searches by Engine
m = lmer(Searches ~ Engine + (1|Subject), data=websearch)
Anova(m, type=3, test.statistic="F")

# perform post hoc pairwise comparisons
library(multcomp) # for glht
summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))

socialvalue = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/socialvalue.csv")
View(socialvalue)
socialvalue$Subject = factor(socialvalue$Subject) # convert to nominal factor
socialvalue$SocialOrder = factor(socialvalue$SocialOrder) # convert to nominal factor
socialvalue$ClipOrder = factor(socialvalue$ClipOrder) # convert to nominal factor
summary(socialvalue)
socialvalue$Subject


library(plyr)
ddply(socialvalue, ~ Social * Clip, function(data) summary(data$Valued))
ddply(socialvalue, ~ Social * Clip, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))



# Q9 libraries for LMMs we'll use on Searches
library(lme4) # for lmer
library(lmerTest)
library(car) # for Anova

# set sum-to-zero contrasts for the Anova calls
contrasts(socialvalue$Social) <- "contr.sum"
contrasts(socialvalue$Clip) <- "contr.sum"

# Linear mixed model (LMM) on Searches by Engine
m = lmer(Valued ~ Social * Clip + (1|Subject), data=socialvalue)
Anova(m, type=3, test.statistic="F")

# two planned pairwise comparisons of how the film clips may 
# have influenced judgments about the value of social media. 
# The first question is whether on Facebook, the number of valued posts 
# was different after people saw a positive film clip versus a negative 
# film clip. The second question is whether on Twitter, the number of 
# valued posts was different after people saw a positive film clip versus 
# a negative film clip.

library(multcomp) # for glht
library(lsmeans) # for lsm
summary(glht(m, lsm(pairwise ~ Social * Clip)), test=adjusted(type="none"))
p.adjust(c(0.000225, 0.594397), method="holm")


teaser2 = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/teaser.csv")
View(teaser2)
teaser2$Subject
teaser2$Subject = factor(teaser2$Subject) # convert to nominal factor
teaser2$Order = factor(teaser2$Order) # convert to nominal factor
teaser2$Liked = factor(teaser2$Liked) # convert to nominal factor
summary(teaser2)

# Q13 most liked
library(plyr)
ddply(teaser2, ~ Teaser, function(data) summary(data$Liked))
ddply(teaser2, ~ Teaser, summarise, Liked.mean=mean(Liked), Liked.sd=sd(Liked))


# Q14 histograms, boxplot  
hist(teaser2[teaser2$Liked == "0",]$Teaser)
hist(teaser2[teaser2$Liked == "1",]$Teaser)

boxplot(Liked ~ Teaser, data=teaser2, xlab="Liked", ylab="Teaser") # boxplots



## q15 Generalized Linear Mixed Model (GLMM) on Liked

# libraries for GLMMs  we'll use on Liked
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova

# set sum-to-zero contrasts for the Anova call
contrasts(teaser2$Teaser) <- "contr.sum"
contrasts(teaser2$Order) <- "contr.sum"
contrasts(teaser2$Liked) <- "contr.sum"

m = glmer(Liked ~ Order + (1|Subject), data=teaser2, family=binomial)
Anova(m, type=3)

# main GLMM test on Liked
m = glmer(Liked ~ Teaser  + (1|Subject), data=teaser2, family=binomial, nAGQ=0)
Anova(m, type=3)

# perform post hoc pairwise comparisons
library(multcomp) # for glht
summary(glht(m, mcp(Teaser="Tukey")), test=adjusted(type="holm"))

vocab2 = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/vocab.csv")
View(vocab2)
vocab2$Subject
vocab2$Subject = factor(vocab2$Subject) # convert to nominal factor
vocab2$Order = factor(vocab2$Order) # convert to nominal factor
#vocab$Liked = factor(vocab$Liked) # convert to nominal factor
summary(vocab2)

# q18 interaction plot
with(vocab2, interaction.plot(Social, Sex, Vocab, ylim=c(0, max(vocab2$Vocab)))) # interaction?



# q19 Kolmogorov-Smirnov goodness-of-fit tests on Vocab for each 
# level of Social using exponential distributions
library(MASS)

fit = fitdistr(vocab2[vocab2$Social == "Facebook",]$Vocab, "exponential")$estimate
ks.test(vocab[vocab2$Social == "Facebook",]$Vocab, "pexp", rate=fit[1], exact=TRUE)

fit = fitdistr(vocab2[vocab2$Social == "Twitter",]$Vocab, "exponential")$estimate
ks.test(vocab2[vocab2$Social == "Twitter",]$Vocab, "pexp", rate=fit[1], exact=TRUE)

fit = fitdistr(vocab2[vocab2$Social == "Gplus",]$Vocab, "exponential")$estimate
ks.test(vocab2[vocab2$Social == "Gplus",]$Vocab, "pexp", rate=fit[1], exact=TRUE)


# 20 GLMM - test of order effects on Vocab to ensure counterbalancing worked

library(lme4)
library(lmerTest)
library(car)

contrasts(vocab2$Sex) <- "contr.sum"
contrasts(vocab2$Order) <- "contr.sum"

m = glmer(Vocab ~ Sex * Order + (1|Subject), data=vocab2, family=Gamma(link="log"))
Anova(m, type=3)

#GLMM - test of Vocab by Sex and Social

m = glmer(Vocab ~ Sex * Social + (1|Subject), data=vocab2, family=Gamma(link="log"))
Anova(m, type=3)

# post hoc pairwise comparisons among levels of Social adjusted Holm proc
library(multcomp)
summary(glht(m, mcp(Social="Tukey")), test=adjusted(type="holm"))
summary(glht(m, lsm(pairwise ~ Sex * Social)), test=adjusted(type="holm"))




dfnew = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/websearch3.csv")
View(dfnew)

dfnew$Subject = factor(dfnew$Subject)
dfnew$Effort = ordered(df$Effort)

summary(dfnew)
dfnew$Effort

library(ordinal)
library(RVAideMemoire)
dfnew <- as.data.frame(df) # quirk
contrasts(dfnew$Engine) <- "contr.sum"
m = clmm(Effort ~ Engine + (1|Subject), data=dfnew)
Anova(m, type=3) # type ignored






