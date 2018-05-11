
websearch2 = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/websearch2.csv")
View(websearch2)
websearch2$Subject = factor(websearch2$Subject) #convert to nominal factor
websearch2$Order = factor(websearch2$Order)
summary(websearch2)


# view descriptive statistics by Technique
library(plyr)
ddply(websearch2, ~ Engine, function(data) summary(data$Searches))
ddply(websearch2, ~ Engine, summarise, Searches=mean(Searches), Searches.sd=sd(Searches))


# graph histograms and boxplot
hist(websearch2[websearch2$Engine == "Bing",]$Searches)
hist(websearch2[websearch2$Engine == "Google",]$Searches)
plot(Searches ~ Engine, data=websearch2) # boxplot

# test anova assumptions
shapiro.test(websearch2[websearch2$Engine == "Bing",]$Searches) # Shapiro-Wilk
shapiro.test(websearch2[websearch2$Engine == "Google",]$Searches)


# now test for an order effect -- did counterbalancing work?
library(reshape2)	
# for a paired-samples t-test we must use a wide-format table; most
# R fns do not require a wide-format table, but the dcast function
# offers a quick way to translate long-format into wide-format when
# we need it.

websearch2.wide.order = dcast(websearch2, Subject ~ Order, value.var="Searches") # go wide
View(websearch2.wide.order) # verify
t.test(websearch2.wide.order$"1", websearch2.wide.order$"2", paired=TRUE, var.equal=TRUE)


# finally, the paired-samples t-test
websearch2.wide.tech = dcast(websearch2, Subject ~ Engine, value.var="Searches") # go wide
View(websearch2.wide.tech)
t.test(websearch2.wide.tech$Bing, websearch2.wide.tech$Google, paired=TRUE, var.equal=TRUE)
plot(Searches ~ Engine, data=websearch2) # confirm

# explore the Errors response; error counts are often Poisson
library(plyr)
ddply(websearch2, ~ Engine, function(data) summary(data$Effort))
ddply(websearch2, ~ Engine, summarise, Effort.mean=mean(Effort), Effort.sd=sd(Effort))

hist(websearch2[websearch2$Engine == "Bing",]$Effort) # histogram
hist(websearch2[websearch2$Engine == "Google",]$Effort) # histogram
plot(Effort ~ Engine, data=websearch2) # boxplot

# our response is ordinal within-Ss, so use nonparametric Wilcoxon signed-rank
library(coin)
wilcoxsign_test(Effort ~ Engine | Subject, data=websearch2, distribution="exact")



websearch3 = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/websearch3.csv")
View(websearch3)
websearch3$Subject = factor(websearch3$Subject) #convert to nominal factor
websearch3$Order = factor(websearch3$Order)
summary(websearch3)

# view descriptive statistics by Technique
library(plyr)
ddply(websearch3, ~ Engine, function(data) summary(data$Searches))
ddply(websearch3, ~ Engine, summarise, Searches=mean(Searches), Searches.sd=sd(Searches))

# graph histograms and boxplot
hist(websearch3[websearch3$Engine == "Bing",]$Searches)
hist(websearch3[websearch3$Engine == "Google",]$Searches)
hist(websearch3[websearch3$Engine == "Yahoo",]$Searches)
plot(Searches ~ Engine, data=websearch3) # boxplot


# repeated measures ANOVA
library(ez)
# ez lets us specify the dependent variable (Time), within-Ss 
# variables (Technique), and the variable that identifies 
# subjects (Subject).
m = ezANOVA(dv=Searches, within=Order, wid=Subject, data=websearch3)
# we then check the model for violations of sphericity. Sphericity is 
# the situation where the variances of the differences between all 
# combinations of levels of a within-Ss factor are equal. It always
# holds for within-Ss factors that have just 2 levels, but for 3+
# levels, sphericity can be tested with Mauchly's Test of Sphericity.
m$Mauchly # p<.05 indicates a violation
# if no violation, examine the uncorrected ANOVA in m$ANOVA. 
# if violation, instead look at m$Sphericity and use the 
# Greenhouse-Geisser correction, GGe.
m$ANOVA
# include the corrected DFs for each corrected effect
pos = match(m$`Sphericity Corrections`$Effect, m$ANOVA$Effect) # positions of within-Ss efx in m$ANOVA
m$Sphericity$GGe.DFn = m$Sphericity$GGe * m$ANOVA$DFn[pos] # Greenhouse-Geisser
m$Sphericity$GGe.DFd = m$Sphericity$GGe * m$ANOVA$DFd[pos]
m$Sphericity$HFe.DFn = m$Sphericity$HFe * m$ANOVA$DFn[pos] # Huynh-Feldt
m$Sphericity$HFe.DFd = m$Sphericity$HFe * m$ANOVA$DFd[pos]
m$Sphericity # show results

# repeated measures ANOVA
library(ez)
# ez lets us specify the dependent variable (Time), within-Ss 
# variables (Technique), and the variable that identifies 
# subjects (Subject).
m = ezANOVA(dv=Searches, within=Engine, wid=Subject, data=websearch3)
# we then check the model for violations of sphericity. Sphericity is 
# the situation where the variances of the differences between all 
# combinations of levels of a within-Ss factor are equal. It always
# holds for within-Ss factors that have just 2 levels, but for 3+
# levels, sphericity can be tested with Mauchly's Test of Sphericity.
m$Mauchly # p<.05 indicates a violation
# if no violation, examine the uncorrected ANOVA in m$ANOVA. 
# if violation, instead look at m$Sphericity and use the 
# Greenhouse-Geisser correction, GGe.
m$ANOVA
# include the corrected DFs for each corrected effect
pos = match(m$`Sphericity Corrections`$Effect, m$ANOVA$Effect) # positions of within-Ss efx in m$ANOVA
m$Sphericity$GGe.DFn = m$Sphericity$GGe * m$ANOVA$DFn[pos] # Greenhouse-Geisser
m$Sphericity$GGe.DFd = m$Sphericity$GGe * m$ANOVA$DFd[pos]
m$Sphericity$HFe.DFn = m$Sphericity$HFe * m$ANOVA$DFn[pos] # Huynh-Feldt
m$Sphericity$HFe.DFd = m$Sphericity$HFe * m$ANOVA$DFd[pos]
m$Sphericity # show results


# manual post hoc pairwise comparisons with paired-samples t-tests
library(reshape2)	
websearch3.wide.tech = dcast(websearch3, Subject ~ Engine, value.var="Searches") # go wide
View(websearch3.wide.tech)
se.sc = t.test(websearch3.wide.tech$Yahoo, websearch3.wide.tech$Bing, paired=TRUE)
se.vc = t.test(websearch3.wide.tech$Google, websearch3.wide.tech$Yahoo, paired=TRUE)
sc.vc = t.test(websearch3.wide.tech$Bing, websearch3.wide.tech$Google, paired=TRUE)
p.adjust(c(se.sc$p.value, se.vc$p.value, sc.vc$p.value), method="holm")

# first, examine Errors for 3 techniques
library(plyr)
ddply(websearch3, ~ Engine, function(data) summary(data$Effort))
ddply(websearch3, ~ Engine, summarise, Effort.mean=mean(Effort), Errors.sd=sd(Effort))
hist(websearch3[websearch3$Engine == "Bing",]$Effort)
hist(websearch3[websearch3$Engine == "Google",]$Effort)
hist(websearch3[websearch3$Engine == "Yahoo",]$Effort) # new one
plot(Effort ~ Engine, data=websearch3) # boxplot

# are the Voice error counts possibly Poisson distributed 
# as they seemed for Scroll and Search?
library(fitdistrplus)
fit = fitdist(websearch3[websearch3$Engine == "Voice",]$Errors, "pois", discrete=TRUE)
gofstat(fit) # goodness-of-fit test

# Friedman test on Errors
library(coin)
friedman_test(Effort ~ Engine | Subject, data=websearch3, distribution="asymptotic")


# manual post hoc Wilcoxon signed-rank test multiple comparisons
se.sc = wilcox.test(websearch3[websearch3$Engine == "Bing",]$Effort, websearch3[websearch3$Engine == "Yahoo",]$Effort, paired=TRUE, exact=FALSE)
se.vc = wilcox.test(websearch3[websearch3$Engine == "Google",]$Effort, websearch3[websearch3$Engine == "Yahoo",]$Effort, paired=TRUE, exact=FALSE)
sc.vc = wilcox.test(websearch3[websearch3$Engine == "Yahoo",]$Effort, websearch3[websearch3$Engine == "Bing",]$Effort, paired=TRUE, exact=FALSE)
p.adjust(c(se.sc$p.value, se.vc$p.value, sc.vc$p.value), method="holm")
