


prefs = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/deviceprefs.csv")
View(prefs)
prefs$Disability = factor(prefs$Disability) # convert to nominal factor
prefs$Subject = factor(prefs$Subject) # convert to nominal factor
prefs$Subject = factor(prefs$Subject) # convert to nominal factor
summary(prefs)
plot(prefs$Pref)


# Pearson chi-square test
prfs = xtabs( ~ Pref, data=prefs)
prfs # show counts
chisq.test(prfs)

# people without disabilities and a preference for touchpad
binom.test(sum(prefs[prefs$Disability == "0", ]$Pref == "touchpad"), 
           nrow(prefs[prefs$Disability == "0",]), p=1/2)

# people with disabilities and a preference for touchpad
binom.test(sum(prefs[prefs$Disability == "1", ]$Pref == "touchpad"), 
           nrow(prefs[prefs$Disability == "1",]), p=1/2)


# Pearson chi-square test of proportions of preferences by disability status
prfs = xtabs( ~ Pref + Disability, data=prefs) # the '+' sign indicates two vars
View(prfs)
chisq.test(prfs)

# G-test, asymptotic like chi-square
library(RVAideMemoire)
G.test(prfs)

# Fisher's exact test
fisher.test(prfs)