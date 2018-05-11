
socialv = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/socialvalue.csv")
View(socialv)

socialv$Subject = factor(socialv$Subject) # convert to nominal factor
socialv$ClipOrder = factor(socialv$ClipOrder) # convert to nominal factor
socialv$SocialOrder = factor(socialv$SocialOrder)
summary(socialv)
socialv$Subject

#22?
library(plyr)
ddply(socialv, ~ Clip * Social, function(data) summary(data$Valued))
ddply(socialv, ~ Clip * Social, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))

# 23 histograms for two factors
hist(socialv[socialv$Clip == "negative" & socialv$Social == "Facebook",]$Valued)
hist(socialv[socialv$Clip == "positive" & socialv$Social == "Facebook",]$Valued)
hist(socialv[socialv$Clip == "negative" & socialv$Social == "Twitter",]$Valued)
hist(socialv[socialv$Clip == "positive" & socialv$Social == "Twitter",]$Valued)

boxplot(Valued ~ Clip * Social, data=socialv, xlab="Clip.Social", ylab="Valued") # boxplots

#23 Interaction plot
with(socialv, interaction.plot(Social, Clip, Valued, ylim=c(0, max(socialv$Valued)))) # interaction plot

#24 Interaction plot
with(socialv, interaction.plot(Clip, Social, Valued, ylim=c(0, max(socialv$Valued)))) # interaction plot

#25

#26 Interaction plot
# test for a Posture order effect to ensure counterbalancing worked
library(ez)
m = ezANOVA(dv=Valued, within=.(ClipOrder,SocialOrder), wid=Subject, data=socialv)
m$Mauchly # n.s.
m$ANOVA 


#27

#29

## Nonparametric approach to factorial ANOVA
## The Aligned Rank Transform (ART) procedure

# Aligned Rank Transform on Error_Rate
library(ARTool) # for art, artlm
m = art(Valued ~ Clip * Social + (1|Subject), data=socialv) # uses LMM
anova(m) # report anova
shapiro.test(residuals(m)) # normality?
qqnorm(residuals(m)); qqline(residuals(m)) # seems to conform

#30
# conduct post hoc pairwise comparisons within each factor
with(socialv, interaction.plot( Clip, Social, Valued, ylim=c(0, max(socialv$Valued)))) # for convenience
library(lsmeans) # for lsmeans
lsmeans(artlm(m, "Clip"), pairwise ~ Clip)
lsmeans(artlm(m, "Social"), pairwise ~ Social)
#lsmeans(artlm(m, "Keyboard : Posture"), pairwise ~ Keyboard : Posture) # don't do this in ART!

library(phia)
testInteractions(artlm(m, "Clip:Social"), pairwise=c("Clip", "Social"), adjustment="holm")
# in the output, A-B : C-D is interpreted as a difference-of-differences, i.e., the difference 
# between (A-B | C) and (A-B | D). in words, is the difference between A and B significantly 
# different in condition C from condition D?

