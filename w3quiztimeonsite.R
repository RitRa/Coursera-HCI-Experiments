

timeonsite = read.csv("//Users/ritaraher/Documents/Coursera/untitled folder/experiments/timeonsite.csv")
View(timeonsite)

timeonsite$Subject = factor(timeonsite$Subject)  # convert to nominal factor
summary(timeonsite)

?ddply

ddply(timeonsite, ~ Site, function(data) summary(data$Time))
ddply(timeonsite, ~ Site, summarise, Time.mean=mean(Time), Time.sd=sd(Time))

# graph histograms and a boxplot
hist(pgviews[pgviews$Site == "A",]$Pages)
hist(pgviews[pgviews$Site == "B",]$Pages)

hist(timeonsite[timeonsite$Site == "A", ]$Time)
hist(timeonsite[timeonsite$Site == "B", ]$Time)

plot(Time ~ Site, data=timeonsite)

# independent-samples t-test
t.test(Time ~ Site, data=timeonsite, var.equal=TRUE)