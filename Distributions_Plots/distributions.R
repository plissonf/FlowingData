# Visualizing Distributions

# Load crime data
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")

# Remove Washington, D.C. and national average
crime.new <- crime[crime$state != "District of Columbia",]
crime.new <- crime.new[crime.new$state != "United States ",]

# Box plot
boxplot(crime.new$robbery, horizontal=TRUE, main="Robbery Rates in US")
boxplot(crime.new[,-1], horizontal=TRUE, main="Crime Rates in US")

# Histogram
hist(crime.new$robbery)
hist(crime.new$robbery, breaks=10)

par(mfrow=c(3, 3))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
  hist(crime[,i], xlim=c(0, 3500), breaks=seq(0, 3500, 100), main=colnames[i], probability=TRUE, col="gray", border="white")
  d <- density(crime[,i])
  #lines(d, col="red")
}


# Density plot
par(mfrow=c(3, 3))
colnames <- dimnames(crime.new)[[2]]
for (i in 2:8) {
  d <- density(crime[,i])
  plot(d, type="n", main=colnames[i])
  polygon(d, col="red", border="gray")
}


# Density + Rug
d <- density(crime$robbery)
plot(d, type="n", main="robbery")
polygon(d, col="lightgray", border="gray")
rug(crime$robbery, col="red")

# Violin plot
library(vioplot)
vioplot(crime.new$robbery, horizontal=TRUE, col="gray")

# Bean plot
library(beanplot)
beanplot(crime.new[,-1])

