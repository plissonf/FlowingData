#####
#
# How to Make a Line Graph
#
#####


#
# Load data
#

life <- read.csv("data/life-expectancy-cleaned.csv", stringsAsFactors=FALSE)

#install.packages("reshape")
library(reshape)
life2 <- melt(life, id=c("Country.Name", "Country.Code"))


#
# Default line plot
#

usa <- subset(life2, Country.Code == "USA")
usa <- usa[order(usa$variable, decreasing=FALSE),]
plot(usa$value, type="l")


#
# Quick changes
#

# Dots and lines
plot(usa$value, type="b", cex=0.8)

# Add x-value (years)
plot(1960:2009, usa$value, type="l", main="USA Life Expectancy, 1960 to 2009", ylab="Years from birth", xlab="", las=1, col="blue", lwd=2, cex.axis=0.8)


#
# Multiple lines
#

# Default (Nope.)
plot(life2[,c(-1,-2)], type="l", main="Life Expectancy, 1960 to 2009", ylab="Years from birth", xlab="")

# Try that again
plot(0, 0, type="n", xlim=c(1960, 2009), ylim=c(25, 80), main="Life Expectancy, 1960 to 2009", ylab="Years from birth", xlab="", las=1, lwd=2, bty="n", cex.axis=0.7)
codes <- unique(life2$Country.Code)
for (i in 1:length(codes)) {
	currCountry <- subset(life2, Country.Code == codes[i])
	currCountry <- currCountry[order(currCountry$variable, decreasing=FALSE),]
	# lines(1960:2009, currCountry$value)
	lines(1960:2009, currCountry$value, col="#002000")
}


# Multiple plots
countries <- read.csv("data/country-regions.csv", stringsAsFactors=FALSE)
life3 <- merge(life2, countries[,c("CountryCode", "RegionName")], by.x="Country.Code", by.y="CountryCode")
regions <- unique(life3$RegionName)
par(mfrow=c(3,3))
for (i in 1:length(regions)) {
	currRegion <- subset(life3, RegionName == regions[i])
	
	# Draw plot for region
	plot(0, 0, type="n", xlim=c(1960, 2009), ylim=c(25, 80), main=regions[i], ylab="Years from birth", xlab="")
	codes <- unique(currRegion$Country.Code)
	for (j in 1:length(codes)) {
		currCountry <- subset(currRegion, Country.Code == codes[j])
		currCountry <- currCountry[order(currCountry$variable, decreasing=FALSE),]
		lines(1960:2009, currCountry$value, col="#002000")
	}
}





#
# Adjust
#


# Line width + Transparency + Grid lines
regionName <- "Sub-Saharan Africa (all income levels)"
currRegion <- subset(life3, RegionName == regionName)
plot(0, 0, type="n", xlim=c(1960, 2009), ylim=c(23, 80), main=regionName, ylab="Years from birth", xlab="", las=1, lwd=2, bty="n", cex.axis=0.8)
grid(NA, NULL, lwd=1.2)
codes <- unique(currRegion$Country.Code)
for (j in 1:length(codes)) {
	currCountry <- subset(currRegion, Country.Code == codes[j])
	currCountry <- currCountry[order(currCountry$variable, decreasing=FALSE),]
	lines(1960:2009, currCountry$value, col="#00200070", lwd=0.8)
}

# Make Rwanda line more obvious
currCountry <- subset(currRegion, Country.Code == "RWA")
currCountry <- currCountry[order(currCountry$variable, decreasing=FALSE),]
lines(1960:2009, currCountry$value, col="#402000", lwd=2)

# Annotate
text(1993 + 0.9, min(currCountry$value)-0.4, "In 1993, Rwanda's average life\nexpectancy was 25 years.", cex=0.85, font=3, pos=4)
symbols(1993, min(currCountry$value), circles=0.5, inches=FALSE, add=TRUE, lwd=2)


# Line type
plot(0, 0, type="n", xlim=c(1960, 2009), ylim=c(23, 80), main="Life Expectancy", ylab="Years from birth", xlab="", las=1, lwd=2, bty="n", cex.axis=0.8)
text(1960, 44, "Rwanda", pos=4)
lines(1960:2009, currCountry$value, col="#000000", lwd=2, lty=2)
text(1960, 69, "United States", pos=4)
lines(1960:2009, usa$value, col="#000000", lwd=2, lty=3)


# Line types for reference
par(mar=c(0,0,0,0))
plot(0, 0, xlim=c(0,1), ylim=c(-1,6), type="n", axes=FALSE)
for (i in 1:6) {
	lines(c(0,1), c(6-i,6-i), lty=i)
	text(0, 6-i+0.2, paste("Line type: ", i, sep=""), pos=4, cex=0.8, font=2)
}



