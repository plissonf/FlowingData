#
#
# Style charts with base R
#
#

startYear <- 1960; endYear <- 2009


#
# Load the data
#

life <- read.csv("data/life-expectancy-cleaned.csv", stringsAsFactors=FALSE)
countryRegions <- read.csv("data/country-regions.csv", stringsAsFactors=FALSE)
lifeComp <- na.omit(life)
lifeReg <- merge(lifeComp, countryRegions, by.x="Country.Code", by.y="CountryCode")

# Subset East Asia and Pacific, for sake of simplicity
eap <- subset(lifeReg, RegionName == "East Asia &amp; Pacific (all income levels)")
minVal <- min(eap[,-c(1,2,53,54)])
maxVal <- max(eap[,-c(1,2,53,54)])
eapYears <- eap[,-c(1,2,53,54)]


#
# Helper function to draw a line for each country
#
lifeLines <- function(series, col="black", hcol="black", lwd=1, hlwd=2, showPoints=FALSE) {
	for (i in 1:length(series[,1])) {
		lines(startYear:endYear, series[i,], col=col, lwd=lwd)
	}
	randomIndex <- sample(1:length(series[,1]), 1)
	lines(startYear:endYear, series[randomIndex,], col=hcol, lwd=hlwd)
	
	if (showPoints) {
		points(startYear:endYear, series[randomIndex,], col=hcol, pch=4)
	}
} 



#
# Default plot
#
plot(0, 0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n")
lifeLines(eapYears)


#
# Default Plot Options
#

plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", main="Life Expectancy in East Asia and Pacific", sub="Subtitle", xlab="Years", ylab="Age", asp=1/2)
lifeLines(eapYears)

#
# Simplified
#

par(las=1)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", main="Life Expectancy in East Asia and Pacific", xlab="", ylab="Age", asp=1/2)
lifeLines(eapYears, col="#cccccc")

#
# Newspaper
#

par(mar=c(4, 4, 3, 1), oma=c(0,0,0,0), xpd=FALSE, xaxs="r", yaxs="i", mgp=c(2.1,.6,0), las=1, lend=1)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", las=1, main="Life Expectancy in East Asia and Pacific", xlab="", ylab=expression(bold("Age")), family="Helvetica", cex.axis=0.8, cex.lab=0.8, asp=1/2)
grid(NA, NULL, col="black", lty="dotted", lwd=0.3)
lifeLines(eapYears, col="dark grey", lwd=0.7, hlwd=3, hcol="#244A5D")



#
# Feltron
#

par(bg="#36394A", mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xpd=FALSE, xaxs="r", yaxs="i", mgp=c(2.8,0.3,0.5), col.lab="white", col.axis="white", col.main="white", font.main=1, cex.main=0.8, cex.axis=0.8, cex.lab=0.8, family="Helvetica", lend=1, tck=0)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", las=1, asp=1/2, main="LIFE EXPECTANCY IN EAST ASIA AND PACIFIC", xlab="", ylab="")
lifeLines(eapYears, col="white", lwd=0.35, hcol="#E3DF0C", hlwd=3)

# Axis change
par(bg="#36394A", mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xpd=FALSE, xaxs="r", yaxs="i", mgp=c(2.8,0.3,0.5), col.lab="white", col.axis="white", col.main="white", font.main=1, cex.main=0.8, cex.axis=0.8, cex.lab=0.8, family="Helvetica", lend=1, tck=0, las=1)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", las=1, asp=1/2, main="LIFE EXPECTANCY IN EAST ASIA AND PACIFIC", xlab="", ylab="", xaxt="n", yaxt="n")
axis(1, tick=FALSE, col.axis="white")
axis(2, tick=FALSE, col.axis="white")
lifeLines(eapYears, col="white", lwd=0.35, hcol="#E3DF0C", hlwd=3)


#
# FiveThirtyEight
#

par(mar=c(3, 4, 3, 2), oma=c(0,0,0,0), bg="#F0F0F0", xpd=FALSE, xaxs="r", yaxs="i", mgp=c(2.1,.3,0), las=1, col.axis="#434343", col.main="#343434", tck=0, lend=1)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", las=1, main="Life Expectancy in East Asia and Pacific", xlab="", ylab="Age", family="Helvetica", cex.main=1.5, cex.axis=0.8, cex.lab=0.8, asp=1/2, xaxt="n", yaxt="n")
grid(NULL, NULL, col="#DEDEDE", lty="solid", lwd=0.9)
axis(1, tick=FALSE, cex.axis=0.9)
axis(2, tick=FALSE, cex.axis=0.9)
lifeLines(eapYears, col="dark grey", lwd=1, hlwd=3, hcol="#008ED4")


#
# The Economist
#

# Red corner rectangle
par(xpd=NA, oma=c(0,0,0,0), mar=c(0,0,0,0), bg="#DCE6EC", xpd=FALSE, xaxs="i", yaxs="i", lend=1)
plot(0, 0, type = "n", bty = "n", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100))
rect(0,100,2,94, col="red", border=NA)

# Actual chart
par(mar=c(4, 3, 3, 2), oma=c(0,0,0,0), xpd=FALSE, xaxs="r", yaxs="i", mgp=c(1.8,.2,0), cex.axis=0.7, cex.lab=0.7, col.lab="black", col.axis="black", col.main="black", tck=0.02, yaxp=c(minVal, maxVal, 2), new=TRUE)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", las=1, main="Life Expectancy in East Asia and Pacific", xlab=expression(italic("Years")), ylab=expression(italic("Age")), family="Helvetica", asp=1/2)
grid(NA, NULL, col="white", lty="solid", lwd=1.5)
lifeLines(eapYears, lwd=1.25, hlwd=2.5, col="#33A5A2", hcol="#244A5D")


#
# Tukey
#

par(las=1, tck=0.02, mgp=c(2.8,0.3,0.5), cex.lab=0.85, cex.axis=0.8, cex.main=0.9)
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", bty="n", main="Life Expectancy in East Asia and Pacific", xlab="", ylab="Age", asp=1/2)
lifeLines(eapYears, col="#cccccc", hlwd=1.2, showPoints=TRUE)


#
# Bright on Dark
#

par(bg="black", las=1, tck=0, mgp=c(2.8,0.3,0), cex.lab=0.85, cex.axis=0.8, cex.main=0.9, col.axis="white", col.main="white", col.lab="white")
plot(0, xlim=c(startYear, endYear), ylim=c(minVal, maxVal), type="n", main="Life Expectancy in East Asia and Pacific", xlab="Year", ylab="Age", asp=1/2)
grid(NULL, NULL, lty="solid", col="white", lwd=0.5)
lifeLines(eapYears, col="#f30baa", lwd=1.2, hcol="green", hlwd=3)

