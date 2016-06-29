#
# Load data
#
jamie <- read.csv('jamie-counts.csv')

#
# Percentages
#
jamie$fpct <- jamie$female / (jamie$female + jamie$male)
jamie$mpct <- 1 - jamie$fpct

#
# Time series chart
#
plot(jamie$year, jamie$fpct, ylim=c(0,1))
lines(jamie$year, jamie$fpct)
lines(c(1880,2012), c(0.5,0.5), col="red")

#
# Connected scatter plot
#
plot(jamie[-(1:30), 2:3])
lines(jamie[-(1:30), 2:3])
lines(c(-1,13000), c(-1,13000), col="red")
text(jamie[-(1:30), 2], jamie[-(1:30), 3]-100, jamie$year[-(1:30)], cex=0.5)

#
# Connected scatter plot, logarithmic scale
#
plot(jamie[-(1:30), 2:3], log="xy")
lines(jamie[-(1:30), 2:3])
lines(c(1,13000), c(1,13000), col="red")

#
# Same logorithmic scale, with some tweaks
#
plot(jamie[-(1:30), 2:3], log="xy", bty="n", las=1, type="n")

# Draw grid lines
xTicks <- axTicks(1)
yTicks <- axTicks(2)
for (i in 1:length(xTicks)) {
	lines(c(xTicks[i], xTicks[i]), c(1, 1.5*max(yTicks)), col="#888888", lwd=0.5, lty=3)
}
for (j in 1:length(yTicks)) {
	lines(c(1, 1.5*max(xTicks)), c(yTicks[j], yTicks[j]), col="#888888", lwd=0.5, lty=3)
}

# Connecting lines and points
lines(jamie[-(1:30), 2:3])
points(jamie[-(1:30), 2:3], bg="#ffffff", pch=21, cex=0.8)

# Labels
text(jamie[-(1:30), 2], jamie[-(1:30), 3]-0.13*jamie[-(1:30), 3], jamie$year[-(1:30)], cex=0.5)



#
# Wrapped in a function
#

connectedScatter <- function(x, y, labels=c(), log="", xlab="x", ylab="y") {
	
	plot(x, y, log=log, bty="n", las=1, type="n", xlab=xlab, ylab=ylab)
	
	# Draw grid lines
	xTicks <- axTicks(1)
	yTicks <- axTicks(2)
	for (i in 1:length(xTicks)) {
		lines(c(xTicks[i], xTicks[i]), c(1, 1.5*max(yTicks)), col="#888888", lwd=0.5, lty=3)
	}
	for (j in 1:length(yTicks)) {
		lines(c(1, 1.5*max(xTicks)), c(yTicks[j], yTicks[j]), col="#888888", lwd=0.5, lty=3)
	}
	
	# Connecting lines and points
	lines(x, y)
	points(x, y, bg="#ffffff", pch=21, cex=0.8)
	
	# Labels
	if (length(labels) > 0) {
		if (log != "") {
			yOffset <- 0.13*y
		} else {
			yOffset <- 0.03*(max(yTicks)-min(yTicks))
		}
		
		text(x, y-yOffset, labels, cex=0.5)
	}
}


# Example usage
connectedScatter(jamie[-(1:30), 2], jamie[-(1:30), 3], labels=jamie$year[-(1:30)], xlab="female", ylab="male")
lines(c(-1,13000), c(-1,13000), col="red")

connectedScatter(jamie[-(1:30), 2], jamie[-(1:30), 3], log="xy")




