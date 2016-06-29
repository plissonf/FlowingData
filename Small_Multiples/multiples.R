# Load data
causes <- read.csv("./data/12s0121-truncated.txt", sep="\t", header=TRUE)

# One bar plot
png("images/00.png", width=625)
i <- 1
firstCause <- causes[i,2:12]
barplot(as.numeric(firstCause))
dev.off()


# Bar plot for each cause
png("images/01.png", width=954)
par(mfrow=c(10,9), mar=c(1,2,1,1))
for (i in 1:length(causes[,1])) {
	currCause <- causes[i,2:12]
	barplot(as.numeric(currCause))
}
dev.off()


# Make bar plots more readable.
png("images/02.png", width=954, height=725)
par(mfrow=c(12,8), mar=c(1,5,3,1))
for (i in 1:length(causes[,1])) {
	currCause <- causes[i,2:12]
	barplot(as.numeric(currCause), main=causes[i,1], cex.main=0.8, cex.axis=0.7, border="white", space=0)
}
dev.off()


# Mess around with axes
png("images/03.png", width=954, height=725)
par(mfrow=c(12,8), mar=c(1,5,3,1))
for (i in 1:length(causes[,1])) {
	currCause <- causes[i,2:12]
	
	# Draw bar plot with no axes
	barplot(as.numeric(currCause), main=causes[i,1], cex.main=0.8, cex.axis=0.7, border="white", col="#e26b43", space=0, axes=FALSE)
	
	# Draw custom axes
	axis(side=1, at=c(0,11), labels=FALSE)
	axis(side=2, at=c(0,max(currCause)), cex=0.7)
}
dev.off()


# Compare counts
png("images/04.png", width=954, height=725)
par(mfrow=c(12,8), mar=c(1,5,3,1))
for (i in 1:length(causes[,1])) {
	currCause <- causes[i,2:12]
	
	# Draw bar plot with no axes, same vertical scale
	barplot(as.numeric(currCause), main=causes[i,1], cex.main=0.8, cex.axis=0.7, border="white", col="#e26b43", space=0, axes=FALSE, ylim=c(0,120000))
	
	# Draw custom axes
	axis(side=1, at=c(0,11), labels=FALSE)
	axis(side=2, at=c(0,max(currCause)), cex=0.7)
}
dev.off()


# Highlighting
png("images/05.png", width=954, height=725)
par(mfrow=c(12,8), mar=c(1,5,3,1))
for (i in 1:length(causes[,1])) {
	currCause <- as.numeric( causes[i,2:12] )
	
	# Find age group with highest count
	maxCnt <- max(currCause)
	groupNum <- which(currCause == maxCnt)
	
	# Color based on max age group
	if (groupNum < 5) {
		barColor <- "#e26b43"	# Orange-red
	} else {
		barColor <- "#cccccc"	# Gray
	}
	
	# Draw the bar plot
	barplot(currCause, main=causes[i,1], cex.main=0.8, cex.axis=0.7, border="white", col=barColor, space=0, axes=FALSE)
	axis(side=1, at=c(0,11), labels=FALSE)
	axis(side=2, at=c(0,max(currCause)), cex=0.8)
}
dev.off()



# Custom chart (just one)
png("images/06.png", width=625)
i <- 1
firstCause <- as.numeric( causes[i,2:12] )
maxCnt <- max(firstCause)
yOffset <- (maxCnt - firstCause) / 2
plot(0, 0, type="n", xlim=c(0,11), ylim=c(0, max(firstCause)), xlab="", ylab="", axes=FALSE, bty="n")
rect(0:10, yOffset, 1:11, firstCause+yOffset, col="#cccccc", border="white")
dev.off()


# Now do it for all causes
png("images/07.png", width=954, height=725)
par(mfrow=c(12,8), mar=c(1,3,3,1))
for (i in 1:length(causes[,1])) {
	currCause <- as.numeric( causes[i,2:12] )
	
	# Find age group with highest count
	maxCnt <- max(currCause)
	
	# Draw the custom chart
	yOffset <- (maxCnt - currCause) / 2
	plot(0, 0, type="n", xlim=c(0,11), ylim=c(0, max(currCause)), xlab="", ylab="", axes=FALSE, bty="n", cex.main=0.8, main=causes[i,1])
	rect(0:10, yOffset, 1:11, currCause+yOffset, col="#e26b43", border="white")
}
dev.off()


# Try a different plot type for kicks
png("images/08.png", width=954, height=850)
par(mfrow=c(12,8), mar=c(1,3,3,1))
source("areaGraph.R")
for (i in 1:length(causes[,1])) {
	currCause <- causes[i,2:12]
	areaGraph(currCause)
}
dev.off()










