####
# Draw shapes and custom plots
####

# Fake data to use later.
x.fakedata <- runif(20, 0, 100)
y.fakedata <- runif(20, 0, 100)

# Basic plot window.
plot(0, 0, xlim=c(0, 100), ylim=c(0, 100), type="n", xlab=NA, ylab=NA)

# From scratch.
plot.new()
plot.window(xlim=c(0, 100), ylim=c(0, 100))
ticks <- seq(0, 100, 20)
par(las=1, cex=0.8)
axis(1, at=ticks, labels=ticks, pos=0)
axis(2, at=ticks, labels=ticks, pos=0)

# Multiple plot windows.
par(mfrow=c(2,3), mar=c(4,3,3,2), las=1, cex=0.6)
for (i in 1:6) {
	plot.new()
	plot.window(xlim=c(0, 100), ylim=c(0, 100))
	ticks <- seq(0, 100, 20)
	axis(1, at=ticks, labels=ticks, pos=0)
	axis(2, at=ticks, labels=ticks, pos=0)
	
	# Add symbols, shapes, and stuff.
	points(x.fakedata, y.fakedata, pch=18+i)
	# symbols(x.fakedata, y.fakedata, circles=x.fakedata, add=TRUE, inches=0.25)
	# symbols(x.fakedata, y.fakedata, squares=x.fakedata, add=TRUE, inches=0.25)
}


# Draw shapes.
par(mfrow=c(1,2), las=1, cex=0.8)
plot.new()
plot.window(xlim=c(0, 100), ylim=c(0, 100))
ticks <- seq(0, 100, 20)
axis(1, at=ticks, labels=ticks, pos=0)
axis(2, at=ticks, labels=ticks, pos=0)


# Draw your own shapes.
x.oct <- c(40, 60, 80, 80, 60, 40, 20, 20, 40)
y.oct <- c(80, 80, 60, 40, 20, 20, 40, 60, 80)
polygon(x.oct, y.oct, col="#821122", border=NA)
x.tri <- c(50, 60, 40, 50)
y.tri <- c(60, 40, 40, 60)
polygon(x.tri, y.tri, col="#f0f0f0", border="#ffffff", lwd=4)

# Draw lines and segments.
x.from <- seq(10, 90, by=10)
y.from <- seq(10, 90, by=10)
x.to <- seq(20, 100, by=10)
y.to <- seq(10, 90, by=10)
lines(x.from, y.from)
segments(x.from, y.from, x.to, y.to)
segments(x.from, y.from, x.to, y.to, lty=2)


# Text
plot.new()
plot.window(xlim=c(0, 100), ylim=c(0, 100))
ticks <- seq(0, 100, 20)
par(las=1, cex=0.8)
axis(1, at=ticks, labels=ticks, pos=0)
axis(2, at=ticks, labels=ticks, pos=0)
#text(x.fakedata, y.fakedata, round(x.fakedata))
for (i in 1:length(x.fakedata)) {
	thesize <- 4*x.fakedata[i]/100
	thelabel <- round(x.fakedata[i])
	text(x.fakedata[i], y.fakedata[i], thelabel, cex=thesize, col="#333333")
}
mtext("x axis", side=1, font=2)
mtext("y axis", side=2, font=2)


# Apply to make sparklines.
par(mfrow=c(10, 3), xpd=NA, mar=c(2,0.5,2,0.5), lwd=0.5, cex=0.5)
colors <- c("#09c912", "#f40053", "#4ba9fd")
for (i in 1:30) {
	plot.new()
	plot.window(xlim=c(0, 50), ylim=c(0, 1))
	# axis(1, lwd=0.5)
	fakedata <- runif(50)
	colindex <- (i %% 3) + 1
	rect(0:49, 0, 1:50, fakedata, border=NA, col=colors[colindex])
	segments(c(10, 40), c(2, 2), c(10, 40), c(-2, -2), col="black", lwd=0.7, lty=3)
	mtext(i, side=3, adj=0, cex=0.5, font=2)
}




