# Hello, world.
plot(0, 0, type="n", xlim=c(0, 2), ylim=c(0, 2), xlab="", ylab="")
text(1, 1, 'Hello, world.')

# Hello, world. x3
plot(0, 0, type="n", xlim=c(0, 2), ylim=c(0, 2), xlab="", ylab="")
text(1, 1, 'Hello, world.')				# Middle
text(1, 2, 'Hello, top of world.') 		# Top
text(1, 0, 'Hello, bottom of world.')	# Bottom

# Hello, world. x3 in one call
plot(0, 0, type="n", xlim=c(0, 2), ylim=c(0, 2), xlab="", ylab="")
x <- c(1, 1, 1)
y <- c(1, 2, 0)
labels <- c('Hello, world.', 'Hello, top of world.', 'Hello, bottom of world.')
text(x, y, labels)

# Available font families
names(pdfFonts())
plot(0, 0, type="n", xlim=c(0, 2), ylim=c(0, 2), xlab="", ylab="")
text(x, y, labels, family='Courier')
text(1, 1.5, 'Helvetica', family='Helvetica')
text(1, 0.5, 'Bookman', family='Palatino')

# Font size
plot(0, 0, type="n", xlim=c(0, 2), ylim=c(0, 2), xlab="", ylab="")
text(x, y, labels, family='Courier', cex=1.0)
text(1, 1.5, 'Helvetica', family='Helvetica', cex=3.0)
text(1, 0.5, 'Bookman', family='Palatino', cex=0.5)

# Color
plot(0, 0, type="n", xlim=c(0, 2), ylim=c(0,20), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
rect(0, 0, 2, 30, col="black")
for (i in 1:40) {
	text(1, i/2, colors()[i], col=colors()[i], cex=runif(1, 0.2, 1.3))
}

# Put it into practice with real data
load('unisexCnts.RData')
nameTotals <- rowSums(unisexCnts)
plot(0, 0, type="n", xlim=c(-5, 105), ylim=c(-5,105), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
x <- runif(length(nameTotals), 0, 100)
y <- runif(length(nameTotals), 0, 100)
text(x, y, names(nameTotals), cex=2*sqrt(nameTotals/max(nameTotals)))

# Supplement the area graph (http://datafl.ws/21a) with labels
source('areagraph.R')
areaGraph(unisexCnts)

