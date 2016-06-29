pctgdp <- read.csv('pct-gdp.txt')

#
# Parallel coordinates plot (not ideal)
#
library(lattice)
parallelplot(pctgdp)
parallelplot(pctgdp[, c("pct1970", "pct1979")], horizontal.axis=FALSE)

#
# Standard line chart
#
plot(0, 0, type="n", main="Receipts of Government as Percent of GDP", xlab="", ylab="", xlim=c(1970,1979), ylim=c(20,60))
for (i in 1:length(pctgdp$country)) {
    vals <- pctgdp[i,]
    lines(c(1970, 1979), c(vals$pct1970, vals$pct1979))
}

# Clean up
plot(0, 0, type="n", main="Receipts of Government\n as Percent of GDP", xlab="", ylab="", xlim=c(1970,1979), ylim=c(20,60), bty="n", las=1)
for (i in 1:length(pctgdp$country)) {
    vals <- pctgdp[i,]
    lines(c(1970, 1979), c(vals$pct1970, vals$pct1979))
}

# Add text
plot(0, 0, type="n", main="Receipts of Government\n as Percent of GDP", xlab="", ylab="", xlim=c(1970,1979), ylim=c(20,60), bty="n", las=1, axes=FALSE)
for (i in 1:length(pctgdp$country)) {
    vals <- pctgdp[i,]
    
    # Draw the line for current country
    lines(c(1970, 1979), c(vals$pct1970, vals$pct1979))
    
    # Left label
    text(1970, vals$pct1970, vals$pct1970)

    # Right label
    text(1979, vals$pct1979, vals$pct1979)
}

#
# Clean up text and add more labels
#
par(family="serif")
plot(0, 0, type="n", main="", xlab="", ylab="", xlim=c(1950,1990), ylim=c(20,60), bty="n", las=1, axes=FALSE)
for (i in 1:length(pctgdp$country)) {
    vals <- pctgdp[i,]
    
    # Draw the line for current country
    lines(c(1970, 1979), c(vals$pct1970, vals$pct1979))
    
    # Left label
    text(1970, vals$pct1970, vals$pct1970, pos=2, cex=0.8)
    text(1968, vals$pct1970, vals$country, pos=2, cex=0.8)

    # Right label
    text(1979, vals$pct1979, vals$pct1979, pos=4, cex=0.8)
    text(1981, vals$pct1979, vals$country, pos=4, cex=0.8)
}

# Title
text(1950, 55, "Current Receipts of Government as a\nPercentage of Gross Domestic\nProduct, 1970 and 1979", cex=0.9, pos=4, family="Helvetica")

# Year labels
text(1970, 60, "1970", cex=0.9, pos=2, offset=1)
text(1979, 60, "1979", cex=0.9, pos=4, offset=0.5)



# Find out available typefaces
names(pdfFonts())


#
# Sans for loop, same output as above
#
par(family="serif", mar=c(0,0,0,0))
plot(0, 0, type="n", main="", xlab="", ylab="", xlim=c(1950,1990), ylim=c(20,60), bty="n", las=1, axes=FALSE)
segments(rep(1970, length(pctgdp$country)), pctgdp$pct1970, rep(1979, length(pctgdp$country)), pctgdp$pct1979)
text(rep(1970, length(pctgdp$country)), pctgdp$pct1970, pctgdp$pct1970, pos=2, cex=0.8)
text(rep(1968, length(pctgdp$country)), pctgdp$pct1970, pctgdp$country, pos=2, cex=0.8)
text(rep(1979, length(pctgdp$country)), pctgdp$pct1979, pctgdp$pct1979, pos=4, cex=0.8)
text(rep(1981, length(pctgdp$country)), pctgdp$pct1979, pctgdp$country, pos=4, cex=0.8)

# Title
text(1950, 55, "Current Receipts of Government as a\nPercentage of Gross Domestic\nProduct, 1970 and 1979", cex=0.9, pos=4, family="Helvetica")

# Year labels
text(1970, 60, "1970", cex=0.9, pos=2, offset=1)
text(1979, 60, "1979", cex=0.9, pos=4, offset=0.5)



#
# Fix positioning, calculate slope
#
x0 <- c()
y0 <- c()
x1 <- c()
y1 <- c()

startyear <- 1970
stopyear <- 1979
xoffset <- 2
yoffset <- 0
ystartprev <- 0
ystopprev <- 0
ythreshold <- ( max(pctgdp$pct1970) - min(pctgdp$pct1970) ) * 0.025
for (i in length(pctgdp$country):1) {
    vals <- pctgdp[i,]
    
    ystartdiff <- (vals$pct1970+yoffset) - ystartprev
    if (abs(ystartdiff) < ythreshold) {
        yoffset <- yoffset + (ythreshold-ystartdiff)
    }
    
    # Calculate slope
    slope <- (vals$pct1979 - vals$pct1970) / (stopyear - startyear)
    
    # Intercept
    intercept <- vals$pct1970 + yoffset
    
    # Start and stop coordinates for lines
    ystart <- intercept
    ystop <- slope * (stopyear-startyear) + intercept
    ystopdiff <- ystop - ystopprev
    if (abs(ystopdiff) < ythreshold) {
        yoffset <- yoffset + (ythreshold-ystopdiff)
        intercept <- vals$pct1970 + yoffset
        ystart <- intercept
        ystop <- slope * (stopyear-startyear) + intercept
    }
    
    # Draw the line for current country
    x0 <- c(x0, startyear)
    y0 <- c(y0, ystart)
    x1 <- c(x1, stopyear)
    y1 <- c(y1, ystop)

    
    ystartprev <- ystart
    ystopprev <- ystop
}

ymin <- min(pctgdp$pct1970)
ymax <- max(c(pctgdp$pct1970, pctgdp$pct1979)) + yoffset

par(family="serif", mar=c(0,0,0,0))
plot(0, 0, type="n", main="", xlab="", ylab="", xlim=c(1950,1990), ylim=c(ymin,ymax*1.1), bty="n", las=1, axes=FALSE)
segments(x0, y0, x1, y1)
text(x0, y0, rev(pctgdp$pct1970), pos=2, cex=0.6)
text(x0-xoffset, y0, rev(pctgdp$country), pos=2, cex=0.6)
text(x1, y1, rev(pctgdp$pct1979), pos=4, cex=0.6)
text(x1+xoffset, y1, rev(pctgdp$country), pos=4, cex=0.6)

# Title
text(1950, ymax*1.06, "Current Receipts of\nGovernment as a Percentage\nof Gross Domestic Product,\n1970 and 1979", cex=0.8, pos=4)

# Year labels
text(startyear, ymax*1.1, "1970", cex=0.7, pos=2, offset=1)
text(stopyear, ymax*1.1, "1979", cex=0.7, pos=4, offset=0.5)



#
# Make it a function for easier usage
#
slopegraph <- function(startpts, endpts, labels) {
	
	x0 <- c()
	y0 <- c()
	x1 <- c()
	y1 <- c()
	
	startyear <- 1970
	stopyear <- 1979
	xoffset <- 2
	yoffset <- 0
	ystartprev <- 0
	ystopprev <- 0
	ythreshold <- ( max(startpts) - min(startpts) ) * 0.025
	
	for (i in length(startpts):1) {

        ystartdiff <- (startpts[i]+yoffset) - ystartprev
        if (abs(ystartdiff) < ythreshold) {
            yoffset <- yoffset + (ythreshold-ystartdiff)
        }

        # Calculate slope
        slope <- (endpts[i] - startpts[i]) / (stopyear - startyear)

        # Intercept
        intercept <- startpts[i] + yoffset

        # Start and stop coordinates for lines
        ystart <- intercept
        ystop <- slope * (stopyear-startyear) + intercept
        ystopdiff <- ystop - ystopprev
        if (abs(ystopdiff) < ythreshold) {
            yoffset <- yoffset + (ythreshold-ystopdiff)
            intercept <- startpts[i] + yoffset
            ystart <- intercept
            ystop <- slope * (stopyear-startyear) + intercept
        }

        # Draw the line for current country
        x0 <- c(x0, startyear)
        y0 <- c(y0, ystart)
        x1 <- c(x1, stopyear)
        y1 <- c(y1, ystop)


        ystartprev <- ystart
        ystopprev <- ystop
    }
    
    ymin <- min(startpts)
    ymax <- max(c(startpts, endpts)) + yoffset

    par(family="serif", mar=c(0,0,0,0))
    plot(0, 0, type="n", main="", xlab="", ylab="", xlim=c(1950,1990), ylim=c(ymin,ymax*1.1), bty="n", las=1, axes=FALSE)
    segments(x0, y0, x1, y1)
    text(x0, y0, rev(startpts), pos=2, cex=0.6)
    text(x0-xoffset, y0, rev(labels), pos=2, cex=0.6)
    text(x1, y1, rev(endpts), pos=4, cex=0.6)
    text(x1+xoffset, y1, rev(labels), pos=4, cex=0.6)
    
    # Year labels
    text(startyear, ymax*1.1, deparse(substitute(startpts)), cex=0.7, pos=2, offset=1)
    text(stopyear, ymax*1.1, deparse(substitute(endpts)), cex=0.7, pos=4, offset=0.5)
}


