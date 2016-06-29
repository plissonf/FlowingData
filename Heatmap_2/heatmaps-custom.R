# Standard heat map (More details here: http://datafl.ws/2x)
leaders <- read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")
leaders <- leaders[order(leaders$PTS),]
row.names(leaders) <- leaders$Name
leaders <- leaders[,2:20]
nba_matrix <- data.matrix(leaders)
heatmap(nba_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))




# Load play-by-play data.
nba <- read.csv("data/2008-09nba.trunc.csv", sep=",")
shots <- nba[nba$etype == "shot",]

# Take a quick look.
shots[1:10,]

# Some are NA. Get only known shots.
shots.known <- shots[!is.na(shots$x) & !is.na(shots$y),]

# Golden State Warriors shots
gsw <- subset(shots.known, team == "GSW")

# Basic symbol shot chart.
symbols(gsw$x, gsw$y, circles=rep(1,length(gsw$x)), asp=1, inches=FALSE)

# Aggregate by shot frequency
library(plyr)
gsw.agg <- count(gsw, c("x","y"))

# Size symbols by number of shots.
symbols(gsw.agg$x, gsw.agg$y, circles=sqrt(gsw.agg$freq)/8, asp=1, inches=FALSE, ylim=c(0,40))
symbols(gsw.agg$x, gsw.agg$y, squares=sqrt(gsw.agg$freq)/8, asp=1, inches=FALSE, ylim=c(0,40))

# Hexbins
library(hexbin)
h <- hexbin(gsw$x, gsw$y, xbins=20, shape=80/50)
plot(h)


# Helper function to get color by shot frequency
getColor <- function(val) {
	
	minVal <- log(1)		# Using logarithmic scale
	maxVal <- log(1065)	
	
	numCols <- 20
	pal <- colorRampPalette(c("#f4d5d0", "#692518"))
	cols <- pal(numCols)
	
	# Get index to pick color.
	colIndex <- round(numCols * (log(val) - minVal) / (maxVal - minVal))
	colIndex <- max(1, colIndex)
	
	return(cols[colIndex])
}

# Get colors and draw heat map of shot frequency.
gridColors <- sapply(gsw.agg$freq, getColor)
symbols(gsw.agg$x, gsw.agg$y, squares=rep(1,length(gsw.agg$x)), asp=1, inches=FALSE, ylim=c(0,40), bg=gridColors, fg=NA)



# Find points per shot for each spot on court.
ptsper <- c()
for (i in 1:length(gsw.agg[,1])) {
	
	xcoord <- gsw.agg$x[i]
	ycoord <- gsw.agg$y[i]
	spot <- subset(gsw, x == xcoord & y == ycoord)
	totalShots <- length(spot[,1])
	totalPts <- sum(spot$points[!is.na(spot$points)])
	
	ptsper <- c(ptsper, totalPts/totalShots)
}



# Helper function to get color by points per shot.
getColorByPoints <- function(val) {
	
	minVal <- 0
	maxVal <- 3
	
	numCols <- 30
	
	pal <- colorRampPalette(c("#eddfab", "#d86853"))
	cols <- pal(numCols)
	
	# Get index to pick color.
	colIndex <- round(numCols * (val - minVal) / (maxVal - minVal))
	colIndex <- max(1, colIndex)
	
	return(cols[colIndex])
}

# Get colors and draw heat map of points per shot.
gridColors <- sapply(ptsper, getColorByPoints)
plot(0, 0, xlim=c(0,50), ylim=c(0,40), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
symbols(gsw.agg$x, gsw.agg$y, squares=sqrt(gsw.agg$freq)/3, asp=1, inches=FALSE, add=TRUE, bg=gridColors, fg=NA)



# Adding an NBA court outline for kicks and giggles.
draw.arc(25, 5.25, 9/12, angle1=0, angle2=2*pi, col="#cccccc", lwd=2) # Hoop
lines(c(22,28), c(4,4), col="#cccccc", lwd=2)	# Backboard
lines(c(2.5, 2.5), c(0, 13.5), col="#cccccc", lwd=2) # Side 3-pt
lines(c(47.5, 47.5), c(0, 13.5), col="#cccccc", lwd=2)
lines(c(19,19), c(0,19), col="#cccccc", lwd=2)	# Inside lane
lines(c(31,31), c(0,19), col="#cccccc", lwd=2)
lines(c(19,31), c(19,19), col="#cccccc", lwd=2)	# Free throw
lines(c(0,50),c(0,0), col="#cccccc", lwd=2)	# Baseline
draw.arc(25, 5.25, 23.75, angle1=pi/9.8, angle2=pi/1.113, col="#cccccc", lwd=2) # 3-pt arc


