# Type 0: stacked area, 1: themeriver, 2: streamgraph
areaGraph <- function(thedata, type=2, smooth=TRUE) {
	
	# Color palette
	nColors <- 15
	# pal <- colorRampPalette(c("#0f7fb4", "#e2e2e2"))
	# pal <- colorRampPalette(c("#6364a9", "#e2e2e2"))  # Purple
	pal <- colorRampPalette(c("#48611d", "#f0f0f0"))
	colors <- pal(nColors)
	
	# Sort the data
	if (type == 0) {					# Stacked area
		
		# Greatest to least weights
		sortedData <- thedata[order(rowSums(thedata), decreasing=TRUE),]
		layerNames <- rownames(sortedData)
		
	} else if (type == 1 || type == 2) {				# Themeriver or streamgraph
		
		# Initialize sorted data frame
		sortedData <- thedata[1,]
		weights <- rowSums(thedata)
		topWeight <- weights[1]
		bottomWeight <- weights[1]
		layerNames <- c(rownames(thedata)[1])
		
		if (length(thedata[,1]) > 1) {
			
			# Commence sorting. Apparently not most efficient way, but whatever.
			for (i in 2:length(thedata[,1])) {
			
				if (topWeight > bottomWeight) {
					sortedData <- rbind(sortedData, thedata[i,])
					layerNames <- c(layerNames, rownames(thedata)[i])
				} else {
					sortedData <- rbind(thedata[i,], sortedData)
					bottomWeight <- bottomWeight + weights[i]
					layerNames <- c(rownames(thedata)[i], layerNames)
				}
			}
		}

	}
	
	# Smooth the data
	if (smooth) {
		
		nPoints <- length(thedata[1,]) * 3
		
		# Initialize smoothed data. Note: Probably a better way to do this, but it works. [NY]
		firstRow <- spline(1:length(sortedData[1,]), sortedData[1,], nPoints)$y
		firstRow <- sapply(firstRow, zeroNegatives)
		
		smoothData <- data.frame( rbind(firstRow, rep(0, length(firstRow))) )
		smoothData <- smoothData[1,]
		
		# Smooth the rest of the data using spline().
		if (length(sortedData[,1]) > 1) {
		
			for (i in 2:length(sortedData[,1])) {	
				newRow <- spline(1:length(sortedData[i,]), sortedData[i,], nPoints)$y
				newRow <- sapply(newRow, zeroNegatives)
				smoothData <- rbind(smoothData, newRow)
			}
		}
		
		finalData <- smoothData
	
	} else {
		
		finalData <- sortedData
	
	}
	
	
	# Totals for each vertical slice
	totals <- colSums(finalData)
	
	# Determine baseline offset
	if (type == 0) {
		
		yOffset <- rep(0, length(totals))
		
	} else if (type == 1) {
		
		yOffset <- -totals / 2
		
	} else if (type == 2) {
		n <- length(finalData[,1])
		i <- 1:length(finalData[,1])
		parts <- (n - i + 1) * finalData
		theSums <- colSums(parts)
		yOffset <- -theSums / (n + 1)	
	}
	
	
	# Axis upper and lower bounds
	yLower <- min(yOffset)	
	yUpper <- max(yOffset + totals)
	
	# Max, min, and span of weights for each layer
	maxRow <- max(rowSums(finalData))
	minRow <- min(rowSums(finalData))
	rowSpan <- if ( (maxRow - minRow) > 0 ) { maxRow - minRow } else { 1 }
	
	# Make the graph.
	par(las=1, cex=0.6, bty="n")
	xtext <- c(); ytext <- c(); textSizes <- c()
	
	plot(0, 0, type="n", xlim=c(1, length(finalData[1,])), ylim=c(yLower, yUpper), xlab=NA, ylab=NA)
	for (i in 1:length(finalData[,1])) {
		
		colIndex <- floor( (nColors-2) * ( (maxRow - sum(finalData[i,])) / rowSpan ) ) + 1
		polygon(c(1:length(finalData[i,]), length(finalData[i,]):1), c(finalData[i,] + yOffset, rev(yOffset)), col=colors[colIndex], border="#ffffff", lwd=0.2)

		
		# Label locations
		xmax <- which.max(finalData[i,])
		xtext <- c(xtext, xmax)
		ytext <- c(ytext, finalData[i,xmax] / 2 + yOffset[xmax])
		textSizes <- c(textSizes, 1.7 * sqrt( (nColors-colIndex) / nColors ))

		# Move up to next layer.
		yOffset <- yOffset + finalData[i,]
	}
	
	
	# Add labels last.
	if (length(layerNames) > 0) {
		text(xtext, ytext, layerNames, cex=textSizes)
	}
	
}


# Helper function to convert negative values to zero
zeroNegatives <- function(x) {
	if (x < 0) { return(0) }
	else { return(x) }
}