# Load data
crime <- read.csv("crimeRatesByState2008.csv", header=TRUE, sep="\t")
symbols(crime$murder, crime$burglary, circles=1)

# Wrong sizes for radius
symbols(crime$murder, crime$burglary, circles=crime$population/1000)

# Correctly sized bubbles
radius <- sqrt( crime$population/ pi )
symbols(crime$murder, crime$burglary, circles=radius)

# Try squares
symbols(crime$murder, crime$burglary, squares=sqrt(crime$population), inches=0.5)

# Size circles smaller
symbols(crime$murder, crime$burglary, circles=radius, inches=0.35, fg="white", bg="red", xlab="Murder Rate", ylab="Burglary Rate")

# Add labels
text(crime$murder, crime$burglary, crime$state, cex=0.5)
