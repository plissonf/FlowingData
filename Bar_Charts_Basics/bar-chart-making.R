#####
#
# How to Make a Bar Chart
#
#####


#
# Load data
#

income <- read.csv("data/income-totals.csv", stringsAsFactors=FALSE, sep=",", colClasses=c("FIPS"="character"))


#
# Default bar chart
#

barplot(income$med_income)


#
# Axes
#

# Labels
barplot(income$med_income, names.arg=income$name)

par(mar=c(8,4,2,2))
barplot(income$med_income, names=income$name, las=2, mgp=c(3,0.3,0))

# Rotate
par(mar=c(5, 6, 2, 2))
barplot(income$med_income, names.arg=income$name, las=2, horiz=TRUE, cex.names=0.6, cex.axis=0.8)




#
# Sorting
#

# By value
income_desc <- income[order(income$med_income, decreasing=FALSE),]
par(mar=c(5, 6, 2, 2))
barplot(income_desc$med_income, names.arg=income_desc$name, las=2, 
        horiz=TRUE, cex.names=0.6, cex.axis=0.8)

# By name
income_desc <- income[order(income$name, decreasing=TRUE),]
par(mar=c(5, 6, 2, 2))
barplot(income_desc$med_income, names.arg=income_desc$name, 
        las=2, horiz=TRUE, cex.names=0.6, cex.axis=0.8)


#
# Grouping
#

# Merge with state names
income0813 <- read.csv("data/income-2008-13.csv", stringsAsFactors=FALSE, 
                       sep=",", colClasses=c("FIPS"="character"))
income0813 <- merge(income[,c("FIPS", "name")], income0813, by="FIPS")


# Grouped bar chart
barplot(as.matrix(income0813[1:3, c("med2008", "med2013")]), beside=TRUE, 
        legend.text=income0813$name[1:3])

par(mar=c(8,4,2,2))
rotate <- function(x) t(apply(x, 2, rev))
income_rotated <- rotate(as.matrix(income0813[, c("med2008", "med2013")]))
barplot(income_rotated[,1:10], beside=TRUE, legend.text=c("2008", "2013"), names.arg=c(income0813$name[1:10]), cex.axis=0.7, cex.names=0.7, las=2)


# Indexed categories
regions <- read.csv("data/state_geocodes_v2011.csv", stringsAsFactors=FALSE, colClasses=c("Region"="character", "Division"="character", "FIPS"="character", "Name"="character"))
income_more <- merge(income, regions, by="FIPS")
unique_regions <- unique(income_more$Region)

bar_names <- c()
bar_heights <- c()
for (i in 1:length(unique_regions)) {
  
  curr_region <- subset(regions, Region==unique_regions[i] & FIPS=="00" & Division == "0")
  bar_names <- c(bar_names, NA, toupper(curr_region$Name))
  bar_heights <- c(bar_heights, NA, NA)
  
  states <- subset(income_more, Region == unique_regions[i])
  states <- states[order(states$med_income, decreasing=TRUE),]
  bar_names <- c(bar_names, states$Name)
  bar_heights <- c(bar_heights, states$med_income)
}

par(mar=c(5, 8, 2, 2))
barplot(rev(bar_heights), names.arg=rev(bar_names), las=2, horiz=TRUE, cex.names=0.7, cex.axis=0.7)



# Separate charts
par(mfrow=c(2,2), mar=c(5, 8, 4, 2))
for (i in 1:length(unique_regions)) {
	curr_region <- subset(regions, Region==unique_regions[i] & FIPS=="00" & Division == "0")
	
	states <- subset(income_more, Region == unique_regions[i])
	states <- states[order(states$med_income, decreasing=TRUE),]
	barplot(states$med_income, names.arg=states$Name, las=2, horiz=TRUE, cex.names=0.7, cex.axis=0.7, xlim=c(0, 75000), main=curr_region$Name)	
}


#
# Change the look
#

par(mar=c(5, 6, 4, 2))

# Space and border
barplot(states$med_income, names.arg=states$Name, las=2, horiz=TRUE, cex.names=0.7, cex.axis=0.7, xlim=c(0, 75000), main=curr_region$Name, border=NA, space=0.4)


# Color
barplot(states$med_income, names.arg=states$Name, las=2, horiz=TRUE, cex.names=0.8, cex.axis=0.7, xlim=c(0, 75000), main=curr_region$Name, border=NA, space=0.2, col="darkred")

barplot(states$med_income, names.arg=states$Name, las=2, horiz=TRUE, cex.names=0.8, cex.axis=0.7, xlim=c(0, 75000), main=curr_region$Name, border=NA, space=0.2, col=c(rep("gray", length(states$Name)-1), "darkred"))


# Random space and color
par(mfrow=c(3,3))
for (i in 1:9) {
	
	random_space <- runif(1, 0.01, 1.9)
	random_colors <- sample(colors(), length(states$Name))
	barplot(states$med_income, names.arg=states$Name, las=2, horiz=TRUE, cex.names=0.8, cex.axis=0.7, xlim=c(0, 75000), main=curr_region$Name, border=NA, space=random_space, col=random_colors)
}


# Grid lines
par(mar=c(5, 8, 5, 2))
barplot(rev(bar_heights), names.arg=rev(bar_names), las=1, horiz=TRUE, cex.names=0.7, cex.axis=0.7, border=NA, space=0.3, main="Median Household Income, 2013")
grid(NULL, NA, col="white", lty="solid", lwd=0.7)



