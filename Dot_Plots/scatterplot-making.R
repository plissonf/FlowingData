#####
#
# How to Make a Scatterplot
#
#####


#
# Load data
#

income <- read.csv("data/ACS_13_1YR_S2401-by-occ.csv", stringsAsFactors=FALSE, sep=",")
income1 <- subset(income, level == 1)

#
# Default dot plot
#

plot(income1$med_salary_male, income1$med_salary_female)



#
# Easier comparison
#

# Summary stat
summary(income1)

# Fixed axis limits
plot(income1$med_salary_male, income1$med_salary_female, xlim=c(10000, 75000), ylim=c(10000, 75000))



#
# Highlight
#

plot(income1$med_salary_male, income1$med_salary_female, xlim=c(10000, 75000), ylim=c(10000, 75000), asp=1)
abline(0, 1)
text(40000, 40000, "Equal median salary")
text(60000, 30000, "Men make more")
text(30000, 60000, "Women make more")


#
# Adjust
#

par(mar=c(5,6,3,2))
plot(income1$med_salary_male, income1$med_salary_female, xlim=c(10000, 75000), ylim=c(10000, 75000), asp=1, xlab="Men's Salary", ylab="", las=1, cex.axis=0.8, main="Median Salary Comparison by Occupation", bty="n", type="n")
grid(NULL, NULL, lwd=1.2)
points(income1$med_salary_male, income1$med_salary_female, pch=19)
abline(0, 1, col="blue", lty=5)
text(60000, 60000, "Equal median salary", srt=45, pos=3, offset=0.5, font=4, col="blue")
text(60000, 30000, "MEN MAKE MORE", cex=0.8)
text(30000, 60000, "WOMEN MAKE MORE", cex=0.8)
title(ylab="Women's salary", line=4)


# Different reference lines
plot(income1$med_salary_male, income1$med_salary_female, xlim=c(10000, 75000), ylim=c(10000, 75000), asp=1, xlab="", ylab="", las=1, cex.axis=0.8, main="", bty="n", type="n")
abline(0, 1, lty=1, lwd=2)
text(60000, 60000, "Equal line", srt=45, pos=3, offset=0.5, font=4, col="black")

for (prop in seq(0.2, 0.9, by=0.1)) {
	abline(0, prop, lty=3)
	text(60000, 60000*prop, paste(100*prop, "%", sep=""), srt=45*prop, pos=3, offset=0.5, font=4, col="darkgray")
}





# Point types for reference
par(mar=c(0,2,2,2))
plot(0, 0, type="n", xlim=c(0,5.1), ylim=c(-0.5,2.1), axes=FALSE, xlab=NA, ylab=NA)
row <- 2; column <- 0; currpch <- 0
for (j in row:0) {
	for (i in column:5) {
		points(i, j, pch=currpch, cex=5)
		text(i, j-0.3, currpch, cex=0.8)
		currpch <- currpch + 1
	}
	
}


#
# Overplotting problem
#

# Blob
counties <- read.csv("data/ACS_13_5YR_B20002/ACS_13_5YR_B20002.csv", stringsAsFactors=FALSE)
plot(counties$HD02_VD03, counties$HD02_VD04, xlim=c(0,60000), ylim=c(0,60000), asp=1)

# Change opacity on points
plot(counties$HD02_VD03, counties$HD02_VD04, xlim=c(0,60000), ylim=c(0,60000), type="n", asp=1)
points(counties$HD02_VD03, counties$HD02_VD04, pch=20, col="#00000010")

# Zoom in
plot(counties$HD02_VD03, counties$HD02_VD04, xlim=c(0,10000), ylim=c(0,10000), type="n", asp=1, xlab="Men's salary", ylab="Women's salary", bty="n", main="Median Salary by County")
grid(NULL, NULL, lwd=1.2)
points(counties$HD02_VD03, counties$HD02_VD04, pch=20, col="#40000015")
abline(0, 1, col="darkblue", lty=5)
text(8000, 8000, "Equal median salary", srt=45, pos=3, offset=0.5, font=4, col="darkblue")



#
# Color
#

dotcol <- rep("#00000020", dim(counties)[1])
dotcol[counties$HD02_VD03 < counties$HD02_VD04] <- "#40000030"
plot(counties$HD02_VD03, counties$HD02_VD04, xlim=c(0,10000), ylim=c(0,10000), type="n", asp=1, xlab="Men's salary", ylab="Women's salary", bty="n", main="Median Salary by County")
grid(NULL, NULL, lwd=1.2)
points(counties$HD02_VD03, counties$HD02_VD04, pch=20, col=dotcol, cex=0.8)
abline(0, 1, col="darkblue", lty=5)
