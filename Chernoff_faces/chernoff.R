library(aplpack)

crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
faces(crime[,2:8])

crime_filled <- cbind(crime[,1:6], rep(0, length(crime$state)), crime[,7:8])
faces(crime_filled[,2:8])
faces(crime_filled[,2:8], labels=crime_filled$state)
