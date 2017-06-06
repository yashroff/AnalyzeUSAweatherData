require(data.table)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
f <- file.path(getwd(), "stormData.csv.bz2")
download.file(url, f)
sd <- data.table(read.csv(f))



