#### Geocoding.
#### Meng Kuang, 09/21/2016

#### combine all the existing location info
files <- dir("Sept15geocoded", full.names = TRUE)
z <- lapply(files, read.csv, as.is=TRUE)
for (i in 2:length(z)) {
    ind <- !is.na(z[[i]][,3])
    z[[1]][which(ind),3:4] <- z[[i]][which(ind),3:4]
}
mydata <- z[[1]]
#### Get and clean up values of the missing lats & cons.
#### Find the range of missing values
miss <- mydata$pid[which(is.na(mydata[,3]))]
#length(miss)
if (!require(ggmap)) install.packages("ggmap")
library(ggmap)
locs <- paste(mydata$location[miss], ", New Haven, CT", sep="")
ans <- geocode(locs)
mydata$lon[miss] <- ans$lon
mydata$lat[miss] <- ans$lat
write.csv(mydata, "geocode_HW_mk2297.csv", row.names = FALSE)

# Compare them with Census Bureau latitudes/longitudes+ sanity check 
Bureau<- read.csv("./GeocodePython/address_lat_long.csv")
both <- data.frame(mydata, Bureau)
sapply(both, length)
sapply(both,function(x) length(which(is.na(x)))) # CB has more NAs
dist <- sqrt((both[, 3]-both[, 7])^2 + (both[, 4]-both[, 8])^2) 
length(dist)
hist(dist)
