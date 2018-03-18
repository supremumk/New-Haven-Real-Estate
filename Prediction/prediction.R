##### Peer Viewed by Meng Kuang. 
#### 1. Training and testing dataset do not contain pid. So indicators 
#### in need of pid below are converted to the order of lines. No extra comments below for that.
#### 2. The "dist" variable in lm model needs loop through the whole dataset. So
#### when testing, it is actually another run of the whole loop. It is not efficient.
#### So in the end, I offered a way to store the coeffs when training. However,
#### "dist" cannot included in linear model.
#### 3. I believe what matters most in this model should be the selection of variables
#### and judge if they have some interactions. Original script is not convincing in 
#### this part.


################## Original script starts here.##################
# Case studies. Revised evaluation model
# Throughout this evaluation we are going to be using the convention that
# pid1 = property we want to evaluate  & pid2 = compareable property
x <- read.csv("NH_training.csv") 
# keepForTest <- df[df$pid == 1733, ] ### mk2297 ### no need to do this
keepStyle <- c("Colonial", "Ranch", "Bungalow", "Raised Ranch", "2 Family", "Condominium","Contemporary", "3 Family",
               "4 Family", NA, "Rectory", "Duplex", "Apt House", "Cottage", "Apt 1-7 st", "Mix Off Apt", "Family Convers",
               "Rooming House", "Apt 8s Hi-Rise", "Half Duplex", "Row House", "Retail Condo", "Indust Condo", "Apt / Res Garage",
               "M/U CONDO", "Apt / Res Garage", "Old Style")
### mk2297 ### It's better to have more lines above & the reason for keeping
### these styles is unclear

dropStyle <- unique(df$style[!df$style %in% keepStyle])
### mk2297 ### by table(df$zone) we can see that they are all from residential
### zones so I think the classfication by occupancy is not convincing.
# drop occupancy = 0 or if occupancy > =20 or is not a residencial type of housing

residencialDf <- df[df$style %in% keepStyle, ]
residencialDf <- residencialDf[ which(residencialDf$occupancy <= 20) , ]
# residencialDf <- rbind(residencialDf, keepForTest)
x <- residencialDf # for simplicity
x <- x[!is.na(x$model), ] ### mk2297 ### unnecessary
# for longitude and latitude, we know that the fifth decimal place is worth up to 1.1 m.
# Lets also say that an average block is about 90 meters => 0.0009 of a change to the right or the left 
radius <- 0.001  #our radius based upon a margin of error for 3 blocks. We will use this to determine
#properties that are in similar areas. We will reject properties not in this radius. 
# define a distance metric
### mk2297 ### radius seem a great measure but not used below!

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
# compute the distance between 2 properties
propDist <- function(pid1, pid2) {
  coords1 <- c(x$longitude[pid1], x$latitude[pid1])
  coords2 <- c(x$longitude[pid2], x$latitude[pid2])
  dist <- euc.dist(coords1, coords2)
  return(dist)
}


# Begin statistics loop 
for( z in 1:dim(x[1]) ){
#pid1 <- x$pid
# We want to take a pid then only compare houses that are in the same neighborhood. 
# for sanity
#i <- which(x$pid==pid1)
pid1 <- z # the property that we want to evaluate. Eventually we will cycle through this.
sameNeighborhood <- which(x$neighborhood == x$neighborhood[pid1])
comprableProperties <- x[sameNeighborhood, ]
# create a distance variable from the property to all others
cc<-c()
for ( k in 1:length(sameNeighborhood )) { cc[k]<-propDist(pid1, sameNeighborhood[k]) }
comprableProperties$dist <- cc
# change some variables to factor
comprableProperties$style <- as.factor(comprableProperties$style)
comprableProperties$grade <- as.factor(comprableProperties$grade)
# create a realized value for avg data distance
comprableProperties$date1 <- as.Date(comprableProperties$date1, "%m/%d/%Y", origin = "1800-01-01")
comprableProperties$date2 <- as.Date(comprableProperties$date2, "%m/%d/%Y", origin = "1800-01-01")
comprableProperties$date3 <- as.Date(comprableProperties$date3, "%m/%d/%Y", origin = "1800-01-01")
comprableProperties$date4 <- as.Date(comprableProperties$date4, "%m/%d/%Y", origin = "1800-01-01")
comprableProperties$date5 <- as.Date(comprableProperties$date5, "%m/%d/%Y", origin = "1800-01-01")
### mk2297 ### Wrong format of date. Has been modified above.
timechange <- vector(mode = "numeric")
avgDate <- vector(mode = "numeric")
for( w in 1:dim(comprableProperties)[1] ) {
  for (i in 0:4) {
    timechange[i + 1] <- abs(as.numeric(comprableProperties[w, 8 + i]) -
                                             as.numeric(comprableProperties[w, 10 + i]))
    }
  avgDate[w] <- sum(timechange, na.rm = TRUE)/5
  timechange <- vector(mode = "numeric")
}
avgDate[avgDate == 0] <- NA
comprableProperties$dateDist <- avgDate
### mk2297 ### It's a fantastic idea to create a variable "avgDate", but it is not
### included in the model below...Maybe the depreciation of dollars should be considered
### along with the date...kind of complicated, and maybe we can simply drop date info.

# create a linear model from this 
mygreatlm <- lm(totval ~  log(sqft) + acres + finalbaths +
                 pctgood + style * dist *
                 grade + garagesqft , data= comprableProperties)
### mk2297 ### Some interactions are considered in this model, though I can't see
### by intuition why it is among style, dist,and grade
# get the total value of the property we want to estimate
totval <- x$totval[z]
# Statistics from this linear model
estval <- as.numeric(predict(mygreatlm, newdata=comprableProperties[z,, drop=FALSE]))
std.err <- coef(summary(mygreatlm))[2, 2]
### mk2297 ### std.err should be assigned a value first so I put it up here. And[2,2] 
### is the std.err only for log(sqft). I can't see why take this one.
# statistic <- (estval - totval) / std.err
statistic <- (estval - totval) / sd(mygreatlm$residuals)
p.value <- 2 * pnorm(-abs(statistic))
conf.int <- estval + c(-2, 2) * sd(mygreatlm$residuals)
# append this to our dataset x
x$estval[z] <- estval
x$statistic[z] <- statistic
x$p.value[z] <- p.value
x$conf.int1[z] <- max(conf.int[1],0) ### mk2297### lower bound should be >=0
x$conf.int2[z] <- conf.int[2]
}
write.csv(x, file = "NH_Prediction_1.csv", row.names=FALSE)
### mk2297 ### It's better to delete the row names here.
### For testing, we only need to let x <- read.csv("NH_test.csv") and run the 
# above lines. 

### mk2297### Below is another way to do this. I think this way is more efficient despite of
### losing the variable "dist"
# get the training model for each neighborhood
x <- read.csv("NH_training.csv") 
x$neighborhood[which(x$neighborhood=="IND5")]<-"0900"
# From the loctaion, we put it into "0900" neighborhood
x$neighborhood<- as.numeric(as.character(x$neighborhood))
# To match the neighorhood format in NH_test.csv
nbname<-names(table(x$neighborhood))
trainmodel <-list()
for (i in 1:(length(nbname))){
  sameNeighborhood <- which(x$neighborhood==nbname[i])
  comprableProperties <- x[sameNeighborhood,]
  # change some variables to factor
  comprableProperties$style <- as.factor(comprableProperties$style)
  comprableProperties$grade <- as.factor(comprableProperties$grade)
  trainmodel[[i]]<- lm(totval ~  log(sqft) + acres + finalbaths + pctgood, 
                    data= comprableProperties)
}
#### Use training model to predict
test <- read.csv("NH_test.csv") 
table(test$neighborhood)
for (i in (1:dim(test)[1])){
nb<-test$neighborhood[i]
ind <- which(nbname==nb)
mygreatlm <- trainmodel[[ind]]
estval <- as.numeric(predict(mygreatlm, newdata=test[i,, drop=FALSE]))
statistic <- (estval - test$totval[i]) / sd(mygreatlm$residuals)
p.value <- 2 * pnorm(-abs(statistic))
conf.int <- estval + c(-2, 2) * sd(mygreatlm$residuals)
test$estval[i] <- estval
test$statistic[i] <- statistic
test$p.value[i] <- p.value
test$conf.int1[i] <- max(conf.int[1],0) ### mk2297### lower bound should be >=0
test$conf.int2[i] <- conf.int[2]
}
#### To see the general trend 
plot(test$totval,test$estval)
y <- data.frame(test$estval, test$statistic, test$p.value, test$conf.int1, test$conf.int2)
write.csv(y, file = "NH_PredTest_1.csv", row.names=FALSE) 
