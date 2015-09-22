ratings <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/ratings.csv")
idmap <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/idmap.csv")

#Does the total number of ratings that a profile got correlate with the mean rating of that user?
getNumRatedAndMeanRatings <- function(itemDF) {
  return(c(nrow(itemDF), mean(itemDF$Rating)))
}

test <- by(ratings, ratings$ProfileID, function(x) getNumRatedAndMeanRatings(x))
test2 <- matrix(unlist(test), ncol = 2, byrow = 2)
test3 <- as.data.frame(test2)
names(test3) <- c("NumRatings", "MeanRating")
plot(test3$NumRatings, test3$MeanRating)
test4 <- subset(test3, NumRatings <= 1000)
plot(test4$NumRatings, test4$MeanRating)
cor(test4$NumRatings, test4$MeanRating)

