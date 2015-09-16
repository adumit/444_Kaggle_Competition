ratings <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/ratings.csv")
idmap <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/idmap.csv")
library(data.table)

takeSubsetRatings <- function(df, userDF) {
  id = userDF$UserID[1]
  temp <- subset(df, UserID == id)
  return(temp$Rating)
}

ratingsSub <- subset(ratings, UserID <= 100)
tempList <- by(ratingsSub, ratingsSub$UserID, function(x) takeSubsetRatings(ratings, x))
