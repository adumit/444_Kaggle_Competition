ratings <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/ratings.csv")
idmap <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/idmap.csv")

giveMeans <- by(ratings$Rating, ratings$UserID, mean)
ratedMeans <- by(ratings$Rating, ratings$ProfileID, mean)

findNumSimilar <- function(userDF, user2DF) {
  if (user2DF$UserID[1] == userDF$UserID[1]) {
    return(0)
  }
  else {
    return(length(intersect(userDF$ProfileID, user2DF$ProfileID)))
  }
}

usersWithMoreThan10CommonRatings <- function(ratingsDF, userID) {
  userDF <- subset(ratingsDF, UserID == userID)
  listOfRated <- userDF$ProfileID
  similarUsers <- by(ratings, ratings$UserID, function(x) findNumSimilar(x, userDF))
  return(similarUsers)
}
#Takes ~1.5minutes
system.time(
  a <- usersWithMoreThan10CommonRatings(ratings, 1)
)


