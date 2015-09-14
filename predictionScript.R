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

numSimilar <- matrix(unlist(a), ncol = 1, byrow = T)
userIDs <- names(a)
similarDF <- data.frame(userIDs, numSimilar)
similarDF <- subset(similarDF, numSimilar >= 10)
ratingsSimilar <- subset(ratings, UserID %in% similarDF$userIDs)

calcSimilarity <- function(userDF, similarUserDF) {
  userRatings <- subset(userDF, ProfileID %in% intersect(userDF$ProfileID, similarUserDF$ProfileID))
  similarUserRatings <- subset(similarUserDF, ProfileID %in% intersect(userDF$ProfileID, similarUserDF$ProfileID))
  return(dist(rbind(userRatings$Rating, similarUserRatings$Rating), method = "euclidean"))
}

calcSimilarUsers <- function(userID, ratingsDF, NcommonRatings, NtopUsers) {
  #Find the number of common ratings between given userID and all other users
  #Returns 0 for given userID so as to not include in final output
  commonRatingUsers <- usersWithMoreThan10CommonRatings(ratingsDF, userID)
  #Create dataframe with userID and numSimilar as columns
  numSimilar <- matrix(unlist(a), ncol = 1, byrow = T)
  userIDs <- names(a)
  similarDF <- data.frame(userIDs, numSimilar)
  #Get all users with equal to or more similar ratings than provided input
  similarDF <- subset(similarDF, numSimilar >= NcommonRatings)
  #Subset entire ratings dataframe to only get relevent ratings for next action - makes the next action faster
  ratingsSub <- subset(ratings, UserID %in% similarDF$userIDs)
  #Get similarities between all users and provided user
  userSimilarities <- by(ratingsSub, ratingsSub$UserID, function(x) calcSimilarity(u1, x))
  #Sort similarities
  sortedSimilarities <- sort(userSimilarities, decreasing = T)
  #Get top N users - input provided
  topN <- sortedSimilarities[1:NtopUsers]
  #Subset ratings further for only the truly relevent ratings
  similarUsers <- subset(ratings, UserID %in% names(topN))
  return(similarUsers)
}

system.time(
  userSimilarities <- by(ratingsSimilar, ratingsSimilar$UserID, function(x) calcSimilarity(u1, x))
)

sortedSimilarities <- sort(userSimilarities, decreasing = T)
top100 <- sortedSimilarities[1:100]
similarUsers <- subset(ratings, UserID %in% names(top100))

mean(similarUsers$Rating[similarUsers$ProfileID == 160])
preds <- c()
for (i in u1$ProfileID) {
  preds[length(preds) + 1] <- mean(similarUsers$Rating[similarUsers$ProfileID == i], na.rm = T)
}
diff <- abs(u1$Rating - preds)
mean(diff)

predictRating <- function(similarUserDF, predID) {
  return(mean(similarUserDF$Rating[similarUserDF$Profile == predID]))
}
predictRating(similarUsers, 79)

a <- subset(idmap, UserID == 1)
b <- subset(similarUsers, ProfileID == 38)

# Try standardizing ratings, then for prediction for user 1, it is: 
#                   mean(user1Ratings) + sd(user1Ratings)*mean(sd(similarUsers))
