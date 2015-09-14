ratings <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/ratings.csv")
idmap <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/idmap.csv")

findNumSimilar <- function(userDF, user2DF) {
  if (user2DF$UserID[1] == userDF$UserID[1]) {
    return(0)
  }
  else {
    return(length(intersect(userDF$ProfileID, user2DF$ProfileID)))
  }
}

getNumCommonRatings <- function(ratingsDF, userDF) {
  similarUsers <- by(ratingsDF, ratingsDF$UserID, function(x) findNumSimilar(x, userDF))
  return(similarUsers)
}

calcSimilarity <- function(userDF, similarUserDF) {
  userRatings <- subset(userDF, ProfileID %in% intersect(userDF$ProfileID, similarUserDF$ProfileID))
  similarUserRatings <- subset(similarUserDF, ProfileID %in% intersect(userDF$ProfileID, similarUserDF$ProfileID))
  return(dist(rbind(userRatings$Rating, similarUserRatings$Rating), method = "euclidean"))
}

system.time(
  c <- calcSimilarity(u1, u2)
)

calcSimilarUsers <- function(userID, ratingsDF, NcommonRatings, NtopUsers) {
  #Create user dataframe for given userID
  userDF <- subset(ratingsDF, UserID == userID)
  #Find the number of common ratings between given userID and all other users
  #Returns 0 for given userID so as to not include in final output
  ratingsSub <- subset(ratingsDF, ProfileID %in% userDF$ProfileID)
  commonRatingUsers <- getNumCommonRatings(ratingsSub, userDF)
  #Create dataframe with userID and numSimilar as columns
  numSimilar <- matrix(unlist(commonRatingUsers), ncol = 1, byrow = T)
  userIDs <- names(commonRatingUsers)
  similarDF <- data.frame(userIDs, numSimilar)
  #Get all users with equal to or more similar ratings than provided input
  similarDF <- subset(similarDF, numSimilar >= NcommonRatings)
  #Subset entire ratings dataframe to only get relevent ratings for next action - makes the next action faster
  ratingsSub <- subset(ratingsDF, UserID %in% similarDF$userIDs)
  #Get similarities between all users and provided user
  userSimilarities <- by(ratingsSub, ratingsSub$UserID, function(x) calcSimilarity(userDF, x))
  #Sort similarities
  sortedSimilarities <- sort(userSimilarities, decreasing = T)
  #Get top N users - input provided
  topN <- sortedSimilarities[1:NtopUsers]
  #Subset ratings further for only the truly relevent ratings
  similarUsersRatings <- subset(ratingsDF, UserID %in% names(topN))
  return(similarUsersRatings)
}
#Took 90 seconds for user 1
system.time(
  a <- calcSimilarUsers(1, ratings, 10, 100)
)

predictRatingAbsolute <- function(similarRatingDF, predID) {
  return(mean(similarRatingDF$Rating[similarRatingDF$Profile == predID]))
}

standardizeRatings <- function(singleUserDF) {
  singleUserDF$meanRating <- rep(mean(singleUserDF$Rating), nrow(singleUserDF))
  singleUserDF$sdRating <- rep(sd(singleUserDF$Rating), nrow(singleUserDF))
  singleUserDF$Rating <- (singleUserDF$Rating - mean(singleUserDF$Rating))/sd(singleUserDF$Rating)
  return(singleUserDF)
}
ratingsStandardizedList <- by(ratings, ratings$UserID, function(x) standardizeRatings(x))
library(data.table)
# <1.0 seconds
system.time(
  ratingsStandardizedDF <- rbindlist(ratingsStandardizedList)
)
#Took 111.5 seconds for user 1
#Now takes 49.240 seconds
system.time(
  standardizedSimilar1 <- calcSimilarUsers(1, ratingsStandardizedDF, 10, 100)
)

predictRatingStandardized <- function(userID, similarRatingDF, predID) {
  userDF <- subset(ratingsStandardizedDF, UserID == userID)
  predVal <- userDF$meanRating[1] + userDF$sdRating[1]*mean(similarRatingDF$Rating[similarRatingDF$ProfileID == predID], na.rm = T)
  return(predVal)
}

c <- subset(standardizedSimilar1, ProfileID == 60)
predictRatingStandardized(1, standardizedSimilar1, 60)

calcRMSEforTrainingData <- function(userID, predFunc, similarDF) {
  preds <- c()
  userDF <- subset(ratings, UserID == userID)
  for (i in userDF$ProfileID) {
    preds[length(preds) + 1] <- predFunc(userID, similarDF, i)
  }
  return(mean(abs(userDF$Rating - preds)))
}

calcRMSEforTrainingData(1, predictRatingStandardized, standardizedSimilar1)

calcTrainingRMSEforUser <- function(userID, ratingsDF, NcommonUsers, NtopUsers, predFunc) {
  simUsers <- calcSimilarUsers(userID, ratingsDF, NcommonUsers, NtopUsers)
  RMSE <- calcRMSEforTrainingData(userID, predFunc, simUsers)
  return(RMSE)
}

system.time(
  u3RMSE <- calcTrainingRMSEforUser(3, ratingsStandardizedDF, 10, 100, predictRatingStandardized)
)

