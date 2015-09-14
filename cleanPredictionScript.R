ratings <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/ratings.csv")
idmap <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/idmap.csv")
library(data.table)

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

predictRatingAbsolute <- function(similarRatingDF, predDF) {
  predID = predDF$ProfileID[1]
  return(mean(similarRatingDF$Rating[similarRatingDF$Profile == predID]))
}

#######
#Centered Ratings functions and data creation
#######
centerRatings <- function(singleUserDF) {
  singleUserDF$meanRating <- rep(mean(singleUserDF$Rating), nrow(singleUserDF))
  singleUserDF$Rating <- singleUserDF$Rating - mean(singleUserDF$Rating)
  return(singleUserDF)
}
# ~82 seconds
system.time(
  ratingsCenteredList <- by(ratings, ratings$UserID, function(x) centerRatings(x))
)
system.time(
  ratingsCenteredDF <- as.data.frame(rbindlist(ratingsCenteredList))
)
predictRatingCentered <- function(userDF, similarRatingDF, predID) {
  predVal <- userDF$meanRating[1] + mean(similarRatingDF$Rating[similarRatingDF$ProfileID == predID], na.rm = T)
  return(predVal)
}
#######
#End of centered stuff
#######

#######
#Standardized stuff
#######
standardizeRatings <- function(singleUserDF) {
  singleUserDF$meanRating <- rep(mean(singleUserDF$Rating), nrow(singleUserDF))
  singleUserDF$sdRating <- rep(sd(singleUserDF$Rating), nrow(singleUserDF))
  singleUserDF$Rating <- (singleUserDF$Rating - mean(singleUserDF$Rating))/sd(singleUserDF$Rating)
  return(singleUserDF)
}
ratingsStandardizedList <- by(ratings, ratings$UserID, function(x) standardizeRatings(x))
# <1.0 seconds
system.time(
  ratingsStandardizedDF <- as.data.frame(rbindlist(ratingsStandardizedList))
)
#Took 111.5 seconds for user 1
#Now takes 49.240 seconds
system.time(
  standardizedSimilar1 <- calcSimilarUsers(1, ratingsStandardizedDF, 10, 100)
)

predictRatingStandardized <- function(userDF, similarRatingDF, predDF) {
  predID = predDF$ProfileID[1]
  predVal <- userDF$meanRating[1] + userDF$sdRating[1]*mean(similarRatingDF$Rating[similarRatingDF$ProfileID == predID], na.rm = T)
  return(predVal)
}

########
#End of standardized stuff
########

calcRMSEforTrainingData <- function(userDF, predFunc, similarDF, predDF) {
  userID = userDF$UserID[1]
  preds <- by(predDF, predDF$ProfileID, function(x) predFunc(userDF, similarDF, x))
  return(mean(abs(userDF$Rating - preds)))
}

calcTrainingRMSEforUser <- function(userID, ratingsDF, NcommonUsers, NtopUsers, predFunc, predDF) {
  if (is.numeric(predDF)) {
    predictDF = subset(ratingsDF, UserID == predDF)
  }
  else {
    predictDF <- predDF
  }
  userDF <- subset(ratingsDF, UserID == userID)
  simUsers <- calcSimilarUsers(userDF, ratingsDF, NcommonUsers, NtopUsers)
  RMSE <- calcRMSEforTrainingData(userDF, predFunc, simUsers, predictDF)
  return(RMSE)
}

system.time(
  simUsers <- calcSimilarUsers(1, ratingsStandardizedDF, 10, 100)
)

predictRatingStandardized(u1s, simUsers, 160)

u4 <- subset(ratingsStandardizedDF, UserID == 4)
system.time(
  u4RMSE <- calcTrainingRMSEforUser(4, ratingsStandardizedDF, 10, 100, predictRatingStandardized, 4)
)
