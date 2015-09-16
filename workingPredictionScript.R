ratings <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/ratings.csv")
idmap <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/idmap.csv")
library(data.table)

findNumSimilar <- function(userDF, user2DF) {
  if (userDF$UserID[1] == user2DF$UserID[1]) {
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

calcSimilarUsers <- function(userID, ratingsDF, NcommonRatings, Npercentile) {
  #Create user dataframe for given userID
  userDF <- subset(ratingsDF, UserID == userID) #Slower command
  #Find the number of common ratings between given userID and all other users
  #Returns 0 for given userID so as to not include in final output
  ratingsSub <- subset(ratingsDF, ProfileID %in% userDF$ProfileID) #Slower command
  commonRatingUsers <- getNumCommonRatings(ratingsSub, userDF)
  #Create dataframe with userID and numSimilar as columns
  numSimilar <- matrix(unlist(commonRatingUsers), ncol = 1, byrow = T)
  userIDs <- names(commonRatingUsers)
  similarDF <- data.frame(userIDs, numSimilar)
  similarDF$numSimilar <- as.numeric(as.character(similarDF$numSimilar))
  similarDF$userIDs <- as.numeric(as.character(similarDF$userIDs))
  #Get all users with equal to or more similar ratings than provided input
  similarDF <- subset(similarDF, numSimilar >= NcommonRatings) #Slower command
  #Subset entire ratings dataframe to only get relevent ratings for next action - makes the next action faster
  ratingsSub <- subset(ratingsDF, UserID %in% similarDF$userIDs) #Slower command than above
  #Get similarities between all users and provided user
  userSimilarities <- by(ratingsSub, ratingsSub$UserID, function(x) calcSimilarity(userDF, x))
  userSimilaritiesMat <- matrix(unlist(userSimilarities), ncol = 1, byrow = T)
  userSimilaritiesID <- names(userSimilarities)
  userSimilaritiesDF <- data.frame(userSimilaritiesID, userSimilaritiesMat)
  #Get numSimilar as well after renaming columns
  names(similarDF) <- c("UserID", "numSimilar")
  names(userSimilaritiesDF) <- c("UserID", "UserSimilarity")
  userSimilaritiesDF <- join(userSimilaritiesDF, similarDF, by = "UserID")
  userSimilaritiesDF$UserID <- as.numeric(as.character(userSimilaritiesDF$UserID))
  #Took out top N similarities
#   #Sort similarities
#   sortedSimilarities <- sort(userSimilarities, decreasing = T)
#   #Get top N users - input provided
#   NpercentUsers <- length(sortedSimilarities)*nPercent 
#   topN <- sortedSimilarities[1:NpercentUsers]
  #Subset ratings further for only the truly relevent ratings
#  similarUsersRatings <- subset(ratingsDF, UserID %in% names(topN))
  similarUsersRatings <- join(ratingsSub, userSimilaritiesDF, by = "UserID")
  similarUsersRatings <- subset(similarUsersRatings, similarUsersRatings$UserSimilarity >= quantile(similarUsersRatings$UserSimilarity, c(Npercentile)))
  return(as.data.frame(similarUsersRatings))
}
######
#Compute standardized rating matrix
######
standardizeRatings <- function(singleUserDF) {
  singleUserDF$meanRating <- rep(mean(singleUserDF$Rating), nrow(singleUserDF))
  singleUserDF$sdRating <- rep(sd(singleUserDF$Rating), nrow(singleUserDF))
  singleUserDF$Rating <- (singleUserDF$Rating - mean(singleUserDF$Rating))/sd(singleUserDF$Rating)
  return(singleUserDF)
}
# ~41 seconds
system.time(
  ratingsStandardizedList <- by(ratings, ratings$UserID, function(x) standardizeRatings(x))
)
# <1.0 seconds
system.time(
  ratingsStandardizedDF <- as.data.frame(rbindlist(ratingsStandardizedList))
)
#######
#End of creating standardized rating matrix
#######

#Took ~6.5 seconds for user 1 w/ 10 common users and .9 percentile
#Takes ~6.5 seconds with 0 percentile users as well
library(plyr)
system.time(
  a <- calcSimilarUsers(1, ratingsStandardizedDF, 10, 0.9)
)

#######
# Predict over a dataframe with weighting
# simRatingsDF !MUST! have weights in it
# UserDF !MUST! also have mean and sd in it
#######
predictRatingWithWeights <- function(userDF, simRatingsDF, predID) {
  if (!is.numeric(predID)) {
    predID = predID$ProfileID[1]
  }
  ratingsOfInterest <- simRatingsDF[simRatingsDF$ProfileID == predID,]
  if (nrow(ratingsOfInterest) == 0) {
    return(userDF$meanRating[1])
  }
  ratingsOfInterest$UserSimilarity <- ratingsOfInterest$UserSimilarity/max(ratingsOfInterest$UserSimilarity)
  prediction <- userDF$meanRating[1] + userDF$sdRating[1]*sum(ratingsOfInterest$Rating*ratingsOfInterest$UserSimilarity, na.rm = T)/sum(ratingsOfInterest$UserSimilarity)
  return(prediction)
}

predictDFby <- function(userDF, ratingsDF, NcommonRatings, Npercentile, predDF) {
  simUsers <- calcSimilarUsers(userDF$UserID[1], ratingsDF, NcommonRatings, Npercentile)
  preds <- by(predDF, predDF$ProfileID, function(x) predictRatingWithWeights(userDF, simUsers, x))
  return(preds)
}

predictDFlapply <- function(userDF, ratingsDF, NcommonRatings, Npercentile, predDF) {
  simUsers <- calcSimilarUsers(userDF$UserID[1], ratingsDF, NcommonRatings, Npercentile)
  preds <- lapply(predDF$ProfileID, function(x) predictRatingWithWeights(userDF, simUsers, x))
  return(preds)
}
####
#Speed testing
####
system.time(
  u1 <- subset(idmap, UserID == 1)
)
system.time(
  user1 <- subset(ratingsStandardizedDF, UserID == 1)
)
system.time(
  a <- predictDFby(user1, ratingsStandardizedDF, 10, .8, u1)
)
system.time(
  b <- predictDFlapply(user1, ratingsStandardizedDF, 10, .8, u1)
)
#Conclusion: lapply seems to be faster
#####
# By function for many users in one call
#####
runPredsForUser <- function(predID, ratingsDF, NcommonRatings, Npercentile) {
  prevRatings <- subset(ratingsDF, UserID == predID)
  predDF <- subset(idmap, UserID == predID)
  return(predictDFby(prevRatings, ratingsDF, NcommonRatings, Npercentile, predDF))
}

system.time(
  results <- lapply(1501:2800, function(x) runPredsForUser(x, ratingsStandardizedDF, 10, .8))
)
save(results, file = "results1501_2800")




calculateTrainingRMSE.weights <- function(userDF, simRatingsDF, predDF) {
  preds <- by(predDF, predDF$ProfileID, function(x) predictRatingWithWeights(userDF, simRatingsDF, x))
  errors <- predDF$Rating - preds
  RMSE <- mean(abs(errors))
  return(RMSE)
}

# predictRatingWithWeightsBase <- function(userDF, simRatingsDF, predID) {
#   if (!is.numeric(predID)) {
#     predID = predID$ProfileID[1]
#   }
#   ratingsOfInterest <- simRatingsDF[simRatingsDF$ProfileID == predID,]
#   if (nrow(ratingsOfInterest) == 0) {
#     return(userDF$meanRating[1])
#   }
#   ratingsOfInterest$UserSimilarity <- ratingsOfInterest$UserSimilarity/sum(ratingsOfInterest$UserSimilarity)
#   prediction <- mean(ratingsOfInterest$Rating*ratingsOfInterest$UserSimilarity, na.rm = T)
#   return(prediction)
# }

# calculateTrainingRMSE.weights.base <- function(userDF, simRatingsDF, predDF) {
#   preds <- by(predDF, predDF$ProfileID, function(x) predictRatingWithWeightsBase(userDF, simRatingsDF, x))
#   errors <- predDF$Rating - preds
#   RMSE <- mean(abs(errors))
#   return(RMSE)
# }

######
# Testing on user1 and user2
######
u1actual <- subset(ratings, UserID == 1)
u1s <- subset(ratingsStandardizedDF, UserID == 1)
u2actual <- subset(ratings, UserID == 2)
u2s <- subset(ratingsStandardizedDF, UserID == 2)

t1 <- calcSimilarUsers(1, ratingsStandardizedDF, 10, 0)

t.2 <- calcSimilarUsers(2, ratingsStandardizedDF, 10, .8)

calculateTrainingRMSE.weights(u1s, t1, u1actual)
calculateTrainingRMSE.weights(u2s, t2, u2actual)

#######
# More extensive testing
#######

RMSETrainingCalc <- function(userID, ratingsDF, NcommonRatings, Npercentile) {
  uActual <- subset(ratings, UserID == userID)
  uStandardized <- subset(ratingsDF, UserID == userID)
  simUsers <- calcSimilarUsers(userID, ratingsDF, NcommonRatings, Npercentile)
  RMSE <- calculateTrainingRMSE.weights(uStandardized, simUsers, uActual)
  return(RMSE)
}

system.time(
  test1 <- RMSETraingCalc(1, ratingsStandardizedDF, 10, 0)
)

system.time(
  test1 <- sapply(sample(1:10000, 100), function(x) RMSETrainingCalc(x, ratingsStandardizedDF, 10, 0))
)


#### PARALLEL DOESN'T CURRENTLY WORK
# #######
# # Parallel testing
# #######
# library(parallel)
# sampleRMSEParallel <- function(nUsersPerCore, ratingsDF, NcommonRatings, Npercentile) {
#   c1 <- c(sample(1:10000, nUsersPerCore))
#   c2 <- c(sample(1:10000, nUsersPerCore))
#   c3 <- c(sample(1:10000, nUsersPerCore))
#   c4 <- c(sample(1:10000, nUsersPerCore))
#   cores <- list(c1, c2, c3, c4)
#   cl = makeCluster(4, "FORK")
#   return(clusterApplyLB(cl, cores, function(c) by(c, c, function(x) RMSETraingCalc(x, ratingsDF, NcommonRatings, Npercentile))))
# }
# 
# system.time(
#   test <- sampleRMSEParallel(2, ratingsStandardizedDF, 10, 0)
# )
# test1 <- mean(unlist(test))


# predictRatingAbsolute <- function(similarRatingDF, predDF) {
#   predID = predDF$ProfileID[1]
#   return(mean(similarRatingDF$Rating[similarRatingDF$Profile == predID]))
# }
# 
# #Took 111.5 seconds for user 1
# #Now takes 49.240 seconds
# system.time(
#   standardizedSimilar1 <- calcSimilarUsers(1, ratingsStandardizedDF, 10, 100)
# )
# 
# predictRatingStandardized <- function(userID, similarRatingDF, predDF) {
#   userDF <- subset(ratingsStandardizedDF, UserID == userID)
#   if (is.numeric(predDF)) {
#     predID = predDF
#   }
#   else {
#     predID = predDF$ProfileID[1] 
#   }
#   if (!(predID %in% similarRatingDF$ProfileID)) {
#     return(userDF$meanRating[1])
#   }
#   predVal <- userDF$meanRating[1] + userDF$sdRating[1]*mean(similarRatingDF$Rating[similarRatingDF$ProfileID == predID], na.rm = T)
#   return(predVal)
# }
# 
# c <- subset(standardizedSimilar1, ProfileID == 60)
# predictRatingStandardized(1, standardizedSimilar1, 60)
# 
# calcRMSEforTrainingData <- function(userID, predFunc, similarDF, predDF) {
#   userDF <- subset(ratings, UserID == userID)
#   preds <- by(predDF, predDF$ProfileID, function(x) predFunc(userID, similarDF, x))
#   return(mean(abs(userDF$Rating - preds)))
# }
# 
# system.time(
#   b <- calcRMSEforTrainingData(1, predictRatingStandardized, standardizedSimilar1, u1)
# )
# 
# #######
# #Centered Ratings functions and data creation
# #######
# centerRatings <- function(singleUserDF) {
#   singleUserDF$meanRating <- rep(mean(singleUserDF$Rating), nrow(singleUserDF))
#   singleUserDF$Rating <- singleUserDF$Rating - mean(singleUserDF$Rating)
#   return(singleUserDF)
# }
# # ~82 seconds
# system.time(
#   ratingsCenteredList <- by(ratings, ratings$UserID, function(x) centerRatings(x))
# )
# system.time(
#   ratingsCenteredDF <- rbindlist(ratingsCenteredList)
# )
# predictRatingCentered <- function(userDF, similarRatingDF, predDF) {
#   predID = predDF$ProfileID[1]
#   predVal <- userDF$meanRating[1] + mean(similarRatingDF$Rating[similarRatingDF$ProfileID == predID], na.rm = T)
#   return(predVal)
# }
# #######
# #End of centered stuff
# #######
# 
# calcTrainingRMSEforUser <- function(userID, ratingsDF, NcommonUsers, NtopUsers, predFunc, predDF) {
#   if (is.numeric(predDF)) {
#     predictDF = subset(ratings, UserID == predDF)
#   }
#   else {
#     predictDF = predDF
#   }
#   simUsers <- calcSimilarUsers(userID, ratingsDF, NcommonUsers, NtopUsers)
#   RMSE <- calcRMSEforTrainingData(userID, predFunc, simUsers, predictDF)
#   return(RMSE)
# }
# 
# calcSimUsersAndSave <- function(userID, ratingsDF, NcommonUsers, NtopPerc) {
#   simUsers <- calcSimilarUsers(userID, ratingsDF, NcommonUsers, NtopPerc)
#   save(simUsers, file = paste0("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/similarityMatriciesStandardized/simUsers", userID, ".rda"))
#   return(simUsers)
# }
# 
# #######
# #Parallel section
# #######
# c1 <- c(10:20)
# system.time(
#   c1result <- lapply(c1, function(x) calcSimUsersAndSave(x, ratingsStandardizedDF, 10, 1))
# )


