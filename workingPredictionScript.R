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
  similarUsers <- by(ratingsDF, ratingsDF$UserID, function(x) findNumSimilarCompiled(x, userDF))
  return(similarUsers)
}

calcSimilarity <- function(userDF, similarUserDF) {
  #userRatings <- subset(userDF, ProfileID %in% intersect(userDF$ProfileID, similarUserDF$ProfileID))
  userRatings <- userDF[userDF$ProfileID %in% intersect(userDF$ProfileID, similarUserDF$ProfileID),]
  #similarUserRatings <- subset(similarUserDF, ProfileID %in% intersect(userDF$ProfileID, similarUserDF$ProfileID))
  similarUserRatings <- similarUserDF[similarUserDF$ProfileID %in% intersect(userDF$ProfileID, similarUserDF$ProfileID),]
  return(dist(rbind(userRatings$Rating, similarUserRatings$Rating), method = "euclidean"))
}

calcSimilarUsers <- function(userID, ratingsDF, NcommonRatings, Npercentile) {
  #Create user dataframe for given userID
  #userDF <- subset(ratingsDF, UserID == userID)
  userDF <- ratingsDF[ratingsDF$UserID == userID,]
  #Find the number of common ratings between given userID and all other users
  #Returns 0 for given userID so as to not include in final output
  ratingsSub <- ratingsDF[ratingsDF$ProfileID %in% userDF$ProfileID,]
  #ratingsSub <- subset(ratingsDF, ProfileID %in% userDF$ProfileID) #Slower command
  commonRatingUsers <- getNumCommonRatingsCompiled(ratingsSub, userDF)
  #Create dataframe with userID and numSimilar as columns
  numSimilar <- matrix(unlist(commonRatingUsers), ncol = 1, byrow = T)
  userIDs <- names(commonRatingUsers)
  similarDF <- data.frame(userIDs, numSimilar)
  similarDF$numSimilar <- as.numeric(as.character(similarDF$numSimilar))
  similarDF$userIDs <- as.numeric(as.character(similarDF$userIDs))
  #Get all users with equal to or more similar ratings than provided input
  #similarDF <- subset(similarDF, numSimilar >= NcommonRatings)
  similarDF <- similarDF[similarDF$numSimilar >= NcommonRatings,]
  #Subset entire ratings dataframe to only get relevent ratings for next action - makes the next action faster
  #ratingsSub <- subset(ratingsDF, UserID %in% similarDF$userIDs)
  ratingsSub <- ratingsDF[ratingsDF$UserID %in% similarDF$userIDs,]
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
  similarUsersRatings <- join(ratingsSub, userSimilaritiesDF, by = "UserID")
  similarUsersRatings$UserSimilarity <- similarUsersRatings$UserSimilarity/similarUsersRatings$numSimilar
  #similarUsersRatings <- subset(similarUsersRatings, similarUsersRatings$UserSimilarity <= quantile(similarUsersRatings$UserSimilarity, c(Npercentile)))
  similarUsersRatings <- similarUsersRatings[similarUsersRatings$UserSimilarity <= quantile(similarUsersRatings$UserSimilarity, Npercentile),]
  return(as.data.frame(similarUsersRatings))
}

library(plyr)
library(compiler)
findNumSimilarCompiled <- cmpfun(findNumSimilar)
getNumCommonRatingsCompiled <- cmpfun(getNumCommonRatings)
calcSimilarityCompiled <- cmpfun(calcSimilarity)
calcSimilarUsersCompiled <- cmpfun(calcSimilarUsers)

system.time(
)
######
#Compute standardized rating matrix
######
standardizeRatings <- function(singleUserDF) {
  singleUserDF$sdRating <- rep(sd(singleUserDF$Rating), nrow(singleUserDF))
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
  a <- calcSimilarUsers(1, ratingsStandardizedDF, 10, 0.1)
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
  #ratingsOfInterest$UserSimilarity <- ratingsOfInterest$UserSimilarity/max(ratingsOfInterest$UserSimilarity)
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
library(plyr)
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
#Conclusion: doesn't particularly matter lol
#####
# By function for many users in one call
#####
runPredsForUser <- function(predID, ratingsDF, NcommonRatings, Npercentile) {
  prevRatings <- subset(ratingsDF, UserID == predID)
  predDF <- subset(idmap, UserID == predID)
  return(predictDFby(prevRatings, ratingsDF, NcommonRatings, Npercentile, predDF))
}

library(parallel)
c1 <- 1:1420
c2 <- 1421:2840
c3 <- 2841:4260
c4 <- 4261:5680
c5 <- 5681:7100
c6 <- 7101:8550
c7 <- 8551:10000
cores <- list(c1, c2, c3, c4, c5, c6, c7)
cl = makeCluster(7, "FORK")
system.time(
  results <- clusterApplyLB(cl, cores, function(c)
    lapply(c, function(x) runPredsForUser(x, ratingsStandardizedDF, 10, .1)))
)
save(results, file = "results1_10000_.1nhScaled.rda")
temp <- unlist(results)
nums <- idmap$KaggleID
temp[temp > 10] <- 10
temp[temp < 1] <- 1
final_resultsDF <- data.frame("ID" = nums, temp)
write.csv(final_resultsDF, file = "submission6.csv")
#first 40 users w/ 10 users per core took 140sec
#first 60 users w/15 users per core took 215 sec
#first 200 users w/100 users per core took 588 sec

r1 <- load("results1_1500.rda")
r1 <- unlist(results)
r2 <- load("results1501_2800.rda")
r2 <- unlist(results)
r3 <- load("results2800_4000.rda")
r3 <- unlist(results)
r4 <- load("results4001_7000.rda")
r4 <- unlist(results)
r5 <- load("results7001_10000.rda")
r5 <- unlist(results)

final_results <- c(r1, r2, r3, r4, r5)
final_results[final_results < 1] = 1
final_results[final_results > 10] = 10



calculateTrainingRMSE.weights <- function(userDF, simRatingsDF, predDF) {
  preds <- by(predDF, predDF$ProfileID, function(x) predictRatingWithWeights(userDF, simRatingsDF, x))
  errors <- predDF$Rating - preds
  RMSE <- mean(abs(errors))
  return(RMSE)
}

######
# Testing on user1 and user2
######
u1actual <- subset(ratings, UserID == 1)
u1s <- subset(ratingsStandardizedDF, UserID == 1)
u2actual <- subset(ratings, UserID == 2)
u2s <- subset(ratingsStandardizedDF, UserID == 2)
u3actual <- subset(ratings, UserID == 3)
u3s <- subset(ratingsStandardizedDF, UserID == 3)

t1 <- calcSimilarUsers(1, ratingsStandardizedDF, 10, .1)
calculateTrainingRMSE.weights(u1s, t1, u1actual)

t2 <- calcSimilarUsers(2, ratingsStandardizedDF, 10, .1)
calculateTrainingRMSE.weights(u2s, t2, u2actual)

t3 <- calcSimilarUsers(3, ratingsStandardizedDF, 10, .1)
calculateTrainingRMSE.weights(u3s, t3, u3actual)

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

