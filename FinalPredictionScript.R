################
# Instructions for running:
# 1) Set working directory to the folder this came in and install libraries if not installed
# 2) Delete/move predictions csv or new ones will overwrite them - not a problem if running for the first time
# 3) Run the R code up until the Java aggregation line
# 3.5) Can edit the R code if cores to be run on < 8 - everything was run locally on 8 cores
# 4) Run the Java main class and wait for ~4 hours for the execution to complete with 8 cores and 16GB of RAM
################

ratings <- read.csv("ratings.csv")
idmap <- read.csv("idmap.csv")
library(data.table)
library(qlcMatrix)
library(plyr)
# Similarity21 predictor
unscaledMat <- sparseMatrix(i=ratings[,1],j=ratings[,2],x=ratings[,3])
Unum = apply(t(unscaledMat)!=0,2,sum)
Usum = apply(t(unscaledMat),2,sum)
Umeans = Usum / Unum
standardizeRatings <- function(singleUserDF) {
  singleUserDF$meanRating <- rep(sum(singleUserDF$Rating)/nrow(singleUserDF), nrow(singleUserDF))
  singleUserDF$sdRating <- rep(sd(singleUserDF$Rating), nrow(singleUserDF))
  singleUserDF$Rating <- singleUserDF$Rating - sum(singleUserDF$Rating)/nrow(singleUserDF)
  return(singleUserDF)
}
# ~41 seconds
system.time(
  ratingsCenteredDFList <- by(ratings, ratings$UserID, function(x) standardizeRatings(x))
)
# <1.0 seconds
system.time(
  ratingsCenteredDF <- as.data.frame(rbindlist(ratingsCenteredDFList))
)

cmat = sparseMatrix(i = ratingsCenteredDF[,1], j = ratingsCenteredDF[,2], x = ratingsCenteredDF[,3])

system.time(
  cosineMatUsers4 <- cosSparse(t(cmat), norm = norm1, weight = "idf")
)
system.time(
  cosineMatrixUsers4 <- as.matrix(cosineMatUsers4)
)
#Write this similarity matrix for Java input
write.csv(cosineMatrixUsers4, file = "CosineSimilarityUsers.csv", row.names = F, col.names = F)
makeSinglePred <- function(itemRow) {
  user = itemRow$UserID
  profile = itemRow$ProfileID
  relevantRatings <- cmat[,profile]
  indicies <- which(relevantRatings != 0)
  similarUsers <- cosineMatrixUsers4[user, ]
  similarUsers[-indicies] <- 0
  similarUsers[similarUsers < tail(sort(similarUsers), 21)[1] | similarUsers == similarUsers[user]] <- 0
  return(Umeans[user] + (similarUsers %*% relevantRatings)/sum(similarUsers))
}
makeManyPreds <- function(userData) {
  preds <- by(userData, userData$ProfileID, function(x) makeSinglePred(x))
  return(preds)
}
u1 <- subset(ratings, UserID == 5)
preds <- makeManyPreds(u1)
preds[preds > 10] <- 10
preds[preds < 1] <- 1
errorsUser <- u1$Rating - preds
sqrt(mean((u1$Rating - preds)^2))
predWrapper <- function(listOfIDs) {
  ratingsSub <- subset(idmap, UserID >= min(listOfIDs) & UserID <= max(listOfIDs))
  return(by(ratingsSub, ratingsSub$UserID, function(x) makeManyPreds(x)))
}
###
# Actual predictions
###
# !!! WARNING - MUST HAVE 8 CORES !!! 
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
  results <- clusterApplyLB(cl, cores, function(c) lapply(c, function(x) predWrapper(x)))
)
temp <- as.numeric(unlist(results))
nums <- idmap$KaggleID
table(temp > 10)
table(temp < 1)
temp[temp > 10] <- 10
temp[temp < 1] <- 1
final_resultsDF <- data.frame("ID" = nums, "Prediction" = temp)
write.csv(final_resultsDF, file = "newSimilarityCenteredData21.csv")

####### Then all making stuff to input to Java/reading in Java output
#Make users that rated profiles
getItemsRatedByUser <- function(UserDF) {
  profiles = UserDF$UserID
  return(c(profiles, rep(0, 10000 - length(profiles))))
}
fullUsersThatRatedProfile <- by(ratings, ratings$ProfileID, function(x) getItemsRatedByUser(x))
fullUsersThatRatedProfileMatrix <- matrix(unlist(fullUsersThatRatedProfile), ncol = 10000, nrow = 10000, byrow = T)
write.csv(fullUsersThatRatedProfileMatrix, file = "fullUsersThatRatedProfiles.csv", row.names = F)
########### 4:30 AM Monday 30th Submission sim matrix
standardizeRatings <- function(singleUserDF) {
  singleUserDF$meanRating <- rep(sum(singleUserDF$Rating)/nrow(singleUserDF), nrow(singleUserDF))
  singleUserDF$sdRating <- rep(sd(singleUserDF$Rating), nrow(singleUserDF))
  singleUserDF$Rating <- singleUserDF$Rating - sum(singleUserDF$Rating)/nrow(singleUserDF)
  return(singleUserDF)
}
# ~41 seconds
system.time(
  ratingsCenteredDFList <- by(ratings, ratings$ProfileID, function(x) standardizeRatings(x))
)
# <1.0 seconds
system.time(
  ratingsCenteredDF <- as.data.frame(rbindlist(ratingsCenteredDFList))
)

cmat = sparseMatrix(i = ratingsCenteredDF[,1], j = ratingsCenteredDF[,2], x = ratingsCenteredDF[,3])

system.time(
  cosineMatUsers4 <- cosSparse(t(cmat), norm = norm1, weight = "idf")
)
system.time(
  cosineMatrixUsers4 <- as.matrix(cosineMatUsers4)
)
write.csv(cosineMatrixUsers4, file = "CosineSimilarityUsersDifferent.csv", row.names = F, col.names = F)


########### Java aggregation
finalPreds <- read.csv("CFOverBaseGradientDescentWithNewSimilarity.csv")
test <- read.csv("gradientDescentCFPredsWithSVD.csv")
test2 <- read.csv("newSimilarityCenteredData21.csv")
test3 <- read.csv("lastAttempt.csv") #Wasn't submitted to kaggle, just included in blend

test5 <- data.frame("ID" = 1:500000, "Prediction" = (test$Prediction + test2$Prediction + test3$Prediction + finalPreds$Prediction)/4)
write.csv(test5, file = "lastAttempt.csv", row.names = F)


