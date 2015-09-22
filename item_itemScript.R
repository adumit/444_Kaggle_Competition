ratings <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/ratings.csv")
idmap <- read.csv("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/idmap.csv")
library(data.table)

findNumSimilarItems <- function(itemDF, item2DF) {
  if (itemDF$ProfileID[1] == item2DF$ProfileID[1]) {
    return(0)
  }
  else {
    return(length(intersect(itemDF$UserID, item2DF$UserID)))
  }
}

getNumCommonRatingsItems <- function(ratingsDF, itemDF) {
  similarItems <- by(ratingsDF, ratingsDF$ProfileID, function(x) findNumSimilarItems(x, itemDF))
  return(similarItems)
}

calcSimilarity <- function(itemDF, similarItemDF) {
  itemRatings <- subset(itemDF, UserID %in% intersect(itemDF$UserID, similarItemDF$UserID))
  similarItemRatings <- subset(similarItemDF, UserID %in% intersect(itemDF$UserID, similarItemDF$UserID))
  return(dist(rbind(itemRatings$Rating, similarItemRatings$Rating), method = "euclidean"))
}

calcSimilarity(subset(ratings, ProfileID == 1), subset(ratings, ProfileID == 3))

calcSimilarItems <- function(itemID, ratingsDF, NcommonRatings, Npercentile) {
  #Create user dataframe for given userID
  itemDF <- subset(ratingsDF, ProfileID == itemID) #Slower command
  #Find the number of common ratings between given userID and all other users
  #Returns 0 for given userID so as to not include in final output
  ratingsSub <- subset(ratingsDF, UserID %in% itemDF$UserID) #Slower command
  commonRatingItems <- getNumCommonRatingsItems(ratingsSub, itemDF)
  #Create dataframe with userID and numSimilar as columns
  numSimilar <- matrix(unlist(commonRatingItems), ncol = 1, byrow = T)
  itemIDs <- names(commonRatingItems)
  similarDF <- data.frame(itemIDs, numSimilar)
  similarDF$numSimilar <- as.numeric(as.character(similarDF$numSimilar))
  similarDF$itemIDs <- as.numeric(as.character(similarDF$itemIDs))
  #Get all users with equal to or more similar ratings than provided input
  similarDF <- subset(similarDF, numSimilar >= NcommonRatings) #Slower command
  #Subset entire ratings dataframe to only get relevent ratings for next action - makes the next action faster
  ratingsSub <- subset(ratingsDF, ProfileID %in% similarDF$itemIDs) #Slower command than above
  #Get similarities between all users and provided user
  itemSimilarities <- by(ratingsSub, ratingsSub$ProfileID, function(x) calcSimilarity(itemDF, x))
  itemSimilaritiesMat <- matrix(unlist(itemSimilarities), ncol = 1, byrow = T)
  itemSimilaritiesID <- names(itemSimilarities)
  itemSimilaritiesDF <- data.frame(itemSimilaritiesID, itemSimilaritiesMat)
  #Get numSimilar as well after renaming columns
  names(similarDF) <- c("ItemID", "numSimilar")
  names(itemSimilaritiesDF) <- c("ItemID", "ItemSimilarity")
  itemSimilaritiesDF <- join(userSimilaritiesDF, similarDF, by = "ProfileID")
  itemSimilaritiesDF$ProfileID <- as.numeric(as.character(userSimilaritiesDF$ProfileID))
  #Took out top N similarities
  #   #Sort similarities
  #   sortedSimilarities <- sort(userSimilarities, decreasing = T)
  #   #Get top N users - input provided
  #   NpercentUsers <- length(sortedSimilarities)*nPercent 
  #   topN <- sortedSimilarities[1:NpercentUsers]
  #Subset ratings further for only the truly relevent ratings
  #  similarUsersRatings <- subset(ratingsDF, UserID %in% names(topN))
  similarItemsRatings <- join(ratingsSub, itemSimilaritiesDF, by = "ProfileID")
  similarItemsRatings <- subset(similarItemsRatings, similarItemsRatings$ItemSimilarity >= quantile(similarItemsRatings$ItemSimilarity, c(Npercentile)))
  return(as.data.frame(similarItemsRatings))
}

calcSimilarItems
