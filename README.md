STAT 444 - Statistical Learning and Data Mining at Rice University Kaggle Competition 

Goal: Predict dating profile ratings given an extremely sparse (~5% data present) matrix of user ratings.

Approaches used:
- User-user collaborative filtering using similarity matricies.
- Item-item collaborative filtering using similarity matricies.
- Approximate SVD based on gradient descent using the non-missing values with additional effects. Based on the Belkor prize winners from the Netflix competition: http://netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf
- Blends and model stacks of the above approaches.

Tools used:
- Many different R packages
- Java for the speed of implementing the approximate gradient descent, which failed speedwise in R and Python comparatively.

