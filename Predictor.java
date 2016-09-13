import java.util.PriorityQueue;

/**
 * Created by Andrew on 11/30/15.
 */
public class Predictor {
    /////////////
    //Variables
    /////////////
    int numTrainingFeatures = 5;
    int trainSetLength = 3179759;
    int testSetLength = 100000;

    public double[][] trueRatings = new double[trainSetLength][3];
    public double[][] p_u = new double[10000][numTrainingFeatures];
    public double[][] q_i = new double[10000][numTrainingFeatures];

    public double[] Bu = new double[10000];
    public double[] Bi = new double[10000];

    private double globalMean = 6.080882;

    private double[] mostRecentPreds = new double[3179759];

    private double[][] ratingMatrix = new double[10000][10000];
    public double[] numberOfUsersRated = new double[10000];

    readCSVClass reader = new readCSVClass();
    public double[] userMeans = reader.readUserMeans("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/CompetitionFinalMonth/userMeans.csv");
    public int[][] itemsRated;
    public double[][] wu_ij = reader.readSimilarityMatrix("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/CompetitionFinalWeek/CosineSimilarityUsers.csv");
    public double[][] wi_ij = reader.readSimilarityMatrix("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/CompetitionFinalWeek/CosineSimilarityItems.csv");
    //For final predictions
    public int[][] usersRated = reader.readItemList("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/CompetitionFinalWeek/fullUsersThatRatedProfiles.csv");
    public int[] numberOfItemsRated = new int[10000];

    public void initializeBase() {
        for (int i = 0; i < 10000; i++) {
            Bi[i] = 0.1;
            Bu[i] = 0.1;
        }
    }


    public void initializeItemsRated() {
        this.itemsRated = reader.readItemList("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/CompetitionFinalWeek/fullProfilesThatUsersRated.csv");
    }


    public void inititalizeRatingMatrix(double[][] ratings) {
        for (int i = 0; i < ratings.length; i++) {
            int user = (int) ratings[i][0] - 1;
            int profile = (int) ratings[i][1] - 1;
            double rating = ratings[i][2];
            ratingMatrix[user][profile] = rating;
        }
    }


    public void initializeNumberItemsRated() {
        for (int p = 0; p < 10000; p++) {
            for (int i = 0; i < 10000; i++) {
                if (itemsRated[p][i] == 0) {
                    break;
                }
                numberOfItemsRated[p] += 1;
            }
        }
    }


    public void initializeNumberUsersRated() {
        for (int p = 0; p < 10000; p++) {
            for (int i = 0; i < 10000; i++) {
                if (usersRated[p][i] == 0) {
                    break;
                }
                numberOfUsersRated[p] += 1;
            }
        }
    }


    public double makeBasePrediction(int user, int profile) {
        return globalMean + Bu[user] + Bi[profile];
    }


    double learningRateBase = 0.01;
    double lambdaBase = 0.1;
    public void updateBaseEffects(double[] ratingRow) {
        int user = (int) ratingRow[0] - 1;
        int profile = (int) ratingRow[1] - 1;
        double pred = makeBasePrediction(user, profile);
        double error = ratingRow[2] - pred;
        Bu[user] += learningRateBase*(error - lambdaBase*Bu[user]);
        Bi[profile] += learningRateBase*(error - lambdaBase*Bi[profile]);
    }

    //////////////////
    //
    // Do user-user on the base effects data
    //
    /////////////////
    int numNeighbors = 20;
    public double makePredictionCF(int user, int profile) {
        double errorSum = 0.0;
        double wSum = 0.0;
        PriorityQueue<IndexSimilarity> topSimilarities = new PriorityQueue<IndexSimilarity>(numNeighbors);
        for (int n = 0; n < numberOfUsersRated[profile]; n++) {
            int otherUser = usersRated[profile][n] - 1;
            IndexSimilarity userPair = new IndexSimilarity(otherUser, wu_ij[user][otherUser]);
            topSimilarities.add(userPair);
            if (topSimilarities.size() > numNeighbors) {
                topSimilarities.poll();
            }
        }
        for (int j = 0; j < numNeighbors; j++) {
            IndexSimilarity index = topSimilarities.poll();
            int ind = index.getIndex();
            double error = ratingMatrix[ind][profile] - makeBasePrediction(ind, profile);
            errorSum += wu_ij[user][index.getIndex()] * error;
            wSum += wu_ij[user][index.getIndex()];
        }

        return makeBasePrediction(user,profile) + errorSum/wSum;
    }


    //// Make the prediction with user-user AND item-item
    public double makePredictionCF2(int user, int profile) {
        double errorSumItem = 0.0;
        double wSumItem = 0.0;
        PriorityQueue<IndexSimilarity> topSimilaritiesItem = new PriorityQueue<IndexSimilarity>(numNeighbors);
        for (int n = 0; n < numberOfItemsRated[user]; n++) {
            int otherItem = itemsRated[user][n] - 1;
            if (Double.isNaN(wi_ij[profile][otherItem])) {
                System.out.println(profile);
                System.out.println(otherItem);
                System.out.println(wi_ij[profile][otherItem]);
            }
            IndexSimilarity userPair = new IndexSimilarity(otherItem, wi_ij[profile][otherItem]);
            topSimilaritiesItem.add(userPair);
            if (topSimilaritiesItem.size() > numNeighbors) {
                topSimilaritiesItem.poll();
            }
        }
        for (int j = 0; j < numNeighbors; j++) {
            IndexSimilarity index = topSimilaritiesItem.poll();
            int ind = index.getIndex();
            double error = ratingMatrix[user][ind] - makeBasePrediction(user, ind);
            errorSumItem += wi_ij[profile][index.getIndex()] * error;
            wSumItem += wi_ij[profile][index.getIndex()];
        }
        return makeBasePrediction(user,profile) + errorSumItem/wSumItem;
    }


    public void initializeSVD() {
        for (int i = 0; i < 10000; i++) {
            for (int f = 0; f < numTrainingFeatures; f++) {
                p_u[i][f] = 0.1;
                q_i[i][f] = 0.1;
            }
        }
    }


    public double makeSVDPrediction(int user, int profile) {
        double svdSum = 0.0;
        for (int f = 0; f < numTrainingFeatures; f++) {
            svdSum += p_u[user][f] * q_i[profile][f];
        }
        return makePredictionCF(user,profile) + svdSum;
    }


    double learningRateSVD = 0.005;
    double lambdaSVD = 0.1;
    public void updateSVDBetas(double[] ratingRow, int f) {
        int user = (int) ratingRow[0] - 1;
        int profile = (int) ratingRow[1] - 1;
        double error = ratingRow[2] - makeSVDPrediction(user, profile);
        p_u[user][f] += learningRateSVD*(error*q_i[profile][f] - lambdaSVD*p_u[user][f]);
        q_i[profile][f] += learningRateSVD*(error*p_u[user][f] - lambdaSVD*q_i[profile][f]);
    }
}
