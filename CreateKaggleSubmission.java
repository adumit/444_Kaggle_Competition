/**
 * Created by Andrew on 11/30/15.
 */
public class CreateKaggleSubmission {

    public static void main(String args[]) {

            readCSVClass newReader = new readCSVClass();
            double[][] trueRatings = newReader.readCSV("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/ratings.csv");
            double[][] idmap = newReader.readidmap("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/Competition/IDMap.csv");

            double[][] testSet = newReader.readCSV("/Users/Andrew/Desktop/Classes/Fall_2015/Stat444/CompetitionFinalWeek/ratingsTest10.csv");

            Predictor predictor = new Predictor();
            predictor.initializeBase();

            int iterations = 100; // 100 is a good number
            //For some number of iterations
            for (int iter = 0; iter < iterations; iter++) {
                //Do the updating
                for (int i = 0; i < trueRatings.length; i++) {
                    predictor.updateBaseEffects(trueRatings[i]);
                }

                //Finally calculate the test error
                double squaredErrorSum = 0.0;
                for (int r = 0; r < 100000; r++) {
                    int user = (int) testSet[r][0] - 1;
                    int profile = (int) testSet[r][1] - 1;
                    double pred = predictor.makeBasePrediction(user, profile);
                    double error = testSet[r][2] - pred;
                    squaredErrorSum += error*error;
                }
                System.out.println("Base test error = " + Math.sqrt(squaredErrorSum/100000));
            }

            predictor.initializeSVD();
            predictor.initializeNumberUsersRated();
            predictor.inititalizeRatingMatrix(trueRatings);

            double squaredErrorSum2 = 0.0;
            for (int r = 0; r < 100000; r++) {
                int user = (int) testSet[r][0] - 1;
                int profile = (int) testSet[r][1] - 1;
                double SVDPred = predictor.makePredictionCF(user, profile);
                double error = testSet[r][2] - SVDPred;
                squaredErrorSum2 += error*error;
            }
            System.out.println("SVD test error for CF = " + Math.sqrt(squaredErrorSum2/100000));

            int svdIterations = 15;
            for (int f = 0; f < 5; f++) {
                for (int svdIter = 0; svdIter < svdIterations; svdIter++) {
                    for (int r = 0; r < trueRatings.length; r++) {
                        predictor.updateSVDBetas(trueRatings[r], f);
                    }
                    double squaredErrorSum = 0.0;
                    for (int r = 0; r < 100000; r++) {
                        int user = (int) testSet[r][0] - 1;
                        int profile = (int) testSet[r][1] - 1;
                        double SVDPred = predictor.makeSVDPrediction(user, profile);
                        double error = testSet[r][2] - SVDPred;
                        squaredErrorSum += error*error;
                    }
                    System.out.println("SVD test error for feature " + f + " and iteration " + svdIter + " = " + Math.sqrt(squaredErrorSum/100000));
                }
            }



            double[][] predictions = new double[500000][2];
            for (int i = 0; i < 500000; i++) {
                int user = (int) idmap[i][0] - 1;
                int profile = (int) idmap[i][1] - 1;
                predictions[i][0] = i;
                predictions[i][1] = predictor.makeSVDPrediction(user, profile);
            }
            csvWriter.writePredictions("gradientDescentCFPredsWithSVD.csv", predictions);


        }

}
