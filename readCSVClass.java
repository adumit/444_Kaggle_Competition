import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.function.DoubleBinaryOperator;

/**
 * Created by Andrew on 11/4/15.
 */
public class readCSVClass {

    public readCSVClass() {

    }

    public double[][] readidmap(String filename) {
        double[][] idmap = new double[500000][3];
        String delimiter = ",";
        int line = 0;
        File file = new File(filename);
        Scanner sc;
        try {
            sc = new Scanner(file);
        }
        catch (FileNotFoundException e) {
            throw new RuntimeException("Broken code.");
        }
        sc.nextLine();
        while (sc.hasNextLine()) {
            String lineStr = sc.nextLine();
            String[] index = lineStr.split(delimiter);
            idmap[line][0] = Integer.parseInt(index[0]);
            idmap[line][1] = Integer.parseInt(index[1]);
            idmap[line][2] = Integer.parseInt(index[2]);
            line += 1;
        }
        return idmap;
    }

    public double[][] readidmapWithLength(String filename, Integer dataLength) {
        double[][] idmap = new double[dataLength][3];
        String delimiter = ",";
        int line = 0;
        File file = new File(filename);
        Scanner sc;
        try {
            sc = new Scanner(file);
        }
        catch (FileNotFoundException e) {
            throw new RuntimeException("Broken code.");
        }
        sc.nextLine();
        while (sc.hasNextLine()) {
            String lineStr = sc.nextLine();
            String[] index = lineStr.split(delimiter);
            idmap[line][0] = Integer.parseInt(index[0]);
            idmap[line][1] = Integer.parseInt(index[1]);
            idmap[line][2] = Integer.parseInt(index[2]);
            line += 1;
        }
        return idmap;
    }

    public double[][] readSimItems(String fileName) {
        double[][] simItems = new double[10000][300];
        String delimiter = ",";
        int line = 0;
        File file = new File(fileName);
        Scanner sc;
        try {
            sc = new Scanner(file);
        }
        catch (FileNotFoundException e) {
            throw new RuntimeException("Broken code.");
        }
        sc.nextLine();
        while (sc.hasNextLine()) {
            String lineStr = sc.nextLine();
            String[] index = lineStr.split(delimiter);
            for (int i = 0; i < 300; i++) {
                simItems[line][i] = Integer.parseInt(index[i]);
            }
            line += 1;
        }
        return simItems;
    }

    public int[][] readItemList(String fileName) {
        int[][] itemList = new int[10000][10000];
        String delimiter = ",";
        int line = 0;
        File file = new File(fileName);
        Scanner sc;
        try {
            sc = new Scanner(file);
        }
        catch (FileNotFoundException e) {
            throw new RuntimeException("Broken code.");
        }
        sc.nextLine();
        while (sc.hasNextLine()) {
            String lineStr = sc.nextLine();
            String[] index = lineStr.split(delimiter);
            for (int i = 0; i < 10000; i++) {
                itemList[line][i] = Integer.parseInt(index[i]);
            }
            line += 1;
        }
        return itemList;
    }

    public double[][] readCSV(String fileName) {
        double[][] ratingsMat = new double[3279759][3];
        String delimiter = ",";
        int line = 0;
        File file = new File(fileName);
        Scanner sc;
        try {
            sc = new Scanner(file);
        }
        catch (FileNotFoundException e) {
            throw new RuntimeException("Broken code.");
        }
        sc.nextLine();
        boolean firstline = true;
        while (sc.hasNextLine()) {
//            if (firstline) {
//                String lineStr =
//                firstline = false;
//                continue;
//            }
            String lineStr = sc.nextLine();
            String[] index = lineStr.split(delimiter);
            ratingsMat[line][0] = Double.parseDouble(index[0]);
            ratingsMat[line][1] = Double.parseDouble(index[1]);
            ratingsMat[line][2] = Double.parseDouble(index[2]);
//            ratingsMat[line][3] = Double.parseDouble(index[3]);
            line += 1;
        }
        return ratingsMat;
    }

    public double[] readUserMeans(String filename) {
        double[] uMeans = new double[10000];
        String delimiter = ",";
        int line = 0;
        File file = new File(filename);
        Scanner sc;
        try {
            sc = new Scanner(file);
        }
        catch (FileNotFoundException e) {
            throw new RuntimeException("Broken code.");
        }
        sc.nextLine();
        while (sc.hasNextLine()) {
            String lineStr = sc.nextLine();
            String[] index = lineStr.split(delimiter);
            uMeans[line] = Double.parseDouble(index[0]);
            line += 1;
        }
        return uMeans;
    }

    public double[][] readCSVWithGender(String fileName) {
        double[][] ratingsMat = new double[3179759][4];
        String delimiter = ",";
        int line = 0;
        File file = new File(fileName);
        Scanner sc;
        try {
            sc = new Scanner(file);
        }
        catch (FileNotFoundException e) {
            throw new RuntimeException("Broken code.");
        }
        sc.nextLine();
        boolean firstline = true;
        while (sc.hasNextLine()) {
//            if (firstline) {
//                String lineStr =
//                firstline = false;
//                continue;
//            }
            String lineStr = sc.nextLine();
            String[] index = lineStr.split(delimiter);
            ratingsMat[line][0] = Double.parseDouble(index[0]);
            ratingsMat[line][1] = Double.parseDouble(index[1]);
            ratingsMat[line][2] = Double.parseDouble(index[2]);
            ratingsMat[line][3] = Double.parseDouble(index[3]);
            line += 1;
        }
        return ratingsMat;
    }

    public double[][] readCSVWithBothGender(String fileName) {
        double[][] ratingsMat = new double[3279759][5];
        String delimiter = ",";
        int line = 0;
        File file = new File(fileName);
        Scanner sc;
        try {
            sc = new Scanner(file);
        }
        catch (FileNotFoundException e) {
            throw new RuntimeException("Broken code.");
        }
        sc.nextLine();
        boolean firstline = true;
        while (sc.hasNextLine()) {
//            if (firstline) {
//                String lineStr =
//                firstline = false;
//                continue;
//            }
            String lineStr = sc.nextLine();
            String[] index = lineStr.split(delimiter);
            ratingsMat[line][0] = Double.parseDouble(index[0]);
            ratingsMat[line][1] = Double.parseDouble(index[1]);
            ratingsMat[line][2] = Double.parseDouble(index[2]);
            ratingsMat[line][3] = Double.parseDouble(index[3]);
            ratingsMat[line][4] = Double.parseDouble(index[4]);
            line += 1;
        }
        return ratingsMat;
    }

    public double[][] readSimilarityMatrix(String filename) {
        double[][] simMat = new double[10000][10000];
        String delimiter = ",";
        int line = 0;
        File file = new File(filename);
        Scanner sc;
        try {
            sc = new Scanner(file);
        }
        catch (FileNotFoundException e) {
            throw new RuntimeException("Broken code.");
        }
        sc.nextLine();
        while (sc.hasNextLine()) {
            String lineStr = sc.nextLine();
            String[] index = lineStr.split(delimiter);
            for (int i = 0; i < 10000; i++) {
                simMat[line][i] = Double.valueOf(index[i]);
            }
            line += 1;
        }
        return simMat;
    }


}
