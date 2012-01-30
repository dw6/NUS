package hashioutputhelper;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class Main {

    // Lay bridge
    public static void layBridge(String bridgeLine, char[][] grid)
    {
        // Get Left / Right Island
        String[] islands = bridgeLine.split("\\|");

        String[] islandA = islands[0].split(",");
        String[] islandB = islands[1].split(",");

        int islandA_x = Integer.parseInt(islandA[0]);
        int islandA_y = Integer.parseInt(islandA[1]);
        int islandB_x = Integer.parseInt(islandB[0]);
        int islandB_y = Integer.parseInt(islandB[1]);


        int startIdx, endIdx;

        // Determine if its horizontal or vertical
        if (islandA_x != islandB_x)
        {
            // Lay horizontal bridges. (y is the same)
            startIdx = Math.min(islandA_x, islandB_x) + 1;
            endIdx = Math.max(islandA_x, islandB_x);

            for( int i = startIdx; i < endIdx; ++i )
            {
                if(grid[islandA_y][i] == '.')
                {
                    grid[islandA_y][i] = '-';
                }
                else
                {
                    grid[islandA_y][i] = '=';
                }
            }
        }
        else
        {
            // Lay vertical bridges. (y is the same)
            startIdx = Math.min(islandA_y, islandB_y) + 1;
            endIdx = Math.max(islandA_y, islandB_y);

            for( int i = startIdx; i < endIdx; ++i )
            {
                if(grid[i][islandA_x] == '.')
                {
                    grid[i][islandA_x] = '|';
                }
                else
                {
                    grid[i][islandA_x] = '"';
                }
            }
        }


    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        if (args.length < 1) {
            System.out.println("Usage: <testN.txt>");
        }
        
        File file = new File(args[0]);
        FileInputStream fis = null;
        BufferedInputStream bis = null;
        DataInputStream dis = null;
        ArrayList<String> fileContent = new ArrayList<String>();

        try {
            fis = new FileInputStream(file);
            bis = new BufferedInputStream(fis);
            dis = new DataInputStream(bis);

            while (dis.available() != 0) {
                fileContent.add(dis.readLine());
            }
            
            fis.close();
            bis.close();
            dis.close();

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        // Print out the file content

        // First line gives the dimension of the grids.
        String[] dimension = fileContent.get(0).split(" ");

        // Create grid based on dimensions.
        char[][] grid = new char[Integer.parseInt(dimension[0])][Integer.parseInt(dimension[1])];

        fileContent.remove(0); // Don't need the dimensions any more

        String line = "";

        // Fill up the grid
        for( int i = 0; i < fileContent.size(); ++i )
        {
            line = fileContent.get(i);
            
            for( int j = 0; j < line.length(); ++j )
            {
                grid[i][j] = line.charAt(j);
            }
        }

        ArrayList<String> results = new ArrayList<String>();

        while (sc.hasNext()) {
            results.add(sc.next());
        }

        for(String r : results)
        {
            layBridge(r, grid);
            //System.out.println(r);
        }


        // Print the grid to double check
        for(int i = 0; i < fileContent.size(); ++i) {
            for(int j = 0; j < fileContent.get(0).length(); ++j) {
                System.out.print(grid[i][j]);
            }
            System.out.println();}
        }
}
