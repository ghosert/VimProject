package basic;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

public class IsLandNumber {

    private Set<String> visitedSet = null;

    public int numIslands(char[][] grid) {
        this.visitedSet = new HashSet<>();
        int result = 0;
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[i].length; j++) {
                if (grid[i][j] == '1') {
                    if (!visitedSet.contains(i + "_" + j)) {
                        result++;
                        this.calculateIsland(grid, i, j);
                    }
                }
            }
        }
        return result;
    }

    private void calculateIsland(char[][] grid, int i, int j) {
        Queue<int[]> queue = new LinkedList<>();
        queue.add(new int[] {i, j});
        this.visitedSet.add(i + "_" + j);
        while (!queue.isEmpty()) {
            int size = queue.size();
            for (int k = 0; k < size; k++) {
                int[] node = queue.poll();
                // left node
                int x = node[0];
                int y = node[1] - 1;
                if (y >= 0) {
                    this.handle(grid, queue, x, y);
                }
                // right node
                y = node[1] + 1;
                if (y < grid[0].length) {
                    this.handle(grid, queue, x, y);
                }
                // up node
                x = node[0] - 1;
                y = node[1];
                if (x >= 0) {
                    this.handle(grid, queue, x, y);
                }
                // down node
                x = node[0] + 1;
                if (x < grid.length) {
                    this.handle(grid, queue, x, y);
                }
            }
        }
    }

    private void handle(char[][] grid, Queue<int[]> queue, int x, int y) {
        String s = x + "_" + y;
        if (!this.visitedSet.contains(s) && grid[x][y] == '1') {
            this.visitedSet.add(s);
            queue.add(new int[] {x, y});
        }

        int[][] st = new int[][] {new int[] {1,2}};
    }

    public static void main(String[] args) {
        char[][] grid = new char[][] {
            {'1','0','0','1','1','1','0','1','1','0','0','0','0','0','0','0','0','0','0','0'}, // 3
            {'1','0','0','1','1','0','0','1','0','0','0','1','0','1','0','1','0','0','1','0'}, // 4
            {'0','0','0','1','1','1','1','0','1','0','1','1','0','0','0','0','1','0','1','0'}, // 2
            {'0','0','0','1','1','0','0','1','0','0','0','1','1','1','0','0','1','0','0','1'}, // 2
            {'0','0','0','0','0','0','0','1','1','1','0','0','0','0','0','0','0','0','0','0'}, // 0
            {'1','0','0','0','0','1','0','1','0','1','1','0','0','0','0','0','0','1','0','1'}, // 4
            {'0','0','0','1','0','0','0','1','0','1','0','1','0','1','0','1','0','1','0','1'}, // 4
            {'0','0','0','1','0','1','0','0','1','1','0','1','0','1','1','0','1','1','1','0'}, // 1
            {'0','0','0','0','1','0','0','1','1','0','0','0','0','1','0','0','0','1','0','1'}, // 2
            {'0','0','1','0','0','1','0','0','0','0','0','1','0','0','1','0','0','0','1','0'}, // 5
            {'1','0','0','1','0','0','0','0','0','0','0','1','0','0','1','0','1','0','1','0'}, // 3
            {'0','1','0','0','0','1','0','1','0','1','1','0','1','1','1','0','1','1','0','0'}, // 4
            {'1','1','0','1','0','0','0','0','1','0','0','0','0','0','0','1','0','0','0','1'},
            {'0','1','0','0','1','1','1','0','0','0','1','1','1','1','1','0','1','0','0','0'},
            {'0','0','1','1','1','0','0','0','1','1','0','0','0','1','0','1','0','0','0','0'},
            {'1','0','0','1','0','1','0','0','0','0','1','0','0','0','1','0','1','0','1','1'},
            {'1','0','1','0','0','0','0','0','0','1','0','0','0','1','0','1','0','0','0','0'},
            {'0','1','1','0','0','0','1','1','1','0','1','0','1','0','1','1','1','1','0','0'},
            {'0','1','0','0','0','0','1','1','0','0','1','0','1','0','0','1','0','0','1','1'},
            {'0','0','0','0','0','0','1','1','1','1','0','1','0','0','0','1','1','0','0','0'}
        };
        int result = new IsLandNumber().numIslands(grid);
        System.out.println(result);
    }
}
