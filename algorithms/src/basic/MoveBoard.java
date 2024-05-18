package basic;

public class MoveBoard {

    enum Direction {
        UP(0),
        DOWN(1),
        LEFT(2),
        RIGHT(3);
        int value;
        Direction(int value) {
            this.value = value;
        }
    }

    private int[][] DIRECTION = new int[][] {
        {-1, 0}, // up
        {1, 0},  // down
        {0, -1}, // left
        {0, 1} // right
    };

    private int[] zeroPosition = null;

    private StringBuffer stringBuffer = new StringBuffer();

    public MoveBoard(int[][] board) {
        for (int i = 0; i < board.length; i++) {
            for (int j = 0; j < board[i].length; j++) {
                if (board[i][j] == 0) {
                    this.zeroPosition = new int[] {i, j};
                }
                stringBuffer.append(board[i][j]);
            }
        }
    }

    private void move(int[][] board, Direction direction) {
        int i = this.zeroPosition[0];
        int j = this.zeroPosition[1];
        int ni = i + DIRECTION[direction.value][0];
        int nj = j + DIRECTION[direction.value][1];
        if (ni < 0 || nj < 0 || ni >= board.length || nj >= board[0].length) {
            return;
        }
        board[i][j] = board[ni][nj];
        board[ni][nj] = 0;
        this.zeroPosition[0] = ni;
        this.zeroPosition[1] = nj;

        int index = i * board[0].length + j;
        int newIndex = ni * board[0].length + nj;
        stringBuffer.setCharAt(index, stringBuffer.charAt(newIndex));
        stringBuffer.setCharAt(newIndex, '0');
    }

    private boolean isfinalized(int[][] board) {
        return stringBuffer.toString().equals("123450");
        /**
        if (this.zeroPosition[0] != 1 && this.zeroPosition[1] != 2) return false;
        int count = 1;
        for (int i = 0; i < board.length; i++) {
            for (int j = 0; j < board[i].length; j++) {
                if (board[i][j] != count) {
                    return false;
                }
                count++;
                if (count == 6) {
                    return true;
                }
            }
        }
        return true;
         **/
    }

    public static void main(String[] args) {
        int[][] board = new int[][] {
                {1, 2, 0},
                {4, 5, 3}
        };
        MoveBoard moveBoard = new MoveBoard(board);
        System.out.println(moveBoard.isfinalized(board));
        moveBoard.move(board, Direction.DOWN);
        for (int i = 0; i < board.length; i++) {
            for (int j = 0; j < board[i].length; j++) {
                System.out.print(board[i][j] + " ");
            }
            System.out.println();
        }
        System.out.println(moveBoard.isfinalized(board));
    }
}
