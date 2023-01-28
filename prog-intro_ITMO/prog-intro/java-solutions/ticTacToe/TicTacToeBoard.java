package ticTacToe;

import java.util.Arrays;
import java.util.Map;

public class TicTacToeBoard implements Board, Position {
    private static final Map<Cell, Character> SYMBOLS = Map.of(
            Cell.X, 'X',
            Cell.O, 'O',
            Cell.E, '.',
            Cell.N, ' '
    );


    private int m, n;
    private final int k;
    private final boolean rhombus;
    private final Cell[][] cells;
    private Cell turn;
    private int filledCount = 0;

    public TicTacToeBoard(final int n, final int m, final int k, final boolean rhombus) {
        this.k = k;
        this.n = n;
        this.m = m;
        this.rhombus = rhombus;
        turn = Cell.X;

        if (rhombus) {
            if (n != m) {
                throw new RuntimeException("If using rhombus, 'n' should be equals 'm'.");
            }
            this.cells = new Cell[n + n - 1][n + n - 1];
            for (Cell[] row : cells) {
                Arrays.fill(row, Cell.N);
            }
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    cells[i + j][n - 1 + i - j] = Cell.E;
                }
            }
        } else {
            this.cells = new Cell[n][m];
            for (Cell[] row : cells) {
                Arrays.fill(row, Cell.E);
            }
        }
    }

    private boolean won(int r, int c, int d1, int d2) {
        return getCnt(r, c, d1, d2) + getCnt(r, c, -d1, -d2) + 1 >= k;
    }

    private boolean extra(int r, int c, int d1, int d2) {
        return getCnt(r, c, d1, d2) + getCnt(r, c, -d1, -d2) + 1 >= 4;
    }

    private int getCnt(int r, int c, int i, int j) {
        int cnt = 0;
        while (r + i >= 0 && c + j >= 0
                && r + i < cells.length
                && c + j < cells[0].length
                && cells[r + i][c + j].equals(turn)) {
            cnt++;
            r += i;
            c += j;
        }
        return cnt;
    }

    @Override
    public Position getPosition() {
        return new ProxyPosition(this);
    }

    @Override
    public Cell getCell() {
        return turn;
    }

    @Override
    public Result makeMove(final Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }

        int r = move.getRow(), c = move.getColumn();
        cells[r][c] = move.getValue();

        if (rhombus) {
            if (won(r, c, 0, 2) || won(r, c, 2, 0)
                    || won(r, c, 1, 1) || won(r, c, 1, -1)) {
                return Result.WIN;
            }
        } else {
            if (won(r, c, 0, 1) || won(r, c, 1, 0)
                    || won(r, c, 1, 1) || won(r, c, 1, -1)) {
                return Result.WIN;
            }
        }

        if (++filledCount == n * m) {
            return Result.DRAW;
        }

        if (rhombus) {
            if (extra(r, c, 0, 2) || extra(r, c, 2, 0)
                    || extra(r, c, 1, 1) || extra(r, c, 1, -1)) {
                return Result.EXTRA;
            }
        } else {
            if (extra(r, c, 0, 1) || extra(r, c, 1, 0)
                    || extra(r, c, 1, 1) || extra(r, c, 1, -1)) {
                return Result.EXTRA;
            }
        }
        if (turn == Cell.X) turn = Cell.O;
        else turn = Cell.X;

        return Result.UNKNOWN;
    }

    @Override
    public boolean isValid(final Move move) {
        return 0 <= move.getRow() && move.getRow() < cells.length
                && 0 <= move.getColumn() && move.getColumn() < cells[0].length
                && cells[move.getRow()][move.getColumn()].equals(Cell.E)
                && getCell().equals(turn);
    }

    @Override
    public Cell getCell(int r, int c) {
        return cells[r][c];
    }

    @Override
    public int getK() {
        return k;
    }

    @Override
    public int getN() {
        return cells.length;
    }

    @Override
    public int getM() {
        return cells[0].length;
    }

    @Override
    public String toString() {
        int lenN = Integer.toString(cells.length).length(), lenM = Integer.toString(cells[0].length).length();
        final StringBuilder sb = new StringBuilder(" ".repeat(lenN + 1));
        for (int i = 1; i <= cells[0].length; i++) {
            sb.append(String.format("%" + lenM +"d ", i));
        }
        for (int r = 0; r < cells.length; r++) {
            sb.append("\n");
            sb.append(String.format("%" + lenN + "d ", (r + 1)));
            for (int c = 0; c < cells[0].length; c++) {
                sb.append(String.format("%" + lenM + "s ", SYMBOLS.get(cells[r][c])));
            }
        }
        return sb.toString();
    }
}
