package game.board;

import game.Cell;
import game.Move;
import game.Result;

import java.util.Arrays;
import java.util.Map;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class MNKBoard implements Board, Position {
    private static final Map<Cell, Character> SYMBOLS = Map.of(
            Cell.X, 'X',
            Cell.O, 'O',
            Cell.E, '.'
    );

    private final int columns, rows, k;

    private final Cell[][] cells;
    private Cell turn;
    private int quantityEmptyCells;

    public MNKBoard(final int rows, final int columns, final int k) {
        if (rows < 1 || columns < 1 || k < 1 || (k > Math.min(rows, columns))) {
            throw new AssertionError("Invalid mnk data");
        }
        this.columns = columns;
        this.rows = rows;
        this.k = k;
        this.cells = new Cell[rows][columns];
        clearBoard();
    }

    public void clearBoard() {
        quantityEmptyCells = columns * rows;
        for (Cell[] row : cells) {
            Arrays.fill(row, Cell.E);
        }
        turn = Cell.X;
    }

    public MNKBoard() {
        this(3, 3, 3);
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public Cell getTurn() {
        return turn;
    }

    @Override
    public Result makeMove(final Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }

        cells[move.getRow()][move.getColumn()] = move.getValue();
        quantityEmptyCells--;

        int inMainDiag = cellsCounter(move, -1, -1) + cellsCounter(move, +1, +1) - 1;
        int inSideDiag = cellsCounter(move, -1, +1) + cellsCounter(move, +1, -1) - 1;
        int inRow = cellsCounter(move, 0, -1) + cellsCounter(move, 0, +1) - 1;
        int inCol = cellsCounter(move, -1, 0) + cellsCounter(move, +1, 0) - 1;

        if (inMainDiag >= k || inSideDiag >= k || inRow >= k || inCol >= k) {
            return Result.WIN;
        }
        if (quantityEmptyCells == 0) {
            return Result.DRAW;
        }

        turn = turn == Cell.X ? Cell.O : Cell.X;
        return Result.CONTINUE;
    }

    private int cellsCounter(final Move move, final int increaseRow, final int increaseColumn) {
        int row = move.getRow();
        int col = move.getColumn();
        int res = 0;

        while (0 <= col && col < columns
                && 0 <= row && row < rows
                && cells[row][col] == turn) {
            row += increaseRow;
            col += increaseColumn;
            res++;
        }

        return res;
    }

    @Override
    public boolean isValid(final Move move) {
        return 0 <= move.getRow() && move.getRow() < rows
                && 0 <= move.getColumn() && move.getColumn() < columns
                && cells[move.getRow()][move.getColumn()] == Cell.E
                && turn == move.getValue();
    }

    @Override
    public int getCountRows() {
        return rows;
    }

    @Override
    public int getCountCols() {
        return columns;
    }

    @Override
    public String toString() {
        final int quantityDigits = Integer.toString(Math.max(columns, rows)).length() + 1;

        final StringBuilder sb = new StringBuilder(" ".repeat(quantityDigits));

        final String keyInteger = "%" + quantityDigits + "d";
        final String keyEnum = "%" + quantityDigits + "c";

        for (int i = 1; i <= columns; i++) {
            sb.append(String.format(keyInteger, i));
        }

        for (int r = 0; r < rows; r++) {
            sb.append(System.lineSeparator());
            sb.append(String.format(keyInteger, r + 1));
            for (int c = 0; c < columns; c++) {
                sb.append(String.format(keyEnum, SYMBOLS.get(cells[r][c])));
            }
        }

        return sb.toString();
    }
}
