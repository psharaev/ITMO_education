package ru.itmo.wp.web.page;

import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused", "RedundantSuppression"})
public class TicTacToePage {

    private static final String CELL_PARAMETER_NAME_PREFIX = "cell_";

    private State getState(HttpServletRequest request) {
        State state = (State) request.getSession().getAttribute("state");
        if (state == null) {
            state = new State();
            request.getSession().setAttribute("state", state);
        }
        return state;
    }

    private void action(HttpServletRequest request, Map<String, Object> view) {
        view.put("state", getState(request));
    }

    private void onMove(HttpServletRequest request, Map<String, Object> view) {
        State state = getState(request);
        Map.Entry<String, String[]> data = request.getParameterMap().entrySet().stream()
                .filter(e -> e.getKey().startsWith(CELL_PARAMETER_NAME_PREFIX))
                .findFirst()
                .orElse(null);
        if (data != null) {
            String key = data.getKey();
            int row = key.charAt(CELL_PARAMETER_NAME_PREFIX.length()) - '0';
            int col = key.charAt(CELL_PARAMETER_NAME_PREFIX.length() + 1) - '0';
            state.makeMove(row, col);
        }
        view.put("state", state);
        throw new RedirectException("/ticTacToe");
    }

    private void newGame(HttpServletRequest request, Map<String, Object> view) {
        State state = (State) request.getSession().getAttribute("state");
        if (state == null) {
            return;
        }
        state.clear();
        view.put("state", state);
    }

    public enum Phase {
        RUNNING,
        WON_X,
        WON_O,
        DRAW
    }

    public static class State {
        private final int DEFAULT_SIZE = 3;

        private boolean crossesMove;
        private Phase phase;
        private final Character[][] cells;
        private int moves;

        private State() {
            cells = new Character[DEFAULT_SIZE][DEFAULT_SIZE];
            clear();
        }

        public int getSize() {
            return DEFAULT_SIZE;
        }

        public boolean getCrossesMove() {
            return crossesMove;
        }

        public Phase getPhase() {
            return phase;
        }

        public Character[][] getCells() {
            return cells.clone();
        }

        private boolean isInRange(int index) {
            return 0 <= index && index < DEFAULT_SIZE;
        }

        private void onMoveEnd(int row, int col) {
            if (checkWin(row, col)) {
                phase = (crossesMove ? Phase.WON_X : Phase.WON_O);
            } else if (moves == DEFAULT_SIZE * DEFAULT_SIZE) {
                phase = Phase.DRAW;
            }
        }

        private boolean checkWin(int row, int col) {
            char c = getMoveChar();
            int inMainDiag = countDirection(row, col, c, -1, -1)
                    + countDirection(row, col, c, +1, +1) - 1;
            int inSideDiag = countDirection(row, col, c, -1, +1)
                    + countDirection(row, col, c, +1, -1) - 1;
            int inRow = countDirection(row, col, c, 0, -1)
                    + countDirection(row, col, c, 0, +1) - 1;
            int inCol = countDirection(row, col, c, -1, 0)
                    + countDirection(row, col, c, +1, 0) - 1;
            return inMainDiag == getSize() || inSideDiag == getSize() || inRow == getSize() || inCol == getSize();
        }

        private char getMoveChar() {
            return crossesMove ? 'X' : 'O';
        }

        private int countDirection(int row, int col, char turn, final int increaseRow, final int increaseColumn) {
            int result = 0;

            while (isInRange(row) && isInRange(col) && cells[row][col] != null && turn == cells[row][col]) {
                row += increaseRow;
                col += increaseColumn;
                result++;
            }

            return result;
        }

        private void makeMove(int row, int col) {
            if (!(isInRange(row) && isInRange(col)) || phase != Phase.RUNNING || cells[row][col] != null) {
                return;
            }
            cells[row][col] = getMoveChar();
            moves++;
            onMoveEnd(row, col);
            crossesMove = !crossesMove;
        }

        private void clear() {
            for (int i = 0; i < DEFAULT_SIZE; ++i) {
                for (int j = 0; j < DEFAULT_SIZE; ++j) {
                    cells[i][j] = null;
                }
            }
            phase = Phase.RUNNING;
            crossesMove = true;
            moves = 0;
        }
    }
}
