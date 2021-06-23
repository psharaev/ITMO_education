package game;

import game.board.Board;
import game.player.Player;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Game {
    private final boolean log;
    private final Player player1, player2;
    private final int numPlayer1, numPlayer2;

    public Game(final boolean log, final Player player1, final Player player2, final int numPlayer1, final int numPlayer2) {
        this.log = log;
        this.player1 = player1;
        this.player2 = player2;
        this.numPlayer1 = numPlayer1;
        this.numPlayer2 = numPlayer2;
    }

    public int play(Board board) {
        while (true) {
            final int result1 = move(board, player1, 1, numPlayer1);
            if (result1 != -1) {
                return result1;
            }

            final int result2 = move(board, player2, 2, numPlayer2);
            if (result2 != -1) {
                return result2;
            }
        }
    }

    private int move(final Board board, final Player player, final int no, final int realNumberPlayer) {
        final Move move = player.move(board.getPosition(), board.getTurn());
        final Result result = board.makeMove(move);
        log("Player " + no + " move: " + move);
        log("Position:" + System.lineSeparator() + board);
        if (result == Result.WIN) {
            log("Player " + realNumberPlayer + " won");
            return no;
        } else if (result == Result.LOSE) {
            log("Player " + realNumberPlayer + " lose");
            return 3 - no;
        } else if (result == Result.DRAW) {
            log("Draw");
            return 0;
        } else if (result == Result.CONTINUE) {
            return -1;
        } else {
            throw new AssertionError("Impossible");
        }
    }

    private void log(final String message) {
        if (log) {
            System.out.println(message);
        }
    }
}
