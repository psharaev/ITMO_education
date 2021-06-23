package game.player;

import game.Cell;
import game.Move;
import game.board.Position;
import game.console.SafeIO;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class HumanPlayer implements Player {

    private final SafeIO io;

    public HumanPlayer(final SafeIO io) {
        this.io = io;
    }

    @Override
    public Move move(final Position position, final Cell cell) {
        while (true) {
            io.println("Position");
            io.println(position);
            io.println(cell + "'s move");
            io.println("Enter row and column");

            final int[] inp = io.readNumbers(2);
            final int row = inp[0] - 1;
            final int column = inp[1] - 1;

            final Move move = new Move(row, column, cell);
            if (position.isValid(move)) {
                return move;
            }

            io.println("Move is invalid");
        }
    }
}
