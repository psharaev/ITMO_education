package game.board;

import game.Cell;
import game.Move;
import game.Result;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Board {
    Position getPosition();
    Cell getTurn();
    Result makeMove(Move move);

    void clearBoard();
}
