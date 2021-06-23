package game.board;

import game.Move;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Position {
    boolean isValid(Move move);
    int getCountRows();
    int getCountCols();
}
