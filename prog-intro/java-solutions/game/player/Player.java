package game.player;

import game.Cell;
import game.Move;
import game.board.Position;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Player {
    Move move(Position position, Cell cell);
}
