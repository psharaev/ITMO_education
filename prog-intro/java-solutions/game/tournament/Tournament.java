package game.tournament;

import game.Game;
import game.board.Board;
import game.console.SafeIO;
import game.player.Player;

public class Tournament {

    private final boolean logging;
    private final Player[] players;

    private final Board board;

    private final int quantityCircles;

    private final SafeIO cmd;

    public Tournament(final boolean logging, final SafeIO cmd, final Player[] players, final Board board, final int quantityCircles) {
        this.logging = logging;
        this.cmd = cmd;
        this.players = players;
        this.board = board;
        this.quantityCircles = quantityCircles;
    }

    public TournamentTable play() {
        final TournamentTable table = new TournamentTable(players.length, 3, 1, 0);

        for (int circle = 0; circle < quantityCircles; circle++) {
            for (int i = 0; i < players.length; i++) {
                for (int j = i + 1; j < players.length; j++) {
                    cmd.println(String.format("Start game for Player %d and Player %d", i + 1, j + 1));

                    final Game game = new Game(logging, players[i], players[j], i + 1, j + 1);

                    board.clearBoard();
                    int result = game.play(board);
                    if (result == 0) {
                        cmd.println("Draw Player " + (i + 1) + " and Player " + (j + 1));
                        table.playerDraw(i);
                        table.playerDraw(j);
                    } else if (result == 1) {
                        cmd.println("Win Player " + (i + 1));
                        table.playerWin(i);
                        table.playerLose(j);
                    } else if (result == 2) {
                        cmd.println("Win Player " + (j + 1));
                        table.playerLose(i);
                        table.playerWin(j);
                    }
                }
            }
        }

        return table;
    }
}
