package game;

import game.board.MNKBoard;
import game.console.SafeIO;
import game.player.*;
import game.tournament.Tournament;
import game.tournament.TournamentTable;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Main {

    public static void main(String[] args) {
        SafeIO cmd = new SafeIO(System.in, System.out);

        cmd.println("Enable logging? [y/n]");
        final boolean logging = cmd.readBoolean();

        cmd.println("Enter number of players");
        final int quantityPlayers = cmd.readNumbers(1, 2)[0];
        cmd.println("Enter count circles");
        final int quantityCircles = cmd.readNumbers(1, 1)[0];

        Player[] players = new Player[quantityPlayers];
        for (int i = 0; i < quantityPlayers; i++) {
            players[i] = choosePlayer("Player " + (i + 1), cmd);
        }

        cmd.println("Enter width, height and how many cells in a row to win");
        final int m, n, k;
        while (true) {
            int[] inp = cmd.readNumbers(3, 1);
            if (inp[2] <= Math.min(inp[0], inp[1])) {
                m = inp[0];
                n = inp[1];
                k = inp[2];
                break;
            }
            cmd.println("Incorrect row value");
        }

        final Tournament tournament = new Tournament(logging, cmd, players, new MNKBoard(m, n, k), quantityCircles);

        do {
            TournamentTable table = tournament.play();

            cmd.println("Tournament table result:");
            cmd.println(table);
            cmd.println("Repeat tournament? [y/n]");
        } while (cmd.readBoolean());
    }

    private static Player choosePlayer(final String name, final SafeIO cmd) {
        cmd.println("Choose who: " + name);
        cmd.println("1 - Human player");
        cmd.println("2 - Random player");
        cmd.println("3 - Sequential player");
        int inp;
        do {
            inp = cmd.readNumbers(1)[0];
            if (1 <= inp && inp <= 3) {
                break;
            }
            cmd.println("Enter number in range 1 and 3");
        } while (true);

        if (inp == 1) {
            return new HumanPlayer(cmd);
        } else if (inp == 2) {
            return new RandomPlayer();
        } else {
            return new SequentialPlayer();
        }

    }
}
