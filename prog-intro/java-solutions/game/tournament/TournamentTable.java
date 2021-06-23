package game.tournament;

import java.util.Arrays;

public final class TournamentTable {

    private final int quantityPlayers;

    private final int[] playerWin;
    private final int[] playerDraw;
    private final int[] playerLose;
    private final int[] playerScore;

    private final int scoreForWin;
    private final int scoreForDraw;
    private final int scoreForLose;

    public TournamentTable(final int quantityPlayers, int scoreForWin, int scoreForDraw, int scoreForLose) {
        this.quantityPlayers = quantityPlayers;

        this.playerWin = new int[quantityPlayers];
        this.playerDraw = new int[quantityPlayers];
        this.playerLose = new int[quantityPlayers];
        this.playerScore = new int[quantityPlayers];

        this.scoreForWin = scoreForWin;
        this.scoreForDraw = scoreForDraw;
        this.scoreForLose = scoreForLose;
    }

    @Override
    public String toString() {
        final String headTable = "Player    |  Wins |  Draw |  Lose |  Score";
        StringBuilder sb = new StringBuilder(headTable);
        sb.append(System.lineSeparator());

        sb.append("-".repeat(headTable.length()));
        sb.append(System.lineSeparator());

        for (int i = 0; i < quantityPlayers; i++) {
            sb.append(String.format("Player %2d |%6d |%6d |%6d | %6d", i + 1,
                    playerWin[i],
                    playerDraw[i],
                    playerLose[i],
                    playerScore[i]));
            sb.append(System.lineSeparator());
        }
        return sb.toString();
    }

    public void playerWin(final int number) {
        playerWin[number]++;
        playerScore[number] += scoreForWin;
    }

    public void playerDraw(final int number) {
        playerDraw[number]++;
        playerScore[number] += scoreForDraw;
    }

    public void playerLose(final int number) {
        playerLose[number]++;
        playerScore[number] += scoreForLose;
    }
}
