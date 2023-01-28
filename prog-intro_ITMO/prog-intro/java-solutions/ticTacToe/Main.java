package ticTacToe;

public class Main {
    public static void main(String[] args) {
        final Game game = new Game(false, new RandomPlayer(), new HumanPlayer());
        int result;
        do {
            result = game.play(new TicTacToeBoard(5, 5, 5, true), true);
            System.out.println("Game result: " + result);
        } while (result != 0);
    }
}
