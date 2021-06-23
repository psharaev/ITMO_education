package game.console;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.Scanner;

public class SafeIO {
    private final Scanner in;
    private final PrintStream out;

    public SafeIO(final InputStream in, final PrintStream out) {
        this.in = new Scanner(in);
        this.out = out;
    }

    public int[] readNumbers(final int size) {
        while (in.hasNextLine()) {
            try {
                String[] strings = in.nextLine().split(" ");

                if (strings.length != size) {
                    throw new Exception("Invalid quantity");
                }

                int[] res = new int[size];
                for (int i = 0; i < size; i++) {
                    res[i] = Integer.parseInt(strings[i]);
                }

                return res;
            } catch (Exception e) {
                out.println("Please enter " + size + " numbers");
            }
        }

        throw new AssertionError("Error input data");
    }

    public int[] readNumbers(final int size, final int allGreater) {
        while (true) {
            int[] res = readNumbers(size);
            boolean flag = true;
            for (int re : res) {
                if (re < allGreater) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                return res;
            }
            out.println("Please enter numbers >= " + allGreater);
        }
    }

    public boolean readBoolean() {
        while (in.hasNextLine()) {
            String str = in.nextLine();
            if (str.equals("y") || str.equals("n")) {
                return str.equals("y");
            }
            out.println("Please enter [y/n]");
        }
        throw new AssertionError("Error input data");
    }

    public void println(final Object message) {
        out.println(message);
    }
}
