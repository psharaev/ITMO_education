package info.kgeorgiy.ja.sharaev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.nio.charset.CharacterCodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

import static info.kgeorgiy.ja.sharaev.hello.Util.log;

/**
 * Basic client implementation {@link HelloClient}
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public abstract class AbstractHelloClient implements HelloClient {
    /**
     * time to interrupt IO
     */
    protected static final int IO_TIMEOUT = 300;

    /**
     * Main arguments instruction
     */
    protected static final String MAIN_USAGE = "usage: <host> <prefix> <port> <number of threads> <number of requests>";

    /**
     * Main with {@link #MAIN_USAGE}
     *
     * @param args           Array of arguments:
     *                       1. host or ip-address;
     *                       2. port number;
     *                       3. prefix request;
     *                       4. number of threads;
     *                       5. number of requests.
     * @param instanceClient {@link Supplier} create new instance {@link AbstractHelloClient}
     */
    public static void main(final String[] args, final Supplier<AbstractHelloClient> instanceClient) {
        if (args == null) {
            log("Args is null");
            return;
        }

        if (args.length != 5) {
            log("Wrong count arguments");
            return;
        }

        for (int i = 0; i < 5; i++) {
            if (args[i] == null) {
                log("Arg number " + i + " is null");
                return;
            }
        }

        instanceClient.get().run(args[0], Integer.parseInt(args[1]),
                args[2], Integer.parseInt(args[3]), Integer.parseInt(args[4]));
    }

    private static int parseIntWithoutLeadingZero(final String s) {
        if (s.length() > 1 && s.charAt(0) == '0') {
            throw new NumberFormatException("Founded leading zero in '" + s + "'");
        }

        return Integer.parseInt(s);
    }

    private static List<Integer> findNumbers(final String s) {
        int startIndex = -1;
        final List<Integer> res = new ArrayList<>();
        for (int i = 0; i < s.length(); i++) {
            if (Character.isDigit(s.charAt(i))) {
                if (startIndex == -1) {
                    startIndex = i;
                }
            } else if (startIndex != -1) {
                try {
                    res.add(parseIntWithoutLeadingZero(s.substring(startIndex, i)));
                } catch (final NumberFormatException ignored) {

                }
                startIndex = -1;
            }
        }
        if (startIndex != -1) {
            try {
                res.add(parseIntWithoutLeadingZero(s.substring(startIndex)));
            } catch (final NumberFormatException ignored) {

            }
        }
        return res;
    }

    /**
     * Verify received data contain threadNumber and requestNumber
     *
     * @param receivedData  - received data
     * @param threadNumber  - thread number who receive data
     * @param requestNumber - request number in thread
     * @return true if receive data contains thread number and request number
     */
    protected static boolean checkRelation(final String receivedData, final int threadNumber, final int requestNumber) {
        final List<Integer> nums = findNumbers(receivedData);
        if (nums.size() != 2) {
            return false;
        }
        return nums.get(0) == threadNumber && nums.get(1) == requestNumber;
    }

    /**
     * Print message if fail decode request
     */
    protected void failDecode(final String request, final CharacterCodingException e) {
        System.err.printf("FAIL decode receive:%n\trequest: %s%n\tmessage: %s%n", request, e.getMessage());
    }

    /**
     * Print message if fail receive request
     */
    protected void failReceive(final String request, final IOException e) {
        System.err.printf("FAIL receive:%n\trequest: %s%n\tmessage: %s%n", request, e.getMessage());
    }

    /**
     * Print message if fail send request
     */
    protected void failSend(final String request, final IOException e) {
        System.err.printf("FAIL send:%n\trequest: %s%n\tmessage: %s%n", request, e.getMessage());
    }

    /**
     * Print message if success check relation request and receive
     */
    protected void successRelation(final String request, final String receive) {
        System.out.printf("SUCCESS:%n\trequest: %s%n\treceive: %s%n", request, receive);
    }

    /**
     * Print message if fail check relation request and receive
     */
    protected void failRelation(final String request, final String receive) {
        System.err.printf("FAIL relation:%n\trequest: %s%n\treceive: %s%n", request, receive);
    }
}
