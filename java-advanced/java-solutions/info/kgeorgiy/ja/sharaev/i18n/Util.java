package info.kgeorgiy.ja.sharaev.i18n;


/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class Util {
    public static void log(final String message) {
        System.err.println(message);
    }

    public static void log(final String message, final Exception e) {
        System.err.println(message + ", error: " + e.getMessage());
    }
}
