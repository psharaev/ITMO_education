package info.kgeorgiy.ja.sharaev.walk;

public class WalkException extends Exception {
    public WalkException() {
        super();
    }

    public WalkException(String message) {
        super(message);
    }

    public WalkException(String message, Throwable cause) {
        super(message, cause);
    }

    public WalkException(Throwable cause) {
        super(cause);
    }

    protected WalkException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
