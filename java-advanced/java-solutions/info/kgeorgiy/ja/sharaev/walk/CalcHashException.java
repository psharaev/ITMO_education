package info.kgeorgiy.ja.sharaev.walk;

public class CalcHashException extends Exception {
    public CalcHashException() {
        super();
    }

    public CalcHashException(String message) {
        super(message);
    }

    public CalcHashException(String message, Throwable cause) {
        super(message, cause);
    }

    public CalcHashException(Throwable cause) {
        super(cause);
    }

    protected CalcHashException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
