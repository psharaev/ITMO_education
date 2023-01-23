package error;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class GenerateException extends RuntimeException {
    public GenerateException(final String message) {
        super("Generation error: " + message);
    }
}
