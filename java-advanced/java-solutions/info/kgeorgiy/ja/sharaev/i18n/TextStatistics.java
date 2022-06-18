package info.kgeorgiy.ja.sharaev.i18n;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

import static info.kgeorgiy.ja.sharaev.i18n.Util.log;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class TextStatistics {
    public static final String PATH_INTERFACE_RESOURCE_BUNDLE = "info.kgeorgiy.ja.sharaev.i18n.InterfaceResourceBundle";

    private static String formatValues(final ResourceBundle bundle, final String formatKey, final Object... values) {
        return MessageFormat.format(bundle.getString(formatKey), values);
    }

    private static String formatKeys(final ResourceBundle bundle, final String formatKey, final String... keys) {
        final Object[] args = new String[keys.length];
        for (int i = 0; i < args.length; i++) {
            args[i] = bundle.getString(keys[i]);
        }
        return formatValues(bundle, formatKey, args);
    }

    public static void main(final String[] args) {
        ResourceBundle interfaceBundle = ResourceBundle.getBundle(PATH_INTERFACE_RESOURCE_BUNDLE);
        if (args == null) {
            log(formatKeys(interfaceBundle, "argsNull", "usage"));
            return;
        }

        if (args.length != 4) {
            log(formatKeys(interfaceBundle, "argsWrongLength", "usage"));
            return;
        }

        for (int i = 0; i < 4; i++) {
            if (args[i] == null) {
                log(formatValues(interfaceBundle, "argsItemNull", interfaceBundle.getString("usage"), i));
                return;
            }
        }

        final Locale textLocale = Locale.forLanguageTag(args[0]);
        final Locale InterfaceLocale = Locale.forLanguageTag(args[1]);
        interfaceBundle = ResourceBundle.getBundle(PATH_INTERFACE_RESOURCE_BUNDLE, InterfaceLocale);
        System.out.println(formatValues(interfaceBundle, "textLocaleUse", textLocale.toLanguageTag()));
        System.out.println(formatValues(interfaceBundle, "interfaceLocaleUse", InterfaceLocale.toLanguageTag()));

        final Path inputPath = getPath(args[2], interfaceBundle, "incorrectInputPath");
        if (inputPath == null) {
            return;
        }

        final Path outputPath = getPath(args[3], interfaceBundle, "incorrectOutputPath");
        if (outputPath == null) {
            return;
        }

        final String inputText;
        try {
            inputText = Files.readString(inputPath);
        } catch (final IOException e) {
            log(formatValues(interfaceBundle, "failedRead", e.getMessage()));
            return;
        }

        final Parser st = new Parser(inputPath, inputText, textLocale);
        final CollectedStats data = st.collectStats();
        try {
            Files.write(outputPath, formatData(InterfaceLocale, data));
        } catch (final IOException e) {
            log(formatValues(interfaceBundle, "failedWrite", e.getMessage()));
        }
    }

    private static Path getPath(final String path, final ResourceBundle bundle, final String formatKey) {
        try {
            return Path.of(path);
        } catch (final InvalidPathException e) {
            log(formatValues(bundle, formatKey, e.getMessage()));
            return null;
        }
    }

    private static List<String> formatData(final Locale outputLocale, final CollectedStats data) {
        return new Formatter(data, outputLocale).getFormattedText();
    }
}
