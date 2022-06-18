package info.kgeorgiy.ja.sharaev.i18n;

import java.text.DateFormat;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.util.*;
import java.util.function.Function;

/**
 * Format {@link CollectedStats}
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class Formatter {
    public static final String PATH_FORMATTER_RESOURCE_BUNDLE = "info.kgeorgiy.ja.sharaev.i18n.FormatterResourceBundle";
    private final CollectedStats textStat;
    private final Locale locale;
    private final ResourceBundle bundle;

    /**
     * Create new instance
     *
     * @param textStat collected statistics
     * @param locale   output locale
     * @throws MissingResourceException if not found bundle for format
     */
    public Formatter(final CollectedStats textStat, final Locale locale) {
        this.textStat = textStat;
        this.locale = locale;
        this.bundle = ResourceBundle.getBundle(PATH_FORMATTER_RESOURCE_BUNDLE, locale);
    }

    /**
     * Build formatted Strings
     *
     * @return text
     */
    public List<String> getFormattedText() {
        final NumberFormat numberFormat = NumberFormat.getNumberInstance(locale);
        final NumberFormat moneyFormat = NumberFormat.getCurrencyInstance(locale);
        final DateFormat dateFormat = DateFormat.getDateInstance(DateFormat.DEFAULT, locale);
        final List<List<String>> data = List.of(
                formatHeader(),
                formatSummaryStatistics(),
                formatCategoryData(textStat.sentences(), "sentence", Function.identity()),
                formatCategoryData(textStat.words(), "word", Function.identity()),
                formatCategoryData(textStat.numbers(), "number", numberFormat::format),
                formatCategoryData(textStat.moneys(), "money", moneyFormat::format),
                formatCategoryData(textStat.dates(), "date", dateFormat::format)
        );
        return data.stream().flatMap(Collection::stream).toList();
    }

    private List<String> formatHeader() {
        return List.of(
                formatStatField("global", "analyzedFile", textStat.inputFIle().toString())
        );
    }

    private <V> List<String> formatCategoryData(
            final CategoryData<V> data, final String name,
            final Function<V, String> valueFormatter
    ) {
        final List<String> res = new ArrayList<>();

        res.add(bundle.getString("header." + name));
        res.add(formatStatField(name, "count", data.getCountOccurrences(), data.getCountDifferentValues()));
        res.add(formatStatField(name, "minValue", valueFormatter.apply(data.getMinValue())));
        res.add(formatStatField(name, "maxValue", valueFormatter.apply(data.getMaxValue())));
        if (data.hasLength()) {
            res.add(formatStatField(name, "minLengthValue", data.getMinLengthValueLength(), data.getMinLengthValue()));
            res.add(formatStatField(name, "maxLengthValue", data.getMaxLengthValueLength(), data.getMaxLengthValue()));
            res.add(formatStatField(name, "averageLength", data.getAverageLength()));
        } else {
            res.add(formatStatField(name, "averageValue", valueFormatter.apply(data.getAverageValue())));
        }
        return res;
    }

    private String formatStatField(final String dataName, final String formatName, final Object... args) {
        final Object[] varargsPain = new Object[args.length + 1];
        for (int i = 0; i < args.length; i++) {
            if (args[i] == null) {
                args[i] = bundle.getString("null");
            }
        }
        varargsPain[0] = bundle.getString(dataName + "." + formatName);
        System.arraycopy(args, 0, varargsPain, 1, args.length);
        return new MessageFormat(bundle.getString("format." + formatName), locale).format(varargsPain);
    }

    private List<String> formatSummaryStatistics() {
        final CategoryData<String> sentences = textStat.sentences();
        final CategoryData<String> words = textStat.words();
        final CategoryData<Number> numbers = textStat.numbers();
        final CategoryData<Number> moneys = textStat.moneys();
        final CategoryData<Date> dates = textStat.dates();
        return List.of(
                bundle.getString("count.all"),
                formatStatField("count", "sentence", sentences.getCountOccurrences()),
                formatStatField("count", "word", words.getCountOccurrences()),
                formatStatField("count", "number", numbers.getCountOccurrences()),
                formatStatField("count", "money", moneys.getCountOccurrences()),
                formatStatField("count", "date", dates.getCountOccurrences())
        );
    }
}
