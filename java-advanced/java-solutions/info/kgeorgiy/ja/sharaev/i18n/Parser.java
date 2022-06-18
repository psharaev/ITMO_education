package info.kgeorgiy.ja.sharaev.i18n;

import java.nio.file.Path;
import java.text.*;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Implement parse text
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class Parser {
    private final Path inputFile;
    private final String text;
    private final Locale inputLocale;
    private final NumberFormat numberFormat;
    private final NumberFormat moneyFormat;
    private final List<DateFormat> dateFormats;

    /**
     * Create new instance
     *
     * @param inputFile   path to input file
     * @param text        text for parse
     * @param inputLocale input locale
     */
    public Parser(final Path inputFile, final String text, final Locale inputLocale) {
        this.inputFile = inputFile;
        this.text = text;
        this.inputLocale = inputLocale;
        numberFormat = NumberFormat.getNumberInstance(inputLocale);
        moneyFormat = NumberFormat.getCurrencyInstance(inputLocale);
        dateFormats = List.of(
                DateFormat.getDateInstance(DateFormat.FULL, inputLocale),
                DateFormat.getDateInstance(DateFormat.LONG, inputLocale),
                DateFormat.getDateInstance(DateFormat.MEDIUM, inputLocale),
                DateFormat.getDateInstance(DateFormat.SHORT, inputLocale)
        );
    }

    public CollectedStats collectStats() {
        return new CollectedStats(
                inputFile,
                getSentences(),
                getWords(),
                getNumbers(),
                getMoneys(),
                getDates()
        );
    }

    private CategoryData<String> getSentences() {
        final List<String> sentenceTokens = getTokens(BreakIterator.getSentenceInstance(inputLocale));
        return new CategoryData<>(sentenceTokens,
                Collator.getInstance(inputLocale)::compare,
                String::length,
                null
        );
    }

    private CategoryData<String> getWords() {
        final List<String> tokens = getTokens(BreakIterator.getWordInstance(inputLocale));
        final List<String> words = new ArrayList<>(tokens.stream().filter(w -> w.codePoints().anyMatch(Character::isLetter)).toList());
        return new CategoryData<>(words,
                Collator.getInstance(inputLocale)::compare,
                String::length,
                null
        );
    }

    private CategoryData<Number> getNumbers() {
        final List<Number> numbers = parseNumbers();
        return new CategoryData<>(
                numbers,
                Comparator.comparingDouble(Number::doubleValue),
                null,
                Collectors.averagingDouble(Number::doubleValue)
        );
    }

    private CategoryData<Number> getMoneys() {
        final ArrayList<Number> numbers = parseType(this::parseMoney);
        return new CategoryData<>(numbers,
                Comparator.comparingDouble(Number::doubleValue),
                null,
                Collectors.averagingDouble(Number::doubleValue)
        );
    }

    private CategoryData<Date> getDates() {
        final ArrayList<Date> dates = parseType(this::parseDate);
        return new CategoryData<>(dates,
                Comparator.comparingLong(Date::getTime),
                null,
                Collectors.collectingAndThen(Collectors.averagingLong(Date::getTime), t -> new Date(t.longValue()))
        );
    }

    private List<Number> parseNumbers() {
        final List<Number> items = new ArrayList<>();
        final BreakIterator boundary = BreakIterator.getWordInstance(inputLocale);
        boundary.setText(text);

        final ParsePosition pos = new ParsePosition(-1);
        int start = boundary.first();
        int endPosSkippedItem = 0;
        for (int end = boundary.next(); end != BreakIterator.DONE; start = end, end = boundary.next()) {
            if (start >= endPosSkippedItem) {
                pos.setIndex(start);
                if (parseMoney(pos) == null && parseDate(pos) == null) {
                    final Number number = numberFormat.parse(text, pos);
                    if (number != null) {
                        items.add(number);
                    }
                }
                endPosSkippedItem = pos.getIndex();
            }
        }
        return items;
    }

    private <V> ArrayList<V> parseType(final Function<ParsePosition, V> parseFunction) {
        final ArrayList<V> items = new ArrayList<>();
        final BreakIterator boundary = BreakIterator.getWordInstance(inputLocale);
        boundary.setText(text);

        final ParsePosition pos = new ParsePosition(-1);
        int start = boundary.first();
        int endPosSkippedItem = 0;
        for (int end = boundary.next(); end != BreakIterator.DONE; start = end, end = boundary.next()) {
            if (start >= endPosSkippedItem) {
                pos.setIndex(start);
                final V item = parseFunction.apply(pos);
                if (item != null) {
                    endPosSkippedItem = pos.getIndex();
                    items.add(item);
                }
            }
        }
        return items;
    }

    private List<String> getTokens(final BreakIterator boundary) {
        final List<String> tokens = new ArrayList<>();
        boundary.setText(text);

        int start = boundary.first();
        for (int end = boundary.next(); end != BreakIterator.DONE; start = end, end = boundary.next()) {
            tokens.add(text.substring(start, end));
        }

        return tokens;
    }

    private Number parseMoney(final ParsePosition pos) {
        return moneyFormat.parse(text, pos);
    }

    private Date parseDate(final ParsePosition pos) {
        for (final DateFormat dateFormat : dateFormats) {
            final Date date = dateFormat.parse(text, pos);
            if (date != null) {
                return date;
            }
        }
        return null;
    }
}
