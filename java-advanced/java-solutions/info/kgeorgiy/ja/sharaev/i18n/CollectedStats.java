package info.kgeorgiy.ja.sharaev.i18n;

import java.nio.file.Path;
import java.util.Date;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public record CollectedStats(Path inputFIle,
                             CategoryData<String> sentences,
                             CategoryData<String> words,
                             CategoryData<Number> numbers,
                             CategoryData<Number> moneys,
                             CategoryData<Date> dates) {
}
