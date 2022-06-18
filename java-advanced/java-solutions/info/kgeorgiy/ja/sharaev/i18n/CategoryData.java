package info.kgeorgiy.ja.sharaev.i18n;

import java.util.Comparator;
import java.util.List;
import java.util.function.ToIntFunction;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * Data class for store collected data from {@link Parser}
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class CategoryData<V> {

    private final int countOccurrences;
    private final int countDifferentValues;

    private final V minValue;
    private final V maxValue;


    private final V minLengthValue;
    private final int minLengthValueLength;
    private final V maxLengthValue;
    private final int maxLengthValueLength;

    private final V averageValue;
    private final double averageLength;
    private final boolean haveLength;


    /**
     * create new instance
     *
     * @param items              collected items
     * @param distinctComparator comparator with a guarantee of consistent unique elements
     * @param calcLength         function for get length, maybe null
     * @param calcAverageValue   collector for calc average value
     * @param <A>                for collector accumulator
     */
    public <A> CategoryData(final List<V> items,
                            final Comparator<? super V> distinctComparator,
                            final ToIntFunction<? super V> calcLength,
                            final Collector<V, A, ? extends V> calcAverageValue) {
        this.countOccurrences = items.size();
        this.haveLength = (calcLength != null);
        if (countOccurrences == 0) {
            this.countDifferentValues = 0;
            this.minValue = null;
            this.maxValue = null;

            this.minLengthValue = null;
            this.minLengthValueLength = 0;
            this.maxLengthValue = null;
            this.maxLengthValueLength = 0;

            this.averageValue = null;
            this.averageLength = 0;
            return;
        }

        final List<V> sortedDistinctList = items.stream().sorted(distinctComparator).distinct().toList();
        this.countDifferentValues = sortedDistinctList.size();
        this.minValue = sortedDistinctList.get(0);
        this.maxValue = sortedDistinctList.get(countDifferentValues - 1);

        if (this.haveLength) {
            items.sort(Comparator.comparingInt(calcLength));
            this.minLengthValue = items.get(0);
            this.maxLengthValue = items.get(items.size() - 1);
            this.minLengthValueLength = calcLength.applyAsInt(minLengthValue);
            this.maxLengthValueLength = calcLength.applyAsInt(maxLengthValue);
            this.averageLength = items.stream().collect(Collectors.averagingDouble(calcLength::applyAsInt));
        } else {
            this.minLengthValue = null;
            this.maxLengthValue = null;
            this.minLengthValueLength = 0;
            this.maxLengthValueLength = 0;
            this.averageLength = 0;
        }

        this.averageValue = calcAverageValue != null ? items.stream().collect(calcAverageValue) : null;
    }

    public int getCountOccurrences() {
        return countOccurrences;
    }

    public int getCountDifferentValues() {
        return countDifferentValues;
    }

    public V getMinValue() {
        return minValue;
    }

    public V getMaxValue() {
        return maxValue;
    }

    public V getMinLengthValue() {
        return minLengthValue;
    }

    public int getMinLengthValueLength() {
        return minLengthValueLength;
    }

    public V getMaxLengthValue() {
        return maxLengthValue;
    }

    public int getMaxLengthValueLength() {
        return maxLengthValueLength;
    }

    public V getAverageValue() {
        return averageValue;
    }

    public double getAverageLength() {
        return averageLength;
    }

    public boolean hasLength() {
        return haveLength;
    }
}
