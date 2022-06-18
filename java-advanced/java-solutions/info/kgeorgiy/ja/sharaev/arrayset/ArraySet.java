package info.kgeorgiy.ja.sharaev.arrayset;

import java.util.*;

public class ArraySet<T> extends AbstractSet<T> implements SortedSet<T> {

    private final List<T> data;
    private final Comparator<? super T> comparator;

    public ArraySet(Collection<? extends T> data, Comparator<? super T> comparator) {
        this.comparator = comparator;
        if (data.isEmpty()) {
            this.data = Collections.emptyList();
        } else {
            TreeSet<T> tree = new TreeSet<>(comparator);
            tree.addAll(data);
            this.data = List.copyOf(tree);
        }
    }

    public ArraySet() {
        this(Collections.emptyList(), null);
    }

    public ArraySet(Comparator<? super T> comparator) {
        this(Collections.emptyList(), comparator);
    }

    public ArraySet(Collection<? extends T> elements) {
        this(elements, null);
    }

    private ArraySet(List<T> data, Comparator<? super T> comparator) {
        this.data = data;
        this.comparator = comparator;
    }

    @Override
    public Comparator<? super T> comparator() {
        return comparator;
    }

    @Override
    public SortedSet<T> subSet(T fromElement, T toElement) {
        if (compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException("toElement must be greater or equal fromElement");
        }
        return new ArraySet<>(data.subList(indexOf(fromElement), indexOf(toElement)), comparator);
    }

    @Override
    public SortedSet<T> headSet(T toElement) {
        return new ArraySet<>(data.subList(0, indexOf(toElement)), comparator);
    }

    @Override
    public SortedSet<T> tailSet(T fromElement) {
        return new ArraySet<>(data.subList(indexOf(fromElement), size()), comparator);
    }

    @Override
    public T first() {
        if (data.isEmpty()) {
            throw new NoSuchElementException();
        }
        return data.get(0);
    }

    @Override
    public T last() {
        if (data.isEmpty()) {
            throw new NoSuchElementException();
        }
        return data.get(data.size() - 1);
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        Objects.requireNonNull(o);
        return Collections.binarySearch(data, (T) o, comparator) >= 0;
    }

    @Override
    public Iterator<T> iterator() {
        return data.iterator();
    }

    @Override
    public int size() {
        return data.size();
    }

    @SuppressWarnings("unchecked")
    private int compare(T a, T b) {
        if (comparator == null) {
            return ((Comparable<? super T>) a).compareTo(b);
        }

        return comparator.compare(a, b);
    }

    private int indexOf(T e) {
        int index = Collections.binarySearch(data, e, comparator);
        return index >= 0 ? index : -1 - index;
    }
}
