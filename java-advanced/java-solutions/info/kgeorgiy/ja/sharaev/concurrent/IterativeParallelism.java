package info.kgeorgiy.ja.sharaev.concurrent;

import info.kgeorgiy.java.advanced.concurrent.AdvancedIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Implementation of parallel computing
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class IterativeParallelism implements AdvancedIP {

    private final ParallelMapper mapper;

    /**
     * Create new instance. To calculate functions, a new one will be created each time threads pool
     */
    public IterativeParallelism() {
        this.mapper = null;
    }

    /**
     * Create new instance with {@link ParallelMapper}. To calculate functions will be used mapper
     *
     * @param mapper mapper for calculate functions
     */
    public IterativeParallelism(final ParallelMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * Returns maximum value. If values is empty return {@link IllegalArgumentException}
     *
     * @param threads    number or concurrent threads.
     * @param values     values to get maximum of.
     * @param comparator value comparator.
     * @param <T>        value type.
     * @return maximum of given values
     * @throws InterruptedException   if executing thread was interrupted.
     * @throws NoSuchElementException if no values are given.
     */
    @Override
    public <T> T maximum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator)
            throws InterruptedException {
        if (values.isEmpty()) {
            throw new NoSuchElementException("values is empty");
        }
        return parallelCalc(threads, values, s -> s.max(comparator).orElseThrow(), s -> s.max(comparator).orElseThrow());
    }

    /**
     * Returns minimum value.
     *
     * @param threads    number or concurrent threads.
     * @param values     values to get minimum of.
     * @param comparator value comparator.
     * @param <T>        value type.
     * @return minimum of given values
     * @throws InterruptedException   if executing thread was interrupted.
     * @throws NoSuchElementException if no values are given.
     */
    @Override
    public <T> T minimum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator)
            throws InterruptedException {
        return maximum(threads, values, comparator.reversed());
    }

    /**
     * Returns whether all values satisfies predicate.
     *
     * @param threads   number or concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @param <T>       value type.
     * @return whether all values satisfies predicate or {@code true}, if no values are given
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> boolean all(final int threads, final List<? extends T> values, final Predicate<? super T> predicate)
            throws InterruptedException {
        return parallelCalc(threads, values, s -> s.allMatch(predicate), s -> s.allMatch(Boolean::booleanValue));
    }

    /**
     * Returns whether any of values satisfies predicate.
     *
     * @param threads   number or concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @param <T>       value type.
     * @return whether any value satisfies predicate or {@code false}, if no values are given
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> boolean any(final int threads, final List<? extends T> values, final Predicate<? super T> predicate)
            throws InterruptedException {
        return !all(threads, values, predicate.negate());
    }

    /**
     * Join values to string.
     *
     * @param threads number of concurrent threads.
     * @param values  values to join.
     * @return list of joined result of {@link #toString()} call on each value.
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public String join(final int threads, final List<?> values) throws InterruptedException {
        return parallelCalc(threads, values, s -> s.map(Objects::toString).collect(Collectors.joining()),
                s -> s.collect(Collectors.joining()));
    }

    /**
     * Filters values by predicate.
     *
     * @param threads   number of concurrent threads.
     * @param values    values to filter.
     * @param predicate filter predicate.
     * @return list of values satisfying given predicated. Order of values is preserved.
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> List<T> filter(final int threads, final List<? extends T> values, final Predicate<? super T> predicate)
            throws InterruptedException {
        return parallelCalc(threads, values, s -> s.filter(predicate).collect(Collectors.toList()),
                s -> s.flatMap(Collection::stream).collect(Collectors.toList()));
    }

    /**
     * Maps values.
     *
     * @param threads number of concurrent threads.
     * @param values  values to filter.
     * @param f       mapper function.
     * @return list of values mapped by given function.
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T, U> List<U> map(final int threads, final List<? extends T> values, final Function<? super T, ? extends U> f)
            throws InterruptedException {
        return parallelCalc(threads, values, s -> s.map(f).collect(Collectors.toList()),
                s -> s.flatMap(Collection::stream).collect(Collectors.toList()));
    }

    /**
     * Reduces values using monoid.
     *
     * @param threads number of concurrent threads.
     * @param values  values to reduce.
     * @param monoid  monoid to use.
     * @return values reduced by provided monoid or {@link Monoid#getIdentity() identity} if not values specified.
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> T reduce(final int threads, final List<T> values, final Monoid<T> monoid) throws InterruptedException {
        final BinaryOperator<T> op = monoid.getOperator();
        final Function<Stream<T>, T> reduceFunc = s -> s.reduce(monoid.getIdentity(), op);
        return parallelCalc(threads, values, reduceFunc, reduceFunc);
    }

    /**
     * Maps and reduces values using monoid.
     *
     * @param threads number of concurrent threads.
     * @param values  values to reduce.
     * @param lift    mapping function.
     * @param monoid  monoid to use.
     * @return values reduced by provided monoid or {@link Monoid#getIdentity() identity} if not values specified.
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T, R> R mapReduce(final int threads, final List<T> values, final Function<T, R> lift, final Monoid<R> monoid)
            throws InterruptedException {
        final BinaryOperator<R> op = monoid.getOperator();
        final R identity = monoid.getIdentity();
        return parallelCalc(threads, values, s -> s.map(lift).reduce(identity, op),
                s -> s.reduce(identity, op));
    }

    private static <T> List<Stream<T>> splitTasks(final int threads, final List<T> values) {
        final int countThreads = Math.min(threads, values.size());

        if (countThreads == 0) {
            return List.of(values.stream());
        }

        final int pieceTask = values.size() / countThreads;
        final int modTask = values.size() % countThreads;
        int shift = 0;

        final List<Stream<T>> tasks = new ArrayList<>(countThreads);
        for (int i = 0; i < countThreads; i++) {
            final int leftBorder = shift + i * pieceTask;
            if (i < modTask) {
                shift++;
            }
            final int rightBorder = shift + (i + 1) * pieceTask;
            tasks.add(values.subList(leftBorder, rightBorder).stream());
        }

        return tasks;
    }

    private <T, R> R parallelCalc(final int threads, final List<T> values,
                                  final Function<Stream<T>, R> handler,
                                  final Function<Stream<R>, R> collector) throws InterruptedException {
        if (threads <= 0) {
            throw new IllegalArgumentException("Expected positive number of threads, actual: " + threads);
        }

        final List<Stream<T>> tasks = splitTasks(threads, values);
        final int countThreads = tasks.size();

        if (mapper == null && countThreads == 1) {
            return handler.apply(tasks.get(0));
        }

        final List<R> result = mapper != null ? mapper.map(handler, tasks) : map(handler, tasks, countThreads);
        return collector.apply(result.stream());
    }

    private static <T, R> List<R> map(
            final Function<Stream<T>, R> handler,
            final List<Stream<T>> tasks,
            final int countThreads
    ) throws InterruptedException {
        final List<R> result;
        result = new ArrayList<>(Collections.nCopies(countThreads, null));
        final List<Thread> poolThreads = new ArrayList<>(countThreads);

        IntStream.range(0, countThreads).forEach(index -> {
            final Thread thread = new Thread(() -> {
                final R funcResult = handler.apply(tasks.get(index));
                synchronized (result) {
                    result.set(index, funcResult);
                }
            });
            poolThreads.add(thread);
            thread.start();
        });

        joinThreads(poolThreads);
        return result;
    }

    private static void joinThreads(final List<Thread> threads) throws InterruptedException {
        InterruptedException exception = null;
        for (int i = 0; i < threads.size(); i++) {
            try {
                threads.get(i).join();
            } catch (final InterruptedException e) {
                if (exception == null) {
                    exception = new InterruptedException("Thread was interrupted");
                    for (int j = i; j < threads.size(); j++) {
                        if (!threads.get(j).isInterrupted()) {
                            threads.get(j).interrupt();
                        }
                    }
                }
                exception.addSuppressed(e);
                i--;
            }
        }
        if (exception != null) {
            throw exception;
        }
    }
}
