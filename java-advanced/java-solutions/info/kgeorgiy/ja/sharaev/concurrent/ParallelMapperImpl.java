package info.kgeorgiy.ja.sharaev.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.stream.IntStream;

/**
 * Implementation of ParallelMapper.
 * Complete map function parallel for any thread.
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class ParallelMapperImpl implements ParallelMapper {
    private final Queue<Runnable> tasks;
    private final List<Thread> workers;

    /**
     * Create new instance with threads size workers
     *
     * @param threads count workers
     */
    public ParallelMapperImpl(final int threads) {
        if (threads <= 0) {
            throw new IllegalArgumentException("Expected positive number of threads, actual: " + threads);
        }

        this.tasks = new ArrayDeque<>();

        final Runnable worker = () -> {
            try {
                while (!Thread.interrupted()) {
                    final Runnable task;
                    synchronized (tasks) {
                        while (tasks.isEmpty()) {
                            tasks.wait();
                        }
                        task = tasks.poll();
                    }
                    task.run();
                }
            } catch (final InterruptedException ignored) {

            } finally {
                Thread.currentThread().interrupt();
            }
        };

        this.workers = IntStream.range(0, threads)
                .mapToObj(i -> new Thread(worker))
                .peek(Thread::start)
                .toList();
    }

    /**
     * Maps function {@code f} over specified {@code args}.
     * Mapping for each element performs in parallel.
     *
     * @throws InterruptedException if calling thread was interrupted
     */
    @Override
    public <T, R> List<R> map(final Function<? super T, ? extends R> f, final List<? extends T> args) throws
            InterruptedException {
        final TaskController<T, R> controller = new TaskController<>(f, args);
        IntStream.range(0, args.size()).forEach(controller::addTask);
        return controller.getResult();
    }

    /**
     * Interrupts and joins all threads. All unfinished mappings leave in undefined state.
     */
    @Override
    public void close() {
        workers.forEach(Thread::interrupt);

        final boolean[] hasInterrupted = new boolean[]{false};
        workers.forEach(thread -> {
            while (true) {
                try {
                    thread.join();
                    break;
                } catch (final InterruptedException ignored) {
                    hasInterrupted[0] = true;
                }
            }
        });

        if (hasInterrupted[0]) {
            Thread.currentThread().interrupt();
        }
    }

    private class TaskController<T, R> {
        private int count;
        private final List<R> res;
        private RuntimeException exception;
        private final Function<? super T, ? extends R> f;
        private final List<? extends T> args;

        private TaskController(final Function<? super T, ? extends R> f, final List<? extends T> args) {
            this.f = f;
            this.args = args;
            this.count = args.size();
            this.res = new ArrayList<>(Collections.nCopies(count, null));
        }

        private Runnable getTaskRunnable(final int index) {
            return () -> {
                R result = null;
                RuntimeException receivedException = null;
                try {
                    result = f.apply(args.get(index));
                } catch (final RuntimeException e) {
                    receivedException = e;
                }
                synchronized (this) {
                    res.set(index, result);
                    if (receivedException != null) {
                        addException(receivedException);
                    }
                    if (--count == 0) {
                        notify();
                    }
                }
            };
        }

        private void addException(final RuntimeException e) {
            if (exception == null) {
                exception = e;
            } else {
                exception.addSuppressed(e);
            }
        }

        public void addTask(final int index) {
            final Runnable runnable = getTaskRunnable(index);
            synchronized (tasks) {
                tasks.add(runnable);
                tasks.notify();
            }
        }

        public synchronized List<R> getResult() throws InterruptedException {
            while (count != 0) {
                wait();
            }
            if (exception != null) {
                throw exception;
            }
            return res;
        }
    }
}
