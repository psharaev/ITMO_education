package info.kgeorgiy.ja.sharaev.crawler;

import java.util.ArrayDeque;
import java.util.Queue;
import java.util.concurrent.ExecutorService;

/**
 * Implements the restriction of tasks from this class
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class LimitedExecutor {
    private final Queue<Runnable> waitingTasks;
    private final ExecutorService executorService;
    private int free;

    /**
     * Create new instance {@link LimitedExecutor}
     *
     * @param executorService - service for tasks
     * @param limitOnExecute  - limiting tasks from this instance
     */
    public LimitedExecutor(final ExecutorService executorService, final int limitOnExecute) {
        this.waitingTasks = new ArrayDeque<>();
        this.executorService = executorService;
        this.free = limitOnExecute;
    }

    private Runnable wrapTask(final Runnable task) {
        return () -> {
            task.run();
            submitNextTask();
        };
    }


    /**
     * Sends the task for execution, if the limit is exceeded, then puts it in the queue
     *
     * @param task - task for execute
     */
    public synchronized void submit(final Runnable task) {
        if (free > 0) {
            executorService.submit(wrapTask(task));
            free--;
        } else {
            waitingTasks.add(task);
        }
    }

    private synchronized void submitNextTask() {
        final Runnable task = waitingTasks.poll();
        if (task == null) {
            free++;
        } else {
            executorService.submit(wrapTask(task));
        }
    }
}
