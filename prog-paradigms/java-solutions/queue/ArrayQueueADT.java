package queue;

import java.util.Arrays;
import java.util.Objects;

public class ArrayQueueADT {

    /*
        enqueue – добавить элемент в очередь;
        element – первый элемент в очереди;
        dequeue – удалить и вернуть первый элемент в очереди;
        size – текущий размер очереди;
        isEmpty – является ли очередь пустой;
        clear – удалить все элементы из очереди.

    без ' - новые значения
    с ' - старые значения

    Model:
        [a1, a2, ... , an]
        n -- размер очереди

    Inv:
        n >= 0
        forall i = 1..n: a[i] != null

    Immutable: n == n' && forall i = 1..n: a[i] == a'[i]

     */

    private int head = 0, size = 0;
    private Object[] elements = new Object[2];

    // Pred: e != null && queue != null
    // Post: n = n' + 1 && a[n] == e && forall i = 1..n': a[i] == a'[i]
    public static void enqueue(final ArrayQueueADT queue, final Object e) {
        Objects.requireNonNull(queue);
        Objects.requireNonNull(e);

        ensureCapacity(queue, ++queue.size);

        queue.elements[(queue.head + queue.size - 1) % queue.elements.length] = e;
    }

    // Pred: n > 0 && queue != null
    // Post: R == a[1] && Immutable
    public static Object element(final ArrayQueueADT queue) {
        Objects.requireNonNull(queue);
        assert size(queue) > 0;

        return queue.elements[queue.head];
    }

    // Pred: n > 0 && queue != null
    // Post: R == a'[1] && n = n' - 1 && forall i = 1..n: a[i] == a'[i+1]
    public static Object dequeue(final ArrayQueueADT queue) {
        Objects.requireNonNull(queue);
        assert size(queue) > 0;
        queue.size--;

        Object result = queue.elements[queue.head];
        queue.elements[queue.head] = null;
        queue.head = (queue.head + 1) % queue.elements.length;
        return result;
    }

    // Pred: queue != null
    // Post: R == n && Immutable
    public static int size(final ArrayQueueADT queue) {
        Objects.requireNonNull(queue);

        return queue.size;
    }

    // Pred: queue != null
    // Post: R == (n == 0) && Immutable
    public static boolean isEmpty(final ArrayQueueADT queue) {
        Objects.requireNonNull(queue);

        return queue.size == 0;
    }

    // Pred: queue != null
    // Post: n == 0
    public static void clear(final ArrayQueueADT queue) {
        Objects.requireNonNull(queue);

        Arrays.fill(queue.elements, null);
        queue.head = queue.size = 0;
    }

    // Pred: n > 0 && 0 <= id < n && queue != null
    // Post: R == a[id] && Immutable
    public static Object get(final ArrayQueueADT queue, final int id) {
        Objects.requireNonNull(queue);
        assert size(queue) > 0;

        return queue.elements[(queue.head + id) % queue.elements.length];
    }

    // Pred: n > 0 && 0 <= id < n && item != null && queue != null
    // Post: a[id] == item && n == n' && forall i = 1..n and i != id: a[i] == a'[i]
    public static void set(final ArrayQueueADT queue, final int id, final Object item) {
        Objects.requireNonNull(queue);
        assert size(queue) > 0;
        Objects.requireNonNull(item);

        queue.elements[(queue.head + id) % queue.elements.length] = item;
    }

    public static Object[] toArray(final ArrayQueueADT queue) {
        Object[] res = new Object[queue.size];
        for (int i = 0; i < queue.size; i++) {
            Object temp = dequeue(queue);
            res[i] = temp;
            enqueue(queue, temp);
        }
        return res;
    }

    private static void ensureCapacity(final ArrayQueueADT queue, final int capacity) {
        Objects.requireNonNull(queue);

        if (capacity >= queue.elements.length) {
            Object[] res = new Object[capacity * 2];
            int tail = (queue.head + queue.size - 1) % queue.elements.length;
            
            if (queue.head > tail) {
                System.arraycopy(queue.elements, queue.head, res, queue.head, queue.elements.length - queue.head);
                System.arraycopy(queue.elements, 0, res, queue.elements.length, tail);
            } else {
                System.arraycopy(queue.elements, queue.head, res, queue.head, tail - queue.head);
            }

            queue.elements = res;
        }
    }
}
