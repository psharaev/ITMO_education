package queue;

import java.util.Arrays;
import java.util.Objects;

public class ArrayQueueModule {

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

    private static int head = 0, size = 0;
    private static Object[] elements = new Object[2];

    // Pred: e != null
    // Post: n = n' + 1 && a[n] == e && forall i = 1..n': a[i] == a'[i]
    public static void enqueue(final Object e) {
        Objects.requireNonNull(e);

        ensureCapacity(++size);

        elements[(head + size - 1) % elements.length] = e;
    }

    // Pred: n > 0
    // Post: R == a[1] && Immutable
    public static Object element() {
        assert size > 0;

        return elements[head];
    }

    // Pred: n > 0
    // Post: R == a'[1] && n = n' - 1 && forall i = 1..n: a[i] == a'[i+1]
    public static Object dequeue() {
        assert size > 0;
        size--;

        Object result = elements[head];
        elements[head] = null;
        head = (head + 1) % elements.length;
        return result;
    }

    // Pred: true
    // Post: R == n && Immutable
    public static int size() {
        return size;
    }

    // Pred: true
    // Post: R == (n == 0) && Immutable
    public static boolean isEmpty() {
        return size == 0;
    }

    // Pred: true
    // Post: n == 0
    public static void clear() {
        Arrays.fill(elements, null);
        head = size = 0;
    }

    // Pred: n > 0 && 0 <= id < n
    // Post: R == a[id] && Immutable
    public static Object get(final int id) {
        assert size > 0;

        return elements[(head + id) % elements.length];
    }

    // Pred: n > 0 && 0 <= id < n && item != null
    // Post: a[id] == item && n == n' && forall i = 1..n and i != id: a[i] == a'[i]
    public static void set(final int id, final Object item) {
        assert size > 0;
        Objects.requireNonNull(item);

        elements[(head + id) % elements.length] = item;
    }

    public static Object[] toArray() {
        Object[] res = new Object[size];
        for (int i = 0; i < size; i++) {
            Object temp = dequeue();
            res[i] = temp;
            enqueue(temp);
        }
        return res;
    }

    private static void ensureCapacity(final int capacity) {
        if (capacity >= elements.length) {
            Object[] res = new Object[capacity * 2];
            int tail = (head + size - 1) % elements.length;
            if (head > tail) {
                System.arraycopy(elements, head, res, head, elements.length - head);
                System.arraycopy(elements, 0, res, elements.length, tail);
            } else {
                System.arraycopy(elements, head, res, head, tail - head);
            }
            elements = res;
        }
    }
}
