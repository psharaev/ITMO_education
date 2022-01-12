package queue;

import java.util.Arrays;
import java.util.Objects;

public class ArrayQueue extends AbstractQueue {

    private int head = 0;
    private Object[] elements = new Object[2];

    @Override
    protected void enqueueImpl(final Object e) {
        ensureCapacity(size);
        elements[(head + size - 1) % elements.length] = e;
    }

    @Override
    public Object element() {
        assert size > 0;

        return elements[head];
    }

    @Override
    public Object dequeueImpl() {
        Object result = elements[head];
        elements[head] = null;
        head = (head + 1) % elements.length;
        return result;
    }

    @Override
    public void clear() {
        Arrays.fill(elements, null);
        head = size = 0;
    }

    @Override
    public Object get(final int id) {
        assert size > 0;
        assert id >= 0;
        assert id < size;


        return elements[(head + id) % elements.length];
    }

    @Override
    public void set(final int id, final Object item) {
        assert size > 0;
        assert id >= 0;
        assert id < size;
        Objects.requireNonNull(item);

        elements[(head + id) % elements.length] = item;
    }

    @Override
    public Object[] toArray() {
        Object[] res = new Object[size];
        int tail = (head + size) % elements.length;
        if (head > tail) {
            System.arraycopy(elements, head, res, 0, elements.length - head);
            System.arraycopy(elements, 0, res, elements.length - head, tail);
        } else {
            System.arraycopy(elements, head, res, 0, tail - head);
        }
        return res;
    }

    private void ensureCapacity(final int capacity) {
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
