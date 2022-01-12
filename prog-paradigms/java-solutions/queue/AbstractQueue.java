package queue;

import java.util.Objects;

public abstract class AbstractQueue implements Queue {

    protected int size = 0;

    @Override
    public void enqueue(final Object e) {
        Objects.requireNonNull(e);
        size++;

        enqueueImpl(e);
    }

    protected abstract void enqueueImpl(final Object e);

    @Override
    public Object dequeue() {
        assert size > 0;
        size--;

        return dequeueImpl();
    }

    protected abstract Object dequeueImpl();

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public Object get(final int id) {
        assert size > 0;
        assert id >= 0;
        assert id < size;

        Object[] temp = toArray();
        return temp[id];
    }

    @Override
    public void set(final int id, final Object item) {
        assert size > 0;
        assert id >= 0;
        assert id < size;
        Objects.requireNonNull(item);

        for (int i = 0; i < size; i++) {
            Object temp = dequeue();
            if (i == id) {
                enqueue(item);
            } else {
                enqueue(temp);
            }
        }
    }

    @Override
    public Object[] toArray() {
        Object[] res = new Object[size];
        for (int i = 0; i < size; i++) {
            Object temp = dequeue();
            res[i] = temp;
            enqueue(temp);
        }
        return res;
    }
}