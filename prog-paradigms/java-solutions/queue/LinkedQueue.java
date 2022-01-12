package queue;

import java.util.ArrayList;
import java.util.Objects;

public class LinkedQueue extends AbstractQueue {

    private Node head = null, tail = null;

    private static class Node {
        private Node next;
        private Object value;

        public Node(Node next, Object value) {
            this.next = next;
            this.value = value;
        }
    }

    @Override
    protected void enqueueImpl(final Object e) {
        Node newTail = new Node(null, e);
        if (head == null) {
            head = tail = newTail;
        } else {
            tail.next = newTail;
            tail = tail.next;
        }
    }

    @Override
    public Object element() {
        assert size > 0;

        return head.value;
    }

    @Override
    protected Object dequeueImpl() {
        Object res = head.value;
        head = head.next;
        return res;
    }

    @Override
    public void clear() {
        head = tail = null;
        size = 0;
    }

    @Override
    public Object get(final int id) {
        assert size > 0;
        assert id >= 0;
        assert id < size;

        Node res = head;
        for (int i = 0; i < id; i++) {
            res = res.next;
        }
        return res.value;
    }

    @Override
    public void set(final int id, final Object item) {
        assert size > 0;
        assert id >= 0;
        assert id < size;
        Objects.requireNonNull(item);

        Node res = head;
        for (int i = 0; i < id; i++) {
            res = res.next;
        }
        res.value = item;
    }

    @Override
    public Object[] toArray() {
        ArrayList<Object> res = new ArrayList<>();
        Node node = head;

        for (int i = 0; i < size; i++) {
            res.add(node.value);
            node = node.next;
        }

        return res.toArray();
    }
}
