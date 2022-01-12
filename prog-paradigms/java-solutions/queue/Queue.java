package queue;


import java.util.Objects;

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
public interface Queue {

    // Pred: e != null
    // Post: n = n' + 1 && a[n] == e && forall i = 1..n': a[i] == a'[i]
    void enqueue(Object e);

    // Pred: n > 0
    // Post: R == a[1] && Immutable
    Object element();

    // Pred: n > 0
    // Post: R == a'[1] && n = n' - 1 && forall i = 1..n: a[i] == a'[i+1]
    Object dequeue();

    // Pred: true
    // Post: R == n && Immutable
    int size();

    // Pred: true
    // Post: R == (n == 0) && Immutable
    boolean isEmpty();

    // Pred: true
    // Post: n == 0
    void clear();

    // Pred: n > 0 && 1 <= id + 1 <= n
    // Post: R == a[id + 1] && Immutable
    Object get(final int id);

    // Pred: n > 0 && 1 <= id + 1 <= n
    // Post: R == a[id + 1] && Immutable
    void set(final int id, final Object item);

    // Pred: true
    // Post: R == Model
    Object[] toArray();
}
