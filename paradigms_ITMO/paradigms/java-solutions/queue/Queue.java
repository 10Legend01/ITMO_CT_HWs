package queue;

// Inv: a[]: a.length >= 0
// && forall i = 0..a.length-1: a[i] != null
public interface Queue {

    // Pre: element != null
    // Post: a.length = a.length' + 1
    // && a[a.length'] = element
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    void enqueue(Object element);

    // Pre: a.length' > 0
    // Post: R = a[0]
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    Object element();

    // Pre: a.length' > 0
    // Post: R = a[0]
    // && a.length = a.length' - 1
    // && forall i = 0..a.length-1: a[i] = a[i + 1]'
    Object dequeue();

    // Post: R = a.length'
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    int size();

    // Post: R = (a.length' == 0)
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    boolean isEmpty();

    // Post: a.length = 0
    void clear();

    // Pre: element != null
    // Post: R = (exists x: a[x]' == element)
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    boolean contains(Object element);

    // Pre: element != null
    // Post: R = (exists x: a[x]' == element) &&
    // 1) exists x = min i in range [0, a.length'): a[i]' == element =>
    // a.length = a.length' - 1 && forall i = 0..x-1: a[i] = a[i]' && forall i = x..a.length-1: a[i] = a[i+1]'
    // 2) a.length = a.length' && forall i = 0..a.length'-1: a[i] = a[i]'
    boolean removeFirstOccurrence(Object element);
}
