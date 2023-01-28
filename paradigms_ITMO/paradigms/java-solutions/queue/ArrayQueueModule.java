package queue;

// Inv: a[]: a.length >= 0
// && forall i = 0..a.length-1: a[i] != null
public class ArrayQueueModule {
    private static int start = 0;
    private static int size = 0;
    private static Object[] elements = new Object[1];

    // Post: elements.length = size' * 2 + 1
    // && start = 0 && size = size'
    // && forall i = 0..size'-1: elements[i] = elements[(start + i) % elements.length]'
    // && forall i = size'..elements.length-1: elements[i] = null
    private static void resize() {
        Object[] newElements = new Object[size * 2 + 1];
        if (start + size < elements.length)
            System.arraycopy(elements, start, newElements, 0, size());
        else {
            System.arraycopy(elements, start, newElements, 0, elements.length - start);
            System.arraycopy(elements, 0,
                    newElements, elements.length - start,
                    (start + size) % elements.length);
        }
        start = 0;
        elements = newElements;
    }

    // Pre: element != null
    // Post: a.length = a.length' + 1
    // && a[a.length'] = element
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static void enqueue(Object element) {
        elements[(start + size) % elements.length] = element;
        size++;
        if (size == elements.length)
            resize();
    }

    // Pre: a.length' > 0
    // Post: R = a[0]
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static Object element() {
        return elements[start];
    }

    // Pre: a.length' > 0
    // Post: R = a[0]
    // && a.length = a.length' - 1
    // && forall i = 0..a.length-1: a[i] = a[i + 1]'
    public static Object dequeue() {
        Object val = elements[start];
        elements[start] = null;
        start = (start + 1) % elements.length;
        size--;
        if (size < elements.length / 4)
            resize();
        return val;
    }

    // Post: R = a.length'
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static int size() {
        return size;
    }

    // Post: R = (a.length' == 0)
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static boolean isEmpty() {
        return size == 0;
    }

    // Post: a.length = 0
    public static void clear() {
        elements = new Object[1];
        start = 0;
        size = 0;
    }

    // Pre: element != null
    // Post: a.length = a.length' + 1
    // && a[0] = element
    // && forall i = 1..a.length': a[i] = a[i - 1]'
    public static void push(Object element) {
        if (start == 0)
            start = elements.length - 1;
        else
            start--;
        elements[start] = element;
        size++;
        if (size == elements.length)
            resize();
    }

    // Pre: a.length' > 0
    // Post: R = a[a.length' - 1]'
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static Object peek() {
        return elements[(start + size - 1) % elements.length];
    }

    // Pre: a.length' > 0
    // Post: R = a[a.length' - 1]'
    // && a.length = a.length' - 1
    // && forall i = 0..a.length-1: a[i] = a[i]'
    public static Object remove() {
        Object val = elements[(start + size - 1) % elements.length];
        elements[(start + size) % elements.length] = null;
        size--;
        if (size < elements.length / 4)
            resize();
        return val;
    }
}