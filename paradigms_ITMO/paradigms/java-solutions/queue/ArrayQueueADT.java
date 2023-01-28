package queue;

// Inv: a[]: a.length >= 0
// && forall i = 0..a.length-1: a[i] != null
public class ArrayQueueADT {
    private int start = 0;
    private int size = 0;
    private Object[] elements;

    public ArrayQueueADT() {
        elements = new Object[1];
    }

    // Pre: queue != null
    // Post: elements.length = size' * 2 + 1
    // && start = 0 && size = size'
    // && forall i = 0..size'-1: elements[i] = elements[(start + i) % elements.length]'
    // && forall i = size'..elements.length-1: elements[i] = null
    private static void resize(ArrayQueueADT queue) {
        Object[] newElements = new Object[size(queue) * 2 + 1];
        if (queue.start + queue.size < queue.elements.length)
            System.arraycopy(queue.elements, queue.start, newElements, 0, size(queue));
        else {
            System.arraycopy(queue.elements, queue.start, newElements, 0, queue.elements.length - queue.start);
            System.arraycopy(queue.elements, 0,
                    newElements, queue.elements.length - queue.start,
                    (queue.start + queue.size) % queue.elements.length);
        }
        queue.start = 0;
        queue.elements = newElements;
    }

    // Pre: queue != null && element != null
    // Post: a.length = a.length' + 1
    // && a[a.length'] = element
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static void enqueue(ArrayQueueADT queue, Object element) {
        queue.elements[(queue.start + queue.size) % queue.elements.length] = element;
        queue.size++;
        if (queue.size == queue.elements.length)
            resize(queue);
    }

    // Pre: queue != null && a.length' > 0
    // Post: R = a[0]
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static Object element(ArrayQueueADT queue) {
        return queue.elements[queue.start];
    }

    // Pre: queue != null && a.length' > 0
    // Post: R = a[0]
    // && a.length = a.length' - 1
    // && forall i = 0..a.length-1: a[i] = a[i + 1]'
    public static Object dequeue(ArrayQueueADT queue) {
        Object val = queue.elements[queue.start];
        queue.elements[queue.start] = null;
        queue.start = (queue.start + 1) % queue.elements.length;
        queue.size--;
        if (size(queue) < queue.elements.length / 4)
            resize(queue);
        return val;
    }

    // Pre: queue != null
    // Post: R = a.length'
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    // Pre: queue != null
    // Post: R = (a.length' == 0)
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    // Pre: queue != null
    // Post: a.length = 0
    public static void clear(ArrayQueueADT queue) {
        queue.elements = new Object[1];
        queue.start = 0;
        queue.size = 0;
    }

    // Pre: queue != null && element != null
    // Post: a.length = a.length' + 1
    // && a[0] = element
    // && forall i = 1..a.length': a[i] = a[i - 1]'
    public static void push(ArrayQueueADT queue, Object element) {
        if (queue.start == 0)
            queue.start = queue.elements.length - 1;
        else
            queue.start--;
        queue.elements[queue.start] = element;
        queue.size++;
        if (queue.size == queue.elements.length)
            resize(queue);
    }

    // Pre: queue != null && a.length' > 0
    // Post: R = a[a.length' - 1]'
    // && a.length = a.length'
    // && forall i = 0..a.length'-1: a[i] = a[i]'
    public static Object peek(ArrayQueueADT queue) {
        return queue.elements[(queue.start + queue.size - 1) % queue.elements.length];
    }

    // Pre: queue != null && a.length' > 0
    // Post: R = a[a.length' - 1]'
    // && a.length = a.length' - 1
    // && forall i = 0..a.length-1: a[i] = a[i]'
    public static Object remove(ArrayQueueADT queue) {
        Object val = queue.elements[(queue.start + queue.size - 1) % queue.elements.length];
        queue.elements[(queue.start + queue.size) % queue.elements.length] = null;
        queue.size--;
        if (queue.size < queue.elements.length / 4)
            resize(queue);
        return val;
    }
}
