package queue;

public class ArrayQueue extends AbstractQueue {
    private int start = 0;
    private Object[] elements = new Object[1];

    public void push(Object element) {
        if (start == 0)
            start = elements.length - 1;
        else
            start--;
        elements[start] = element;
        size++;
        if (size == elements.length)
            resize();
    }

    public Object peek() {
        return elements[(start + size - 1) % elements.length];
    }

    public Object remove() {
        Object val = elements[(start + size - 1) % elements.length];
        elements[(start + size) % elements.length] = null;
        size--;
        return val;
    }

    @Override
    public void enqueue(Object element) {
        elements[(start + size) % elements.length] = element;
        size++;
        if (size == elements.length) {
            resize();
        }
    }

    @Override
    public Object element() {
        return elements[start];
    }

    @Override
    public Object dequeue() {
        Object val = elements[start];
        elements[start] = null;
        start = (start + 1) % elements.length;
        size--;
        return val;
    }

    private void resize() {
        Object[] newElements = new Object[size * 2 + 1];
        if (start + size < elements.length) {
            System.arraycopy(elements, start, newElements, 0, size());
        } else {
            System.arraycopy(elements, start, newElements, 0, elements.length - start);
            System.arraycopy(elements, 0,
                    newElements, elements.length - start,
                    (start + size) % elements.length);
        }
        start = 0;
        elements = newElements;
    }
}
