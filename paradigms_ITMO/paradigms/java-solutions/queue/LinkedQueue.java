package queue;

public class LinkedQueue extends AbstractQueue {
    private Node start = null;
    private Node end = null;

    @Override
    public void enqueue(Object element) {
        if (end == null) {
            end = start = new Node(element, null);
        } else {
            end.setNext(new Node(element, null));
            end = end.getNext();
        }
        size++;
    }

    @Override
    public Object element() {
        return start.getElement();
    }

    @Override
    public Object dequeue() {
        Object val = start.getElement();
        start = start.getNext();
        size--;
        if (size == 0) {
            end = null;
        }
        return val;
    }
}
