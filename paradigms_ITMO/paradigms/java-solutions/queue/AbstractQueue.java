package queue;

public abstract class AbstractQueue implements Queue {
    protected int size = 0;

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public void clear() {
        while (size > 0) {
            dequeue();
        }
    }

    @Override
    public boolean contains(Object element) {
        return findElement(element, false);
    }

    @Override
    public boolean removeFirstOccurrence(Object element) {
        return findElement(element, true);
    }

    private boolean findElement(Object element, boolean removeFirstOccurrence) {
        boolean ans = false;
        int n = size;
        for (int i = 0; i < n; i++) {
            Object current = dequeue();
            if (!ans && element.equals(current)) {
                ans = true;
                if (removeFirstOccurrence) {
                    continue;
                }
            }
            enqueue(current);
        }
        return ans;
    }
}
