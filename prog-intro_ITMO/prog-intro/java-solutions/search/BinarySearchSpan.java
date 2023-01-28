package search;

import java.util.Arrays;
import base.Pair;

public class BinarySearchSpan {

    static int binSearchRecursionLeft(int l, int r, int[] a) {
        if (r - l == 1) {
            return l;
        }

        int m = (l + r) / 2;

        if (a[0] >= a[m]) {
            return binSearchRecursionLeft(l, m, a);
        } else {
            return binSearchRecursionLeft(m, r, a);
        }
    }

    static int binSearchRecursionRight(int l, int r, int[] a) {
        if (r - l == 1) {
            return l;
        }

        int m = (l + r) / 2;

        if (a[0] > a[m]) {
            return binSearchRecursionRight(l, m, a);
        } else {
            return binSearchRecursionRight(m, r, a);
        }
    }

    static Pair binSearchSpanRecursion(int l, int r, int[] a) {
        int ll = binSearchRecursionLeft(0, a.length, a);
        return new Pair(ll, binSearchRecursionRight(0, a.length, a) - ll);
    }

    static Pair binSearchSpanIteration(int[] a) {

        int l = 0, r = a.length;

        // left side - ____>|||___
        while (1 < r - l) {
            int m = (l + r) / 2;
            if (a[0] >= a[m]) {
                r = m;
            } else {
                l = m;
            }
        }

        if (r == a.length || a[r] != a[0]) {
            // r - 1 == l
            return new Pair(l, 0);
        }

        int ll = l;

        l = 0; r = a.length;

        // right side - ___|||<___
        while (1 < r - l) {
            int m = (l + r) / 2;
            if (a[0] > a[m]) {
                r = m;
            } else {
                l = m;
            }
        }
        return new Pair(ll, l - ll);
    }

    public static void main(String[] args) {
        int[] a = Arrays.stream(args).mapToInt(Integer::parseInt).toArray();

        Pair s = binSearchSpanIteration(a);
        //Pair s = binSearchSpanRecursion(0, a.length, a);

        System.out.print(s.first);
        System.out.print(" ");
        System.out.print(s.second);
    }
}
