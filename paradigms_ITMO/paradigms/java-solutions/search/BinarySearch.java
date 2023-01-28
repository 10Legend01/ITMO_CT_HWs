package search;

import java.util.Arrays;

public class BinarySearch {

    // a[1..a.length + 1) = a[0..a.length)
    // a[0] = x
    // any i = 1..a.length: a[i] == constant
    // Pre: any i, j in [1, a.length + 1) && i < j: a[i] >= a[j]
    // && l == 0 && r >= 1
    // Post: Z in [0, a.length) && Z = min i in [0, a.length): a[i + 1] <= x
    static int binSearchRecursion(int l, int r, int[] a) {
        if (r - l == 1) {
            // r - l == 1 => r - 1 == l
            return l;
        }

        // x in [l, r) && 1 < r - l

        int m = (l + r) / 2;

        if (a[0] >= a[m]) {
            return binSearchRecursion(l, m, a);
        } else {
            return binSearchRecursion(m, r, a);
        }
    }

    // a[1..a.length + 1) = a[0..a.length)
    // a[0] = x
    // any i = 1..a.length: a[i] == constant
    // Pre: any i, j in [1, a.length + 1) && i < j: a[i] >= a[j]
    // Post: Z in [0, a.length) && Z = min i in [0, a.length): a[i + 1] <= x
    static int binSearchIteration(int[] a) {

        int l = 0, r = a.length;
        // x in [l, r) && 1 <= r - l

        while (1 < r - l) {
            int m = (l + r) / 2;
            if (a[0] >= a[m]) {
                // a[m] <= x => any i in [l,m) a[i] <= x
                r = m;
            } else {
                l = m;
            }
        }
        // r - l == 1 => r - 1 == l
        return l;
    }

    public static void main(String[] args) {
        int[] a = Arrays.stream(args).mapToInt(Integer::parseInt).toArray();

        //int s = binSearchIteration(a);
        int s = binSearchRecursion(0, a.length, a);

        System.out.println(s);
    }
}
