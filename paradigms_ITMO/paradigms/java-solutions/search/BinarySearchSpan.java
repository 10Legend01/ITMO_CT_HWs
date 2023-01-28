package search;

import base.Pair;

public class BinarySearchSpan {

    // Pre: x && a[]: a.length >= 0 && forall i,j = 0..a.length-1: i < j: a[i] >= a[j]
    // Post: Z in range [0, a.length]:
    // b[] = copy a[] and -inf in the end: forall i,j = 0..a.length: i < j: b[i] >= b[j] && b.length > 0
    // Z = min i in range [0, a.length]: b[i] <= x
    static int binSearchSpanIterationLeft(final int x, final int[] a) {
        int l = -1, r = a.length;
        // l == -1 && r == a.length >= 0

        // Inv: -1 <= l < r <= a.length && a == a' && x == x'
        while (1 < r - l) {
            // 1 < r - l

            final int m = (l + r) / 2;
            // m = (l + r) / 2 && 1 < r - l && l < m < r

            if (x >= a[m]) {
                // x >= a[m] => forall i = m..r-1: x >= a[i]
                r = m;
                // r = m && l == l' && a == a' && x == x'
            } else {
                // a[m] > x => forall i = l+1..m: a[i] > x
                l = m;
                // l = m && r == r' && a == a' && x == x'
            }
            // -1 <= l < r <= a.length
        }
        // b[] = copy a[] and -inf in the end: forall i,j = 0..a.length: i < j: b[i] >= b[j] && b.length > 0
        // && a == a' && x == x' && 1 == r - l => l == r - 1 && (b[l] > x >= b[r])
        // return id(b[r]) => return r
        return r;
    }

    // Pre: x && a[]: a.length >= 0 && -1 <= l < r <= a.length 
    // && forall i,j = 0..a.length-1: i < j: a[i] >= a[j]
    // Post: Z in range [0, a.length]:
    // b[] = copy a[] and -inf in the end: forall i,j = 0..a.length: i < j: b[i] >= b[j] && b.length > 0
    // Z = min i in range [0, a.length]: b[i] < x
    static int binSearchRecursionRight(final int x, final int l, final int r, final int[] a) {
        // 1 <= r - l        
        if (1 == r - l) {
            // b[] = copy a[] and -inf in the end: forall i,j = 0..a.length: i < j: b[i] >= b[j] && b.length > 0
            // 1 == r - l  =>  l == r - 1 && b[l] >= x > b[r]
            // return id(b[r]) => return r
            return r;
        }
        // 1 < r - l

        final int m = (l + r) / 2;
        // m = (l + r) / 2 && 1 < r - l && l < m < r
        
        if (x > a[m]) {
            // a == a' && x == x' && r = m && l == l'
            // && l < r
            // x > a[m] => forall i = m..r-1: x > a[i]
            return binSearchRecursionRight(x, l, m, a);
        } else {
            // a == a' && x == x' && l = m && r == r' 
            // && l < r
            // a[m] >= x => forall i = l+1..m: a[i] >= x
            return binSearchRecursionRight(x, m, r, a);
        }
    }

    // Pre: x && a[]: a.length >= 0 && forall i,j = 0..a.length-1: i < j: a[i] >= a[j]
    // Post: Zl,L in range [0, a.length]:
    // b[] = copy a[] and -inf in the end: forall i,j = 0..a.length: i < j: b[i] >= b[j] && b.length > 0
    // Zl = min i in range [0, a.length]: b[i] <= x
    // Zr = min i in range [0, a.length]: b[i] < x
    // L = Zr - Zl
    static Pair<Integer, Integer> binSearchSpan(final int x, final int[] a) {
        // Pre: x && a[]: a.length >= 0
        // && forall i,j = 0..a.length-1: i < j: a[i] >= a[j]
        final int ll = binSearchSpanIterationLeft(x, a);
        // Post: Zl = ll

        // a == a' && x == x'
        // Pre: x && a[]: a.length >= 0 && l == -1 && r == a.length && l < r 
        // && forall i,j = 0..a.length-1: i < j: a[i] >= a[j]
        final int rr = binSearchRecursionRight(x, -1, a.length, a);
        // Post: Zr = rr

        // L = Zr - Zl
        // 1) L == 0 => Zl == Zr, a[Zl - 1] > x > a[Zl]
        // 2) L > 0 => a[Zl] == x, a[Zr - 1] == x
        return new Pair<>(ll, rr - ll);
    }

    // Pre: args[] - array of String can be converted to array of Int && args != null
    // && args.length > 0 && args[args.length] = -inf && forall i,j = 1..args.length-1: i < j: a[i] >= a[j]
    // Post: print Zl, L in range [0, args.length - 1]:
    // b[] = copy args[] and -inf in the end: forall i,j = 1..args.length: i < j: b[i] >= b[j] 
    // && b.length > 1
    // Zl = min i in range [0, args.length - 1]: b[i + 1] <= args[0]
    // Zr = min i in range [0, args.length - 1]: b[i + 1] < args[0]
    // L = Zr - Zl
    public static void main(final String[] args) {

        if (args.length == 0) {
            System.out.print("Please, input 'x' and try again.");
            return;
        }

        final int[] a = new int[args.length - 1];
        final int x = Integer.parseInt(args[0]);
        for (int i = 1 ; i < args.length ; i++) {
            a[i - 1] = Integer.parseInt(args[i]);
        }
        // x = args[0]
        // a[] = forall i = 0..args.length-2: a[i] = args[i+1]
        
        // Pre: x && a[]: a.length >= 0 && forall i,j = 0..a.length-1: i < j: a[i] >= a[j]
        final Pair<Integer, Integer> s = binSearchSpan(x, a);
        // Post: Pair <Zl, L>

        System.out.print(s.first);
        System.out.print(" ");
        System.out.print(s.second);
    }
}
