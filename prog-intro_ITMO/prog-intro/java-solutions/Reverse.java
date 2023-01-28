import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.NoSuchElementException;

class ReverseChecker implements ScannerLab.Checker {
    @Override
    public boolean check(int c) {
        return !Character.isWhitespace(c);
    }
}
public class Reverse {
    public static void main(String[] args) {
        ReverseChecker check = new ReverseChecker();
        List<int[]> intList = new ArrayList<>();
        try (ScannerLab scanLine = new ScannerLab(System.in,check)) {
            while (!scanLine.isEndOfInput()) {
                int[] row = new int[4];
                int ptr = 0;
                while (!scanLine.isEndOfLine()){
                    if (ptr == row.length) {
                        row = Arrays.copyOf(row, row.length * 2);
                    }
                    try {
                        row[ptr++] = Integer.parseInt(scanLine.next());
                    } catch (NumberFormatException e) {
                        System.err.printf("Couldn't convert to number: %s%n", e.getMessage());
                    } catch (NoSuchElementException e) {
                        System.err.println(e.getMessage());
                    }
                }
                scanLine.newLine();
                intList.add(Arrays.copyOf(row, ptr));
            }
            for (int i = intList.size() - 1; i >= 0; i--) {
                for (int j = intList.get(i).length - 1; j >= 0; j--) {
                    long ans = intList.get(i)[j];
                    System.out.printf("%d ", ans);
                }
                System.out.println();
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

    }
}