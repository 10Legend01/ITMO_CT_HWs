import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.NoSuchElementException;

class ReverseHexAbcChecker implements ScannerLab.Checker {
    @Override
    public boolean check(int c) {
        return !Character.isWhitespace(c);
    }
}
public class ReverseHexAbc {
    public static void main(String[] args) {
        ReverseHexAbcChecker check = new ReverseHexAbcChecker();
        List<int[]> ints = new ArrayList<>();

        try (ScannerLab ScanLine = new ScannerLab(System.in, check)) {
            while (!ScanLine.isEndOfInput()) {
                int[] row = new int[4];
                int ptr = 0;
                while (!ScanLine.isEndOfLine()) {
                    String str = ScanLine.next();
                    try {
                        int zn;
                        if (str.length() >= 2 && str.charAt(0) == '0' && (str.charAt(1) == 'x' || str.charAt(1) == 'X')) {
                            zn = Integer.parseUnsignedInt(str.substring(2), 16);
                        } else if (Character.toLowerCase(str.charAt(0)) >= 'a' && Character.toLowerCase(str.charAt(0)) <= 'j'
                                || str.charAt(0) == '-' && Character.toLowerCase(str.charAt(1)) >= 'a' && Character.toLowerCase(str.charAt(1)) <= 'j') {
                            StringBuilder x = new StringBuilder();
                            for (int i : str.toCharArray()) {
                                if (i >= 'a' && i <= 'j') {
                                    x.append((char) (i - 49));
                                } else {
                                    x.append((char) i);
                                }
                            }
                            zn = Integer.parseInt(new String(x));
                        } else {
                            zn = Integer.parseInt(str);
                        }

                        if (ptr == row.length) {
                            row = Arrays.copyOf(row, row.length * 2);
                        }
                        row[ptr++] = zn;
                    } catch (NumberFormatException e) {
                        System.err.printf("Couldn't convert to number: %s%n", e.getMessage());
                    } catch (NoSuchElementException e) {
                        System.err.println(e.getMessage());
                    }
                }
                ScanLine.newLine();
                ints.add(Arrays.copyOf(row, ptr));
            }

            for (int i = ints.size() - 1; i >= 0; i--) {
                for (int j = ints.get(i).length - 1; j >= 0; j--) {
                    long ans = ints.get(i)[j];
                    System.out.printf("%d ", ans);
                }
                System.out.println();
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

    }
}