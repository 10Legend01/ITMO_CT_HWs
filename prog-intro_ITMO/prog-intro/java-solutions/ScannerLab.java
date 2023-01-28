import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;
import java.io.*;
public class ScannerLab implements AutoCloseable {

    private BufferedReader read;
    private int last = -2;
    private Checker check;

    public ScannerLab (String input, Checker check) throws IOException {
        this(new ByteArrayInputStream(input.getBytes()), "UTF-8", check);
    }

    public ScannerLab (String input, String charset, Checker check) throws IOException {
        this(new ByteArrayInputStream(input.getBytes()), charset, check);
    }

    public ScannerLab (File file, Checker check) throws IOException {
        this(new FileInputStream(file), "UTF-8", check);
    }

    public ScannerLab (InputStream input, Checker check) throws IOException {
        this(input, "UTF-8", check);
    }

    public ScannerLab (File file, String charset, Checker check) throws IOException {
        this(new FileInputStream(file), charset, check);
    }

    public ScannerLab (InputStream input, String charset, Checker check) throws IOException {
        read = new BufferedReader(new InputStreamReader(input, charset));
        this.check = check;
    }

    public ScannerLab (Checker check) {
        read = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
        this.check = check;
    }


    private static boolean endChecker(int c) {
        return c == -1;
    }


    private boolean hasNextPrivate(Checker checker) throws NoSuchElementException, IOException {
        if (checker.check(last)) {
            return true;
        } else {
            last = -2;
        }
        int c = read.read();
        while (!checker.check(c) && c != -1) {
            c = read.read();
        }
        last = c;
        return !endChecker(c);
    }


    private String takeNextPrivate(Checker checker) throws NoSuchElementException, IOException {
        if (!hasNextPrivate(checker)) {
            throw new NoSuchElementException("No such element in input");
        }
        StringBuilder str = new StringBuilder();
        str.append((char) last);
        int c = read.read();
        while (checker.check(c)) {
            str.append((char) c);
            c = read.read();
        }
        last = c;
        return str.toString();
    }

    public interface Checker {
        boolean check(int c);
    }

    public boolean hasNext() throws NoSuchElementException, IOException {
        return hasNextPrivate(check);
    }
    public String next() throws NoSuchElementException, IOException {
        return takeNextPrivate(check);
    }

    private boolean endLineChecker(Checker checker) throws IOException {
        if (last == '\n' || last == '\r'){
            return true;
        }
        int c = last;
        while (!checker.check(c) && !endChecker(c) && c != '\n' && c != '\r') {
            c = read.read();
        }
        last = c;
        return last == '\n' || last == '\r';
    }
    private boolean endOfInput(Checker checker) throws IOException {
        if (endChecker(last)){
            return true;
        }
        int c = last;
        if (last == -2){
            c = read.read();
        }
        while (!checker.check(c) && !endChecker(c) && c != '\n' && c != '\r') {
            c = read.read();
        }
        last = c;
        return endChecker(last);
    }

    public boolean isEndOfLine() throws IOException {
        return endLineChecker(check);
    }
    public boolean isEndOfInput() throws IOException {
        return endOfInput(check);
    }
    public void newLine() throws IOException {
        while (last != '\n' && last != '\r') {
            last = read.read();
        }
        int c = read.read();
        if (last == '\n' && c == '\r' || last == '\r' && c == '\n') {
            c = read.read();
        }
        last = c;
    }

    public void close() throws IOException {
        read.close();
    }
}