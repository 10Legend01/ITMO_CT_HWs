import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

class WordStatChecker implements ScannerLab.Checker {
    @Override
    public boolean check(int c) {
        return Character.isLetter((char) c) || Character.DASH_PUNCTUATION == Character.getType((char) c) || c == '\'';
    }
}

public class WordStatCountShingles {
    public static void main(String[] args) {
        Map<String, Integer> map = new LinkedHashMap<>();
        WordStatChecker check = new WordStatChecker();
        try (ScannerLab reader = new ScannerLab(new File(args[0]), check)) {
            if (!reader.isEndOfInput()) {
                do {
                    String str = reader.next().toLowerCase();
                    int len = str.length();
                    for (int j = 0; j < len - 2; j++) {
                        String s = str.substring(j, j + 3);
                        map.put(s, map.getOrDefault(s, 0) + 1);
                    }
                } while (reader.hasNext());
            }
            try (BufferedWriter writer = new BufferedWriter(new FileWriter(args[1], StandardCharsets.UTF_8))) {
                for (Map.Entry<String,Integer> out : map.entrySet().stream().sorted(Map.Entry.<String, Integer>comparingByValue()).collect(Collectors.toList())) {
                    //System.out.println(out.getKey() + " " + out.getValue());
                    writer.append(out.getKey()).append(" ").append(String.valueOf(out.getValue())).append('\n');
                }
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }
        } catch (FileNotFoundException e) {
            System.err.println(e.getMessage());
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }
}