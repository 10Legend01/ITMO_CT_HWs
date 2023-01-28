import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

class WordStatCountFirstIndexChecker implements ScannerLab.Checker {
    @Override
    public boolean check(int c) {
        return Character.isLetter((char) c) || Character.DASH_PUNCTUATION == Character.getType((char) c) || c == '\'';
    }
}

public class WordStatCountFirstIndex {
    static class Pair implements Comparable<Pair> {
        Integer fst;
        Integer snd;

        public Pair(Integer fst, Integer snd) {
            this.fst = fst;
            this.snd = snd;
        }
        @Override
        public int compareTo(Pair o) {
            return fst - o.fst;
        }
    }


    static class IntList {
        private int c = 0;
        private int[] ma = new int[1];
        public void add(int a) {
            if (c == ma.length) {
                ma = Arrays.copyOf(ma, ma.length * 2);
            }
            ma[c++] = a;
        }
        public int get(int a) {
            return ma[a];
        }
        public int size() {
            return c;
        }
    }

    public static void main(String[] args) {
        Map<String, Pair> mapCount = new LinkedHashMap<>();
        List<IntList> list = new ArrayList<>();
        WordStatCountFirstIndexChecker check = new WordStatCountFirstIndexChecker();
        try (ScannerLab reader = new ScannerLab(new File(args[0]), check)) {
            if (!reader.isEndOfInput()) {
                int count = 0;
                int onLine = 1;
                Set<String> wordOnLine = new LinkedHashSet<>();
                do {
                    if (reader.isEndOfLine()) {
                        reader.newLine();
                        onLine = 1;
                        wordOnLine = new LinkedHashSet<>();
                        continue;
                    }
                    String str = reader.next().toLowerCase();
                    if (!mapCount.containsKey(str)) {
                        mapCount.put(str, new Pair(1, count));
                        list.add(new IntList());
                        list.get(count++).add(onLine);
                        wordOnLine.add(str);
                    } else {
                        mapCount.put(str, new Pair(mapCount.get(str).fst + 1, mapCount.get(str).snd));
                        if (!wordOnLine.contains(str)) {
                            wordOnLine.add(str);
                            list.get(mapCount.get(str).snd).add(onLine);
                        }
                    }
                    onLine++;
                } while (!reader.isEndOfInput());
            }
            try (BufferedWriter writer = new BufferedWriter(new FileWriter(args[1], StandardCharsets.UTF_8))) {
                for (Map.Entry<String,Pair> out : mapCount.entrySet().stream().sorted(Map.Entry.<String, Pair>comparingByValue()).collect(Collectors.toList())) {
                    writer.append(out.getKey()).append(" ").append(String.valueOf(out.getValue().fst));
                    IntList x = list.get(out.getValue().snd);
                    for (int i = 0, j = x.size(); i < j; i++) {
                        writer.append(" ").append(String.valueOf(x.get(i)));
                    }
                    writer.append('\n');
                }
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }

}
