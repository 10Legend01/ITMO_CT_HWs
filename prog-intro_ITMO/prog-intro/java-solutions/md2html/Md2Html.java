package md2html;

import java.io.*;
import java.util.*;
import java.nio.charset.StandardCharsets;


public class Md2Html {
    static private final Map<String, String> marks = Map.of(
            "*", "em",
            "_", "em",
            "**", "strong",
            "__", "strong",
            "--", "s",
            "`", "code"
    );

    static private final Map<Character, String> specialSymbols = Map.of(
            '<', "&lt;",
            '>', "&gt;",
            '&', "&amp;"
    );

    static private final Map<String, String> linkSymbolsMap = Map.of(
            ")", "(",
            "]", "["
    );

    static private final List<String> linkSymbolsList = List.of(
            ")", "(", "]", "["
    );

    public static void main(String[] args) {
        List<String> lines = new ArrayList<>();
        try (BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), StandardCharsets.UTF_8))) {
            while (true) {
                String line = bufferedReader.readLine();
                if (line == null) {
                    break;
                }
                lines.add(line);
            }
        } catch (FileNotFoundException e) {
            System.out.println("Can not find input file " + e.getMessage());
            return;
        } catch (IOException e) {
            System.out.println("Error reading input file: " + e.getMessage());
            return;
        }
        String html = toHtml(lines);
        try (BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8))) {
            bufferedWriter.write(html);
        } catch (FileNotFoundException e) {
            System.out.println("Can not create or open output file: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Error writing output file: " + e.getMessage());
        }
    }

    public static String toHtml(List<String> lines) {
        StringBuilder html = new StringBuilder();
        for (int i = 0; i < lines.size(); i++) {
            StringBuilder paragraph = new StringBuilder();
            while (i < lines.size() && !lines.get(i).isEmpty()) {
                paragraph.append(lines.get(i++)).append(System.lineSeparator());
            }
            if (paragraph.length() == 0) {
                continue;
            }
            paragraph.setLength(paragraph.length() - System.lineSeparator().length());
            //html.append(parseParagraph(paragraph.toString()));
            parseParagraph(paragraph.toString(), html);
        }
        return html.toString();
    }

    private static void parseParagraph(String paragraph, StringBuilder html) {
        //StringBuilder html = new StringBuilder();
        int headerLevel = headerLevelCounter(paragraph);
        Map<Integer, Integer> markers = new LinkedHashMap<>();
        html.append(getLevel(headerLevel, true));
        findMarkers(markers, paragraph);
        parseString(markers, html, paragraph, headerLevel == 0 ? 0 : headerLevel + 1, paragraph.length());
        html.append(getLevel(headerLevel, false));
        html.append(System.lineSeparator());
        //return html;
    }

    private static int headerLevelCounter(String string) {
        int headerLevel = 0;
        while (headerLevel < string.length() && string.charAt(headerLevel) == '#') {
            headerLevel++;
        }
        if (headerLevel <= 6 && headerLevel < string.length() && string.charAt(headerLevel) == ' ') {
            return headerLevel;
        }
        return 0;
    }

    private static String getLevel(int headerLevel, boolean isStart) {
        return "<" + (isStart ? "" : "/") + (headerLevel == 0 ? "p" : "h" + headerLevel)  + ">";
    }

    private static boolean isSlash(String string, int position) {
        return 0 <= position && string.charAt(position) == '\\';
    }

    private static boolean isMarker(String string) {
        return marks.containsKey(string) || linkSymbolsList.contains(string);
    }

    private static void findMarkers(Map<Integer, Integer> markers, String paragraph) {
        Map<String, Integer> lastMarker = new HashMap<>();
        for (int i = 0; i < paragraph.length(); i++) {
            while (isSlash(paragraph, i)) {
                i += 2;
                if (paragraph.length() <= i) {
                    break;
                }
            }
            if (checkMarker(paragraph, i)) {
                String marker = getMarker(paragraph, i);
                pushMarker(markers, lastMarker, marker, i);
                i += marker.length() - 1;
            }
        }
    }

    private static boolean checkMarker(String string, int position) {
        for (int size = 1; size <= 2; size++) {
            if (position + size <= string.length() && isMarker(string.substring(position, position + size))) {
                return true;
            }
        }
        return false;
    }

    private static String getMarker(String string, int position) {
        for (int size = 2; ; size--) {
            if (position + size <= string.length()) {
                final String marker = string.substring(position, position + size);
                if (isMarker(marker)) {
                    return linkSymbolsMap.getOrDefault(marker, marker);
                }
            }
        }
    }

    private static void pushMarker(Map<Integer, Integer> markers, Map<String, Integer> lastMarker, String marker, int position) {
        if (lastMarker.containsKey(marker)) {
            markers.put(lastMarker.get(marker), position);
            lastMarker.remove(marker);
        } else {
            lastMarker.put(marker, position);
        }
    }

    private static void parseString(Map<Integer, Integer> markers, StringBuilder html, String paragraph, int l, int r) {
        for (int i = l; i < r; i++) {
            if (markers.containsKey(i)) {
                i = putMarkers(markers, html, paragraph, i);
            } else if (specialSymbols.containsKey(paragraph.charAt(i))) {
                html.append(specialSymbols.get(paragraph.charAt(i)));
            } else if (!isSlash(paragraph, i)) {
                html.append(paragraph.charAt(i));
            }
        }
    }

    private static int putMarkers(Map<Integer, Integer> markers, StringBuilder html, String paragraph, int position) {
        String marker = getMarker(paragraph, position);
        int closePosition = markers.get(position);
        if (marks.containsKey(marker)) {
            String htmlMarker = marks.get(marker);
            html.append("<").append(htmlMarker).append(">");
            parseString(markers, html, paragraph, position + marker.length(), closePosition);
            html.append("</").append(htmlMarker).append(">");
            position = closePosition + marker.length() - 1;
            return position;
        } else if (isLink(markers, paragraph, position, closePosition)) { // link
            html.append("<a href='").append(paragraph, closePosition + 2, markers.get(closePosition + 1)).append("'>");
            parseString(markers, html, paragraph, position + 1, closePosition);
            html.append("</a>");
            return markers.get(closePosition + 1);
        } else { // just not link
            html.append(paragraph.charAt(position));
            return position;
        }
    }

    private static boolean isLink(Map<Integer, Integer> markers, String paragraph, int position, int closePosition) {
        return paragraph.charAt(position) == '[' && paragraph.charAt(closePosition) == ']' &&
                        paragraph.charAt(closePosition + 1) == '(' &&
                        markers.containsKey(closePosition + 1) && paragraph.charAt(markers.get(closePosition + 1)) == ')';
    }
}
