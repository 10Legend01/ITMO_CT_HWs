package markup;

import java.util.List;

public abstract class Pattern {

    private List<? extends Element> elements;

    Pattern(List<? extends Element> elements) {
        this.elements = elements;
    }

    protected void toMarkdownWhSpace(StringBuilder text) {
        for (Element current : elements) {
            current.toMarkdown(text);
        }
    }

    protected void toMarkdown(StringBuilder text, String round) {
        text.append(round);
        for (Element current : elements) {
            current.toMarkdown(text);
        }
        text.append(round);
    }

    protected void toBBCodeWhSpace(StringBuilder text) {
        for (Element current : elements) {
            current.toBBCode(text);
        }
    }

    protected void toBBCode(StringBuilder text, String left, String right) {
        text.append(left);
        for (Element current : elements) {
            current.toBBCode(text);
        }
        text.append(right);
    }
}
