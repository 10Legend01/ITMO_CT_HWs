package markup;

import java.util.List;

public class Emphasis extends Pattern implements Element, ElementMarkup {
    public Emphasis(List<ElementMarkup> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder text) {
        toMarkdown(text, "*");
    }

    @Override
    public void toBBCode(StringBuilder text) {
        toBBCode(text, "[i]", "[/i]");
    }
}
