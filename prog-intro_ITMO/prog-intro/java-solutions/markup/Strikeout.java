package markup;

import java.util.List;

public class Strikeout extends Pattern implements Element, ElementMarkup {
    public Strikeout(List<ElementMarkup> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder text) {
        toMarkdown(text, "~");
    }

    @Override
    public void toBBCode(StringBuilder text) {
        toBBCode(text, "[s]", "[/s]");
    }
}
