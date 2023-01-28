package markup;

import java.util.List;

public class Paragraph extends Pattern implements Element, ElementList {
    public Paragraph(List<ElementMarkup> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder text) {
        toMarkdownWhSpace(text);
    }

    @Override
    public void toBBCode(StringBuilder text) {
        toBBCodeWhSpace(text);
    }
}
