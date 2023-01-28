package markup;

import java.util.List;

public class Strong extends Pattern implements Element, ElementMarkup {
    public Strong(List<ElementMarkup> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder text) {
        toMarkdown(text, "__");
    }

    @Override
    public void toBBCode(StringBuilder text) {
        toBBCode(text, "[b]", "[/b]");
    }
}
