package markup;

import java.util.List;

public class UnorderedList extends Pattern implements Element, ElementList {
    public UnorderedList(List<ListItem> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder result) {
    }

    @Override
    public void toBBCode(StringBuilder text) {
        toBBCode(text, "[list]", "[/list]");
    }
}