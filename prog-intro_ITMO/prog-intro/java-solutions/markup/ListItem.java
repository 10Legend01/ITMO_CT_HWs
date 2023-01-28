package markup;

import java.util.List;

public class ListItem extends Pattern implements Element {
    public ListItem(List<ElementList> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder result) {
    }

    @Override
    public void toBBCode(StringBuilder text) {
        toBBCode(text, "[*]", "");
    }
}