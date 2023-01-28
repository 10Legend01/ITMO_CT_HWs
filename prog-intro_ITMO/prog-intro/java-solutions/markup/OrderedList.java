package markup;

import java.util.List;

public class OrderedList extends Pattern implements Element, ElementList {
    public OrderedList(List<ListItem> elements) {
        super(elements);
    }

    @Override
    public void toMarkdown(StringBuilder result) {
    }

    @Override
    public void toBBCode(StringBuilder text) {
        toBBCode(text, "[list=1]", "[/list]");
    }
}