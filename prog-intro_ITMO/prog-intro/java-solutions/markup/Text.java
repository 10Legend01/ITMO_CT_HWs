package markup;

public class Text implements Element, ElementMarkup {
    private String text;

    public Text(String text) {
        this.text = text;
    }

    @Override
    public void toMarkdown(StringBuilder text) {
        text.append(this.text);
    }

    @Override
    public void toBBCode(StringBuilder text) {
        text.append(this.text);
    }
}
