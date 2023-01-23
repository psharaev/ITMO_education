package meta;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public record RuleSelector(List<Product> production) {
    public List<String> getProductWithoutCode() {
        return production.stream()
                .flatMap(p -> p.ruleCallings()
                        .stream()
                        .map(RuleCalling::nameRule)
                )
                .collect(Collectors.toList());
    }
}
