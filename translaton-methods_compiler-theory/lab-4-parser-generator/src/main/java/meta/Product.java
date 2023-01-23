package meta;

import java.util.List;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public record Product(List<RuleCalling> ruleCallings, String code) {
}
