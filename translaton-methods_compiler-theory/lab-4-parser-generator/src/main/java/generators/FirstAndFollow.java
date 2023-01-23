package generators;

import meta.GrammarInfo;
import meta.RuleAndSelector;

import java.util.*;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class FirstAndFollow {
    private static final String EPS = "ε";

    private final List<RuleAndSelector> rules;
    private final Map<String, Set<String>> first = new HashMap<>();
    private final Map<String, Set<String>> follow = new HashMap<>();


    public FirstAndFollow(final GrammarInfo grammarInfo) {
        this.rules = grammarInfo.rules();

        if (rules.size() == 0) {
            return;
        }

        constructFirst();
        constructFollow();
    }


    public Map<String, Set<String>> getFirst() {
        return first;
    }

    public Map<String, Set<String>> getFollow() {
        return follow;
    }

    public Set<String> getFirstString(final List<String> products) {
        if (products.isEmpty()) {
            return new HashSet<>(Set.of(EPS));
        }

        final HashSet<String> res = new HashSet<>();
        for (final var c : products) {
            if (c.matches("[A-Z]+")) {
                res.add(c);
            } else if (first.containsKey(c)) {
                res.addAll(first.get(c));
                if (first.get(c).contains(EPS)) {
                    continue;
                }
            }
            break;
        }

        return res;
    }

    private void constructFirst() {
        boolean changed = true;

        while (changed) {
            changed = false;

            for (final var rule : rules) {
                final String name = rule.signature().name();
                final int curSize = first.computeIfAbsent(name, k -> new HashSet<>()).size();
                first.get(name)
                        .addAll(getFirstString(rule
                                .selector()
                                .getProductWithoutCode())
                        );

                changed |= curSize < first.get(name).size();
            }
        }
    }

    private void constructFollow() {
        boolean change = true;

        follow.put(rules.get(0).signature().name(), new HashSet<>(Set.of("END")));

        while (change) {
            change = false;

            for (final var rule : rules) {
                final List<String> products = rule.selector().getProductWithoutCode();
                final String A = rule.signature().name();
                for (int i = 0; i < products.size(); i++) {
                    final String B = products.get(i);
                    if (!B.matches("[A-Z]+") && !B.equals(EPS)) {
                        final int curSize = follow.computeIfAbsent(B, k -> new HashSet<>()).size();
                        final Set<String> firstGamma = getFirstString(products.subList(i + 1, products.size()));
                        if (firstGamma.contains(EPS)) {
                            follow.get(B).addAll(follow.computeIfAbsent(A, k -> new HashSet<>()));
                        }
                        firstGamma.remove(EPS);

                        follow.get(B).addAll(firstGamma);

                        change |= curSize < follow.get(B).size();
                    }
                }
            }
        }
    }
}
