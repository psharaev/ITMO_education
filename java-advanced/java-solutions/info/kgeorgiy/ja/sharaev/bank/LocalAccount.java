package info.kgeorgiy.ja.sharaev.bank;

/**
 * When transmitting via remote, a copy will be created via serialization
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class LocalAccount extends AbstractAccount {
    /**
     * Create new instance
     *
     * @param id     passport id
     * @param amount current amount
     */
    public LocalAccount(final String id, final int amount) {
        super(id, amount);
    }
}
