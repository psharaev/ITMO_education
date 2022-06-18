package info.kgeorgiy.ja.sharaev.bank;

/**
 * When transmitting via remote, a remote link will be transmitted
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class RemoteAccount extends AbstractAccount {
    /**
     * Create new instance. Current amount equals zero
     *
     * @param id     passport id
     * @param amount account amount
     */
    public RemoteAccount(final String id, final int amount) {
        super(id, amount);
    }
}
