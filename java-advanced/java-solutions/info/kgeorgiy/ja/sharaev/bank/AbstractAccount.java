package info.kgeorgiy.ja.sharaev.bank;

import java.io.Serializable;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public abstract class AbstractAccount implements Account, Serializable {
    protected final String id;
    protected int amount;

    /**
     * Create new instance
     *
     * @param id     passport id
     * @param amount current amount
     */
    protected AbstractAccount(final String id, final int amount) {
        this.id = id;
        this.amount = amount;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public synchronized int getAmount() {
        return amount;
    }

    @Override
    public synchronized void setAmount(final int amount) {
        this.amount = amount;
    }

    @Override
    public synchronized int addAmount(final int delta) {
        return amount += delta;
    }
}
