package info.kgeorgiy.ja.sharaev.bank;

import java.io.Serializable;
import java.util.Collections;
import java.util.Map;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public abstract class AbstractPerson implements Person, Serializable {
    protected final String name;
    protected final String surname;
    protected final String passportID;
    protected final Map<String, Account> accounts;

    /**
     * Create new instance
     *
     * @param name       person name
     * @param surname    person surname
     * @param passportID person passport id
     */
    protected AbstractPerson(final String name, final String surname, final String passportID, final Map<String, Account> accounts) {
        this.name = name;
        this.surname = surname;
        this.passportID = passportID;
        this.accounts = accounts;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getSurname() {
        return surname;
    }

    @Override
    public String getPassportID() {
        return passportID;
    }

    @Override
    public Map<String, Account> getAccounts() {
        return Collections.unmodifiableMap(accounts);
    }

    @Override
    public Account getAccount(final String subID) {
        return accounts.get(subID);
    }
}
