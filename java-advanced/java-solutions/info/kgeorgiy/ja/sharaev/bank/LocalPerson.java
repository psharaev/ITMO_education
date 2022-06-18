package info.kgeorgiy.ja.sharaev.bank;

import java.util.Map;

import static info.kgeorgiy.ja.sharaev.bank.Util.buildAccountID;

/**
 * When transmitting via remote, a copy will be created via serialization
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class LocalPerson extends AbstractPerson {
    public LocalPerson(final String name, final String surname, final String passportID, final Map<String, Account> accounts) {
        super(name, surname, passportID, accounts);
    }

    @Override
    public Account createAccount(final String subID) {
        return accounts.computeIfAbsent(subID, key -> new LocalAccount(buildAccountID(passportID, key), 0));
    }
}
