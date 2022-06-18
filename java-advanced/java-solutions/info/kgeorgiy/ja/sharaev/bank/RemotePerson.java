package info.kgeorgiy.ja.sharaev.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Map;

import static info.kgeorgiy.ja.sharaev.bank.Util.buildAccountID;
import static info.kgeorgiy.ja.sharaev.bank.Util.computeIfAbsent;

/**
 * When transmitting via remote, a remote link will be transmitted
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class RemotePerson extends AbstractPerson {
    private final int port;

    public RemotePerson(final String name, final String surname, final String passportID, final Map<String, Account> accounts, final int port) {
        super(name, surname, passportID, accounts);
        this.port = port;
    }

    @Override
    public Account createAccount(final String subID) throws RemoteException {
        return computeIfAbsent(accounts, subID, key -> {
            final Account person = new RemoteAccount(buildAccountID(passportID, key), 0);
            UnicastRemoteObject.exportObject(person, port);
            return person;
        });
    }
}
