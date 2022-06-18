package info.kgeorgiy.ja.sharaev.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import static info.kgeorgiy.ja.sharaev.bank.Util.computeIfAbsent;

/**
 * implements {@link Bank}
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<String, Person> persons = new ConcurrentHashMap<>();

    /**
     * Creates new instance
     *
     * @param port for remote objects
     */
    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Account createAccount(final String id) throws RemoteException {
        return invokePersonMethod(id, Person::createAccount);
    }

    @Override
    public Account getAccount(final String id, final RemoteType type) throws RemoteException {
        final Account remoteAccount = invokePersonMethod(id, Person::getAccount);
        return switch (type) {
            case LOCAL -> new LocalAccount(id, remoteAccount.getAmount());
            case REMOTE -> remoteAccount;
        };
    }

    @Override
    public Person createPerson(final String name, final String surname, final String passportID) throws RemoteException {
        return computeIfAbsent(persons, passportID, key -> {
            final RemotePerson person = new RemotePerson(name, surname, passportID, new ConcurrentHashMap<>(), port);
            UnicastRemoteObject.exportObject(person, port);
            return person;
        });
    }

    @Override
    public Person getPerson(final String passportID, final RemoteType remoteType) throws RemoteException {
        return switch (remoteType) {
            case LOCAL -> getLocalPerson(passportID);
            case REMOTE -> persons.get(passportID);
        };
    }

    private LocalPerson getLocalPerson(final String passportID) throws RemoteException {
        final Person person = persons.get(passportID);
        if (person == null) {
            return null;
        }

        final Map<String, Account> remoteAccounts = person.getAccounts();
        final Map<String, Account> localAccounts = new HashMap<>();

        for (final Map.Entry<String, Account> entry : remoteAccounts.entrySet()) {
            final Account account = entry.getValue();
            localAccounts.put(entry.getKey(), new LocalAccount(account.getId(), account.getAmount()));
        }

        return new LocalPerson(person.getName(), person.getSurname(), person.getPassportID(), localAccounts);
    }

    private Account invokePersonMethod(final String id, final PersonWithIdMethod f) throws RemoteException {
        final String[] p = id.split(":", 2);
        if (p.length != 2) {
            throw new IllegalArgumentException("Incorrect account id");
        }
        final Person person = persons.get(p[0]);
        if (person == null) {
            throw new IllegalArgumentException("Person not exist");
        }
        return f.apply(person, p[1]);
    }

    private interface PersonWithIdMethod {
        Account apply(Person person, String subID) throws RemoteException;
    }
}
