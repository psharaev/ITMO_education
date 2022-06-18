package info.kgeorgiy.ja.sharaev.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Bank API
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public interface Bank extends Remote {
    /**
     * Creates a new account with specified identifier if it is not already exists.
     *
     * @param id account id
     * @return created or existing account.
     */

    Account createAccount(String id) throws RemoteException;

    /**
     * Returns account by identifier.
     *
     * @param id account id
     * @return account with specified identifier or {@code null} if such account does not exist.
     */
    Account getAccount(final String id, final RemoteType type) throws RemoteException;

    /**
     * Creates a new person with specified information if is not already exists.
     * If exists new person not created
     *
     * @param name       person name
     * @param surname    person surname
     * @param passportID person passport id
     * @return created or existing person.
     */
    Person createPerson(String name, String surname, String passportID) throws RemoteException;

    /**
     * Returns person by passport id.
     *
     * @param id         passport id person
     * @param remoteType Remote or Local person type
     * @return person with specified identifier or {@code null} if such account does not exist.
     */
    Person getPerson(String id, RemoteType remoteType) throws RemoteException;
}
