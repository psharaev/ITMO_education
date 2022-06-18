package info.kgeorgiy.ja.sharaev.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Map;

/**
 * Person presentation
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public interface Person extends Remote {
    /**
     * Returns name person.
     */
    String getName() throws RemoteException;

    /**
     * Returns surname person.
     */
    String getSurname() throws RemoteException;

    /**
     * Returns passport id person.
     */
    String getPassportID() throws RemoteException;

    /**
     * Returns accounts
     */
    Map<String, Account> getAccounts() throws RemoteException;

    /**
     * Return Account
     */
    Account getAccount(final String subID) throws RemoteException;

    /**
     * Return added Account
     */
    Account createAccount(final String subID) throws RemoteException;
}
