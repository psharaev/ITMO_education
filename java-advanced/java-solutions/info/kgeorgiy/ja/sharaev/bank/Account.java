package info.kgeorgiy.ja.sharaev.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Account presentation
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public interface Account extends Remote {
    /**
     * Returns account identifier.
     */
    String getId() throws RemoteException;

    /**
     * Returns amount of money at the account.
     */
    int getAmount() throws RemoteException;

    /**
     * Sets amount of money at the account.
     */
    void setAmount(int amount) throws RemoteException;

    /**
     * add delta to amount
     *
     * @param delta number for add to amount
     * @return current value
     */
    int addAmount(int delta) throws RemoteException;
}
