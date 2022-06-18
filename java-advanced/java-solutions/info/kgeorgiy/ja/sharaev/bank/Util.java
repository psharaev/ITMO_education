package info.kgeorgiy.ja.sharaev.bank;

import java.io.UncheckedIOException;
import java.rmi.RemoteException;
import java.util.Map;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class Util {
    public interface RemoteFunction<K, V> {
        V get(K arg) throws RemoteException;
    }

    public static String buildAccountID(final String passportID, final String subID) {
        return String.format("%s:%s", passportID, subID);
    }

    public static <K, V> V computeIfAbsent(final Map<K, V> map, final K key, final RemoteFunction<K, V> func)
            throws RemoteException {
        try {
            return map.computeIfAbsent(key, x -> {
                try {
                    return func.get(x);
                } catch (final RemoteException e) {
                    throw new UncheckedIOException(e);
                }
            });
        } catch (final UncheckedIOException e) {
            if (e.getCause() instanceof RemoteException) {
                throw (RemoteException) e.getCause();
            }
            throw e;
        }
    }
}
