package info.kgeorgiy.ja.sharaev.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

/**
 * Example for use API bank
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public final class Client {

    /**
     * Usage client <name> <surname> <passport> <subID> <delta>
     * - If there is no information about the specified individual, then it should be added. Otherwise, his data should be checked.
     * - If an individual does not have an account with the specified number, then it is created with a zero balance.
     *
     * @param args <name> <surname> <passport> <subID> <delta>
     */
    public static void main(final String... args) throws RemoteException {
        if (args == null) {
            System.err.println("Args is null");
            return;
        }
        if (args.length != 5) {
            System.err.println("Wrong count args, usage client <name> <surname> <passport> <subID> <delta>");
            return;
        }

        for (int i = 0; i < args.length; i++) {
            if (args[i] == null) {
                System.err.println("Arg " + i + " is null");
                return;
            }
        }

        final String name = args[0];
        final String surname = args[1];
        final String passportID = args[2];
        final String subID = args[3];
        final int delta;
        try {
            delta = Integer.parseInt(args[4]);
        } catch (final NumberFormatException e) {
            System.err.println("delta is not a number " + e.getMessage());
            return;
        }


        final Bank bank;
        try {
            bank = (Bank) Naming.lookup("//localhost/bank");
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
            return;
        } catch (final MalformedURLException e) {
            System.out.println("Bank URL is invalid");
            return;
        }

        Person person = bank.getPerson(passportID, RemoteType.REMOTE);
        if (person == null) {
            System.out.println("Creating person");
            person = bank.createPerson("Pavel", "Sharaev", passportID);
        } else {
            System.out.println("Person already exists");
        }
        System.out.println("Name: " + person.getName());
        System.out.println("Surname: " + person.getSurname());
        System.out.println("passport: " + person.getPassportID());

        if (!name.equals(person.getName())) {
            System.out.println("Names not equals");
        }
        if (!surname.equals(person.getSurname())) {
            System.out.println("Surnames not equals");
        }

        Account account = bank.getAccount(passportID + ":" + subID, RemoteType.REMOTE);
        if (account == null) {
            System.out.println("Creating account");
            account = person.createAccount(subID);
        } else {
            System.out.println("Account already exists");
        }

        System.out.println("Accounts count: " + person.getAccounts().size());

        System.out.println("Account id: " + account.getId());
        System.out.println("Money: " + account.getAmount());
        System.out.println("Adding money");
        account.setAmount(account.getAmount() + delta);
        System.out.println("Money: " + account.getAmount());
    }
}
