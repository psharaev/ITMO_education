package info.kgeorgiy.ja.sharaev.bank;

import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.*;
import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

import static info.kgeorgiy.ja.sharaev.bank.Util.buildAccountID;
import static org.junit.Assert.*;


/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
@FixMethodOrder(MethodSorters.JVM)
public class BankTest {
    private static Bank bank;
    private static final String BANK_URL = "//localhost/bank";
    private static final int PORT = 8881;
    private static final int PERSONS_COUNT = 5;
    private static final int ACCOUNTS_COUNT = 5;

    @BeforeClass
    public static void beforeClass() {
        try {
            try {
                final RemoteBank temp = new RemoteBank(PORT);
                UnicastRemoteObject.exportObject(temp, PORT);
                Naming.rebind(BANK_URL, temp);
                bank = (Bank) Naming.lookup(BANK_URL);
            } catch (final NotBoundException e) {
                System.err.println("Bank not found");
            }
            System.out.println("Bank bound");
        } catch (final RemoteException e) {
            System.out.println("Cannot export bank: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        } catch (final MalformedURLException e) {
            System.out.println("Malformed URL");
        }
    }

    private static void assertEqualsPersons(final Person a, final Person b) throws RemoteException {
        assertEquals(a.getName(), b.getName());
        assertEquals(a.getSurname(), b.getSurname());
        assertEquals(a.getPassportID(), b.getPassportID());
    }

    private static void assertEqualsAccounts(final Account expected, final Account actual) throws RemoteException {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getAmount(), actual.getAmount());
    }

    @Test
    public void test_1_1_createPersons() throws RemoteException {
        assertNull(bank.getPerson("-1", RemoteType.LOCAL));
        assertNull(bank.getPerson("-1", RemoteType.REMOTE));


        for (int i = 0; i < PERSONS_COUNT; ++i) {
            final String id = Integer.toString(i);
            assertNotNull(bank.createPerson("Name_" + id, "Surname_" + id, id));

            final Person remotePerson = bank.getPerson(id, RemoteType.REMOTE);
            assertEquals("Name_" + id, remotePerson.getName());
            assertEquals("Surname_" + id, remotePerson.getSurname());
            assertEquals(id, remotePerson.getPassportID());

            assertEqualsPersons(remotePerson, bank.getPerson(id, RemoteType.LOCAL));
        }
    }

    @Test
    public void test_1_2_createAccounts() throws RemoteException {
        for (int i = 0; i < PERSONS_COUNT; ++i) {
            final String passport = Integer.toString(i);
            final Person remotePerson = bank.getPerson(passport, RemoteType.REMOTE);

            for (int j = 0; j < ACCOUNTS_COUNT / 2; j++) {
                assertNotNull(remotePerson.createAccount(Integer.toString(j)));
            }

            for (int j = ACCOUNTS_COUNT / 2; j < ACCOUNTS_COUNT; j++) {
                assertNotNull(bank.createAccount(buildAccountID(passport, Integer.toString(j))));
            }
        }
    }

    private void testRemoteTypePerson(final RemoteType type, final int delta, final boolean expectDifference) throws RemoteException {
        for (int i = 0; i < PERSONS_COUNT; ++i) {
            final String id = Integer.toString(i);
            final Person personA = bank.getPerson(id, type);
            final Person personB = bank.getPerson(id, type);
            assertNotSame(personA, personB);

            for (int j = 0; j < ACCOUNTS_COUNT; j++) {
                final String accountID = Integer.toString(j);
                final Account accountA = personA.getAccount(accountID);
                final Account accountB = personB.getAccount(accountID);
                assertNotSame(accountA, accountB);
                assertEquals(accountA.getAmount() + delta, accountA.addAmount(delta));
                if (expectDifference) {
                    assertEquals(accountA.getAmount(), accountB.getAmount() + delta);
                } else {
                    assertEquals(accountA.getAmount(), accountB.getAmount());
                }

            }
        }
    }

    @Test
    public void test_2_1_RemotePerson() throws RemoteException {
        testRemoteTypePerson(RemoteType.REMOTE, 83, false);
    }

    @Test
    public void test_2_2_LocalPerson() throws RemoteException {
        testRemoteTypePerson(RemoteType.LOCAL, 97, true);
    }

    @Test
    public void test_3_1_multithreading() throws RemoteException {
        final Person person = bank.getPerson("0", RemoteType.REMOTE);
        final Account account = person.getAccount("0");
        final int amount = account.getAmount();

        final ExecutorService pool = Executors.newFixedThreadPool(PERSONS_COUNT);

        IntStream.range(0, PERSONS_COUNT).mapToObj(number -> pool.submit(() -> {
                account.addAmount(100);
                return null;
        })).toList().forEach(f -> {
            while (true) {
                try {
                    f.get();
                    break;
                } catch (final InterruptedException ignored) {
                } catch (final ExecutionException e) {
                    throw new UncheckedIOException((RemoteException) e.getCause());
                }
            }
        });

        assertEquals(account.getAmount(), amount + 100 * PERSONS_COUNT);
    }

    @Test
    public void test_3_2_serializationPerson() throws IOException, ClassNotFoundException {
        final Person person = bank.getPerson("0", RemoteType.LOCAL);
        assertTrue(person instanceof Serializable);
        final Person copyPerson = copy(person);
        assertEqualsPersons(person, copyPerson);
    }

    @Test
    public void test_3_3_serializationAccount() throws IOException, ClassNotFoundException {
        final Account account = bank.getAccount("0:0", RemoteType.LOCAL);
        assertTrue(account instanceof Serializable);
        final Account copyAccount = copy(account);
        assertEqualsAccounts(account, copyAccount);
    }

    @Test
    public void test_4_1_clientTest() throws RemoteException {
        final String name = "Pavel";
        final String surname = "Sharaev";
        final String passportID = "1234";
        final String subID = "567";
        final int delta = 89;
        assertNull(bank.getPerson(passportID, RemoteType.REMOTE));
        Client.main(name, surname, passportID, subID, Integer.toString(delta));
        final Person person = bank.getPerson(passportID, RemoteType.LOCAL);
        assertEquals(name, person.getName());
        assertEquals(surname, person.getSurname());
        assertEquals(passportID, person.getPassportID());

        final String accountID = buildAccountID(passportID, subID);
        final Account localAccount = bank.getAccount(accountID, RemoteType.LOCAL);
        assertNotNull(localAccount);
        assertEquals(localAccount.getId(), accountID);
        assertEquals(localAccount.getAmount(), delta);


        final Account remoteAccount = bank.getAccount(buildAccountID(passportID, subID), RemoteType.REMOTE);
        final int beforeAmount = remoteAccount.getAmount();
        Client.main(name, surname, passportID, subID, Integer.toString(delta));
        assertEquals(remoteAccount.getAmount(), beforeAmount + delta);
    }

    @SuppressWarnings("unchecked")
    private static <T> T copy(final T obj) throws IOException, ClassNotFoundException {
        try (final ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
            try (final ObjectOutputStream oos = new ObjectOutputStream(baos)) {
                oos.writeObject(obj);
            }

            try (final ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray())) {
                try (final ObjectInputStream ois = new ObjectInputStream(bais)) {
                    return (T) ois.readObject();
                }
            }
        }
    }
}
