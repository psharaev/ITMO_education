package info.kgeorgiy.ja.sharaev.hello;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Utils for {@link HelloUDPClient} and {@link HelloUDPServer}
 */
public final class Util {

    /**
     * Shutdown executors by this time in seconds
     */
    private final static int TIME_TO_SHUTDOWN_SECOND = 60;

    /**
     * Specified Charset for {@link HelloUDPClient} and {@link HelloUDPServer}
     */
    public static final Charset CHARSET = StandardCharsets.UTF_8;


    /**
     * Extract data from {@link DatagramPacket} and convert to {@link String}.
     * Use {@link #CHARSET}
     *
     * @param packet - packet for extract data and after convert
     * @return converted data
     */
    public static String getString(final DatagramPacket packet) throws CharacterCodingException {
        return getString(wrapByteBuffer(packet));
    }

    /**
     * Convert {@link DatagramPacket} to {@link ByteBuffer}
     *
     * @param packet packet for convert
     * @return {@link ByteBuffer} with position on start
     */
    public static ByteBuffer wrapByteBuffer(final DatagramPacket packet) {
        return ByteBuffer.wrap(packet.getData(), packet.getOffset(), packet.getLength());
    }

    /**
     * Create {@link DatagramPacket} with buffer with length {@link DatagramSocket#getReceiveBufferSize()}
     *
     * @param socket socket for get {@link DatagramSocket#getReceiveBufferSize()}
     * @return packet with buffer
     * @throws SocketException if there is an error in the underlying protocol, such as an UDP error.
     */
    public static DatagramPacket createPacket(final DatagramSocket socket) throws SocketException {
        final int receiveBufferSize = socket.getReceiveBufferSize();
        return new DatagramPacket(new byte[receiveBufferSize], receiveBufferSize);
    }

    /**
     * Read {@link String} from {@link ByteBuffer}. The buffer should be ready to read
     *
     * @param buffer the buffer from which the reading will be performed
     * @return {@link String} from the buffer with {@link #CHARSET} decode
     * @throws CharacterCodingException if fail convert
     */
    public static String getString(final ByteBuffer buffer) throws CharacterCodingException {
        return CHARSET.newDecoder().decode(buffer).toString();
    }

    /**
     * Print message to {@link System#err}
     *
     * @param message message to println
     */
    public static void log(final String message) {
        System.err.println(message);
    }

    /**
     * Print message with {@link Exception} to {@link System#err}
     *
     * @param message message with {@link Exception} to println
     */
    public static void log(final String message, final Exception e) {
        System.err.println(message + ", error: " + e.getMessage());
    }

    /**
     * Trying to close {@link ExecutorService} with {@link #TIME_TO_SHUTDOWN_SECOND}
     *
     * @param poolName pool name for print messages
     * @param pool     pool for closing
     */
    public static void shutdownAndAwaitTermination(final String poolName, final ExecutorService pool) {
        if (pool == null) {
            return;
        }

        try {
            if (!pool.awaitTermination(TIME_TO_SHUTDOWN_SECOND, TimeUnit.SECONDS)) {
                pool.shutdownNow();
                if (!pool.awaitTermination(TIME_TO_SHUTDOWN_SECOND, TimeUnit.SECONDS)) {
                    log(poolName + " pool did not terminate");
                }
            }
        } catch (final InterruptedException e) {
            pool.shutdownNow();
            log(poolName + " pool not waited did to terminate");
            Thread.currentThread().interrupt();
        }
    }

    public static void closeNullable(final String name, final AutoCloseable closeable) {
        if (closeable != null) {
            try {
                closeable.close();
            } catch (final Exception e) {
                log("Fail close" + name, e);
            }
        }
    }
}
