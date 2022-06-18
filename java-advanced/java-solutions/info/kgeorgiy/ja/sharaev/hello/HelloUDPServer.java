package info.kgeorgiy.ja.sharaev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.CharacterCodingException;
import java.util.function.Supplier;

import static info.kgeorgiy.ja.sharaev.hello.Util.*;

/**
 * Implementation of {@link HelloServer}
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class HelloUDPServer extends AbstractHelloServer implements HelloServer {
    private DatagramSocket socket;
    private int socketBufferSize;

    /**
     * see {@link AbstractHelloServer#main(String[], Supplier)}
     *
     * @param args {@link AbstractHelloServer#MAIN_USAGE}
     */
    public static void main(final String[] args) {
        main(args, HelloUDPServer::new);
    }

    @Override
    public void start(final int port, final int threads) {
        try {
            socket = new DatagramSocket(port);
            socketBufferSize = socket.getReceiveBufferSize();
        } catch (final SocketException e) {
            log("Failed create socket on port " + port, e);
            return;
        }

        startWorkersWithReceivers(threads);
    }

    @Override
    protected void receiver() {
        final DatagramPacket request = new DatagramPacket(new byte[socketBufferSize], socketBufferSize);
        final DatagramPacket response = new DatagramPacket(new byte[0], 0);
        while (!Thread.currentThread().isInterrupted() && !socket.isClosed()) {
            try {
                socket.receive(request);
            } catch (final IOException e) {
                System.err.println("FAIL receive request: " + e.getMessage());
                continue;
            }

            try {
                final String text = Util.getString(wrapByteBuffer(request));
                response.setData(("Hello, " + text).getBytes(CHARSET));
                response.setSocketAddress(request.getSocketAddress());
            } catch (final CharacterCodingException e) {
                System.err.println("Bad request: " + e.getMessage());
                return;
            }

            try {
                socket.send(response);
            } catch (final IOException e) {
                System.err.println("Failed send response: " + e.getMessage());
            }
        }
    }

    @Override
    public void close() {
        closeNullable("socket", socket);

        closeWorkers();
    }
}
