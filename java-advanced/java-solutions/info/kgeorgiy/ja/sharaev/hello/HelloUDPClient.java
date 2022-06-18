package info.kgeorgiy.ja.sharaev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketException;
import java.nio.charset.CharacterCodingException;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Supplier;
import java.util.stream.IntStream;

import static info.kgeorgiy.ja.sharaev.hello.Util.*;

/**
 * Implementation of {@link HelloClient}
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class HelloUDPClient extends AbstractHelloClient {

    private InetSocketAddress serverAddress;

    /**
     * see {@link AbstractHelloClient#main(String[], Supplier)}
     *
     * @param args {@link AbstractHelloClient#MAIN_USAGE}
     */
    public static void main(final String[] args) {
        main(args, HelloUDPClient::new);
    }

    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
        final ExecutorService threadPool = Executors.newFixedThreadPool(threads);
        serverAddress = new InetSocketAddress(host, port);
        try {
            threadPool.invokeAll(
                    IntStream.range(0, threads)
                            .<Callable<Void>>mapToObj(threadNumber -> () -> {
                                sendAndReceive(prefix, threadNumber, requests);
                                return null;
                            }).toList()
            );
        } catch (final InterruptedException e) {
            System.err.println("Fail send, interrupted: " + e.getMessage());
        }
        threadPool.shutdown();
    }

    private void sendAndReceive(final String prefix,
                                final int threadNumber, final int requests
    ) {
        try (final DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(IO_TIMEOUT);

            final DatagramPacket requestPacket = new DatagramPacket(new byte[0], 0, serverAddress);
            final DatagramPacket receivePacket;
            try {
                receivePacket = createPacket(socket);
            } catch (final SocketException e) {
                log("Failed get receive buffer size", e);
                return;
            }

            IntStream.range(0, requests).forEach(requestNumber -> {
                final String request = String.format("%s%d_%d", prefix, threadNumber, requestNumber);
                requestPacket.setData(request.getBytes(CHARSET));

                while (!Thread.currentThread().isInterrupted() && !socket.isClosed()) {
                    try {
                        socket.send(requestPacket);
                    } catch (final IOException e) {
                        failSend(request, e);
                        continue;
                    }
                    try {
                        socket.receive(receivePacket);
                    } catch (final IOException e) {
                        failReceive(request, e);
                        continue;
                    }

                    final String receive;
                    try {
                        receive = getString(receivePacket);
                    } catch (final CharacterCodingException e) {
                        failDecode(request, e);
                        continue;
                    }

                    if (checkRelation(receive, threadNumber, requestNumber)) {
                        successRelation(request, receive);
                        break;
                    } else {
                        failRelation(request, receive);
                    }
                }
            });
        } catch (final SocketException e) {
            System.err.println("Failed socket: " + e.getMessage());
        }
    }
}
