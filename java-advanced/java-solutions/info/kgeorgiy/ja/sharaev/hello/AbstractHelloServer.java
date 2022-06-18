package info.kgeorgiy.ja.sharaev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.util.concurrent.*;
import java.util.function.Supplier;

import static info.kgeorgiy.ja.sharaev.hello.Util.CHARSET;
import static info.kgeorgiy.ja.sharaev.hello.Util.log;

/**
 * Basic server implementation {@link HelloServer}
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public abstract class AbstractHelloServer implements HelloServer {
    /**
     * How long can a task wait to be completed
     */
    protected final static int KEEP_ALIVE_REQUEST_TIME = 1000;

    /**
     * The unit of measurement of the waiting time
     */
    protected final static TimeUnit TIME_UNIT_KEEP_ALIVE_TIME = TimeUnit.MILLISECONDS;

    /**
     * Job queue size for a single thread
     */
    protected final static int TASKS_CAPACITY_FOR_THREAD = 100;
    /**
     * Task Rejection strategy
     */
    private final static RejectedExecutionHandler DISCARD_POLICY = new ThreadPoolExecutor.DiscardOldestPolicy();

    /**
     * Thread executing IO operations
     */
    protected ExecutorService receiver;

    /**
     * Threads performing received tasks
     */
    protected ExecutorService workers;

    /**
     * Main arguments instruction
     */
    protected static final String MAIN_USAGE = "usage: <port> <number of threads>";

    /**
     * Main with {@link #MAIN_USAGE}
     *
     * @param args           Array of arguments:
     *                       1. port number;
     *                       2. number of threads;
     * @param instanceServer {@link Supplier} create new instance {@link AbstractHelloServer}
     */
    protected static void main(final String[] args, final Supplier<AbstractHelloServer> instanceServer) {
        if (args == null) {
            log("Args is null");
            return;
        }
        if (args.length != 2) {
            log("Wrong count arguments");
            return;
        }

        for (int i = 0; i < 2; i++) {
            if (args[i] == null) {
                log("Arg number " + i + " is null");
                return;
            }
        }

        final int port = Integer.parseInt(args[0]);
        final int threads = Integer.parseInt(args[1]);

        try (final AbstractHelloServer server = instanceServer.get()) {
            server.start(port, threads);
            int cp;
            while (true) {
                try {
                    if ((cp = System.in.read()) == -1) {
                        break;
                    }
                } catch (final IOException e) {
                    break;
                }
                if (cp == 's') {
                    break;
                }
            }
        }
    }

    /**
     * Implements the logic of request processing
     *
     * @param address the address to send the result to
     * @param request received request data
     * @return {@link Response} request ready to send
     * @throws CharacterCodingException if the request could not be decrypted
     */
    protected static Response processRequest(final SocketAddress address, final ByteBuffer request) throws CharacterCodingException {
        final String text = Util.getString(request);

        request.clear();
        request.put("Hello, ".getBytes(CHARSET));
        request.put(text.getBytes(CHARSET));
        request.flip();

        return new Response(address, request);
    }

    /**
     * Creates and start the specified number of {@link #workers} and one more {@link #receiver}
     *
     * @param threads count workers
     */
    protected final void startWorkersAndReceiver(final int threads) {
        receiver = Executors.newSingleThreadExecutor();
        workers = new ThreadPoolExecutor(1, threads,
                KEEP_ALIVE_REQUEST_TIME,
                TIME_UNIT_KEEP_ALIVE_TIME,
                new ArrayBlockingQueue<>(threads * TASKS_CAPACITY_FOR_THREAD),
                DISCARD_POLICY);
        receiver.submit(this::receiver);
    }

    /**
     * Creates and start the specified number of {@link #workers} with {@link #receiver}
     *
     * @param threads count workers
     */
    protected final void startWorkersWithReceivers(final int threads) {
        workers = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            workers.submit(this::receiver);
        }
    }

    /**
     * Listener Logic
     */
    protected abstract void receiver();

    /**
     * closes {@link #workers} and {@link #receiver}
     */
    protected final void closeWorkers() {
        if (receiver != null) {
            receiver.shutdown();
        }

        if (workers != null) {
            workers.shutdown();
        }

        Util.shutdownAndAwaitTermination("receiver", receiver);
        Util.shutdownAndAwaitTermination("workers", workers);
    }

    /**
     * Response data
     */
    public final static class Response {
        private final SocketAddress address;
        private final ByteBuffer response;

        private Response(final SocketAddress address, final ByteBuffer response) {
            this.address = address;
            this.response = response;
        }

        /**
         * Closes workers and listener
         *
         * @return {@link SocketAddress}
         */
        public SocketAddress getAddress() {
            return address;
        }

        /**
         * Binary data to send ready for reading
         *
         * @return {@link ByteBuffer}
         */
        public ByteBuffer getResponse() {
            return response;
        }
    }
}
