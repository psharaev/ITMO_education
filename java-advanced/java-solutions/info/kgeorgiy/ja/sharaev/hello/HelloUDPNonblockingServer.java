package info.kgeorgiy.ja.sharaev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.CharacterCodingException;
import java.util.Iterator;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.function.Supplier;

import static info.kgeorgiy.ja.sharaev.hello.Util.closeNullable;
import static info.kgeorgiy.ja.sharaev.hello.Util.log;

/**
 * Implementation of {@link HelloServer}
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class HelloUDPNonblockingServer extends AbstractHelloServer {
    private ByteBufferPool byteBufferPool;
    private Selector selector;
    private BlockingQueue<Response> responses;
    private DatagramChannel serverChannel;

    /**
     * see {@link AbstractHelloServer#main(String[], Supplier)}
     *
     * @param args {@link AbstractHelloServer#MAIN_USAGE}
     */
    public static void main(final String[] args) {
        main(args, HelloUDPNonblockingServer::new);
    }

    @Override
    public void start(final int port, final int threads) {
        try {
            selector = Selector.open();
            serverChannel = DatagramChannel.open();
            serverChannel.bind(new InetSocketAddress(port));
            serverChannel.configureBlocking(false);
            serverChannel.register(selector, SelectionKey.OP_READ);
            final int receiveBufferSize = serverChannel.getOption(StandardSocketOptions.SO_RCVBUF);
            responses = new ArrayBlockingQueue<>(threads * TASKS_CAPACITY_FOR_THREAD);
            byteBufferPool = new ByteBufferPool(threads * TASKS_CAPACITY_FOR_THREAD, receiveBufferSize);
        } catch (final IOException e) {
            log("Fail start server", e);
            return;
        }

        startWorkersAndReceiver(threads);
    }

    @Override
    protected void receiver() {
        while (!Thread.currentThread().isInterrupted() && selector.isOpen()) {
            try {
                selector.select();
            } catch (final IOException e) {
                log("Fail select", e);
                return;
            }

            for (final Iterator<SelectionKey> i = selector.selectedKeys().iterator(); i.hasNext(); ) {
                final SelectionKey key = i.next();
                try {
                    if (key.isReadable()) {
                        readKey(key);
                    }
                    if (key.isWritable()) {
                        writeKey(key);
                    }
                } finally {
                    i.remove();
                }
            }
        }
    }

    private void readKey(final SelectionKey key) {
        final DatagramChannel channel = (DatagramChannel) key.channel();
        final ByteBuffer receiveBuffer = byteBufferPool.poll();
        if (receiveBuffer == null) {
            return;
        }

        final SocketAddress address;
        try {
            address = channel.receive(receiveBuffer);
        } catch (final IOException e) {
            log("Failed receive buffer", e);
            return;
        }

        try {
            workers.submit(() -> {
                final Response response;
                try {
                    receiveBuffer.flip();
                    response = processRequest(address, receiveBuffer);
                } catch (final CharacterCodingException e) {
                    log("Bad request", e);
                    byteBufferPool.release(receiveBuffer);
                    return;
                }

                try {
                    responses.add(response);
                } catch (final IllegalStateException e) {
                    log("Failed add response", e);
                    return;
                }
                key.interestOpsOr(SelectionKey.OP_WRITE);
                selector.wakeup();
            });
        } catch (final RejectedExecutionException e) {
            log("Fail submit task", e);
        }
    }

    private void writeKey(final SelectionKey key) {
        final DatagramChannel channel = (DatagramChannel) key.channel();
        final Response response = responses.poll();
        if (response == null) {
            key.interestOps(SelectionKey.OP_READ);
            return;
        }

        try {
            channel.send(response.getResponse(), response.getAddress());
        } catch (final IOException e) {
            log("Failed send buffer", e);
        } finally {
            byteBufferPool.release(response.getResponse());
        }
    }

    @Override
    public void close() {
        closeNullable("server channel", serverChannel);
        closeNullable("selector", selector);

        closeWorkers();
    }
}
