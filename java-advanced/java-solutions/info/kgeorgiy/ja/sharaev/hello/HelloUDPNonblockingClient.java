package info.kgeorgiy.ja.sharaev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.CharacterCodingException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Supplier;

import static info.kgeorgiy.ja.sharaev.hello.Util.*;

/**
 * Implementation of {@link HelloClient}
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class HelloUDPNonblockingClient extends AbstractHelloClient {
    private int inWorkCountChannels;
    private String prefix;
    private final List<DatagramChannel> channels = new ArrayList<>();

    private class Request {
        private final int threadNumber;
        private final int countRequest;
        private final ByteBuffer buffer;
        private int requestNumber;

        private Request(final int threadNumber, final int countRequest, final ByteBuffer buffer) {
            this.threadNumber = threadNumber;
            this.countRequest = countRequest;
            this.buffer = buffer;
            this.requestNumber = 0;
        }

        private String getText() {
            return String.format("%s%d_%d", prefix, threadNumber, requestNumber);
        }

        private void readKey(final SelectionKey key) {
            final DatagramChannel channel = (DatagramChannel) key.channel();
            buffer.clear();
            try {
                channel.read(buffer);
            } catch (final IOException e) {
                failReceive(getText(), e);
                return;
            }

            final String response;
            try {
                buffer.flip();
                response = getString(buffer);
            } catch (final CharacterCodingException e) {
                failDecode(getText(), e);
                return;
            }

            if (checkRelation(response, threadNumber, requestNumber)) {
                successRelation(getText(), response);
                if (++requestNumber == countRequest) {
                    try {
                        channel.close();
                    } catch (final IOException e) {
                        log("Failed close channel", e);
                    }
                    --inWorkCountChannels;
                    return;
                }
            } else {
                failRelation(getText(), response);
            }

            key.interestOpsOr(SelectionKey.OP_WRITE);
        }

        private void writeKey(final SelectionKey key) {
            final DatagramChannel channel = (DatagramChannel) key.channel();
            buffer.clear();
            buffer.put(getText().getBytes(CHARSET));
            buffer.flip();
            try {
                channel.write(buffer);
            } catch (final IOException e) {
                failSend(getText(), e);
                return;
            }
            key.interestOps(SelectionKey.OP_READ);
        }
    }

    /**
     * see {@link AbstractHelloClient#main(String[], Supplier)}
     *
     * @param args {@link AbstractHelloClient#MAIN_USAGE}
     */
    public static void main(final String[] args) {
        main(args, HelloUDPNonblockingClient::new);
    }

    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
        final InetSocketAddress serverAddress = new InetSocketAddress(host, port);
        try (final Selector selector = Selector.open()) {
            ByteBuffer buffer = null;
            for (int thread = 0; thread < threads; thread++) {
                try {
                    final DatagramChannel channel = DatagramChannel.open();
                    channels.add(channel);
                    channel.configureBlocking(false);
                    channel.connect(serverAddress);
                    if (thread == 0) {
                        final int bufferSize = channel.getOption(StandardSocketOptions.SO_RCVBUF);
                        buffer = ByteBuffer.allocate(bufferSize);
                    }
                    channel.register(selector, SelectionKey.OP_WRITE, new Request(thread, requests, buffer));
                } catch (final IOException e) {
                    log("Fail create Datagram channel", e);
                    return;
                }
            }

            inWorkCountChannels = threads;
            this.prefix = prefix;
            receiver(selector);
        } catch (final IOException e) {
            log("Fail open selector", e);
        } finally {
            closeChannels();
        }
    }

    private void receiver(final Selector selector) {
        while (!Thread.currentThread().isInterrupted() && selector.isOpen() && inWorkCountChannels > 0) {
            final int selectedCount;
            try {
                selectedCount = selector.select(IO_TIMEOUT);
            } catch (final IOException e) {
                log("Fail select", e);
                return;
            }

            if (selectedCount == 0) {
                selector.keys().forEach(key -> key.interestOpsOr(SelectionKey.OP_WRITE));
                continue;
            }

            for (final Iterator<SelectionKey> i = selector.selectedKeys().iterator(); i.hasNext(); ) {
                final SelectionKey key = i.next();
                try {
                    final Request request = (Request) key.attachment();

                    if (key.isReadable()) {
                        request.readKey(key);
                    }
                    if (key.isValid() && key.isWritable()) {
                        request.writeKey(key);
                    }
                } finally {
                    i.remove();
                }
            }
        }
    }

    private void closeChannels() {
        for (final DatagramChannel channel : channels) {
            try {
                channel.close();
            } catch (final IOException e) {
                log("Fail close channel", e);
            }
        }
    }
}
