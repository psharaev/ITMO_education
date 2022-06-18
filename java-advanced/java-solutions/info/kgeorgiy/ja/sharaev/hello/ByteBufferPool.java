package info.kgeorgiy.ja.sharaev.hello;

import java.nio.ByteBuffer;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/**
 * Thread-safe ByteBuffer pool
 *
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class ByteBufferPool {
    private final BlockingQueue<ByteBuffer> pool;

    /**
     * Create new instance with specified count {@link ByteBuffer} with specified buffer capacity
     *
     * @param size           count {@link ByteBuffer}
     * @param bufferCapacity the capacity of each {@link ByteBuffer}
     */
    public ByteBufferPool(final int size, final int bufferCapacity) {
        this.pool = new ArrayBlockingQueue<>(size);
        for (int i = 0; i < size; i++) {
            pool.add(ByteBuffer.allocate(bufferCapacity));
        }
    }

    /**
     * Returns a free and cleared {@link ByteBuffer} or null if not exist
     *
     * @return {@link ByteBuffer}
     */
    public ByteBuffer poll() {
        final ByteBuffer buffer = pool.poll();
        if (buffer == null) {
            return null;
        }
        buffer.clear();
        return buffer;
    }

    /**
     * release a busy {@link ByteBuffer}
     *
     * @param buffer release this buffer
     */
    public void release(final ByteBuffer buffer) {
        pool.add(buffer);
    }
}
