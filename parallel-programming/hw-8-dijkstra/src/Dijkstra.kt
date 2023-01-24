package dijkstra

import kotlinx.atomicfu.locks.ReentrantLock
import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.atomic.AtomicInteger
import kotlin.Comparator
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> o1!!.distance.compareTo(o2!!.distance) }

fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    start.distance = 0
    val q = MultiQueue(workers, NODE_DISTANCE_COMPARATOR)
    q.add(start)
    val onFinish = Phaser(workers + 1)
    val activeNodes = AtomicInteger(1)
    repeat(workers) {
        thread {
            while (activeNodes.get() > 0) {
                val from: Node = q.poll() ?: continue
                for (edge in from.outgoingEdges) {
                    while (true) {
                        val newDistance = from.distance + edge.weight
                        val oldDistance = edge.to.distance

                        if (oldDistance <= newDistance) {
                            break
                        }

                        if (edge.to.casDistance(oldDistance, newDistance)) {
                            q.add(edge.to)
                            activeNodes.incrementAndGet()
                            break
                        }

                    }
                }
                activeNodes.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}

class MultiQueue(private val n: Int, private val comparator: Comparator<Node>) {
    private val queues = Collections.nCopies(4 * n, PriorityBlockingQueue(comparator))
    private val random = Random()
    private fun getRandomIndex(): Int = random.nextInt(n)

    private inner class PriorityBlockingQueue(comparator: Comparator<Node>) {
        val pq = PriorityQueue(comparator)
        val lock = ReentrantLock(true)

        fun peek(): Node? {
            return pq.peek()
        }
    }

    fun poll(): Node? {
        while (true) {
            val firstIndex = getRandomIndex()
            val secondIndex = getRandomIndex()
            val first = queues[firstIndex]
            val second = queues[secondIndex]

            if (first.lock.tryLock()) {
                try {
                    return if (second.lock.tryLock()) {
                        try {
                            minOf(
                                first,
                                second,
                                compareBy(nullsLast(comparator), PriorityBlockingQueue::peek)
                            ).pq.poll()
                        } finally {
                            second.lock.unlock()
                        }
                    } else {
                        first.pq.poll()
                    }
                } finally {
                    first.lock.unlock()
                }
            }
        }
    }


    fun add(node: Node) {
        while (true) {
            val index = getRandomIndex()
            val blockingQueue = queues[index]

            if (blockingQueue.lock.tryLock()) {
                try {
                    blockingQueue.pq.add(node)
                    return
                } finally {
                    blockingQueue.lock.unlock()
                }
            }
        }
    }
}