import java.util.*
import kotlinx.atomicfu.*
import java.util.concurrent.*


class FCPriorityQueue<E : Comparable<E>> {
    private val q = PriorityQueue<E>()

    inner class Query<E>(val op: () -> E?) {
        var result: E? = null
        var isApplied = false
        fun apply() {
            result = op()
            isApplied = true
        }
    }

    private val locked = atomic(false)
    private val queries = atomicArrayOfNulls<Query<E>>(BUFFER_ARRAY_SIZE)

    private fun insertQuery(query: Query<E>) {
        var index: Int
        do {
            index = ThreadLocalRandom.current().nextInt(0, BUFFER_ARRAY_SIZE)
        } while (!queries[index].compareAndSet(null, query))
    }

    private fun apply(op: () -> E?): E? {
        val query = Query(op)
        insertQuery(query)
        while (!query.isApplied) {
            if (locked.compareAndSet(false, true)) {
                for (i in 0 until BUFFER_ARRAY_SIZE) {
                    val q = queries[i].value
                    if (q === null || q.isApplied) {
                        continue
                    }
                    q.apply()
                    queries[i].getAndSet(null)
                }
                locked.compareAndSet(true, false)
            }
        }
        return query.result
    }


    /**
     * Retrieves the element with the highest priority
     * and returns it as the result of this function;
     * returns `null` if the queue is empty.
     */
    fun poll(): E? {
        return apply { q.poll() }
    }

    /**
     * Returns the element with the highest priority
     * or `null` if the queue is empty.
     */
    fun peek(): E? {
        return apply { q.peek() }
    }


    /**
     * Adds the specified element to the queue.
     */
    fun add(element: E) {
        apply {
            q.add(element)
            null
        }
    }
}

private val BUFFER_ARRAY_SIZE = Runtime.getRuntime().availableProcessors()