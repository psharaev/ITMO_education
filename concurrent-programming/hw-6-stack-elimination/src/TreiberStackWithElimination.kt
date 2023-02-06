package mpp.stackWithElimination

import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.atomicArrayOfNulls
import java.util.*


class TreiberStackWithElimination<E> {
    private val top = atomic<Node<E>?>(null)
    private val eliminationArray = atomicArrayOfNulls<Any?>(ELIMINATION_ARRAY_SIZE)

    private val randomGenerator: Random = Random()
    private fun getRandomIndex(): Int = randomGenerator.nextInt(ELIMINATION_ARRAY_SIZE)

    /**
     * Adds the specified element [x] to the stack.
     */
    fun push(x: E) {
        for (offset in 0 until MAX_COUNT_PUSH) {
            val randomIndex = getRandomIndex()
            val wrapped = eliminationArray[randomIndex]
            if (wrapped.compareAndSet(null, x)) {
                for (searchIteration in 0 until MAX_COUNT_FINISH_PUSH) {
                    val value = wrapped.value
                    if (value == null || wrapped.compareAndSet(DONE, null)) {
                        return
                    }
                }

                if (wrapped.compareAndSet(x, null)) {
                    break
                }

                return
            }
        }

        // Basic push lock free algorithm
        return pushOnTop(x)
    }

    private fun pushOnTop(x: E) {
        do {
            val curTop = top.value
            val newTop = Node(x, curTop)
        } while (!top.compareAndSet(curTop, newTop))
    }

    /**
     * Retrieves the first element from the stack
     * and returns it; returns `null` if the stack
     * is empty.
     */
    @Suppress("UNCHECKED_CAST")
    fun pop(): E? {
        for (offset in 0 until MAX_COUNT_POP) {
            val randomIndex = getRandomIndex()
            val wrapped = eliminationArray[randomIndex]
            val value = wrapped.value
            if (value != null && value != DONE && wrapped.compareAndSet(value, DONE)) {
                return value as E?
            }
        }

        return popFromTop()
    }

    private fun popFromTop(): E? {
        while (true) {
            val curTop = top.value ?: return null
            val newTop = curTop.next
            if (top.compareAndSet(curTop, newTop)) {
                return curTop.x
            }
        }
    }
}

private class Node<E>(val x: E, val next: Node<E>?)

private val DONE = object {}

private const val MAX_COUNT_POP = 8
private const val MAX_COUNT_PUSH = 8
private const val MAX_COUNT_FINISH_PUSH = 8

private const val ELIMINATION_ARRAY_SIZE = 2 // DO NOT CHANGE IT