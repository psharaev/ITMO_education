package mpp.dynamicarray

import kotlinx.atomicfu.*

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = atomic(Core<E>(INITIAL_CAPACITY))
    private val _size = atomic(0)

    override val size: Int
        get() = _size.value

    override fun get(index: Int): E {
        require(index < _size.value)
        while (true) {
            val curCore = core.value
            when (val wrapper = curCore.get(index)) {
                is Value -> return wrapper.elem
                is Fixed -> return wrapper.elem
                else -> {}
            }
            moveData()
        }
    }

    override fun put(index: Int, element: E) {
        require(index < _size.value)
        while (true) {
            val curCore = core.value
            val wrapper = curCore.get(index)
            if ((wrapper === null || wrapper is Value) &&
                curCore.cas(index, wrapper, Value(element))
            ) {
                return
            }
            moveData()
        }
    }

    override fun pushBack(element: E) {
        while (true) {
            val index = _size.value
            ensureCapacity(index + 1)
            when (core.value.get(index)) {
                null -> {
                    if (core.value.cas(index, null, Value(element))) {
                        casIncrementSize(index)
                        return
                    }
                }

                is Moved -> {}

                is Fixed -> {
                    casIncrementSize(index)
                }

                is Value -> {
                    casIncrementSize(index)
                }
            }

            moveData()
        }
    }

    private fun ensureCapacity(capacity: Int) {
        while (core.value.capacity < capacity) {
            val cur = core.value
            if (cur.hasNext()) {
                moveData()
            } else {
                val newSize = if (2 * cur.capacity < capacity) 2 * capacity else 2 * cur.capacity
                if (cur.casNewNext(newSize)) {
                    moveData()
                    break
                }
            }
        }
    }

    private fun casIncrementSize(expected: Int): Boolean {
        return _size.compareAndSet(expected, expected + 1)
    }

    private fun moveData() {
        val curr = core.value
        val next = curr.next.value
        if (next === null) {
            return
        }
        (0 until curr.capacity).forEach { i ->
            while (true) {
                val oldRef = curr.array[i]
                val successReplace = when (val oldVal = oldRef.value) {
                    null -> oldRef.compareAndSet(null, Moved())

                    is Value -> {
                        val res = oldRef.compareAndSet(oldVal, Fixed(oldVal.elem))
                        if (res) {
                            next.array[i].compareAndSet(null, Value(oldVal.elem))
                        }
                        res
                    }

                    is Moved -> {
                        true
                    }

                    is Fixed -> {
                        next.array[i].compareAndSet(null, Value(oldVal.elem))
                        true
                    }
                }
                if (successReplace) {
                    break
                }
            }
        }
        core.compareAndSet(curr, next)
    }
}

private sealed interface Wrapper<E>

/**
 * Value only for read
 */
private class Fixed<E>(val elem: E) : Wrapper<E>

/**
 * Value not for read/write
 */
private class Moved<E> : Wrapper<E>

/**
 * Value for read/write
 */
private class Value<E>(val elem: E) : Wrapper<E>

private class Core<E>(val capacity: Int) {
    val array = atomicArrayOfNulls<Wrapper<E>>(capacity)
    val next = atomic<Core<E>?>(null)

    fun get(index: Int): Wrapper<E>? =
        array[index].value

    fun hasNext(): Boolean {
        return next.value !== null
    }

    fun cas(index: Int, expect: Wrapper<E>?, update: Wrapper<E>?): Boolean {
        return array[index].compareAndSet(expect, update)
    }

    fun casNewNext(capacity: Int): Boolean {
        return next.compareAndSet(null, Core(capacity))
    }
}
