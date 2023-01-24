import kotlinx.atomicfu.AtomicIntArray
import kotlinx.atomicfu.atomic

/**
 * Int-to-Int hash map with open addressing and linear probes.
 */
class IntIntHashMap {
    private val core = atomic(Core(INITIAL_CAPACITY))

    /**
     * Returns value for the corresponding key or zero if this key is not present.
     *
     * @param key a positive key.
     * @return value for the corresponding or zero if this key is not present.
     * @throws IllegalArgumentException if key is not positive.
     */
    operator fun get(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        while (true) {
            val curCore = core.value
            val wrapper = curCore.getInternal(key)
            if (!isMoved(wrapper)) {
                return wrapper
            }
            moveData()
        }
    }

    /**
     * Changes value for the corresponding key and returns old value or zero if key was not present.
     *
     * @param key   a positive key.
     * @param value a positive value.
     * @return old value or zero if this key was not present.
     * @throws IllegalArgumentException if key or value are not positive, or value is equal to
     * [Integer.MAX_VALUE] which is reserved.
     */
    fun put(key: Int, value: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        require(isValue(value)) { "Invalid value: $value" }
        return putImpl(key, value)
    }

    /**
     * Removes value for the corresponding key and returns old value or zero if key was not present.
     *
     * @param key a positive key.
     * @return old value or zero if this key was not present.
     * @throws IllegalArgumentException if key is not positive.
     */
    fun remove(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        return putImpl(key, DEL_VALUE)
    }

    private fun putImpl(key: Int, value: Int): Int {
        while (true) {
            val curCore = core.value
            val old = curCore.putInternal(key, value)
            if (old == NEEDS_REHASH) {
                rehash()
                continue
            }
            if (old == NEEDS_HELP_MOVE) {
                moveData()
                continue
            }
            return toValue(old)
        }
    }

    private fun rehash() {
        val cur = core.value
        while (cur === core.value) {
            if (cur.hasNext()) {
                moveData()
                break
            } else if (cur.casNewNext(2 * cur.capacity)) {
                moveData()
                break
            }
        }
    }

    private fun moveData(): Int {
        val curr = core.value
        val next = curr.next.value
        if (next === null) {
            return -1
        }
        var index = 0
        val mapSize = curr.map.size
        while (index < mapSize) {
            while (true) {
                val refKey = curr.map[index]
                val value = curr.getByIndex(index + 1)
                val success = when {
                    isValue(value) -> {
                        val res = curr.cas(index + 1, value, toFixed(value))
                        if (res) {
                            next.lazyPut(refKey.value, value)
                        }
                        res
                    }

                    value == NULL_VALUE || value == DEL_VALUE -> {
                        curr.cas(index + 1, value, MOVED_VALUE)
                    }

                    isFixed(value) -> {
                        next.lazyPut(refKey.value, toValue(value))
                        true
                    }

                    else -> {
                        true
                    }
                }
                if (success) {
                    break
                }
            }
            index += 2
        }
        return if (core.compareAndSet(curr, next)) 1 else 0
    }

    private class Core internal constructor(val capacity: Int) {
        // Pairs of <key, value> here, the actual
        // size of the map is twice as big.
        val map: AtomicIntArray = AtomicIntArray(2 * capacity)
        val shift: Int

        val next = atomic<Core?>(null)

        init {
            val mask = capacity - 1
            assert(mask > 0 && mask and capacity == 0) { "Capacity must be power of 2: $capacity" }
            shift = 32 - Integer.bitCount(mask)
        }

        fun hasNext(): Boolean {
            return next.value !== null
        }

        fun cas(index: Int, expect: Int, update: Int): Boolean {
            return map[index].compareAndSet(expect, update)
        }

        fun casNewNext(capacity: Int): Boolean {
            return next.compareAndSet(null, Core(capacity))
        }

        fun getByIndex(index: Int): Int =
            map[index].value

        fun getInternal(key: Int): Int {
            var index = calcHash(key)
            var probes = 0
            while (true) { // optimize for successful lookup
                val k = map[index].value
                if (k == key) {
                    return toValue(map[index + 1].value)
                }

                if (k == NULL_KEY) return NULL_VALUE // not found -- no value
                if (++probes >= MAX_PROBES) return NULL_VALUE
                if (index == 0) index = map.size
                index -= 2
            }
        }

        fun putInternal(key: Int, value: Int): Int {
            var index = calcHash(key)
            var probes = 0
            while (true) { // optimize for successful lookup
                val k = map[index].value
                if (k == key) {
                    return injectValue(index + 1, value)
                }
                if (k == NULL_KEY) {
                    if (value == DEL_VALUE) {
                        return NULL_VALUE
                    }
                    if (cas(index, k, key)) {
                        return injectValue(index + 1, value)
                    } else if (map[index].value == key) {
                        return injectValue(index + 1, value)
                    }
                }
                if (++probes >= MAX_PROBES) {
                    if (value == DEL_VALUE) {
                        return NULL_VALUE
                    }
                    return NEEDS_REHASH
                }
                if (index == 0) index = map.size
                index -= 2
            }
        }

        fun lazyPut(key: Int, value: Int): Int {
            var index = calcHash(key)
            var probes = 0
            while (true) { // optimize for successful lookup
                val k = map[index].value
                if (k == key) {
                    cas(index + 1, NULL_VALUE, value)
                    return NULL_VALUE
                }
                if (k == NULL_KEY) {
                    if (cas(index, k, key)) {
                        cas(index + 1, NULL_VALUE, value)
                        return NULL_VALUE
                    } else if (map[index].value == key) {
                        cas(index + 1, NULL_VALUE, value)
                        return NULL_VALUE
                    }
                }
                if (++probes >= MAX_PROBES) {
                    if (value == DEL_VALUE) {
                        return NULL_VALUE
                    }
                    return NEEDS_REHASH
                }
                if (index == 0) index = map.size
                index -= 2
            }
        }

        private fun injectValue(indexValue: Int, value: Int): Int {
            while (true) {
                val oldVal = map[indexValue].value
                if (isMoved(oldVal) || isFixed(oldVal)) {
                    return NEEDS_HELP_MOVE
                }
                if (cas(indexValue, oldVal, value)) {
                    return toValue(oldVal)
                }
            }
        }

        /**
         * Returns an initial index in map to look for a given key.
         */
        fun calcHash(key: Int): Int = (key * MAGIC ushr shift) * 2
    }
}

private const val MAGIC = -0x61c88647 // golden ratio
private const val INITIAL_CAPACITY = 2 // !!! DO NOT CHANGE INITIAL CAPACITY !!!
private const val MAX_PROBES = 8 // max number of probes to find an item

private const val NULL_KEY = 0 // missing key (initial value)

private const val NULL_VALUE = 0 // missing value (initial value)
private const val DEL_VALUE = Int.MAX_VALUE // mark for removed value
private const val MOVED_VALUE = Int.MIN_VALUE
private const val NEEDS_REHASH = -1
private const val NEEDS_HELP_MOVE = -2

// Checks is the value is in the range of allowed values
private fun isValue(value: Int): Boolean = value in (1 until DEL_VALUE)
private fun isFixed(value: Int): Boolean = -DEL_VALUE < value && value <= -1
private fun isMoved(value: Int): Boolean = value == MOVED_VALUE
private fun isDeleted(value: Int): Boolean = value == DEL_VALUE

// Converts internal value to the public results of the methods
private fun toFixed(value: Int): Int = -value
private fun toValue(value: Int): Int =
    if (isValue(value)) {
        value
    } else if (isFixed(value)) {
        -value
    } else if (isDeleted(value) || value == NULL_VALUE) {
        NULL_VALUE
    } else { // is moved
        MOVED_VALUE
    }