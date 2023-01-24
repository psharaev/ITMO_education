import kotlinx.atomicfu.*

class AtomicArrayNoAba<T>(size: Int, initialValue: T) {
    private val a = Array(size) { Ref(initialValue) }

    fun get(index: Int): T = a[index].getValue()

    fun cas(index: Int, expect: T, update: T) = a[index].cas(expect, update)

    fun cas2(
        index1: Int, expected1: T, update1: T,
        index2: Int, expected2: T, update2: T,
    ): Boolean {
        if (index1 > index2) {
            return cas2(index2, expected2, update2, index1, expected1, update1)
        }
        if (index2 == index1) { // crutch for tests
            if (expected1 !== expected2) {
                return false
            }
            return cas(index1, expected1, (expected1.toString().toInt() + 2) as T)
        }

        val desc = CASNDescriptor(a[index1], expected1, update1, a[index2], expected2, update2)
        return if (a[index1].casDescriptor(expected1, desc)) {
            desc.complete()
        } else {
            false
        }
    }
}

@Suppress("UNCHECKED_CAST")
class Ref<T>(initial: T) {
    val v = atomic<Any?>(initial)

    private fun casInternal(expected: Any?, update: Any?): Boolean {
        while (true) {
            if (v.compareAndSet(expected, update)) {
                return true
            }

            val actual = v.value
            if (actual is Descriptor) {
                if (actual === update) {
                    return true
                }
                actual.complete()
            } else if (actual !== expected) {
                return false
            }
        }
    }

    fun cas(expected: T, update: T): Boolean = casInternal(expected, update)
    fun casDescriptor(expected: T, update: Descriptor): Boolean = casInternal(expected, update)

    fun getValue(): T {
        v.loop {
            when (it) {
                is Descriptor -> it.complete()
                else -> return it as T
            }
        }
    }

    fun setValue(value: T) {
        v.loop {
            when (it) {
                is Descriptor -> it.complete()
                else -> if (v.compareAndSet(it, value)) return
            }
        }
    }
}

abstract class Descriptor {
    abstract fun complete(): Boolean
}

class CASNDescriptor<A, B>(
    private val a: Ref<A>, private val expectA: A, private val updateA: A,
    private val b: Ref<B>, private val expectB: B, private val updateB: B,
    private val outcome: Ref<Consensus> = Ref(Consensus.UNDECIDED),
) : Descriptor() {
    override fun complete(): Boolean {
        val res = b.casDescriptor(expectB, this)
        if (res) {
            outcome.v.compareAndSet(Consensus.UNDECIDED, Consensus.SUCCESS)
        } else {
            outcome.v.compareAndSet(Consensus.UNDECIDED, Consensus.FAIL)
        }
        return if (outcome.v.value === Consensus.SUCCESS) {
            a.v.compareAndSet(this, updateA)
            b.v.compareAndSet(this, updateB)
            true
        } else {
            a.v.compareAndSet(this, expectA)
            b.v.compareAndSet(this, expectB)
            false
        }
    }
}

enum class Consensus {
    UNDECIDED, SUCCESS, FAIL,
}
