package mpp.linkedlistset

import org.jetbrains.kotlinx.lincheck.*
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.*
import org.jetbrains.kotlinx.lincheck.strategy.stress.*
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState
import org.junit.*
import java.io.File

class LinkedListSetTest {
    private val s = LinkedListSet<Int>()

    @Operation
    fun add(element: Int): Boolean = s.add(element)

    @Operation
    fun remove(element: Int): Boolean = s.remove(element)

    @Operation
    fun contains(element: Int): Boolean = s.contains(element)

    @Test
    fun modelCheckingTest() = try {
        ModelCheckingOptions()
            .iterations(100)
            .invocationsPerIteration(10_000)
            .threads(3)
            .actorsPerThread(3)
            .checkObstructionFreedom()
            .sequentialSpecification(IntSetSequential::class.java)
            .check(this::class.java)
    } catch (t: Throwable) {
        throw t
    }

    @Test
    fun stressTest() = try {
        StressOptions()
            .iterations(100)
            .invocationsPerIteration(10_000)
            .threads(3)
            .actorsPerThread(3)
            .sequentialSpecification(IntSetSequential::class.java)
            .check(this::class.java)
    } catch (t: Throwable) {
        throw t
    }
}

class IntSetSequential : VerifierState() {
    private val s = HashSet<Int>()

    fun add(element: Int): Boolean = s.add(element)

    fun remove(element: Int): Boolean = s.remove(element)

    fun contains(element: Int): Boolean = s.contains(element)

    override fun extractState() = s
}