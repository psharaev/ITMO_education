import org.jetbrains.kotlinx.lincheck.*
import org.jetbrains.kotlinx.lincheck.annotations.*
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.paramgen.*
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.*
import org.jetbrains.kotlinx.lincheck.strategy.stress.*
import org.jetbrains.kotlinx.lincheck.verifier.*
import org.junit.*
import java.io.*

@Param.Params(
    Param(name = "index", gen = IntGen::class, conf = "0:4"),
)
class AtomicArrayNoAbaTest {
    private val a = AtomicArrayNoAba(5, 0)

    @Operation(params = ["index"])
    fun get(index: Int) =
        a.get(index)

    @Operation(params = ["index"])
    fun inc(index: Int) {
        while (true) {
            val value = a.get(index)
            if (a.cas(index, value, value + 1)) break
        }
    }

    @Operation(params = ["index", "index"])
    fun inc(index1: Int, index2: Int) {
        while (true) {
            val value1 = a.get(index1)
            val value2 = a.get(index2)
            if (a.cas2(index1, value1, value1 + 1, index2, value2, value2 + 1)) return
        }
    }

    @Test
    fun stressTest() = try {
        StressOptions()
            .iterations(100)
            .invocationsPerIteration(10_000)
            .actorsBefore(0)
            .actorsAfter(0)
            .threads(3)
            .actorsPerThread(3)
            .sequentialSpecification(AtomicArrayIntSequential::class.java)
            .check(this::class.java)
    } catch (t: Throwable) {
        throw t
    }

    @Test
    fun modelCheckingTest() = try {
        ModelCheckingOptions()
            .iterations(100)
            .invocationsPerIteration(10_000)
            .actorsBefore(0)
            .actorsAfter(0)
            .threads(3)
            .actorsPerThread(3)
            .sequentialSpecification(AtomicArrayIntSequential::class.java)
            .checkObstructionFreedom(true)
            .check(this::class.java)
    } catch (t: Throwable) {
        throw t
    }
}

class AtomicArrayIntSequential : VerifierState() {
    private val a = IntArray(5)

    fun get(index: Int): Int? = a[index]

    fun inc(index: Int) {
        a[index]++
    }

    fun inc(index1: Int, index2: Int) {
        a[index1]++
        a[index2]++
    }
    override fun extractState() = a.toList()
}