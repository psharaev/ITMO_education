import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.check
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.ModelCheckingOptions
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressOptions
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState
import org.junit.Test
import java.io.File
import java.util.*

class FCPriorityQueueTest {
    private val q = FCPriorityQueue<Int>()

    @Operation
    fun poll(): Int? = q.poll()

    @Operation
    fun peek(): Int? = q.peek()

    @Operation
    fun add(element: Int): Unit = q.add(element)

    @Test
    fun modelCheckingTest() = try {
        ModelCheckingOptions()
            .iterations(100)
            .invocationsPerIteration(10_000)
            .threads(3)
            .actorsPerThread(3)
            .sequentialSpecification(PriorityQueueSequential::class.java)
            .check(this::class.java)
    } catch (t: Throwable) {
        throw t
    }

    @Test
    fun stressTest() = try {
        StressOptions()
            .iterations(100)
            .invocationsPerIteration(50_000)
            .threads(3)
            .actorsPerThread(3)
            .sequentialSpecification(PriorityQueueSequential::class.java)
            .check(this::class.java)
    } catch (t: Throwable) {
        throw t
    }
}

class PriorityQueueSequential : VerifierState() {
    private val q = PriorityQueue<Int>()

    fun poll(): Int? = q.poll()
    fun peek(): Int? = q.peek()
    fun add(element: Int) {
        q.add(element)
    }

    override fun extractState() = ArrayList(q)
}