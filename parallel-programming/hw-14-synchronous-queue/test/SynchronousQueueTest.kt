import org.jetbrains.kotlinx.lincheck.*
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.*
import org.jetbrains.kotlinx.lincheck.strategy.stress.*
import org.jetbrains.kotlinx.lincheck.verifier.*
import org.junit.*
import java.io.*
import kotlin.coroutines.*

class SynchronousQueueTest {
    private val q = SynchronousQueue<Int>()

    @Operation(cancellableOnSuspension = false)
    suspend fun send(element: Int) {
        q.send(element)
    }

    @Operation(cancellableOnSuspension = false)
    suspend fun receive(): Int = q.receive()

    @Test
    fun modelCheckingTest() = try {
        ModelCheckingOptions()
            .iterations(100)
            .invocationsPerIteration(10_000)
            .actorsBefore(0)
            .threads(3)
            .actorsPerThread(3)
            .actorsAfter(0)
            .checkObstructionFreedom()
            .sequentialSpecification(SynchronousQueueSequential::class.java)
            .check(this::class.java)
    } catch (t: Throwable) {
        throw t
    }

    @Test
    fun stressTest() = try {
        StressOptions()
            .iterations(100)
            .invocationsPerIteration(50_000)
            .actorsBefore(0)
            .threads(3)
            .actorsPerThread(3)
            .actorsAfter(0)
            .sequentialSpecification(SynchronousQueueSequential::class.java)
            .check(this::class.java)
    } catch (t: Throwable) {
        throw t
    }
}

class SynchronousQueueSequential : VerifierState() {
    private val senders = ArrayList<Pair<Continuation<Unit>, Int>>() // pair = continuation + element
    private val receivers = ArrayList<Continuation<Int>>()

    suspend fun send(element: Int) {
        if (receivers.isNotEmpty()) {
            val r = receivers.removeAt(0)
            r.resume(element)
        } else {
            suspendCoroutine<Unit> { cont ->
                senders.add(cont to element)
            }
        }
    }

    suspend fun receive(): Int {
        if (senders.isNotEmpty()) {
            val (s, elem) = senders.removeAt(0)
            s.resume(Unit)
            return elem
        } else {
            return suspendCoroutine { cont ->
                receivers.add(cont)
            }
        }
    }

    override fun extractState() = Unit
}
