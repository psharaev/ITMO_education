import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine

/**
 * An element is transferred from sender to receiver only when [send] and [receive]
 * invocations meet in time (rendezvous), so [send] suspends until another coroutine
 * invokes [receive] and [receive] suspends until another coroutine invokes [send].
 */
class SynchronousQueue<E> {
    private val head: AtomicRef<Node<E>>
    private val tail: AtomicRef<Node<E>>

    init {
        val dummy = Node<E>(null, null)
        head = atomic(dummy)
        tail = atomic(dummy)
    }

    /**
     * Sends the specified [element] to this channel, suspending if there is no waiting
     * [receive] invocation on this channel.
     */
    suspend fun send(element: E) {
        while (true) {
            val curHead = head.value
            val curTail = tail.value
            if (curHead === curTail || curTail.isSend()) {
                if (suspendAndWait(curTail, Node(Type.SEND, element))) {
                    return
                }
            } else {
                val curNext = curHead.next.value ?: continue
                if (head.compareAndSet(curHead, curNext)) {
                    curNext.element = element
                    curNext.continuation!!.resume(true)
                    return
                }
            }
        }
    }

    /**
     * Retrieves and removes an element from this channel if there is a waiting [send] invocation on it,
     * suspends the caller if this channel is empty.
     */
    suspend fun receive(): E {
        while (true) {
            val curHead = head.value
            val curTail = tail.value
            if (curHead === curTail || curTail.isReceive()) {
                val node = Node<E>(Type.RECEIVE, null)
                if (suspendAndWait(curTail, node)) {
                    return node.element!!
                }
            } else {
                val curNext = curHead.next.value ?: continue
                if (head.compareAndSet(curHead, curNext)) {
                    curNext.continuation!!.resume(true)
                    return curNext.element!!
                }
            }
        }
    }

    private suspend fun suspendAndWait(curTail: Node<E>, node: Node<E>): Boolean {
        return suspendCoroutine {
            node.continuation = it
            if (curTail.next.compareAndSet(null, node)) {
                tail.compareAndSet(curTail, node)
            } else {
                tail.compareAndSet(curTail, curTail.next.value!!)
                it.resume(false)
            }
        }
    }
}

private enum class Type {
    SEND, RECEIVE
}

private class Node<E>(val type: Type?, var element: E?) {
    var continuation: Continuation<Boolean>? = null

    val next: AtomicRef<Node<E>?> = atomic(null)

    fun isSend() = type === Type.SEND
    fun isReceive() = type === Type.RECEIVE
}