import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

class Solution(private val env: Environment) : Lock<Solution.Node> {
    private val tail = AtomicReference<Node?>(null)

    override fun lock(): Node {
        val node = Node()
        val prev = tail.getAndSet(node) ?: return node

        prev.next.value = node
        while (node.locked.get()) {
            env.park()
        }

        return node
    }

    override fun unlock(node: Node) {
        if (node.next.value == null) {
            if (tail.compareAndSet(node, null)) {
                return
            }
            while (node.next.value == null) {
            }
        }

        val next = node.next.value ?: return
        next.locked.value = false
        env.unpark(next.thread)
    }

    class Node {
        val thread: Thread = Thread.currentThread()

        val locked = AtomicReference<Boolean>(true)
        val next = AtomicReference<Node?>(null)
    }
}