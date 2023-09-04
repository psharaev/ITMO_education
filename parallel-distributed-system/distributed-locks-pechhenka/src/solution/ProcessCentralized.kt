package solution

import internal.Environment
import java.util.*

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Pavel Sharaev
 */
class ProcessCentralized(private val env: Environment) : MutexProcess {
    private val processes: Queue<Int>?
    private val isCoordinator: Boolean = env.processId == 1
    private var isLocked = false

    init {
        this.processes = if (isCoordinator) {
            ArrayDeque()
        } else {
            null
        }
    }

    override fun onMessage(sourcePid: Int, message: Any) {
        onMessage(sourcePid, message as Message)
    }

    private fun onMessage(sourcePid: Int, message: Message) {
        when (message) {
            Message.REQUEST -> {
                assertCoordinator()
                if (isLocked) {
                    processes!!.add(sourcePid)
                    return
                }
                if (processes!!.isEmpty()) {
                    coordinatorSendLock(sourcePid)
                } else {
                    processes.add(sourcePid)
                }
            }

            Message.OK -> {
                env.lock()
                // Critical section
            }

            Message.RELEASE -> {
                assertCoordinator()
                isLocked = false
                if (!processes!!.isEmpty()) {
                    coordinatorSendLock(processes.poll())
                }
            }
        }
    }

    override fun onLockRequest() {
        send(1, Message.REQUEST)
    }

    override fun onUnlockRequest() {
        env.unlock()
        send(1, Message.RELEASE)
    }

    private fun coordinatorSendLock(destinationPid: Int) {
        assertCoordinator()
        isLocked = true
        send(destinationPid, Message.OK)
    }

    private fun send(destinationPid: Int, message: Message) {
        if (env.processId == destinationPid) {
            onMessage(destinationPid, message)
        } else {
            env.send(destinationPid, message)
        }
    }

    private fun assertCoordinator() {
        check(isCoordinator) { "not a coordinator" }
    }

    private enum class Message {
        REQUEST, OK, RELEASE
    }
}