package solution

import internal.Environment
import java.util.*

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Pavel Sharaev
 */
class ProcessPhilosophers(private val env: Environment) : MutexProcess {
    private var wantLock = false
    private var isLocked = false
    private val fork = Array(env.numberOfProcesses + 1) { pid ->
        if (pid < env.processId) Fork.UNKNOWN else Fork.DIRTY
    }
    private val wantFork = BooleanArray(env.numberOfProcesses + 1)

    init {
        fork[0] = Fork.CLEAR
    }

    override fun onMessage(sourcePid: Int, message: Any) {
        onMessage(sourcePid, message as Message)
    }

    private fun onMessage(sourcePid: Int, type: Message) {
        when (type) {
            Message.REQUEST -> {
                if (fork[sourcePid] == Fork.DIRTY) {
                    fork[sourcePid] = Fork.UNKNOWN
                    send(sourcePid, Message.OK)
                    if (wantLock) {
                        send(sourcePid, Message.REQUEST)
                    }
                } else {
                    wantFork[sourcePid] = true
                }
            }

            Message.OK -> {
                fork[sourcePid] = Fork.CLEAR
                tryGetLock()
            }
        }
    }

    override fun onLockRequest() {
        wantLock = true
        tryGetLock()
        broadcast { pid ->
            if (fork[pid] == Fork.UNKNOWN) {
                send(pid, Message.REQUEST)
            }
        }
    }

    override fun onUnlockRequest() {
        env.unlock()
        isLocked = false

        broadcast { pid ->
            if (wantFork[pid]) {
                wantFork[pid] = false
                fork[pid] = Fork.UNKNOWN

                send(pid, Message.OK)
            } else {
                fork[pid] = Fork.DIRTY
            }
        }
    }

    private fun tryGetLock() {
        if (isLocked) return
        if (fork.any { it == Fork.UNKNOWN }) return

        Arrays.fill(fork, Fork.CLEAR)

        wantLock = false
        isLocked = true
        env.lock()
    }

    private fun broadcast(operation: (Int) -> Unit) {
        for (pid in 1..env.numberOfProcesses) {
            if (pid != env.processId) {
                operation(pid)
            }
        }
    }

    private fun send(destinationPid: Int, type: Message) {
        env.send(destinationPid, type)
    }

    private enum class Message {
        REQUEST, OK
    }

    private enum class Fork {
        CLEAR, DIRTY, UNKNOWN
    }
}