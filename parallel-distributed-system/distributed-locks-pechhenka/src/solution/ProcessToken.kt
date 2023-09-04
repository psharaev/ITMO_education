package solution

import internal.Environment

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Pavel Sharaev
 */
class ProcessToken(private val env: Environment) : MutexProcess {
    private var status = Status.FREE
    private val nextId = env.processId % env.numberOfProcesses + 1

    init {
        if (env.processId == 1) env.send(nextId, 1)
    }

    override fun onMessage(sourcePid: Int, message: Any) {
        if (status == Status.REQUEST) {
            status = Status.LOCK
            env.lock()
        } else {
            env.send(nextId, message)
        }
    }

    override fun onLockRequest() {
        status = Status.REQUEST
        if (env.numberOfProcesses == 1) {
            status = Status.LOCK
            env.lock()
        }
    }

    override fun onUnlockRequest() {
        status = Status.FREE
        env.unlock()
        env.send(nextId, 1)
    }

    private enum class Status { REQUEST, FREE, LOCK }
}