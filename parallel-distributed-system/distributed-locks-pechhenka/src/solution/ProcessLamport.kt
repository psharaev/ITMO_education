package solution

import internal.Environment
import kotlin.math.max

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Pavel Sharaev
 */
class ProcessLamport(private val env: Environment) : MutexProcess {
    private var lamportTime = 0
    private var isLocked = false
    private val requestTimes = IntArray(env.numberOfProcesses + 1) { INF }
    private val okTimes = IntArray(env.numberOfProcesses + 1)
    private val pidLamportComparator = Comparator.comparingInt<Int> { requestTimes[it] }
            .thenComparingInt { it }

    override fun onMessage(sourcePid: Int, message: Any) {
        onMessage(sourcePid, message as Message)
    }

    private fun onMessage(sourcePid: Int, message: Message) {
        lamportTime = max(lamportTime, message.lamportTime) + 1
        when (message.type) {
            REQUEST -> {
                requestTimes[sourcePid] = message.requestTime
                send(sourcePid, OK, -1)
            }

            OK -> {
                okTimes[sourcePid] = message.lamportTime
            }

            RELEASE -> {
                requestTimes[sourcePid] = INF
            }
        }
        tryGetLock()
    }

    override fun onLockRequest() {
        val requestTime = ++lamportTime
        requestTimes[env.processId] = requestTime
        broadcast(REQUEST, requestTime)
        tryGetLock()
    }

    private fun tryGetLock() {
        val myPid = env.processId
        val requestTime = requestTimes[myPid]
        if (requestTime == INF || isLocked) return
        for (pid in 1..env.numberOfProcesses) {
            if (pid != myPid) {
                if (pidLamportComparator.compare(pid, myPid) < 0) return
                if (okTimes[pid] <= requestTime) return
            }
        }
        isLocked = true
        env.lock()
    }

    override fun onUnlockRequest() {
        env.unlock()
        isLocked = false
        requestTimes[env.processId] = INF
        broadcast(RELEASE, -1)
    }

    private fun broadcast(type: MessageType, requestTime: Int) {
        for (pid in 1..env.numberOfProcesses) {
            if (pid != env.processId) {
                send(pid, type, requestTime)
            }
        }
    }

    private fun send(destinationPid: Int, type: MessageType, requestTime: Int) {
        env.send(destinationPid, Message(++lamportTime, type, requestTime))
    }
}

private const val INF = Int.MAX_VALUE
