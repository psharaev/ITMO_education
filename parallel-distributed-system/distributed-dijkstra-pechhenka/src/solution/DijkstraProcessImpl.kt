package solution

import internal.Environment

/**
 * Distributed Dijkstra algorithm implementation.
 * All functions are called from the single main thread.
 *
 * @author Pavel Sharaev
 */
class DijkstraProcessImpl(private val env: Environment) : DijkstraProcess {
    private var balance = 0
    private var childs = 0
    private var parentId = -1
    private var dist = Long.MAX_VALUE
    private var isRed = false
    private var isRoot = false

    override fun onMessage(srcPid: Int, msg: Any) {
        when (msg) {
            is Confirm -> balance--
            is RegisterChild -> childs++
            is RemoveChild -> childs--

            is Distance -> {
                if (!isRed) {
                    isRed = true
                    parentId = srcPid
                    env.send(srcPid, RegisterChild)
                }
                if (dist > msg.distance) {
                    dist = msg.distance
                    updateNeighbours()
                }
                env.send(srcPid, Confirm)
            }
        }
        tryFinish()
    }

    override fun getDistance(): Long? {
        return if (dist == Long.MAX_VALUE) {
            null
        } else {
            dist
        }
    }

    override fun onComputationStart() {
        dist = 0
        isRed = true
        isRoot = true
        updateNeighbours()
        tryFinish()
    }

    private fun updateNeighbours() {
        for ((id, edgeDist) in env.neighbours) {
            if (env.processId == id) {
                continue
            }
            env.send(id, Distance(dist + edgeDist))
            balance++
        }
    }

    private fun tryFinish() {
        if (childs == 0 && balance == 0) {
            if (isRoot) {
                env.finishExecution()
            } else {
                isRed = false
                env.send(parentId, RemoveChild)
                parentId = -1
            }
        }
    }
}