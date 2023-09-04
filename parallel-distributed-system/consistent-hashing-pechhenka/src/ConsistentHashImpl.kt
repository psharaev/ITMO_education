import java.util.*
import kotlin.collections.ArrayList
import kotlin.collections.HashMap
import kotlin.collections.HashSet

class ConsistentHashImpl<K> : ConsistentHash<K> {

    private val ring = HashMap<Int, Shard>()
    private val vnodes = ArrayList<Int>()

    override fun getShardByKey(key: K): Shard {
        return ring[upperBound(key.hashCode())] ?: throw NoSuchElementException()
    }

    override fun addShard(newShard: Shard, vnodeHashes: Set<Int>): Map<Shard, Set<HashRange>> {
        if (vnodes.isEmpty()) {
            insertVnodeHashes(newShard, vnodeHashes)
            return Collections.emptyMap()
        }

        val res = calcDiff(vnodeHashes)
        insertVnodeHashes(newShard, vnodeHashes)
        return res
    }

    override fun removeShard(shard: Shard): Map<Shard, Set<HashRange>> {
        val hashes = vnodes.filter { ring[it] == shard }
        vnodes.removeIf { ring[it] == shard }
        return calcDiff(hashes)
    }

    private fun calcDiff(vnodeHashes: Iterable<Int>): Map<Shard, Set<HashRange>> {
        val res = HashMap<Shard, MutableSet<HashRange>>()
        val ranges = HashMap<Int, Int>()

        for (vnodeHash in vnodeHashes) {
            val l = lowerBound(vnodeHash)
            ranges.merge(l, vnodeHash) { oldR, newR ->
                if (len(l, newR) > len(l, oldR)) newR else oldR
            }
        }

        for (range in ranges) {
            val l = range.key
            val r = range.value
            val set = res.computeIfAbsent(ring[upperBound(r)]!!) { HashSet() }
            set.add(HashRange(l + 1, r))
        }

        return res
    }

    private fun insertVnodeHashes(newShard: Shard, vnodeHashes: Iterable<Int>) {
        for (vnodeHash in vnodeHashes) {
            insertVnodeHash(vnodeHash)
            ring[vnodeHash] = newShard
        }
    }

    private fun insertVnodeHash(vnodeHash: Int) {
        val i = Collections.binarySearch(vnodes, vnodeHash)
        if (i < 0) {
            val j = -(i + 1)
            vnodes.add(j, vnodeHash)
        } else {
            vnodes.add(i, vnodeHash)
        }
    }

    private fun lowerBound(vnodeHash: Int): Int {
        val i = Collections.binarySearch(vnodes, vnodeHash)
        if (i < 0) {
            val j = -(i + 1)
            if (j == 0) {
                return vnodes.last()
            }
            return vnodes[j - 1]
        }
        return vnodes[i]
    }

    private fun upperBound(vnodeHash: Int): Int {
        val i = Collections.binarySearch(vnodes, vnodeHash)
        if (i < 0) {
            val j = -(i + 1)
            if (j == vnodes.size) {
                return vnodes.first()
            }
            return vnodes[j]
        }
        return vnodes[i]
    }

    private fun len(l: Int, r: Int): Long {
        return if (r < l) {
            (r.toLong() - Int.MIN_VALUE) + (Int.MAX_VALUE.toLong() - l)
        } else {
            r.toLong() - l
        }
    }
}
