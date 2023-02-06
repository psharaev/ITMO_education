import kotlinx.atomicfu.*
import kotlin.math.max
import kotlin.math.min


class BankImpl(override val numberOfAccounts: Int) : Bank {
    private val accounts = atomicArrayOfNulls<Account>(numberOfAccounts)

    init {
        for (i in 0 until numberOfAccounts) accounts[i].value = Account(0)
    }

    private fun account(index: Int) = accounts[index].value!!

    override fun getAmount(index: Int): Long {
        while (true) {
            val account = account(index)

            if (!account.invokeOperation()) return account.amount
        }
    }

    override val totalAmount: Long
        get() {
            val op = TotalAmountOp()
            op.invokeOperation()
            return op.sum
        }

    override fun deposit(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        check(amount <= MAX_AMOUNT) { "Overflow" }

        while (true) {
            val account = account(index)

            if (account.invokeOperation()) continue
            check(account.amount + amount <= MAX_AMOUNT) { "Overflow" }
            val updated = Account(account.amount + amount)
            if (accounts[index].compareAndSet(account, updated)) return updated.amount
        }
    }

    override fun withdraw(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        while (true) {
            val account = account(index)

            if (account.invokeOperation()) continue
            check(account.amount - amount >= 0) { "Underflow" }
            val updated = Account(account.amount - amount)
            if (accounts[index].compareAndSet(account, updated)) return updated.amount
        }
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }
        check(amount <= MAX_AMOUNT) { "Underflow/overflow" }

        val op = TransferOp(fromIndex, toIndex, amount)
        op.invokeOperation()
        op.errorMessage?.let { error(it) }
    }


    private fun acquire(index: Int, op: Op): AcquiredAccount? {
        while (true) {
            val account = account(index)
            when {
                op.completed -> return null
                account is AcquiredAccount && account.op === op -> return account
                !account.invokeOperation() -> {
                    val acquiredAccount = AcquiredAccount(account.amount, op)
                    if (accounts[index].compareAndSet(account, acquiredAccount)) return acquiredAccount
                }
            }
        }
    }


    private fun release(index: Int, op: Op) {
        assert(op.completed)
        val account = account(index)
        if (account is AcquiredAccount && account.op === op) {
            val updated = Account(account.newAmount)
            accounts[index].compareAndSet(account, updated)
        }
    }


    private open class Account(val amount: Long) {
        open fun invokeOperation(): Boolean = false
    }


    private class AcquiredAccount(
        var newAmount: Long,
        val op: Op,
    ) : Account(newAmount) {
        override fun invokeOperation(): Boolean {
            op.invokeOperation()
            return true
        }
    }


    private abstract inner class Op {
        @Volatile
        var completed = false

        abstract fun invokeOperation()
    }


    private inner class TotalAmountOp : Op() {
        var sum = 0L

        override fun invokeOperation() {
            var sum = 0L
            var acquired = 0
            while (acquired < numberOfAccounts) {
                val account = acquire(acquired, this) ?: break
                sum += account.amount
                acquired++
            }

            if (acquired == numberOfAccounts) {

                this.sum = sum
                completed = true
            }

            for (i in 0 until numberOfAccounts) {
                release(i, this)
            }
        }
    }


    private inner class TransferOp(val fromIndex: Int, val toIndex: Int, val amount: Long) : Op() {
        var errorMessage: String? = null

        override fun invokeOperation() {
            val minIndex = min(fromIndex, toIndex)
            val maxIndex = max(fromIndex, toIndex)

            acquire(minIndex, this)

            val from = acquire(fromIndex, this)
            val to = acquire(toIndex, this)

            if (from != null && to != null) {
                when {
                    amount > from.amount -> errorMessage = "Underflow"
                    to.amount + amount > MAX_AMOUNT -> errorMessage = "Overflow"
                    else -> {
                        from.newAmount = from.amount - amount
                        to.newAmount = to.amount + amount
                    }
                }
            }

            completed = true
            release(minIndex, this)
            release(maxIndex, this)
        }
    }
}