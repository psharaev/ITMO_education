/**
 * Время из трех целых чисел.
 */
data class Time(val hours: Int, val minutes: Int, val seconds: Int) : Comparable<Time> {
    override fun compareTo(other: Time): Int =
        compareValuesBy(this, other, Time::hours, Time::minutes, Time::seconds)
}

/**
 * Монотонные часы.
 */
interface MonotonicClock {
    fun write(time: Time)
    fun read(): Time
}
