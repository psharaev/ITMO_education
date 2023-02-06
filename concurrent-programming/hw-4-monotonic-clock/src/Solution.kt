/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author Sharaev Pavel
 */
class Solution : MonotonicClock {


    private var defectedHours by RegularInt(0)
    private var defectedMinutes by RegularInt(0)

    private var actualHours by RegularInt(0)
    private var actualMinutes by RegularInt(0)
    private var actualSeconds by RegularInt(0)

    override fun write(time: Time) {
        // write right-to-left
        actualHours = time.hours
        actualMinutes = time.minutes
        actualSeconds = time.seconds

        defectedMinutes = time.minutes
        defectedHours = time.hours
    }

    override fun read(): Time {
        // read left-to-right
        val snapshotHours = defectedHours
        val snapshotMinutes = defectedMinutes

        val localSeconds = actualSeconds
        val localMinutes = actualMinutes
        val localHours = actualHours

        if (snapshotHours == localHours) {
            return if (snapshotMinutes == localMinutes) {
                Time(localHours, localMinutes, localSeconds)
            } else {
                Time(localHours, localMinutes, 0)
            }
        }
        return Time(localHours, 0, 0)
    }
}