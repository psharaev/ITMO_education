package solution

import java.io.Serializable

sealed class Message : Serializable

object Confirm : Message()

object RegisterChild : Message()

object RemoveChild : Message()

data class Distance(val distance: Long) : Message()
