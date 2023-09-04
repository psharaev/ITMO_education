package solution

import java.io.Serializable

@JvmRecord
internal data class Message(val lamportTime: Int, val type: MessageType, val requestTime: Int) : Serializable

internal typealias MessageType = Int

internal const val REQUEST: MessageType = 0
internal const val OK: MessageType = 1
internal const val RELEASE: MessageType = 2