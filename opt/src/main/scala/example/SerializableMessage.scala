// package BSPModel

// sealed trait SerializableMessage{
//     type M
//     val default: M
// }

// sealed abstract class SerializableMessageType[T] extends SerializableMessage {
//     type M = T
// }

// object SerializableMessageType {
//     implicit object IntMessage extends SerializableMessageType[Int]{
//         val default: Int = 0
//     }

//     implicit object DoubleMessage extends SerializableMessageType[Double]{
//         val default: Double = 0.0
//     }
// }