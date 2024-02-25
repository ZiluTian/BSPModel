package BSPModel

trait PartitionMessage {
    type Value
    type ValueIndex

    // seq of sender values
    val value: Seq[Value]
    // seq of sender ids
    val messageEncoding: Seq[ValueIndex]
    // sender id, seq of receivers
    val schema: Map[ValueIndex, List[ValueIndex]]
}