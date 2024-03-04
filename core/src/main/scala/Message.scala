package BSPModel

trait PartitionMessage {
    type M
    type Idx

    // seq of sender values
    val value: Seq[M]
    // seq of sender ids
    val messageEncoding: Seq[Idx]
    // sender id, seq of receivers
    val schema: Map[Idx, Seq[Idx]]
}