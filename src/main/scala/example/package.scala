package object BSPModel {
    type BSPId = Long
    type PartitionId = Long

    def debug(msg: () => String) = {
        println(f"Debug: ${msg()}")
    }
}