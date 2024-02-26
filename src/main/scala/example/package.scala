package object BSPModel {
    type BSPId = Long
    type PartitionId = Long

    def debug(msg: () => String) = {
        println(f"Debug: ${msg()}")
    }

    def partitionIdToBSPId(pid: PartitionId): BSPId = {
        pid
    }

    private var lastId: Long = 0

    def getNextId(): Long = this.synchronized {
        lastId = lastId + 1
        lastId
    }
}