package BSPModel

trait SpecializedBSP extends BSP {
    def localLookup(s: BSPId): Message
}