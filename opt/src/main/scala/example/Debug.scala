package BSPModel

trait DebugTrace {
    val debugOn: Boolean = false
    val verbose: Boolean = false

    def dbg(f: => String): Unit = {
        if (debugOn) {
            println(f)
        }
    }

    def ddbg(f: => String): Unit = {
        if (verbose) {
            println(f)
        }
    }
}