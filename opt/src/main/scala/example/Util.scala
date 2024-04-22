package BSPModel

object Util {
    def debug[V](value: sourcecode.Text[V])(implicit enclosing: sourcecode.Enclosing): Unit = {
        println(enclosing.value + " [" + value.source + "]: " + value.value)
    }

    def benchmarkTool[R](block: => R): R = {
        val t0 = System.currentTimeMillis()
        val result = block    // call-by-name
        val t1 = System.currentTimeMillis()
        println("Elapsed time: " + (t1 - t0) + "ms")
        result
    }
}