package BSPModel
package test

object Util {
    def debug[V](value: sourcecode.Text[V])(implicit enclosing: sourcecode.Enclosing): Unit = {
        println(enclosing.value + " [" + value.source + "]: " + value.value)
    }

    def benchmarkTool[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0)/1000 + "ms")
        result
    }
}