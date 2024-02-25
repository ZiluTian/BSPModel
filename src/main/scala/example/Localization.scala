package BSPModel

// transform sending messages to neighbors in the same partition 
object Localization extends Optimization {

    def transform(x: Partition): Partition = {
        x match {
            case part: BSPPartition => {
                part.indexedLocalValue.foreach(p => {
                    p match {
                        case (bspId, bsp) => {
                            // external neighbors have been lifted to partition outEdges during preprocessing
                            // for each local neighbor, transform a value from being sent to read
                            bsp.outEdges.foreach(i => part.indexedLocalValue(i).curry(bspId))
                            // remove any edges in the actor
                            bsp.outEdges.clear()
                        }
                    }
                })
            }
            case _ => 
        }
        x
    } 
}