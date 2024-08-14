package BSPModel

import scala.collection.mutable.{Map => MutMap}

trait Graph[NodeId] {
    // local vertices
    val vertices: Set[NodeId]
    // transitive closure of internal vertices
    val edges: Map[NodeId, List[NodeId]]
    // preallocated cache for incoming messages
    val inEdges: Map[PartitionId, List[NodeId]]
    val outEdges: Map[PartitionId, List[NodeId]]
}

// Pad topo with cache to buffer messages
case class ArrayGraph[NodeId](
    val g: Graph[NodeId],
    val inCache: Map[PartitionId, Array[Any]],
) extends Graph[NodeId] {
    val vertices = g.vertices
    val edges = g.edges
    val inEdges = g.inEdges
    val outEdges = g.outEdges

    def getInboxCacheIndex(i: NodeId): Option[(PartitionId, Int)] = {
        // return the index of a value in a cache
        inEdges.collectFirst {
            case (key, list) if list.contains(i) =>
            (key, list.indexOf(i))
        }
    }
}

object ArrayGraph{
    def fromGraph[NodeId](g: Graph[NodeId]): ArrayGraph[NodeId] = {
        ArrayGraph[NodeId](g, 
            g.inEdges.map(i => {
                (i._1, new Array[Any](i._2.size))
            })
        )
    }
}