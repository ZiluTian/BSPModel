// package BSPModel

// // Instead of keeping two copies of BSP state, keep both state and message, 
// // where message represents publicly visible state that can be read 
// trait DoubleBufferBSP extends BSP {
//     var publicState: Message
// }

// object DoubleBufferBSP {
//     import SerializableMessageType._

//     def fromBSP(c: BSP)(implicit ev: SerializableMessageType[c.Message]): DoubleBufferBSP = {
//         new DoubleBufferBSP {
//             val compute: ComputeMethod = c.compute
//             // Keep the same state values
//             var state: State = c.state.asInstanceOf[this.State]
//             id = c.id
//             outEdges ++= c.outEdges

//             var publicState: Message = ev.default.asInstanceOf[this.Message]
//         }
//     }
// }