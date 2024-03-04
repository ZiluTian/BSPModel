package BSPModel

trait Scope {
    // Member can be compound with a different index type
    type M <: Member
    val members: List[M]
}

// object TopLevel extends Scope {
//     type Member = Any
//     val members = Nil
// }