package BSPModel

trait Scope {
    // Member can be compound with a different index type
    type Member
    val members: List[Member]
}