package BSPModel

trait StagedExpr {
    type Message

    // a closure that stores compile-time references
    def compile(): Option[Message]
}