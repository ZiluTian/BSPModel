package BSPModel

trait Stage {
    this: BSP & ComputeMethod =>

    // cascaded staging, allow for multiple staged exprs
    val stagedComputation: List[StagedExpr]

    override def run(ms: Iterable[Message]): Unit = {
        val messages = stagedComputation.map(c => {
            c.compile().asInstanceOf[Option[Message]]
        }).filter(m => !m.isEmpty).map(m => m.get)
        state = run(state, messages ++ ms)
    }
}