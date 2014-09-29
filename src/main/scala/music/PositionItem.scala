package music

case class PositionItem(start: Float, end: Float)

trait PositionItemBuilder {
  def buildPositionItems(size: Int): List[PositionItem]
}

case class PatternPositionItemBuilder(pattern: PatternItem[PositionItem]) extends PositionItemBuilder {
  override def buildPositionItems(size: Int): List[PositionItem] =
    (1 to size).map(i => pattern.takeItem()).toList
}
