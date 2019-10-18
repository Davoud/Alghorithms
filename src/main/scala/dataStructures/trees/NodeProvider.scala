package dataStructures.trees

import scala.collection.mutable

object ChildInfo extends Enumeration {
	val None = Value(0)
	val Black = Value(1)
	val Red = Value(2)
}

case class NodeInfo(value: String, left: ChildInfo.Value = ChildInfo.None, right: ChildInfo.Value = ChildInfo.None)

trait NodeProvider[Key] {
	type NodesInfo = mutable.Map[(Int, Int), NodeInfo]
	
	def nodes(key: Option[Key] = None): NodesInfo
	
	def depth(key: Option[Key] = None): Int
}
