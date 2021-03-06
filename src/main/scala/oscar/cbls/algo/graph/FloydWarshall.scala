package oscar.cbls.algo.graph

/**
 * this is a FloydWarshall algorithm for the [[ConditionalGraph]] data structure.
 * this data structure is non-directed graph, the FloydWarshall is therefore tuned accordingly
 */
object FloydWarshall {
  def buildDistanceMatrix(g: ConditionalGraph,
                          isConditionalEdgeOpen: Int => Boolean): Array[Array[Long]] = {
    val m = buildAdjacencyMatrix(g, isConditionalEdgeOpen)
    saturateAdjacencyMatrixToDistanceMatrix(m, g)
    m
  }

  def buildDistanceMatrixAllConditionalEdgesSame(g: ConditionalGraph,
                                                 allConditionsState: Boolean): Array[Array[Long]] = {
    buildDistanceMatrix(g, isConditionalEdgeOpen = _ => allConditionsState)
  }

  def anyConditionalEdgeOnShortestPath(g: ConditionalGraph,
                                       distanceMatrixAllConditionalEdgesOpen: Array[Array[Long]]): Array[Array[Boolean]] = {
    val n = g.nbNodes
    val matrixAllClosed = buildDistanceMatrixAllConditionalEdgesSame(g, allConditionsState = false)
    Array.tabulate(n)(i => Array.tabulate(n)(j => distanceMatrixAllConditionalEdgesOpen(i)(j) != matrixAllClosed(i)(j)))
  }

  def buildAdjacencyMatrix(g: ConditionalGraph,
                           isConditionalEdgeOpen: Int => Boolean): Array[Array[Long]] = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    val n = g.nbNodes
    val distanceMatrix: Array[Array[Long]] = Array.tabulate(n)(_ => Array.fill(n)(Long.MaxValue))

    for (node <- g.nodes.indices) {
      distanceMatrix(node)(node) = 0
    }

    for (edge <- g.edges if isEdgeOpen(edge)) {
      val sl = edge.length min distanceMatrix(edge.nodeIDA)(edge.nodeIDB)
      distanceMatrix(edge.nodeIDA)(edge.nodeIDB) = sl
      distanceMatrix(edge.nodeIDB)(edge.nodeIDA) = sl
    }

    distanceMatrix
  }

  def buildAdjacencyHalfMatrix(g: ConditionalGraph,
                               isConditionalEdgeOpen: Int => Boolean): Array[Array[Long]] = {

    def isEdgeOpen(edge: Edge): Boolean =
      edge.conditionID match {
        case None => true
        case Some(condition) => isConditionalEdgeOpen(condition)
      }

    val n = g.nbNodes
    val matrix: Array[Array[Long]] = Array.tabulate(n)(n2 => Array.fill(n2 + 1)(Long.MaxValue))

    for (node <- g.nodes.indices) {
      matrix(node)(node) = 0L
    }

    for (edge <- g.edges if isEdgeOpen(edge)) {

      val idA = edge.nodeIDA
      val idB = edge.nodeIDB

      val (minNode, maxNode) = if (idA > idB) (idB, idA) else (idA, idB)

      val sl = edge.length min matrix(maxNode)(minNode)
      matrix(maxNode)(minNode) = sl
    }

    matrix
  }

  def saturateAdjacencyMatrixToDistanceMatrix(w: Array[Array[Long]], graph: ConditionalGraph): Unit = {
    val n = w.length
    for (k <- 0 until n) {
      for (i <- 0 until n) {
        for (j <- 0 until n) {
          if (w(i)(k) != Long.MaxValue && w(k)(j) != Long.MaxValue) {
            val newDistance = w(i)(k) + w(k)(j)
            if (newDistance < w(i)(j)) {
              w(i)(j) = newDistance
            }
          }
        }
      }
    }
  }
}
