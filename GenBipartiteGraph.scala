package com.scalalot.sir.algorithms
import scala.util.Random

object GenBipartiteGraphRunner {

  def main(args: Array[String]): Unit = {
    println("Generating a Bipartite Graph")
    val generator: GenBipartiteGraph =  new GenBipartiteGraph()
    val graph: BipartiteGraph = generator.gen(20, 20)
    println(s"graph is ${graph} ")
  }
}

class GenBipartiteGraph() {
  def gen(numVertices: Int, numEdges: Int): BipartiteGraph = {
    // Let's generate randomly how many vertices we want in U.
    val random: Random = new Random()
    val numUVertices: Int = math.floor(numVertices / 2).toInt + random.nextInt(math.floor(numVertices / 10).toInt)
    val numVVertices: Int = numVertices - numUVertices
    val graph: BipartiteGraph = new BipartiteGraph(new Array[Int](0), new Array[Int](0), new Array[Array[Int]](numVertices))
    splitUAndV(graph, numUVertices, numVVertices)
    println(s"Size of U:${graph.U.size} and Size of V:${graph.V.size}")
    generateEdges(graph, numEdges)
    println(s" adjacency matrix of size ${graph.graphAdjacencyMatrix.size}")
    return graph
  }

  def splitUAndV(graph: BipartiteGraph, numUVertices: Int, numVVertices:Int) : BipartiteGraph = {
    val random: Random = new Random()
    val numVertices: Int = numUVertices + numVVertices
    for(i <- 1 to numVertices) {
      val n: Int = random.nextInt(numVertices)
      if (n < numUVertices) {
        graph.U = graph.U :+ i
      } else {
        graph.V = graph.V :+ i
      }
    }
    return graph
  }

  def generateEdges(graph: BipartiteGraph, numEdges: Int): BipartiteGraph = {
    val random: Random = new Random()
    val numVertices: Int = graph.U.size + graph.V.size
    for(i <- 0 to (numVertices - 1)) {
      graph.graphAdjacencyMatrix(i) = new Array[Int](numVertices)
    }
    for (i <- 1 to numEdges) {
      val u_vertex_index: Int = random.nextInt(graph.U.size)
      val v_vertex_index: Int = random.nextInt(graph.V.size)

      if (graph.graphAdjacencyMatrix(graph.U(u_vertex_index)-1)(graph.V(v_vertex_index)) == 0) {
        graph.graphAdjacencyMatrix(graph.U(u_vertex_index)-1)(graph.V(v_vertex_index)-1) = 1
        graph.graphAdjacencyMatrix(graph.V(v_vertex_index)-1)(graph.U(u_vertex_index)-1) = 1
        graph.numEdges += 1
      }
    }
    return graph
  };
}

class BipartiteGraph(var U: Array[Int], var V: Array[Int], var graphAdjacencyMatrix: Array[Array[Int]]) {
  var numEdges = 0
  override def toString(): String = {
    var uString: String = ""
    var vString : String = ""
    var adjacencyMatrixString: String= ""
    var printString: String = ""

    for(i <- U) {
//      println(i)
      uString = uString + i.toString + ","
    }
    for(i <- V) {
//      println(i)
      vString = vString + i.toString + ","
    }
    adjacencyMatrixString += "\t"

    for (i <- 1 to (this.U.size + this.V.size)) {
      adjacencyMatrixString += i + "\t"
    }
    adjacencyMatrixString += "\n"

    for (i <- 0 to (this.U.size + this.V.size - 1)) {
      adjacencyMatrixString += (i + 1) + "\t"
//      println(s"graphAdjacencyMatrix(i): ${this.graphAdjacencyMatrix(i)}")
//      println(s"graphAdjacencyMatrix(i).size: ${this.graphAdjacencyMatrix(i).size}")
      for (j <- 0 to (this.U.size + this.V.size - 1) ) {
//        println(s"graphAdjacencyMatrix(i,j): ${this.graphAdjacencyMatrix(i)(j)}")
        adjacencyMatrixString += this.graphAdjacencyMatrix(i)(j) + "\t"

      }
      adjacencyMatrixString += "\n"
    }
    printString += s"U: ${uString} V: ${vString} numEdges: ${this.numEdges}" + "\n"
    printString += adjacencyMatrixString
    return printString
  }
}


