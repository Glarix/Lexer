import scala.annotation.tailrec
import scala.collection.mutable

class AST (var operation:String, var right:AST,  var left:AST) {
}

object AST {

  /**
   *   check if operation is atom
    */
  def isAtom(operation:String): Boolean = {
    if (operation != "CONCAT" &&
      operation != "UNION" &&
      operation != "STAR" &&
      operation != "PLUS" &&
      operation != "MAYBE") {
      return true
    }
    false
  }

  /**
   * method to create an ast from a list of tokens
   * @param prenex the list of tokens
   * @return the AST
   */
  def createAST(prenex:List[String]): AST = {

    if (prenex.isEmpty) return null

    val s = mutable.Stack[AST]()

    // create a new node for this operation, insert the new node in stack
    def takeCareOfOperations(firstOperation: String, remainingPrenex: List[String]): AST = {
      var finalPrenex = remainingPrenex
      // if operation is MAYBE -> transform it in UNION of eps and next token
      if (firstOperation == "MAYBE") {
        s.push(new AST("UNION", new AST("eps", null, null), null))
      } else if (firstOperation == "PLUS") {
        // if operation is PLUS -> transform it in CONCAT of next token and STAR of next token
        val tmp = new AST(remainingPrenex.head, null, null)
        val tmp2 = new AST("STAR", null, tmp)
        s.push(new AST("CONCAT",
          tmp2,
          tmp))
        s.push(tmp2)
        s.push(tmp)
        finalPrenex = remainingPrenex.drop(1)
      } else {
        // if STAR or CONCAT or UNION or ATOM -> create node and push to stack
        s.push(new AST(firstOperation, null, null))
      }

      completeAllPossibleNodesAndCreateNextOne(finalPrenex)
    }

    // check if a node is completed
    def checkIfCompleted(currentNode: AST): Boolean = {
      val currentOperation:String = currentNode.operation

      if (isAtom(currentOperation)) { return true }
      if (currentOperation == "STAR" && currentNode.left != null) { return true }
      if (currentNode.left != null && currentNode.right != null) { return true }
      false
    }

    // check if top most node is completed, if completed, go down the stack and complete the other nodes until
    // bottom node is completed or until a node is not completed
    def goBackUntilIncompleteNode(node: AST): Unit = {
      if (s.isEmpty) {
        s.push(node)
        return
      }
      val currentNode = s.pop()

      // STAR incomplete node
      if(currentNode.operation == "STAR") {
        currentNode.left = node
        goBackUntilIncompleteNode(currentNode)
        return
      }

      // CONCAT or UNION incomplete node
      if (currentNode.left == null) {
        currentNode.left = node;
        s.push(currentNode)
      } else {
        // CONCAT or UNION complete node
        currentNode.right = node
        goBackUntilIncompleteNode(currentNode)
      }
    }

    //
    @tailrec
    def completeAllPossibleNodesAndCreateNextOne(prenex: List[String]):AST = {
      // pop the top element
      var currentNode = s.pop()
      // check if is complete
      val isComplete:Boolean = checkIfCompleted(currentNode)
      if (isComplete) {
        // if isComplete and stack empty then AST is done
        if (s.isEmpty) {
          // ultimate exit
          return currentNode
        }
        // if isComplete but stack not empty go down until and complete as many nodes as possible
        goBackUntilIncompleteNode(currentNode)
        // after going down as much as possible
        currentNode = s.pop()
        // if node is completed and at very bottom, AST is done
        if (s.isEmpty && checkIfCompleted(currentNode)) {
          currentNode
        } else { // push node back and construct the rest of the AST
          s.push(currentNode)
          completeAllPossibleNodesAndCreateNextOne(prenex)
        }
      } else { // if node is not complete, get next token and construct the next node
        prenex match {
          case x :: xs => {
            s.push(currentNode)
            takeCareOfOperations(x, xs)
          }
        }
      }
    }

    // if first token is Atom then return AST with only this node
    val firstOperation = prenex.head
    if (isAtom(firstOperation)) {
      return new AST(firstOperation, null, null)
    }

    def startCreatingAST() = {
      takeCareOfOperations(firstOperation, prenex.tail)
    }

    startCreatingAST()
  }
}
