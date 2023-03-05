import scala.annotation.tailrec

class Nfa[A](var firstState:A) {

  var allNodes: Set[A] = Set[A]()
  var nfaGraph: Map[A, List[(A, String)]] = Map[A, List[(A, String)]]()
  var finalState: Set[A] = Set[A](firstState)


  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = {
    val theNewNfa = new Nfa[B](f(firstState))
    var allNodesAux = Set[B]()
    allNodes.foreach(
      x => allNodesAux += f(x)
    )
    theNewNfa.allNodes = allNodesAux
    theNewNfa.finalState = Set(f(finalState.head))

    nfaGraph.foreach(
      x => {
        val key = x._1
        val value = x._2
        var bTypeValues: List[(B, String)] = List[(B, String)]()
        value.foreach(
          y => {
            bTypeValues = (f(y._1), y._2) :: bTypeValues
          }
        )
        theNewNfa.nfaGraph += (f(key) -> bTypeValues)
      }
    )
    theNewNfa
  }

  def next(state:A, c: Char): Set[A] = {
    var epsilonWay = Set[A]()
    var charWay = Set[A]()
    var verifiedEpsilons = Set[A]()

    @tailrec
    def getAllPossibleFromThisState(currentState: A): Unit = {
      val allConnections = nfaGraph.get(currentState) // get all states this state has a connection to
      allConnections.get.foreach(
        x => {
          if (x._2.==(c.toString)) { // if to this state I can get by char c then is a next state
            charWay += x._1
          } else if (x._2.==("eps") && !verifiedEpsilons.contains(x._1)) { // if to this state I get by epsilon
            epsilonWay += x._1 // add the state to be verified
          }
        }
      )
      verifiedEpsilons += currentState // mark that this state has been verified for epsilons

      if(epsilonWay.nonEmpty) { // as long as I have states to verify for epsilons, verify them
        val nextState = epsilonWay.head
        epsilonWay = epsilonWay.tail
        getAllPossibleFromThisState(nextState) // verify state
      }
    }

    // get all possible states from this state
    getAllPossibleFromThisState(state)
    charWay
  }

  def accepts(str: String): Boolean = {

    // in case prenex is void
    if (finalState.isEmpty) {
      return false
    }
    // array of characters
    var tokens = str.toCharArray
    // set with possible states to go from one state by consuming a character
    var nextPossibleStates = Set[A]()
    // intermediary set of states to be checked after all states from next have been verified
    var nextStatesToCheck = Set[A]()
    // set with states that have already been verified when trying to get to final state by epsilon
    var verifiedEpsilons = Set[A]()

    // method that tries to get to a final state only by epsilons from set of states when a word is consumed
    def getToFinalStateByEpsilons(currentState: A): Boolean = {
      // mark that this state has been verified for epsilon
      verifiedEpsilons += currentState
      val allConnections = nfaGraph.get(currentState)
      allConnections.get.foreach(
        x => {
          if (x._2.==("eps")) { // if I can get to this state by epsilon
            if (finalState.contains(x._1)) { // check if final
              return true
            } else {
              if (!verifiedEpsilons.contains(x._1)) { // if not already verified, put in set for future verification
                nextStatesToCheck += x._1
              }
            }
          }
        }
      )
      if (nextStatesToCheck.nonEmpty) { // if there are still states to be checked, check them
        val nextState = nextStatesToCheck.head
        nextStatesToCheck = nextStatesToCheck.tail
        return getToFinalStateByEpsilons(nextState)
      }
      false
    }

    if (tokens.isEmpty) { // if empty string is given, check if is possible to get to final state only by epsilons
      return getToFinalStateByEpsilons(firstState)
    }

    // get next states for firstState with first token
    nextStatesToCheck = next(firstState, tokens.head)

    tokens = tokens.tail
    // while there are still tokens left and states to check
    while (tokens.nonEmpty && nextStatesToCheck.nonEmpty) {
      nextStatesToCheck.foreach(
        x => {
          val foundStates = next(x, tokens.head)
          foundStates.foreach(
            y => nextPossibleStates += y
          )
        }
      )
      nextStatesToCheck = nextPossibleStates
      nextPossibleStates = Set[A]()
      tokens = tokens.tail
    }

    // if word is consumed and we reached a final state
    if(tokens.isEmpty && nextStatesToCheck.nonEmpty && nextStatesToCheck.contains(finalState.head)) return true

    // if no possible states remain than is sink
    if(nextStatesToCheck.isEmpty) return false

    // if word was consumed try to get to a final state by epsilons
    if (tokens.isEmpty) {
      val currentState = nextStatesToCheck.head
      nextStatesToCheck = nextStatesToCheck.tail
      return getToFinalStateByEpsilons(currentState)
    }

    false
  }

  def getStates : Set[A] = {
    allNodes
  }

  def isFinal(state: A): Boolean = {
    if (state == finalState.head)
      return true

    false
  }

}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  def fromPrenex(str: String): Nfa[Int] = {
    val myAST: AST = AST.createAST(parsePrenexForTokens(str))

    createNFA(myAST, 0)
  }

  def fromPrenex(str: String, startState: Int): Nfa[Int] = {
    val myAST: AST = AST.createAST(parsePrenexForTokens(str))

    createNFA(myAST, startState)
  }

  // You can add more methods to this object

  /**
   * method that transforms the prenex into tokens and takes into consideration characters surrounded by quotes
   * @param str the prenex string
   * @return list of tokens
   */
  def parsePrenexForTokens(str: String): List[String] = {
    var theTokens = List[String]()

    // method that separates tokens by space and also takes into consideration characters surrounded by quotes as tokens
    def separateTokens(prenex: String): Unit = {
      var tempString = ""
      var isInSingleQuotes = false
      var readSingleQuotedChar = false
      val chars = prenex.toCharArray
      chars.foreach(
        x => {
          if(readSingleQuotedChar) { // if this character is between quotes it is taken as token
            //theTokens :::= List(x.toString)
            if (x.==('\'')) {
              theTokens :::= List(tempString)
              tempString = ""
              isInSingleQuotes = false
              readSingleQuotedChar = false
            } else {
              tempString = tempString.concat(x.toString)
            }
          } else if(x.==('\'') && !isInSingleQuotes) { // this character is the first quote
            isInSingleQuotes = true
            readSingleQuotedChar = true
          } else if(x.==(' ') && !isInSingleQuotes) { // space encountered so add token to list of tokens
            if (tempString.nonEmpty) {
              theTokens :::= List(tempString)
              tempString = ""
            }
          } else { // add character to token constructor
            tempString = tempString.concat(x.toString)
          }
        }
      )
      if (isInSingleQuotes) { // in case there was only a single quote (for the isolated case when str is just "'")
        theTokens :::= List("'")
      }
      if (tempString.nonEmpty) { // add the last token
        theTokens :::= List(tempString)
      }
      theTokens = theTokens.reverse // get tokens in correct order
    }

    separateTokens(str)
    theTokens
  }


  /**
   * method that creates a NFA from an AST
   * @param theAST
   * @return NFA of type Int
   */
  def createNFA(theAST: AST, startState: Int): Nfa[Int] = {
    val theNfa = new Nfa[Int](startState) // initialize nfa
    // set current states and initial final state
    val currentStates = (theNfa.firstState, theNfa.firstState)
    theNfa.finalState = Set(currentStates._2)

    /**
     * method that applies Thompson algorithm on atom AST nodes
     * @param theAtom the atom operation
     * @param stateToStart the state to start from
     * @return tuple with startNodeNumber and EndNodeNumber
     */
    def thompsonAtom(theAtom: String, stateToStart: Int): (Int, Int) = {
      val stateToEnd: Int = stateToStart + 1

      theNfa.allNodes += stateToStart
      theNfa.allNodes += stateToEnd

      theNfa.nfaGraph += (stateToStart -> List((stateToEnd, theAtom)))
      theNfa.nfaGraph += (stateToEnd -> List())

      theNfa.finalState = Set(stateToEnd)

      (stateToStart, stateToEnd)
    }

    /**
     * Method that applies Thompson algorithm on any AST node and creates the whole NFA
     * @param theNode an AST node that must be processed
     * @param currentState the current start state of this section of NFA
     * @return Tuple representing first and last node numbers of this section of NFA
     */
    def processASTNode(theNode: AST, currentState: Int): (Int, Int) = {
      theNode.operation match {
        case "UNION" => {
          val startState = currentState
          val newPositions = processASTNode(theNode.left, startState + 1)
          val newPositions2 = processASTNode(theNode.right, newPositions._2 + 1)
          theNfa.allNodes += startState
          // add new finalState
          val endState = newPositions2._2 + 1
          theNfa.allNodes += endState
          theNfa.finalState = Set(endState)
          theNfa.nfaGraph += (startState -> List((newPositions._1, "eps"), (newPositions2._1, "eps")))
          theNfa.nfaGraph += (newPositions._2 -> List((endState, "eps")))
          theNfa.nfaGraph += (newPositions2._2 -> List((endState, "eps")))
          theNfa.nfaGraph += (endState -> List())
          (startState, endState)
        }

        case "CONCAT" => {
          val newPositions = processASTNode(theNode.left, currentState)
          val newPositions2 = processASTNode(theNode.right, newPositions._2 + 1)
          theNfa.nfaGraph += (newPositions._2 -> List((newPositions2._1, "eps")))
          (currentState, newPositions2._2)
        }

        case "STAR" => {
          val newPositions = processASTNode(theNode.left, currentState + 1)
          val startPosition = currentState
          val endPosition = newPositions._2 + 1
          theNfa.allNodes += startPosition
          theNfa.allNodes += endPosition
          theNfa.finalState = Set(endPosition)
          theNfa.nfaGraph += (startPosition -> List((newPositions._1, "eps"), (endPosition, "eps")))
          theNfa.nfaGraph += (newPositions._2 -> List((newPositions._1, "eps"), (endPosition, "eps")))
          theNfa.nfaGraph += (endPosition -> List())
          (startPosition, endPosition)
        }

        case "void" => {
          theNfa.finalState = Set()
          (-1,-1)
        }

        case _ => thompsonAtom(theNode.operation, currentState)
      }
    }

    processASTNode(theAST, currentStates._1)
    theNfa
  }

}

