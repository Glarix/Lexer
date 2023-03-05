
class Dfa[A] (var firstState: A){

  var allNodes: Set[A] = Set[A]()
  var finalState: Set[A] = Set[A]()
  var sink: Set[A] = Set[A]()
  var dfaGraph: Map[A, Set[(A, String)]] = Map[A, Set[(A, String)]]()
  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = {
    val theNewDfa = new Dfa[B](f(firstState))
    var allNodesAux = Set[B]()
    allNodes.foreach(
      x => allNodesAux += f(x)
    )
    theNewDfa.allNodes = allNodesAux

    theNewDfa.sink = Set[B](f(sink.head))

    var allFinalStatesAux = Set[B]()
    finalState.foreach(
      x => allFinalStatesAux += f(x)
    )
    theNewDfa.finalState = allFinalStatesAux

    dfaGraph.foreach(
      x => {
        val key = x._1
        val value = x._2
        var bTypeValues = Set[(B, String)]()
        value.foreach(
          y => {
            bTypeValues += Tuple2(f(y._1), y._2)
          }
        )
        theNewDfa.dfaGraph += (f(key) -> bTypeValues)
      }
    )
    theNewDfa
  }

  def next(state:A, c: Char): A = {
    val nextState = sink.head // in case we are in sink
    val connections = dfaGraph(state)
    connections.foreach(
      x => {
        if (x._2.==(c.toString)) { // if found a connection to get to by char c then this is next state
          return x._1
        }
      }
    )
    // if is impossible to consume this character from this state, go to sink
    nextState
  }

  def accepts(str: String): Boolean = {
    // vor void case
    if (allNodes.isEmpty) return false
    // for cases when prenex has only eps as atoms
    if (dfaGraph(firstState).isEmpty && str.isEmpty) return true
    if (dfaGraph(firstState).isEmpty && str.nonEmpty) return false
    // array of characters
    var tokens = str.toCharArray
    var currentState = firstState

    // as long as there are characters, find next state for all characters
    while (tokens.nonEmpty) {
      var currentToken = tokens.head
      if (currentToken == '\\' && tokens.take(2).last == 'n') {
        currentToken = '\n'
        tokens = tokens.tail
      }
      currentState = next(currentState, currentToken)
      tokens = tokens.tail
    }

    if (isFinal(currentState)) return true

    false
  }

  def getStates : Set[A] = {
    allNodes
  }

  def isFinal(state: A): Boolean = {
    finalState.contains(state)
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = {
    val alphabet = getAlphabet(Nfa.parsePrenexForTokens(str))
    val theNfa = Nfa.fromPrenex(str);
    createAFD(theNfa, alphabet)
  }

  def dfaFromNfa(theNfa: Nfa[Int], alphabet: Set[Char]): Dfa[Set[Int]] = {
    createDfa(theNfa, alphabet)
  }

  /**
   * method that returns the character alphabet for nfa (used at dfa)
   * @param tokens the list of tokens
   * @return the alphabet
   */
  def getAlphabet(tokens: List[String]):Set[Char] = {
    var alphabet = Set[Char]()
    alphabet = alphabet ++ tokens.filter(x => x.length == 1).map(x => x.toCharArray.head).toSet
    alphabet = alphabet ++ tokens.filter(x => x.equals("\\n")).map(x => '\n')
    alphabet = alphabet ++ tokens.filter(x => x.equals("\\t")).map(x => '\t')
    alphabet
  }

  /**
   * Method to hash a Set of Ints to Int
   * @param theSet
   * @return
   */
  def hashSetToInt(theSet: Set[Int]): Int = {
    ((theSet.max + theSet.min + theSet.size + theSet.head) * theSet.last) * (theSet.max * 3 - theSet.min)
  }

  def createDfa(theNfa: Nfa[Int], alphabet: Set[Char]): Dfa[Set[Int]] = {
    // method that gets the epsilon closure of a state
    var closedEpsilonClosureStates = Set[Int]()

    def epsilonClosure(state: Int) : Set[Int] = {
      var values:Set[Int] = Set[Int]()
      val allConnections = theNfa.nfaGraph.get(state)
      closedEpsilonClosureStates = closedEpsilonClosureStates ++ Set(state)
      allConnections.get.foreach(
        x => {
          if(x._2.==("eps") && !closedEpsilonClosureStates.contains(x._1)) {
            values += x._1
            values ++= epsilonClosure(x._1)
          }
        }
      )
      values
    }

    var visitedStates = Set[Set[Int]]()
    // method that creates the DFA by creating all possible states
    def computeConnectionsForAlphabet(theDfa: Dfa[Set[Int]], firstState: Set[Int], sinkState: Set[Int]) : Int = {
      if (!visitedStates.contains(firstState)) {
        visitedStates += firstState
        alphabet.foreach( // for each character from alphabet create the state containing all NFA states that are reachable from this state
          x => {
            var stateForThisChar: Set[Int] = Set[Int]() // the new state that will be populated
            firstState.foreach(
              y => {
                val connectionsFromNfa: List[(Int, String)] = theNfa.nfaGraph(y)
                connectionsFromNfa.foreach(
                  z => {
                    if (z._2.==(x.toString) || z._2 == "\\n" && x.toString == "\n" || z._2 == "\\t" && x.toString == "\t") {
                      stateForThisChar += z._1 // if state is reachable by char x, add state to stateForThisChar
                      closedEpsilonClosureStates = Set()
                      stateForThisChar ++= epsilonClosure(z._1) // also add it's epsilonClosure to stateForThisChar
                    }
                  }
                )
              }
            )
            if (stateForThisChar.isEmpty) { // if is impossible to get anywhere by this char then add a way to sinkState by this char
              val newSet: Set[(Set[Int], String)] = theDfa.dfaGraph(firstState) + Tuple2(sinkState, x.toString)
              theDfa.dfaGraph += (firstState -> newSet)
            } else {
              if (!theDfa.allNodes.contains(stateForThisChar)) { // if the newly found state is new in DFA, add it to nodes
                theDfa.allNodes += stateForThisChar
                theDfa.dfaGraph += (stateForThisChar -> Set())
              }

              // add a way from current state to new state by character x
              val newSet = theDfa.dfaGraph(firstState) + Tuple2(stateForThisChar, x.toString)
              theDfa.dfaGraph += (firstState -> newSet)
              computeConnectionsForAlphabet(theDfa, stateForThisChar, sinkState)
            }
          }
        )
      }
      1
    }

    // function that computes all the final states
    def computeFinalStates(nfaFinalState: Int, theDfa: Dfa[Set[Int]]): Int = {
      theDfa.allNodes.foreach(
        x => {
          if (x.contains(nfaFinalState)) {
            theDfa.finalState += x
          }
        }
      )
      1
    }

    if(theNfa.allNodes.isEmpty) { // prenex is void
      return new Dfa[Set[Int]](Set(-2)) // special Dfa that knows that prenex is void and it should never accept
    }


    var firstState = Set[Int](theNfa.firstState)
    firstState ++= epsilonClosure(theNfa.firstState) // create firstState
    val sinkState = Set[Int](-1) // create sinkState

    //    visitedStates += firstState // mark firstState as visited as we are about to visit it
    val theDfa = new Dfa[Set[Int]](firstState)
    theDfa.sink += sinkState
    theDfa.dfaGraph += (sinkState -> Set())
    theDfa.dfaGraph += (firstState -> Set())
    theDfa.allNodes += firstState;
    theDfa.allNodes += sinkState;

    computeConnectionsForAlphabet(theDfa, firstState, sinkState)
    theNfa.finalState.foreach(fs => {
      computeFinalStates(fs, theDfa)
    })

    theDfa
  }
  /**
   * Method that creates an DFA of type Int from NFA of type Int
   * @param theNfa
   * @param alphabet the alphabet used by DFA
   * @return the DFA
   */
  def createAFD(theNfa: Nfa[Int], alphabet: Set[Char]): Dfa[Int] = {
    val theDfa = createDfa(theNfa, alphabet)
    if (theDfa.allNodes.isEmpty) {
      new Dfa[Int](-2)
    } else {
      theDfa.map(hashSetToInt)
    }
  }

  // You can add more methods to this object
}
