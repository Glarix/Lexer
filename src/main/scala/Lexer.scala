import scala.collection.mutable.ListBuffer

case class Lexer (spec: String) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String,List[(String,String)]] = {

    val theSpec = splitSpec()

    val theNfas = createNFAs(theSpec)

    val finalNfa = createFinalNfa(theNfas)

    val wholeAlphabet = getWholeAlphabet(theSpec)

    val theDfa = createDfa(finalNfa, wholeAlphabet)

    val allFinalStatesInPriorityOrder = getAllFinalStatesInPriorityOrder(theNfas)

    parseWord(word, theDfa, theSpec, wholeAlphabet, allFinalStatesInPriorityOrder)
  }

  /**
   * Method that splits the specification into tuples to create Nfas
   */
  def splitSpec(): ListBuffer[(String, String)] = {
    val arr = spec.split(";\n")
    val parsedSpecification: ListBuffer[(String, String)] = ListBuffer()
    arr.foreach(expr => {
      val res = expr.split(": ")
      parsedSpecification += Tuple2(res(0), res(1))
    })
    parsedSpecification
  }

  /**
   * Method that creates a list containing all nfas for the given specification
   * @param listBuffer list with parsed specification
   */
  def createNFAs(listBuffer: ListBuffer[(String, String)]): ListBuffer[Nfa[Int]] = {
    var currentStartState = 1
    val allNfas = ListBuffer[Nfa[Int]]()
    listBuffer.foreach(spec => {
      val prenex = Regex.toPrenex(spec._2)
      val theNfa = Nfa.fromPrenex(prenex, currentStartState)
      currentStartState = theNfa.allNodes.max + 1
      allNfas += theNfa
    })

    allNfas
  }

  /**
   * Method that returns a list with every nfa final state in their priority order
   * @param theNfas the list of nfas
   */
  def getAllFinalStatesInPriorityOrder(theNfas: ListBuffer[Nfa[Int]]): List[Int] = {
    var allFinalStatesInPriorityOrder = List[Int]()
    theNfas.foreach(nfa => {
      allFinalStatesInPriorityOrder = allFinalStatesInPriorityOrder :+ nfa.finalState.head
    })

    allFinalStatesInPriorityOrder
  }


  /**
   * Method that unifies all nfas into a single nfa
   * @param listBuffer the list of nfas
   */
  def createFinalNfa(listBuffer: ListBuffer[Nfa[Int]]): Nfa[Int] = {
    val finalNfa = new Nfa[Int](0)
    finalNfa.finalState = Set()
    var transitionsFromStateZero = List[(Int, String)]()

    listBuffer.foreach(nfa => {
      finalNfa.allNodes = finalNfa.allNodes ++ nfa.allNodes
      finalNfa.nfaGraph = finalNfa.nfaGraph ++ nfa.nfaGraph
      finalNfa.finalState = finalNfa.finalState ++ nfa.finalState
      transitionsFromStateZero :+= Tuple2(nfa.firstState, "eps")
    })

    finalNfa.allNodes += 0
    finalNfa.nfaGraph += (0 -> transitionsFromStateZero)

    finalNfa
  }

  /**
   * Method that gets the whole alphabet from all specification tokens
   */
  def getWholeAlphabet(splitSpecification: ListBuffer[(String, String)]): Set[Char] = {
    var wholeAlphabet = Set[Char]()

    splitSpecification.foreach(sp => {
      val prenex = Regex.toPrenex(sp._2)
      wholeAlphabet = wholeAlphabet ++ Dfa.getAlphabet(Nfa.parsePrenexForTokens(prenex))
    })

    wholeAlphabet
  }

  def createDfa(theNfa: Nfa[Int], alphabet: Set[Char]): Dfa[Set[Int]] = {
    Dfa.createDfa(theNfa, alphabet)
  }

  /**
   * Method that parses the given word and splits it into a list of lexems
   * in the format (LEXEM, TOKEN)
   */
  def parseWord(word: String,
                theDfa: Dfa[Set[Int]],
                splitSpec: ListBuffer[(String, String)],
                alphabet: Set[Char],
                allFinalStatesInPriorityOrder: List[Int]): Either[String,List[(String,String)]] = {
    var foundLexems = List[(String,String)]()
    var aLexemHasBeenFound = false
    var currentLongestLexem = Tuple2("", "")
    var currentLastIndex = 0
    var currentCharsInLexem = 0
    var charNumInMaxLexem = 0
    var currentDfaState = theDfa.firstState
    var lineNumber = 0
    var index = 0

    def resetVars(): Unit = {
      // reset every measure var
      currentLongestLexem = Tuple2("", "")
      aLexemHasBeenFound = false
      currentLastIndex = currentLastIndex + charNumInMaxLexem
      index = currentLastIndex
      currentCharsInLexem = 0
      charNumInMaxLexem = 0
      currentDfaState = theDfa.firstState
    }

    while (index < word.length) {
      var currentChar = word.charAt(index)

      if (!alphabet.contains(currentChar)) {
        return Left("No viable alternative at character " + index + ", line " + lineNumber)
      }

      if (currentChar == '\n' && !aLexemHasBeenFound) {
        lineNumber += 1
      }
      currentDfaState = theDfa.next(currentDfaState, currentChar)


      // If state is final
      if (theDfa.finalState.contains(currentDfaState)) {
        var i = 0
        var found = false
        allFinalStatesInPriorityOrder.foreach(nfaState => {
          if (currentDfaState.contains(nfaState) && !found) {
            found = true
            aLexemHasBeenFound = true
            currentCharsInLexem += 1
            charNumInMaxLexem = currentCharsInLexem
            currentLongestLexem = Tuple2(word.slice(currentLastIndex, currentLastIndex + currentCharsInLexem),
              splitSpec(i)._1)
          }
          i += 1
        })
        index += 1
      } else if (theDfa.sink.contains(currentDfaState)) { // If state is sink
        if (aLexemHasBeenFound) {
          // add longest lexem to final list
          foundLexems = foundLexems :+ currentLongestLexem
        } else {
          return Left("No viable alternative at character " + index + ", line " + lineNumber)
        }
        resetVars()
      } else {
        // If state is neither sink nor final
        currentCharsInLexem += 1
        index += 1
        if (index == word.length && !aLexemHasBeenFound) {
          return Left("No viable alternative at character EOF, line " + lineNumber)
        }
      }

      if (index == word.length && aLexemHasBeenFound) {
        foundLexems = foundLexems :+ currentLongestLexem
        resetVars()
      }
    }

    if (aLexemHasBeenFound) {
      foundLexems = foundLexems :+ currentLongestLexem
    }
    Right(foundLexems)
  }

}