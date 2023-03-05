import scala.collection.mutable

object Regex {

  /**
   * method that checks if character is operator
   */
  def isOperator(op: Char): Boolean = {
    if (op == '|' || op == '*' || op == '_' || op == '?' || op == '+') {
      return true
    }
    return false
  }

  /**
   * method that checks if character is a one way operator
   */
  def isOneWayOperator(op: Char) : Boolean = {
    if (op == '+' || op == '?' || op == '*') {
      return true
    }
    return false
  }

  /**
   * method that eliminates the syntactic sugars from a string
   * @param str initial regex
   * @return sugarless regex
   */
  def eliminateSugars(str: Array[Char]): String = {
    var finalStr = ""

    var i = 0
    while (i < str.length) {
      if (str(i) == '[') {
        val fst = str(i + 1)
        val lst = str(i + 3)
        val diff:Int = lst - fst
        var j = 0
        finalStr = finalStr.concat("(")
        while (j <= diff) {
          finalStr = finalStr.concat((fst + j).toChar.toString + "|")
          j += 1
        }
        finalStr = finalStr.take(finalStr.length - 1)
        finalStr = finalStr.concat(")")
        i += 4
      } else {
        finalStr = finalStr.concat(str(i).toString)
      }

      i += 1
    }

    finalStr
  }

  /**
   * method that processes a regex and adds concatenation character where due
   * @param str
   * @return
   */
  def addConcatWhereDue(str: Array[Char]): String = {
    var i = 0;
    var finalString = ""
    var shouldAddConcat: Boolean = false

    while (i < str.length) {
      if (isOneWayOperator(str(i))) {
        finalString += str(i).toString
        shouldAddConcat = true // after a one way operator we should add concat
      } else if (str(i) == '|') {
        finalString += str(i).toString
        shouldAddConcat = false // before and after | never add concat
      } else if (str(i) == '(') {
        if (shouldAddConcat) {
          finalString += "_"
          finalString += str(i).toString
        } else {
          finalString += str(i).toString
        }
        shouldAddConcat = false // after ( never add concat
      } else if (str(i) == ')') {
        finalString += str(i).toString
        shouldAddConcat = true // after ) always add concat
      } else if (str(i) == '\'') { // like a normal char but take all 3 and they will be taken care of by PRENEX processing
        if (shouldAddConcat) {
          finalString += "_"
        }
        finalString += str(i).toString
        while (str(i + 1) != '\'') {
          finalString += str(i + 1).toString
          i += 1
        }
        finalString += str(i + 1).toString
        i += 1
        shouldAddConcat = true // after a character always add concat
      } else { // if is a character
        if (shouldAddConcat) {
          finalString += "_"
        }
        finalString += str(i).toString
        shouldAddConcat = true // after a character we should add concat
      }

      i += 1
    }

    finalString
  }

  def getPriority(op: Char): Int = {
    if (op == '?' || op == '+' || op == '*') {
      return 3
    }
    if (op == '_') {
      return 2
    }

    if (op == '|') {
      return 1
    }
    0
}

  /**
   * method that gets a processed regex and transforms it in prenex form
   * @param str the processed regex
   * @return prenex for (only with symbols)
   */
  def transformInPrenex(str: String) : String = {
      val expression = mutable.Stack[String]() // expression stack
      val opStack = mutable.Stack[Char]() // operators stack

      var i = 0;

      while (i < str.length) {
        if(str.charAt(i) == '\'') {
          var united = "".concat(str.charAt(i).toString)
          while (str.charAt(i + 1) != '\'') {
            united = united.concat(str.charAt(i + 1).toString)
            i += 1
          }
          united = united.concat(str.charAt(i + 1).toString);
          expression.push(united)
          i += 1
        } else if (str.charAt(i) == '(') {
          opStack.push(str.charAt(i))
        } else if (str.charAt(i) == ')') { // if ( found, pop from operator stack until a ) is found and construct the expression with new operators
          while (opStack.nonEmpty && opStack.top != '(') {
            val currentOp = opStack.pop()
            if (isOneWayOperator(currentOp)) {
              val s1 = expression.pop()
              val united = "".concat(currentOp.toString).concat(s1)
              expression.push(united)
            } else {
              val s1 = expression.pop()
              val s2 = expression.pop()
              val united = "".concat(currentOp.toString).concat(s2).concat(s1)
              expression.push(united)
            }
          }
          opStack.pop() // eliminate (
        } else if (!isOperator(str.charAt(i))) { // push character to expression
          expression.push(str.charAt(i).toString)
        } else {
          // while current character is lower priority that top of stack
          // add top of stack to expression
          while (opStack.nonEmpty && getPriority(str.charAt(i)) <= getPriority(opStack.top)) {
            val currentOp = opStack.pop()
            if (isOneWayOperator(currentOp)) {
              val s1 = expression.pop()
              val united = "".concat(currentOp.toString).concat(s1)
              expression.push(united)
            } else {
              val s1 = expression.pop()
              val s2 = expression.pop()
              val united = "".concat(currentOp.toString).concat(s2).concat(s1)

              expression.push(united)
            }
          }
          opStack.push(str.charAt(i))
        }

        i += 1
      }

      while (opStack.nonEmpty) { // add all left operators to final expression
        val currentOp = opStack.pop()
        if (currentOp != '(' && currentOp != ')') {
          if (isOneWayOperator(currentOp)) {
            val s1 = expression.pop()
            val united = "".concat(currentOp.toString).concat(s1)
            expression.push(united)
          } else {
            val s1 = expression.pop()
            val s2 = expression.pop()
            val united = "".concat(currentOp.toString).concat(s2).concat(s1)
            expression.push(united)
          }
        }
      }

      expression.pop()
    }

  /**
   * method that change operator symbols into operators for final prenex expressions
   */
  def getOperatorForPrenex(op: Char): String = {
    if (op == '_') {
      return "CONCAT"
    }
    if (op == '|') {
      return "UNION"
    }

    if (op == '*') {
      return "STAR"
    }

    if (op == '?') {
      return "MAYBE"
    }

    "PLUS"
  }

  /**
   * method that takes a symbol type prenex and converts it to regular prenex
   */
  def writePrenexOperators(almostPrenex: String): String = {
    var toReturn = ""

    var i = 0

    while (i < almostPrenex.length) {
      if (almostPrenex.charAt(i) == '\'') {
//        toReturn += "".concat(almostPrenex.charAt(i).toString)
//                      .concat(almostPrenex.charAt(i + 1).toString)
//                      .concat(almostPrenex.charAt(i + 2).toString).concat(" ")
//        i += 2
        toReturn += "".concat(almostPrenex.charAt(i).toString)
        while (almostPrenex.charAt(i + 1) != '\'') {
          toReturn = toReturn.concat(almostPrenex.charAt(i + 1).toString)
          i += 1
        }
        toReturn = toReturn.concat(almostPrenex.charAt(i + 1).toString + " ")
        i += 1
      } else if (!isOperator(almostPrenex.charAt(i))) {
        toReturn += "".concat(almostPrenex.charAt(i).toString + " ")
      } else {
        toReturn += "".concat(getOperatorForPrenex(almostPrenex.charAt(i)) + " ")
      }

      i += 1
    }
    toReturn.take(toReturn.length - 1)
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    if (str.equals("eps")) {
      return "eps"
    }
    val sugarlessString = eliminateSugars(str.toCharArray)
    val concatedString = addConcatWhereDue(sugarlessString.toCharArray)
    val almostPrenex = transformInPrenex(concatedString)
    val toReturn = writePrenexOperators(almostPrenex)

    toReturn
  }

  def main(args: Array[String]): Unit = {
    val d = Dfa.fromPrenex(Regex.toPrenex("' '"))
    var a = 1
  }

}
