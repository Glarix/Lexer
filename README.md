#### Codreanu Dan LFA 2022-2023

## Description

<p>In this project I implement a lexer in Scala language.</p>

---

## Stage 1

<p>
In this stage I had to implement a DFA starting from a PRENEX type input.

To get from Prenex to DFA I went through 3 programatic steps:
* Prenex -> AST
* AST -> NFA
* NFA -> DFA
</p>

---

## Prenex to AST transformation

<p>
    To create the AST I first parse the prenex to tokens and then start to build the AST with the help of a stack.
    The main idea is: When I take a token I determine which operation it is and then I build an AST node with that operation and push it to stack.
    Then I pop last node from stack and check if is completed, if so, I go down the stack and try to complete as many nodes as possible, if incomplete I push it back to stack and create next AST node.
</p>

---

## AST to NFA transformation

<p>
    Once I got the AST I start building the NFA from it. The building process is a recursive one. 
    I take a node from AST and determine it's type, once I have the type, I start aplying Thomson's algorithm on it by recursively aplying it on it's leafes and then establishing the new epsilon transitions for this operation.
</p>

---

## NFA to DFA transformation

<p>
    To create DFA I first need the NFA and the alphabet that DFA uses (I have a method that determines the alphabet from the tokens).
    Starting with firstState of NFA I compute the EpsilonClosure for this state and that will be my DFA's start state. After this I create the DFA recursively by determining the node to which I can get from current state by consuming a caracter from alphabet. This process is done for every character in alphabet.
</p>

---

### I also implemented the accept logic for both NFA and DFA

## Stage 2

<p>
    In this stage I had to implement the part of transforming an expression from normal regex form to prenex form that my NFA and DFA can process.
</p>

---

<p>
    To implement this part I split this transformation into smaller processing parts:
</p>

    1. Eliminated syntactic sugars such as: [a-z] or [0-9]
    2. Added a concatenation symbol where it was necessary. I used '_' as the concatenation symbol, 
        it was added in places like: "abc" -> "a_b_c"
    3. Using two stacks I transform the formatted regex into a prenex
    4. I write the regex special symbols as prenex operators.

<p>
    Flow example: 
    (ab(b|c)*)* => (a_b_(b|c)*)* => *__ab*|bc => STAR CONCAT CONCAT a b STAR UNION b c
</p>

---

## Stage 3

<p>
    In this stage I had to implement the actual Lexer functionality consisting of reading a specification and a block of text that will be divided into lexems always choosing the longest lexem or if of equal length choosing the one with the higher priority.
</p>

---


This are the steps this stage was deivided into:
*   Firstly I format the specification.
*   After that, I create an NFA for each token from the specification.
*   Next, I create a combined NFA and with it a DFA that will accept the longest lexem.
*   After all that, I parse the block of text given at input and return all the found lexems thus obtaining a parser functionality.