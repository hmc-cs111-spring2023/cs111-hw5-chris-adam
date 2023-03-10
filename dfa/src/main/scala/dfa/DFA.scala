package dfa // leave this line in the file

case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)

class DFA(val states: Set[State], val transitions: Set[Transition], val start: State, val accept: Set[State]):
    // run the given input on our DFA and determine whether it accepts or rejects
    def accepts(input: String) = 
        var currentState = start
        var currentInput = ""
        var result = false

        // go through each instruction, find proper transition, update state
        for (i <- input) {
            var validTransitions = transitions.filter((n:Transition) => (n.from == currentState && n.symbol == i))
            if (validTransitions.size != 1) then println("NonValid DFA!!!!")
            currentState = validTransitions.head.to
        }

        // check if we're in a accepting state
        if (accept.contains(currentState)) then true
        else false
