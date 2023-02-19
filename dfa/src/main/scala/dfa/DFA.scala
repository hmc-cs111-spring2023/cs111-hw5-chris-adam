package dfa // leave this line in the file

// TODO: replace this comment with your implementation


case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)

class DFA(states: Set[State], transitions: Set[Transition], start: State, accept: Set[State]):
    def accepts(input: String) = 
        var currentState = startState
        var currentInput = ""
        var result = false
        for (i <- input) {
            var validTransitions = transitions.filter((n:Transition) => (n.from == currentState && n.symbol == i))
            if (validTransitions.size != 1) then println("NonValid DFA!!!!")
            currentState = validTransitions.head.to
        }

        // check if we're in a accepting state
        if (acceptingStates.contains(currentState)) then true
        else false
            
    // for loop it (over length of input)
    // pull off relevant character of input
    // 