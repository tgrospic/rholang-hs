# Leslie Lamport about concurrency - May 8, 2019

Part 1 https://youtu.be/JhexdgiFWfA  
Part 2 https://youtu.be/hxDXwG6h32s

## Papers

A New Solution of Dijkstra's Concurrent Programming Problem  
https://lamport.azurewebsites.net/pubs/bakery.pdf

Time, Clocks, and the Ordering of Events in a Distributed System  
https://lamport.azurewebsites.net/pubs/time-clocks.pdf

Paxos Made Simple  
https://lamport.azurewebsites.net/pubs/paxos-simple.pdf

## Video notes

Bakery algorithm  
https://youtu.be/JhexdgiFWfA?t=1418  
- does not require any underlying mutual exclusion

With digital signatures consensus only needs majority  
https://youtu.be/JhexdgiFWfA?t=2998

Total order - distributed systems, relativity, causality  
https://youtu.be/JhexdgiFWfA?t=4925

Infinite state machine to describe anything  
https://youtu.be/JhexdgiFWfA?t=5142

Mutual exclusion problem (programming, mathematical or physical problem)  
https://youtu.be/JhexdgiFWfA?t=5650  
- (I)   A process which has been granted the resource must release it before it can be granted to another process.  
- (II)  Different requests for the resource must be granted in the order in which they are made.  
- (III) If every process which is granted the resource eventually releases it, then every request is eventually granted. 

Async circuits  
https://youtu.be/JhexdgiFWfA?t=7240

Algorithm is not a program  
https://youtu.be/JhexdgiFWfA?t=7961

Byzantine agreement  
https://youtu.be/JhexdgiFWfA?t=9084

FLP impossibility  
https://youtu.be/JhexdgiFWfA?t=9955

Single global invariant      // _Is this a tuple space?_  
https://youtu.be/hxDXwG6h32s?t=398

"Temporal logic was the right way to reason about liveness"  
https://youtu.be/hxDXwG6h32s?t=1040  
- every property can be expressed as conjunction of safety and liveness property
  - safety   = something does not produce bad answer
  - liveness = something good eventually happens (termination, responsiveness)
  - temporal logic provided a way to use safety properties in proving liveness properties (paper)

Branching time & linear time (temporal logic)  
https://youtu.be/hxDXwG6h32s?t=1289
- liner time logic for liveness  

Operational semantics = how to interpret program as a state machine  
https://youtu.be/hxDXwG6h32s?t=2037

TLA - state machine as a temporal logic formula  
https://youtu.be/hxDXwG6h32s?t=2153
- Temporal Logic of Actions
- assertion about pairs of states (state, next state)

Type safety can be stated as an invariant  
https://youtu.be/hxDXwG6h32s?t=2879
- use of types restrict expressiveness  
  _What about dependent types?_

`x' = x - 1 : Nat`  
_Why is this a problem with types? It's not a valid term._  
https://youtu.be/hxDXwG6h32s?t=3015
- variables and modules useful from programming
- formal definition of a definition
- hierarchical proof structure

TLA+ language  
https://youtu.be/hxDXwG6h32s?t=3529
- model checker to handle modular specifications  

TLA+ tools, proof assistant  
https://youtu.be/hxDXwG6h32s?t=4182

Abstraction != hiding information  
https://youtu.be/hxDXwG6h32s?t=5250
