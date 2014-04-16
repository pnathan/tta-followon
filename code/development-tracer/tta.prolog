% (C) paul nathan 2014.
%
% what if we reframe the problem as a specification of the known facts
% about the program execution and provide the set of inferences from
% said facts. WHAT THEN?

% weld the lattice top to the threads
arc(top, nil, aleph, 0).
arc(top, nil, bet,   0).

% the top has no number or directionality
node(top, top, nil).

% messages
node(aleph, 0, out).
node(bet, 0, in).

node(aleph, 1, out).
node(bet, 1, in).

node(aleph, 2, out).
node(bet, 2, in).

node(bottom, bottom, nil).

arc(bet,   2, bottom, nil).
arc(aleph, 2, bottom, nil).


%%% here the facts stop

not(P) :- (call(P) -> fail ; true).

% check for node validity
valid_node_p(Node, Number) :- node(Node, Number, _).

correct_order_p(ValueA, _) :- ValueA = bottom, !.
correct_order_p(ValueA, ValueB) :- ValueA < ValueB.

% CPU causality
before_p(NodeA, NumberA, NodeB, NumberB) :- arc(NodeA, NumberA, NodeB, NumberB).
before_p(NodeA, NumberA, NodeB, NumberB) :-
        valid_node_p(NodeA, NumberA),
        valid_node_p(NodeB, NumberB),
        NodeA == NodeB,
        correct_order_p(NumberA, NumberB).

% A sends to B.
arc_able_p(NodeA, NumA, NodeB, NumB) :-
        node(NodeA, NumA, out),
        node(NodeB, NumB, in).

% Message causality -  IDK.
causality(NodeA, NumberA, NodeB, NumberB) :-
        arc_able_p(NodeA, NumberA, NodeB, NumberB).