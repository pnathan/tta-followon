%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (C) paul nathan 2014.
%
% what if we reframe the problem as a specification of the known facts
% about the program execution and provide the set of inferences from
% said facts. WHAT THEN?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a priori weld the lattice top to the threads
arc(top   , top, aleph, 0).
arc(top   , top, bet,   0).
arc(top   , top, gimel, 0).

arc(aleph, 1, bottom, bottom).
arc(bet  , 2, bottom, bottom).
arc(gimel, 0, bottom, bottom).

% the and bottom have no number or directionality
node(top    , top,    top).
node(bottom , bottom, bottom).

% messages
node(aleph  , 0, in).
node(aleph  , 1, out).

node(bet    , 0, out).
node(bet    , 1, in).
node(bet    , 2, in).

node(gimel  , 0, out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% here the facts stop

% a legit move can be backward from an in up one if up-one is an out OR
% any particular out

% previous node in the cpu sequence, IF it's an out.
causality(CpuA, NumberA, CpuB, NumberB) :-
        node(CpuA, NumberA, out),
        NumberB is NumberA + 1,
        node(CpuB, NumberB, in).

% a priori knowledge
causality(CpuA, NumberA, CpuB, NumberB) :-
        arc(CpuA, NumberA, CpuB, NumberB).

% pick up any OUT-IN pair.
causality(CpuA, NumberA, CpuB, NumberB) :-
        node(CpuA, NumberA, out),
        node(CpuB, NumberB, in),
        not(CpuA = CpuB).


% we landed at the bottom.
what_happened(CpuA, NumberA, CpuB, NumberB,
              [[CpuA, NumberA], [CpuB, NumberB]] ) :-
        CpuB = bottom,
        NumberB = bottom,
        arc(CpuA, NumberA, CpuB, NumberB).

what_happened(CpuA, NumberA, CpuB, NumberB,
              [[CpuA, NumberA], [CpuB, NumberB], [CpuC, NumberC], Rest]) :-
        causality(CpuA, NumberA, CpuB, NumberB),
        causality(CpuB, NumberB, CpuC, NumberC),
        what_happened(CpuB, NumberB, CpuC, NumberC, Rest).
