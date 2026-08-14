%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tta_nbody.pl -- N-body Targeted Trace Algorithm
% (C) Paul Nathan 2014, 2026. Channel-aware rewrite.
%
% Reconstructs feasible communication orderings from per-core traces
% of IN/OUT events on CSP-style channels.
%
% Key fix over 2014 version: channel-aware sequencing check.
% In synchronous CSP, every OUT blocks until matched with its IN,
% so in any valid linearisation every OUT is immediately followed
% by an IN on the SAME channel from a DIFFERENT core.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(lists)).
:- use_module(library(apply)).

%% ---------------------------------------------------------------------------
%% Dynamic state -- cleared between examples
%% ---------------------------------------------------------------------------

:- dynamic node/4.           % node(Core, SeqNum, Type, Channel)
:- dynamic channel/3.        % channel(Name, SenderCore, ReceiverCore)
:- dynamic edge/2.           % edge(From, To)  -- possibility-graph edges
:- dynamic all_comm_nodes/1. % cached sorted list of communication node ids

%% ---------------------------------------------------------------------------
%% Utility: node identity
%% ---------------------------------------------------------------------------

%% node_id(+Core, +SeqNum, -Id)
node_id(Core, SeqNum, Core/SeqNum).

%% all_cores(-Cores) -- sorted distinct cores
all_cores(Cores) :-
    findall(C, node(C, _, _, _), Bag),
    sort(Bag, Cores).

%% min_seq(+Core, -Min)
min_seq(Core, Min) :-
    findall(S, node(Core, S, _, _), Seqs),
    min_list(Seqs, Min).

%% max_seq(+Core, -Max)
max_seq(Core, Max) :-
    findall(S, node(Core, S, _, _), Seqs),
    max_list(Seqs, Max).

%% ---------------------------------------------------------------------------
%% Step 1 -- Build the possibility graph
%%
%% Edges:
%%   top -> first event (min seq) of each core
%%   last event (max seq) of each core -> bottom
%%   Within core: C/N -> C/(N+1)
%%   Cross-core:  every event on core A <-> every event on core B (A \= B)
%%
%% The cross-core edges are deliberately complete -- they represent all
%% POSSIBLE orderings.  The pruning step (sequencing check) discards
%% orderings that violate CSP synchronisation semantics.
%% ---------------------------------------------------------------------------

build_graph :-
    retractall(edge(_, _)),
    retractall(all_comm_nodes(_)),
    all_cores(Cores),
    %% Cache all communication node ids
    findall(Id, (node(C, S, _, _), node_id(C, S, Id)), AllNodes),
    sort(AllNodes, SortedNodes),
    assert(all_comm_nodes(SortedNodes)),
    %% top -> first event of each core
    forall(member(C, Cores),
           ( min_seq(C, Min),
             node_id(C, Min, Id),
             assert(edge(top, Id))
           )),
    %% last event of each core -> bottom
    forall(member(C, Cores),
           ( max_seq(C, Max),
             node_id(C, Max, Id),
             assert(edge(Id, bottom))
           )),
    %% Sequential within-core: C/N -> C/(N+1)
    forall(( node(C, N, _, _),
             N1 is N + 1,
             node(C, N1, _, _)
           ),
           ( node_id(C, N, IdA),
             node_id(C, N1, IdB),
             assert(edge(IdA, IdB))
           )),
    %% Cross-core: every pair of events on different cores (both directions)
    forall(( node(CoreA, SeqA, _, _),
             node(CoreB, SeqB, _, _),
             CoreA \= CoreB
           ),
           ( node_id(CoreA, SeqA, IdA),
             node_id(CoreB, SeqB, IdB),
             ( edge(IdA, IdB) -> true ; assert(edge(IdA, IdB)) )
           )).

%% ---------------------------------------------------------------------------
%% Step 2 -- Find all Hamiltonian paths from top to bottom
%%
%% Constraint during search: a node Core/N with N > 0 may only be
%% visited if Core/(N-1) has already been visited.  This enforces
%% within-core ordering without a separate pruning pass.
%% ---------------------------------------------------------------------------

find_all_paths(Paths) :-
    all_comm_nodes(AllNodes),
    length(AllNodes, Total),
    RequiredLen is Total + 2,   % + top + bottom
    findall(Path, ham_path(RequiredLen, Path), Paths).

ham_path(RequiredLen, Path) :-
    ham_path_(top, [top], RevPath),
    reverse(RevPath, Path),
    length(Path, RequiredLen).

ham_path_(bottom, Visited, Visited) :- !.
ham_path_(Current, Visited, Result) :-
    edge(Current, Next),
    \+ member(Next, Visited),
    core_order_ok(Next, Visited),
    ham_path_(Next, [Next|Visited], Result).

%% core_order_ok(+Node, +Visited)
%%   Succeeds if visiting Node does not violate within-core ordering.
%%   For Core/N with N > 0, Core/(N-1) must already be in Visited.
%%   top and bottom always pass.
core_order_ok(top, _) :- !.
core_order_ok(bottom, _) :- !.
core_order_ok(Core/0, _) :- !,
    node(Core, 0, _, _).   % just verify it is a real node
core_order_ok(Core/N, Visited) :-
    N > 0,
    Prev is N - 1,
    member(Core/Prev, Visited).

%% ---------------------------------------------------------------------------
%% Step 3 -- Prune infeasible paths
%% ---------------------------------------------------------------------------

%% feasible(+Path)
feasible(Path) :-
    check_length(Path),
    check_bad_start(Path),
    check_sequencing(Path).

%% check_length(+Path)
%%   Path must contain all communication nodes plus top and bottom.
check_length(Path) :-
    all_comm_nodes(AllNodes),
    length(AllNodes, N),
    length(Path, Len),
    Len =:= N + 2.

%% check_bad_start(+Path)
%%   First node after top must be an OUT.
check_bad_start([top, Core/Seq | _]) :-
    node(Core, Seq, out, _).

%% check_sequencing(+Path)
%%   In synchronous CSP every OUT is immediately followed by the
%%   matching IN on the same channel from a different core.
%%   An OUT followed by anything else (same core next, wrong channel,
%%   wrong type, or bottom) is infeasible.
check_sequencing(Path) :-
    \+ has_sequencing_violation(Path).

has_sequencing_violation([]) :- !, fail.
has_sequencing_violation([_]) :- !, fail.
has_sequencing_violation([top | Rest]) :- !,
    has_sequencing_violation(Rest).
has_sequencing_violation([CoreA/SeqA, Next | _]) :-
    node(CoreA, SeqA, out, Ch),
    \+ valid_out_successor(CoreA, Ch, Next),
    !.
has_sequencing_violation([_ , Next | Rest]) :-
    has_sequencing_violation([Next | Rest]).

%% valid_out_successor(+OutCore, +OutChannel, +NextNode)
%%   The node immediately following an OUT must be an IN on the same
%%   channel from a different core.
valid_out_successor(OutCore, Ch, CoreB/SeqB) :-
    node(CoreB, SeqB, in, Ch),
    OutCore \= CoreB.

%% ---------------------------------------------------------------------------
%% Step 4 -- Top-level analysis
%% ---------------------------------------------------------------------------

find_feasible_paths(Feasible) :-
    find_all_paths(AllPaths),
    include(feasible, AllPaths, Feasible).

%% ---------------------------------------------------------------------------
%% Pretty printing
%% ---------------------------------------------------------------------------

print_nodes :-
    format("~n--- Communication Events ---~n"),
    all_cores(Cores),
    forall(member(C, Cores),
           ( format("  Core ~w:~n", [C]),
             findall(S-T-Ch, node(C, S, T, Ch), Events),
             msort(Events, Sorted),
             forall(member(S-T-Ch, Sorted),
                    format("    [~w] ~w ~w on ~w~n", [S, C, T, Ch]))
           )).

print_channels :-
    format("~n--- Channel Topology ---~n"),
    ( channel(_, _, _) ->
        forall(channel(Name, Sender, Receiver),
               format("  ~w : ~w -> ~w~n", [Name, Sender, Receiver]))
    ;
        format("  (none declared)~n")
    ).

print_path(Path) :-
    format("  ["),
    print_path_items(Path),
    format("]~n").

print_path_items([]) :- !.
print_path_items([X]) :- !,
    format("~w", [X]).
print_path_items([X|Rest]) :-
    format("~w, ", [X]),
    print_path_items(Rest).

print_analysis :-
    print_nodes,
    print_channels,
    build_graph,
    format("~n--- Possibility Graph ---~n"),
    findall(A-B, edge(A, B), Edges),
    length(Edges, NumEdges),
    format("  ~w edges in graph~n", [NumEdges]),
    format("~n--- Finding Hamiltonian Paths ---~n"),
    find_all_paths(AllPaths),
    length(AllPaths, NumAll),
    format("  Total Hamiltonian paths: ~w~n", [NumAll]),
    format("~n--- Feasibility Pruning ---~n"),
    include(feasible, AllPaths, Feasible),
    length(Feasible, NumFeas),
    format("  Feasible paths: ~w~n", [NumFeas]),
    ( Feasible = [] ->
        format("  (none)~n")
    ;
        forall(member(P, Feasible), print_path(P))
    ).

%% ---------------------------------------------------------------------------
%% Clean up between examples
%% ---------------------------------------------------------------------------

cleanup :-
    retractall(node(_, _, _, _)),
    retractall(channel(_, _, _)),
    retractall(edge(_, _)),
    retractall(all_comm_nodes(_)).

%% ---------------------------------------------------------------------------
%% Example 1: Simple 2-core producer-consumer
%%   Producer (core p) sends 3 messages; Consumer (core c) receives 3.
%%   Single channel: ch_p_c
%%
%%   Expected: exactly 1 feasible path (strict interleave)
%%     top, p/0, c/0, p/1, c/1, p/2, c/2, bottom
%% ---------------------------------------------------------------------------

setup_producer_consumer :-
    cleanup,
    assert(channel(ch_p_c, p, c)),
    assert(node(p, 0, out, ch_p_c)),
    assert(node(p, 1, out, ch_p_c)),
    assert(node(p, 2, out, ch_p_c)),
    assert(node(c, 0, in, ch_p_c)),
    assert(node(c, 1, in, ch_p_c)),
    assert(node(c, 2, in, ch_p_c)).

%% ---------------------------------------------------------------------------
%% Example 2: 3-body ring, one timestep
%%   Bodies: body0, body1, body2 in a ring.
%%   Even-indexed bodies send first; odd-indexed receive first.
%%   This avoids the circular-wait deadlock that arises when all
%%   bodies try to send simultaneously in synchronous CSP.
%% ---------------------------------------------------------------------------

setup_3body_1step :-
    cleanup,
    assert(channel(ch_0_1, body0, body1)),
    assert(channel(ch_1_2, body1, body2)),
    assert(channel(ch_2_0, body2, body0)),
    % body0 (even): send first, then receive
    assert(node(body0, 0, out, ch_0_1)),
    assert(node(body0, 1, in,  ch_2_0)),
    % body1 (odd): receive first, then send
    assert(node(body1, 0, in,  ch_0_1)),
    assert(node(body1, 1, out, ch_1_2)),
    % body2 (even): send first, then receive
    assert(node(body2, 0, out, ch_2_0)),
    assert(node(body2, 1, in,  ch_1_2)).

%% ---------------------------------------------------------------------------
%% Example 3: 3-body ring, two timesteps
%%   Same topology + alternation, each body does 2 rounds.
%% ---------------------------------------------------------------------------

setup_3body_2step :-
    cleanup,
    assert(channel(ch_0_1, body0, body1)),
    assert(channel(ch_1_2, body1, body2)),
    assert(channel(ch_2_0, body2, body0)),
    % body0 (even): OUT, IN, OUT, IN
    assert(node(body0, 0, out, ch_0_1)),
    assert(node(body0, 1, in,  ch_2_0)),
    assert(node(body0, 2, out, ch_0_1)),
    assert(node(body0, 3, in,  ch_2_0)),
    % body1 (odd): IN, OUT, IN, OUT
    assert(node(body1, 0, in,  ch_0_1)),
    assert(node(body1, 1, out, ch_1_2)),
    assert(node(body1, 2, in,  ch_0_1)),
    assert(node(body1, 3, out, ch_1_2)),
    % body2 (even): OUT, IN, OUT, IN
    assert(node(body2, 0, out, ch_2_0)),
    assert(node(body2, 1, in,  ch_1_2)),
    assert(node(body2, 2, out, ch_2_0)),
    assert(node(body2, 3, in,  ch_1_2)).

%% ---------------------------------------------------------------------------
%% Example 4: 3-body ring DEADLOCK demo
%%   All bodies try to send first -- circular wait in synchronous CSP.
%%   The TTA correctly reports 0 feasible paths.
%% ---------------------------------------------------------------------------

setup_3body_deadlock :-
    cleanup,
    assert(channel(ch_0_1, body0, body1)),
    assert(channel(ch_1_2, body1, body2)),
    assert(channel(ch_2_0, body2, body0)),
    % All bodies: OUT first, IN second -- deadlock!
    assert(node(body0, 0, out, ch_0_1)),
    assert(node(body0, 1, in,  ch_2_0)),
    assert(node(body1, 0, out, ch_1_2)),
    assert(node(body1, 1, in,  ch_0_1)),
    assert(node(body2, 0, out, ch_2_0)),
    assert(node(body2, 1, in,  ch_1_2)).

%% ---------------------------------------------------------------------------
%% N-body ring generator (generic)
%% ---------------------------------------------------------------------------

%% setup_nbody(+N)  -- N bodies, 1 timestep
setup_nbody(N) :-
    setup_nbody(N, 1).

%% setup_nbody(+N, +Steps) -- N bodies, Steps timesteps
setup_nbody(N, Steps) :-
    cleanup,
    N > 1, Steps > 0,
    MaxI is N - 1,
    forall(between(0, MaxI, I), setup_one_body(I, N, Steps)).

setup_one_body(I, N, Steps) :-
    J is (I + 1) mod N,
    K is (I - 1 + N) mod N,
    atom_number(AI, I),
    atom_number(AJ, J),
    atom_number(AK, K),
    atomic_list_concat([ch_, AI, '_', AJ], ChOut),
    atomic_list_concat([ch_, AK, '_', AI], ChIn),
    atomic_list_concat([body, AI], Core),
    atomic_list_concat([body, AJ], CoreJ),
    %% Declare channel (avoid duplicates)
    ( channel(ChOut, _, _) -> true
    ; assert(channel(ChOut, Core, CoreJ))
    ),
    %% Even-indexed bodies: OUT first, IN second (per step)
    %% Odd-indexed bodies:  IN first, OUT second (per step)
    %% This alternation avoids the CSP circular-wait deadlock.
    MaxStep is Steps - 1,
    EvenBody is I mod 2,
    forall(between(0, MaxStep, Step),
           ( Seq0 is Step * 2,
             Seq1 is Step * 2 + 1,
             ( EvenBody =:= 0 ->
                 assert(node(Core, Seq0, out, ChOut)),
                 assert(node(Core, Seq1, in,  ChIn))
             ;
                 assert(node(Core, Seq0, in,  ChIn)),
                 assert(node(Core, Seq1, out, ChOut))
             )
           )).

%% ---------------------------------------------------------------------------
%% Driver predicates
%% ---------------------------------------------------------------------------

run_example(producer_consumer) :-
    format("~n====================================================~n"),
    format("  Example: 2-core Producer-Consumer (3 msgs)~n"),
    format("====================================================~n"),
    setup_producer_consumer,
    print_analysis.

run_example('3body_deadlock') :-
    format("~n====================================================~n"),
    format("  Example: 3-body Ring DEADLOCK (all send first)~n"),
    format("====================================================~n"),
    setup_3body_deadlock,
    print_analysis.

run_example('3body_1step') :-
    format("~n====================================================~n"),
    format("  Example: 3-body Ring -- 1 timestep~n"),
    format("====================================================~n"),
    setup_3body_1step,
    print_analysis.

run_example('3body_2step') :-
    format("~n====================================================~n"),
    format("  Example: 3-body Ring -- 2 timesteps~n"),
    format("====================================================~n"),
    setup_3body_2step,
    print_analysis.

main :-
    run_example(producer_consumer),
    run_example('3body_deadlock'),
    run_example('3body_1step'),
    run_example('3body_2step'),
    format("~n====================================================~n"),
    format("  All examples complete.~n"),
    format("====================================================~n").
