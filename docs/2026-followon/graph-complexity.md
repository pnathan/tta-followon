# Graph Complexity of the Targeted Trace Algorithm

**A formal complexity review for the n-process CSP extension**

*2026 follow-on work. Reviews `code/development-tracer/tta.prolog` (2014),
`code/development-tracer/tta.lisp`, and Chapter 3 of the Master's report
(`masters-work/report.pdf`, esp. pp. 27–33).*

---

## Executive summary

The TTA, as formulated in the report and in the 2014 Prolog sketch, is an
**enumerate-then-filter** algorithm: build a "possibility graph," enumerate all
⊤→⊥ paths, then discard infeasible ones. This review establishes the following:

1. The path-enumeration framing is **the wrong complexity frame**. The problem
   TTA actually solves is not Hamiltonian path; for point-to-point channels it
   is the **linear extension problem for a partial order**, and the partial
   order itself is computable in **O(N)** time (N = total events). The
   exponential cost in the thesis formulation is an artifact of the encoding,
   not intrinsic to the question "what happened?"
2. The intrinsic hardness, precisely located: (a) **counting** feasible
   linearizations is #P-hard in general (it contains counting linear
   extensions, which is #P-complete, Brightwell–Winkler 1991) but polynomial
   for a **fixed number of cores**; (b) with **shared channels** (multiple
   senders), even *deciding* feasibility becomes NP-complete-flavored
   (analogous to Gibbons–Korach's verification of sequential consistency);
   (c) **enumeration** is exponential only because the *output* is exponential
   — per-solution delay can be made O(N), even O(1) amortized.
3. The correct n-process deliverable is not "the set of feasible paths" but
   **the happens-before partial order (one per feasible OUT/IN matching)**.
   Under Mazurkiewicz trace equivalence, *all* linear extensions of one
   matching are a single equivalence class: the set of paths the thesis
   enumerates is one object wearing exponentially many disguises.
4. The author's goal — "the O(n) complexity is not straightforward" — is
   resolvable: **linear time in trace size is achievable** for the
   dedicated-channel case (ring n-body included), and **provably not
   achievable** for exhaustive linearization output or for shared-channel
   matching search (unless P = NP). Section 8 gives the minimum viable
   pipeline.

Concrete numbers (Section 7): a ring of 8 bodies traced for 3 timesteps —
48 events total — has **5,138,284,544 feasible linearizations** but exactly
**one** happens-before order, computable and renderable in microseconds.

---

## 1. Formal model

### 1.1 Traces, matchings, and collapse

**Definition 1 (trace instance).** An instance is a tuple (n, {T_i}, C, chan)
where there are *n* cores; core *i* has a finite sequence
T_i = e_{i,1} … e_{i,k_i} of events; each event carries a direction
dir(e) ∈ {OUT, IN} and a channel chan(e) ∈ C, |C| = c. Let
N = Σ k_i (write k for the common per-core length when uniform, so N = nk).
**Program order** po is the union of the n per-core total orders.

**Definition 2 (matching).** A matching M is a bijection from OUT events to IN
events such that matched events share a channel and (for cross-core
communication) lie on distinct cores. M formalizes "which send was consumed by
which receive" — the information the trace does *not* record.

**Definition 3 (collapse; synchronous semantics).** CSP rendezvous is
synchronous: an OUT and its matched IN complete together (report p. 28, rules
1–2). Form the quotient graph G_M by merging each matched pair {o, M(o)} into a
single **rendezvous node**, and taking the image of the po edges. G_M has N/2
rendezvous nodes (plus any unmatched local events).

**Definition 4 (feasibility).** M is *feasible* iff G_M is acyclic. When it is,
G_M generates a partial order ≺_M — the **happens-before order** — and the
**feasible linearizations** under M are exactly the linear extensions of ≺_M,
each expanded by writing each rendezvous node as its OUT immediately followed
by its IN.

This last expansion *is* the thesis's alternation rule ("cross-core transitions
must alternate OUT/IN," report pp. 28–29): a cross-core hop in a feasible path
is precisely the interior of a rendezvous. Definition 4 makes the rule a
consequence of the semantics rather than a post-hoc filter — the pivotal move
for everything below.

The equivalence "feasible ⟺ acyclic quotient" is the classical
**crown-free / RSC criterion** of Charron-Bost, Mattern and Tel (1996): a
distributed computation is realizable with synchronous communication iff the
graph obtained by contracting each message has no cycle ("crown"). TTA's
feasibility test is exactly RSC-checking, which is **polynomial** once the
matching is known.

**Definition 5 (the TTA problems).** For an instance I:

- **TTA-Exists**: does a feasible (M, linearization) pair exist?
- **TTA-Order**: output {≺_M : M feasible}.
- **TTA-Count**: output the number of feasible linearizations.
- **TTA-Enum**: output all feasible linearizations (the thesis's phase 2).

### 1.2 Two readings of the possibility graph

The report's Algorithm 1 (p. 32) builds a graph whose edges are (i) ⊤ to each
core's first event, last events to ⊥; (ii) per-core successors; (iii) every IN
to "all possible OUTs." A ⊤→⊥ path visiting all nodes is a candidate, then
filtered. Two distinct objects are conflated here:

- **Causal chains** (edges = possible causality): a path is a chain in a
  candidate happens-before relation. Chains do not visit all nodes when
  communication is not totally ordered — the report itself observes this for
  disjoint pairs 0↔1 and 2↔3 (p. 30) and for 1:n "multipaths" (tta.lisp
  header). The all-nodes rule 3 is unsatisfiable on such instances.
- **Temporal linearizations** (consecutive = adjacent in time): here
  consecutive events need *no* causal edge at all — a context switch may
  follow any event with any enabled event on another core. Modeling
  linearizations as *edge*-paths therefore forces edges between causally
  unrelated events, which is exactly what the channel-blind rule 3 of
  `tta.prolog` supplies, and why it explodes.

The clean resolution: linearizations are not paths in a fixed sparse graph;
they are **topological orders of G_M** (equivalently, maximal chains in the
distributive lattice of order ideals of ≺_M — Birkhoff's lattice, which is the
honest version of the report's "lattice" remark). The possibility graph should
be retired in favor of (matching candidates, collapse, DAG).

### 1.3 Correcting the n! remark

Report p. 32: "in a complete graph G, there are n! paths possible between ⊤
and ⊥." The relevant count for n cores of k events, with program order
respected and nothing else constrained, is the number of interleavings — the
multinomial

    M(n, k) = (nk)! / (k!)^n ,   log M(n,k) = nk·log n · (1 + o(1)).

So the growth is n^Θ(nk), *worse* than n! for k > 1. Computed values in
Section 7, Table 3. If program order is *not* used to restrict candidate paths
(as in a fully naive all-paths over a dense digraph), the space is the set of
simple ⊤→⊥ paths, up to Θ((nk)!) — and merely *counting* simple s–t paths in a
digraph is #P-complete (Valiant 1979). Enumerate-then-filter is therefore
doomed at the "enumerate" stage, before feasibility filtering does any work.

---

## 2. Findings from the 2014 Prolog formulation (empirical)

The example in `tta.prolog` was run under SWI-Prolog with the queries
`findall(_, causality(A,NA,B,NB), _)` and
`findall(P, what_happened(top, top, _, _, P), Ps)`. Results:

- **15 causality edges** are derived; `what_happened` yields exactly **2
  solutions**: `[top, gimel0, bottom]` and `[top, gimel0, bet2, bottom]`.
  Neither visits more than one or two of the six real events; the program has
  no all-nodes rule and no feasibility rules, so it enumerates causal
  *fragments*, not linearizations. The file is a sketch of phase 1 only.
- **Bug (known): channel-blind rule 3** (lines 49–52) pairs any OUT with any IN
  on another core. In the example it contributes 6 cross edges; with channel
  labels most would vanish. In general it contributes Θ(n²k²) edges (Section
  4.1).
- **Bug (new): rule 1 leaves `CpuB` unbound** (lines 39–42):
  ```prolog
  causality(CpuA, NumberA, CpuB, NumberB) :-
          node(CpuA, NumberA, out),
          NumberB is NumberA + 1,
          node(CpuB, NumberB, in).
  ```
  Intended as "per-core successor if it is an IN," it actually links an OUT to
  an IN with the successor *index* on **any** core. Empirically it derives the
  spurious edge `gimel0 → bet1` (index coincidence across cores). Fix:
  add `CpuB = CpuA`.
- **Bug (new): no IN→successor program-order edges.** `causality` provides no
  edge out of an IN node except a-priori arcs, so every chain dies at the first
  IN that is not welded to ⊥. This is why only `gimel0`-initial chains appear.
  Program order must be asserted for *all* consecutive per-core pairs,
  independent of direction.
- **Bug (new): nontermination risk.** `what_happened` keeps no visited set. In
  the present example the derived edge set happens to be a DAG, but the
  intended rule set (all OUT→IN cross pairs, both directions between cores,
  plus full program order) contains cycles — the report notes cycles arise
  (p. 31–32) — and Prolog depth-first enumeration over a cyclic `causality`
  relation does not terminate. A visited list (or the reformulation in
  Section 5) is mandatory.
- **Duplicate derivations**: `causality(aleph,1,bet,2)` is derivable by both
  rule 1 (via the unbound-CpuB bug) and rule 3, so paths through it are
  enumerated twice. Any counting on top of raw Prolog backtracking will be
  wrong without `distinct/2` or a set-of-solutions discipline.

`tta.lisp` stops before the interesting part: `find-channels-in` and
`link-up-nodes-in-channels` are stubs; no all-paths or feasibility code exists.
Its header contains the correct problem statement, including the 1:n
"multipath" caveat.

---

## 3. Complexity of the TTA problems

Throughout: n cores, k events/core, N = nk events, c channels.

### 3.1 Structural lemmas

**Lemma 1 (uncrossing; FIFO is forced).** On a channel with a single sender
core and single receiver core, every feasible matching is the order-preserving
bijection (i-th OUT ↔ i-th IN).
*Proof.* Suppose o₁ ≺ o₂ in sender program order and M crosses: M(o₁) = i₂,
M(o₂) = i₁ with i₁ ≺ i₂ on the receiver. Collapse: node u = {o₁,i₂},
v = {o₂,i₁}. Sender order gives u → v; receiver order gives v → u; G_M has a
2-cycle, contradiction. Induction on the leftmost crossing gives the claim. ∎

Consequently, for **dedicated point-to-point channels the matching is unique
and computable in O(N)** by counting occurrences per channel. All "possibility"
disappears from phase 1; only the order ambiguity of phase 2 remains.

**Lemma 2 (no dead ends, fixed matching).** If G_M is acyclic, every prefix of
a linear extension of ≺_M extends to a complete one.
*Proof.* Removing a down-set from a DAG leaves a DAG, which is nonempty ⇒ has a
minimal element; append any minimal element and induct. ∎

Lemma 2 is what licenses **backtrack-free online enumeration** (Section 5.1):
a partially built feasible schedule can never get stuck, so search-tree size =
output size. With shared channels Lemma 2 fails — a locally valid rendezvous
choice can strand a later OUT with no remaining co-enabled IN — and that
failure is precisely where NP-hardness enters (Section 3.2).

### 3.2 Decision: TTA-Exists

**Theorem 1 (dedicated channels).** With single-sender single-receiver
channels, TTA-Exists is decidable in O(N) time: compute the forced matching
(Lemma 1), collapse with union–find, topologically sort (Kahn 1962). A cycle
certifies infeasibility — and, for a trace that claims to be a real execution,
diagnoses either trace corruption or a deadlocked/instrumentation-truncated
run. (The all-OUT-first ring deadlock is caught exactly this way; verified
computationally for n = 4: the collapsed graph is cyclic.)

**Claim 1 (shared channels).** With multi-sender channels (or XC `select`,
which makes even the *channel* of an IN a choice among a set), TTA-Exists is
NP-complete.
*Membership*: guess M and a linearization; verify in O(N).
*Hardness (sketch)*: this is the rendezvous analogue of **verifying sequential
consistency**, NP-complete even with known read-mapping (Gibbons–Korach 1997),
and of NP-hard trace-consistency problems surveyed by Chini et al. A direct
reduction from SAT can be assembled in the same style: one two-sender channel
per variable, where which sender's OUT matches the receiver's first IN encodes
the truth value (Lemma 1's uncrossing no longer forces it); clause gadgets
chain each literal's "false" choice into a potential collapsed cycle that is
broken iff some literal is chosen true, so G_M is acyclic for some M iff the
formula is satisfiable. *Status: proof sketch; writing out the clause gadget
and verifying it against the crown criterion is a worthwhile, publishable
lemma for the follow-on work — the closest published results are for
shared-memory consistency, not CSP rendezvous.*

### 3.3 Counting: TTA-Count

**Theorem 2.** TTA-Count ∈ #P; it is #P-hard for unbounded n; and for fixed n
it is computable in O(n · D) time where D is the number of order ideals
(down-sets) of the collapsed poset, D ≤ (k/2 + 1)^n.

*In #P*: a feasible linearization is a polynomial-size witness verified in
polynomial time.

*Hardness*: counting linear extensions of an arbitrary poset is #P-complete
(Brightwell–Winkler 1991), and remains #P-complete for height-2 posets
(Dittmer–Pak 2020). A poset of width w embeds into a w-core rendezvous trace
by Dilworth chain decomposition, with each cross-chain covering relation
x ⋖ y enforced by an auxiliary dedicated-channel rendezvous inserted after x
on x's core and before y on y's core. *Caveat, stated honestly:* the auxiliary
rendezvous nodes subdivide the poset, so the reduction as sketched is Turing
rather than parsimonious (the augmented extension count is not a clean
function of the original); the community's standard reading is that
linearization-counting over message-passing traces inherits #P-hardness, but a
fully rigorous write-up needs either an interpolation argument over gadget
multiplicities or a direct #P-hardness proof for the realizable poset class.
Flagged as a second publishable lemma.

*Fixed n upper bound*: the collapsed poset has width ≤ n, so its ideals are
determined by an n-vector of per-core frontiers; dynamic programming over the
ideal lattice (Birkhoff) counts extensions by summing over covers. D ≤
(k/2+1)^n gives O(n·(k/2+1)^n) — polynomial in trace length for fixed core
count, exponential in core count. This is the *right* way to report "how many
executions are consistent with the trace" without materializing any of them.
(The n=8, T=3 count of 5.1 × 10⁹ in Section 7 was produced by exactly this DP
over 24 nodes in milliseconds; enumeration at, say, 10⁷ paths/sec would take
~9 minutes and ~terabytes of output.)

### 3.4 Enumeration: TTA-Enum

**Theorem 3.** For a fixed feasible matching, feasible linearizations can be
enumerated with O(N) delay and O(N) space by the online construction of
Section 5.1 (correctness by Lemma 2), and in **constant amortized time** per
extension by Pruesse–Ruskey (1994); Varol–Rotem (1981) gives a simple O(N) per
extension scheme. Total time Θ(output), which is optimal for explicit
enumeration.

The complexity-theoretic status of TTA-Enum is thus: **output-polynomial /
polynomial-delay enumerable** in the dedicated-channel case. It is *not* in
any classical decision class — the cost is the output, and the correct
research move is to change the output (Section 5.2).

### 3.5 Relations to named problems

- **Hamiltonian path.** Feasible thesis-paths are Hamiltonian ⊤→⊥ paths of the
  possibility graph, but the instances are not general graphs: once semantics
  are encoded correctly, they are Hamiltonian paths in the DAG closure of
  ≺_M, i.e., topological orders — poly-delay enumerable, existence in P (a DAG
  has a Hamiltonian path iff its topological order is unique — note the
  inversion: for TTA, a *unique* feasible linearization is the degenerate easy
  case). NP-completeness of Hamiltonian path is therefore **not** evidence of
  TTA hardness; the encoding manufactured Hamiltonicity out of a
  linear-extension problem. Genuine hardness lives in (i) matching choice
  (NP, Claim 1), (ii) counting (#P, Theorem 2), (iii) output size.
- **Counting s–t paths**: #P-complete (Valiant 1979) — condemns the
  possibility-graph candidate space itself.
- **Linear extensions**: the exact fixed-matching problem (Brightwell–Winkler;
  Pruesse–Ruskey).
- **Sequential consistency verification** (Gibbons–Korach): the shared-channel
  analogue; the source of Claim 1.
- **RSC/crown criterion** (Charron-Bost–Mattern–Tel): the feasibility test.
- **Graph coloring**: no useful correspondence; the constraint structure here
  is precedence/matching, not symmetric conflict. (Listed for completeness
  since it was raised; nothing in TTA maps to chromatic structure.)
- **Mazurkiewicz traces / partial-order reduction**: the equivalence classes
  of TTA-Enum's output (Section 5.2).

### 3.6 Summary table (n cores, k events/core, N = nk, c channels)

| Problem | Dedicated p2p channels | Shared channels / `select` |
|---|---|---|
| Build candidate structure | O(N) (matching forced, Lemma 1) | Σ_c |O_c|·|I_c| ≤ Θ(N²/c) candidate pairs |
| TTA-Exists | **O(N)** | NP-complete (Claim 1) |
| One witness execution | O(N) | NP-hard |
| TTA-Order (all ≺_M) | O(N), unique | one per feasible matching; up to Π_c (s_c m_c)!/(m_c!)^{s_c} |
| TTA-Count | #P-hard general; **O(n(k/2+1)^n)** fixed n | #P-hard |
| TTA-Enum | O(1) amortized per solution; output ≤ M(n,k) | exponential backtracking (dead ends exist) |

---

## 4. Effect of channel constraints

### 4.1 Edge counts in the possibility graph

Cross OUT→IN candidate edges, with N/2 OUTs and N/2 INs total:

| Rule | Edge count | For n=16, k=8 (N=128) |
|---|---|---|
| Channel-blind (tta.prolog rule 3) | (N/2)²·(1 − 1/n) = **Θ(n²k²)** | 3,840 |
| Channel-aware, no FIFO assumption | Σ_c |O_c|·|I_c| = **Θ(nk²)** for a ring | 512 |
| Channel-aware + Lemma 1 (forced) | N/2 = **Θ(nk)** | 64 |

The channel fix is not merely a constant-factor cleanup: it moves the
candidate structure from quadratic in *both* n and k to linear in N, and —
via Lemma 1 — from a "graph of possibilities" to a single DAG.

### 4.2 Ring topology, dedicated channels (the n-body target)

Ring: channel i carries core i → core i+1 (mod n); each timestep every core
does one OUT and one IN (even/odd phased to avoid the all-OUT-first deadlock,
which the collapse test correctly rejects as cyclic — verified for n = 4).

- Matching: forced (Lemma 1). **TTA-Exists, TTA-Order: O(N) total.**
- The one happens-before order ≺ has nT rendezvous nodes on a cylindrical
  grid; its linear-extension count grows as ~λ_nᵀ (transfer-matrix over
  antichain states). Computed exactly (memoized ideal-lattice DP):

| n | T | rendezvous nodes | raw events N | # linear extensions |
|---|---|---|---|---|
| 4 | 1 | 4 | 8 | 4 |
| 4 | 3 | 12 | 24 | 64 |
| 6 | 1 | 6 | 12 | 48 |
| 6 | 3 | 18 | 36 | 196,608 |
| 8 | 1 | 8 | 16 | 1,088 |
| 8 | 2 | 16 | 32 | 2,363,392 |
| 8 | 3 | 24 | 48 | **5,138,284,544** |

Measured per-timestep growth factors λ₄ = 4, λ₆ = 64, λ₈ ≈ 2.17 × 10³ — the
factor itself grows super-exponentially in n. **Conclusion for the ring: the
answer object (≺) is linear-size and linear-time; the linearization *set* is
astronomically redundant even at toy scale.** Note also the comparison against
the unconstrained interleaving bound: M(8,6) ≈ 1.7 × 10³⁸ candidate
interleavings for those same 48 events — channel + rendezvous constraints
remove ~29 orders of magnitude, and collapse removes the remaining ~10 by
declaring them equivalent.

### 4.3 All-to-all communication

Two regimes, sharply different:

- **All-to-all over n(n−1) dedicated pairwise channels**: still Lemma 1 —
  matching forced, feasibility O(N). The *count* of linear extensions
  approaches the unconstrained multinomial as the order gets sparser, but the
  decision/order problems stay linear. All-to-all is *not* intrinsically bad.
- **All-to-all over shared channels** (s senders funneling into one receiver
  channel, e.g., a work queue): matchings per channel = interleavings of
  sender subsequences, (sm)!/(m!)^s for s senders of m messages:

| s senders | m msgs each | feasible matchings (upper bound) |
|---|---|---|
| 2 | 4 | 70 |
| 2 | 8 | 12,870 |
| 3 | 8 | 9.47 × 10⁹ |
| 4 | 4 | 6.31 × 10⁷ |
| 8 | 4 | 2.39 × 10²⁴ |
| 8 | 8 | 1.82 × 10⁵⁵ |

  This is where TTA genuinely meets NP/#P (Claim 1, Theorem 2), and where the
  SAT/unfolding machinery of Section 6 is not optional.

A practical mitigant belongs in the tracer, not the algorithm: logging a
per-channel sequence number, sender id (the tta.lisp header already assumes
core id is logged), or a Lamport/vector timestamp (Lamport 1978, Mattern/Fidge
1988–89) collapses the matching ambiguity at the source. One word of trace
metadata per event converts an NP-complete reconstruction into an O(N) one —
the cheapest theorem in this document.

---

## 5. Pruning strategies

### 5.1 Online feasibility (build only feasible objects)

Replace enumerate-then-filter with frontier simulation over the collapsed
structure. State = n per-core program counters (equivalently an order ideal of
≺). A step fires a rendezvous whose OUT and IN are both at their cores'
frontiers (plus any local events). Recurse over all enabled choices;
backtracking pops one step.

- Every emitted sequence is feasible **by construction**: the alternation
  rule, the "IN preceded by OUT" rule, and the all-nodes rule (report rules
  1–3) are all absorbed into "fire enabled rendezvous until all counters
  exhaust." No filtering phase exists.
- Fixed matching: Lemma 2 ⇒ no dead ends ⇒ search tree = output tree; delay
  O(N); this is Theorem 3. The alternating-OUT/IN constraint has become the
  *definition* of a step — guidance, not filter, answering the question posed.
- Shared channels: enabled-pair choice doubles as matching search; dead ends
  now possible (an OUT can be stranded), so add propagation: prune a branch as
  soon as some remaining OUT has no remaining co-reachable IN (a bipartite
  Hall-condition check, polynomial per node). This is exactly
  constraint-propagation in the CP sense; worst case remains exponential
  (must, by Claim 1).

### 5.2 Partial-order reduction: collapse the output

Two linearizations differing by swapping adjacent *independent* rendezvous
(disjoint core pairs) are Mazurkiewicz-equivalent: same happens-before order,
same debugging content. The report already sensed this (p. 30: interleavings
of the 0↔1 and 2↔3 groups "do not depend on each other").

**Key observation: for a fixed matching there is exactly one Mazurkiewicz
trace.** All linear extensions of ≺_M form a single equivalence class. So a
POR-optimal TTA — in the sense of Abdulla–Aronis–Jonsson–Sagonas (POPL 2014 /
JACM 2017): explore exactly one representative per trace — emits **one object
per feasible matching**, not one per interleaving. For the ring n-body:
5.1 × 10⁹ → 1. If a canonical schedule is wanted for display, use the **Foata
normal form** (maximally parallel step sequence — layers of concurrently-fired
rendezvous), which is also the most legible rendering for a debugging tool.

This reframing is the single largest available improvement, and it is a
*specification* change, not an optimization: the deliverable becomes
{(M, ≺_M)}, with linearizations enumerable on demand (Theorem 3) if a user
drills in.

### 5.3 Symmetry breaking

When cores run identical code (SPMD n-body), the instance has an automorphism
group — Z_n rotations (dihedral D_n if the ring is bidirectional). Distinct
feasible matchings/executions related by an automorphism are isomorphic bugs;
quotient the search by the group as in symmetry-reduced model checking
(Emerson–Sistla 1996; Ip–Dill 1996): factor up to |G| = n (or 2n) savings on
matching search, and — more useful — deduplicate *reported* orders up to
isomorphism. Orbit computation is cheap for cyclic groups; don't bother with
general graph-automorphism machinery.

Note symmetry composes with POR (they reduce along different axes: relabeling
vs. commutation) — both should sit in front of any enumeration.

---

## 6. Alternative algorithmic approaches

### 6.1 SAT / SMT

Encode: Boolean p_{o,i} for each same-channel candidate pair (o OUT, i IN,
different cores); exactly-one per IN and per OUT (synchronous 1:1); acyclicity
of the collapse via integer event times in difference logic — one variable
t_u per rendezvous node, t_u < t_v for each collapsed po edge. The formula is
in SMT(IDL), size O(N + #candidates); satisfiable iff a feasible matching
exists. Enumerate matchings (not linearizations) via AllSAT with blocking
clauses over the p variables; count matchings with #SAT (sharpSAT, GANAK);
MonoSAT's native acyclicity theory fits the collapse constraint directly.
This is the pragmatic tool for the shared-channel regime: NP-completeness
(Claim 1) says *some* such engine is necessary, and trace instances are far
from worst case (candidates are interval-structured; Lemma 1's uncrossing
generalizes to strong symmetry-breaking clauses: for same-sender pairs,
matching must be order-preserving — add these as lex constraints).

### 6.2 Constraint programming

Same model, different propagation style: `alldifferent` on the matching,
precedence propagation on times; CP shines if the follow-on wants *targeted*
queries ("was e₁ before e₂ in every feasible execution?" — post the negation,
UNSAT ⇒ invariant; this query, "precedes in all linearizations," is just
reachability in ≺_M in the dedicated case, O(N) with one BFS). CP's search
heuristics subsume Section 5.1's online pruning.

### 6.3 Partial-order unfolding (McMillan)

The natural home for this problem. Model the instance as a 1-safe Petri net:
a place per (core, position); a transition per candidate rendezvous consuming
(core_A at p) and (core_B at q), producing (A at p+1), (B at q+1). Because
positions only advance, the net's unfolding (McMillan 1992; ERV algorithm,
Esparza–Römer–Vogler 2002; Esparza–Heljanko 2008) is finite without cutoff
subtleties, its **events are exactly the plausible rendezvous**, its
**maximal configurations are exactly the feasible matchings' posets ≺_M**,
and no interleaving is ever materialized. Size: O(#candidate pairs) events in
the dedicated case (the unfolding *is* the collapsed DAG); branching only
where genuine matching choice exists. Unfolding-based POR
(Rodríguez et al. 2015) gives the one-per-trace guarantee structurally. If
the follow-on wants one theoretical framework to stand on, this is it: "TTA =
unfolding of the trace net; feasible executions = its maximal configurations."

### 6.4 Symbolic model checking / BDDs

Encode the frontier vector (n counters, each in 0..k) in ⌈n log(k+1)⌉ bits;
the reachable-state BDD represents the ideal lattice of ≺; one image step per
rendezvous. Counting linear extensions = weighted path counting over the
lattice BDD/ADD (the Section 3.3 DP, symbolically). For ring topologies the
transition relation is nearest-neighbor, and frontier skew between neighbors
is bounded by the channel discipline, so BDDs with a ring-respecting variable
order stay compact — the classic Burch–Clarke–McMillan–Dill–Hwang "10²⁰
states" regime. Worth it only if n·k pushes the explicit DP out of memory;
for n ≤ 12 the explicit ideal DP is simpler and fast.

### 6.5 Sampling and approximate counting (large n)

When exact counting is out of reach (n ≳ 12 shared-channel, or width > ~20):
uniform sampling of linear extensions by MCMC — Karzanov–Khachiyan chain,
mixing O(N³ log N) (Bubley–Dyer 1999); Huber's exact-sampling variants.
Uniform sampling over feasible matchings via hashing-based samplers (UniGen)
on the Section 6.1 encoding. Debugging use: sample 10³ executions, check the
suspected invariant on each — a statistical TTA with quantifiable coverage,
which is the defensible fallback once completeness is priced out.

---

## 7. Practical scalability

### 7.1 The n-body ring

For n bodies, 2 events/core/timestep, T timesteps: N = 2nT events, nT
rendezvous. From Section 4.2: the feasible-linearization count reaches
5.1 × 10⁹ at n = 8, T = 3 (48 events). Extrapolating with λ₈ ≈ 2.2 × 10³:
n = 8, T = 10 gives ~10³⁶ linearizations from 160 events. Meanwhile the
happens-before DAG at n = 64, T = 10,000 (N = 1.28 M events) is built and
checked in well under a second at ~O(N) with small constants.

| Deliverable | Feasible n (ring, dedicated channels) |
|---|---|
| ≺ (happens-before order) + feasibility verdict | n, T essentially unbounded (millions of events) |
| Exact count of linearizations (ideal DP, O(n(T+1)ⁿ)-ish states) | n ≲ 10–12 for T ~ 10; larger T fine at small n |
| Exhaustive linearization enumeration | dead by n = 6, T = 3 (≈ 2 × 10⁵) … n = 8, T = 2 (2.4 × 10⁶); hard wall ~10⁷–10⁸ outputs |
| Sampled linearizations | unbounded n (MCMC mixing poly in N) |

### 7.2 Where exhaustive enumeration dies in general

Unconstrained interleaving counts M(n,k) = (nk)!/(k!)ⁿ (the thesis's search
space, corrected):

| n | k | N | M(n,k) |
|---|---|---|---|
| 3 | 4 | 12 | 34,650 |
| 4 | 4 | 16 | 6.3 × 10⁷ |
| 4 | 6 | 24 | 2.3 × 10¹² |
| 6 | 4 | 24 | 3.2 × 10¹⁵ |
| 8 | 4 | 32 | 2.4 × 10²⁴ |
| 16 | 8 | 128 | 7.9 × 10¹⁴¹ |

Rule of thumb: anything that touches each interleaving individually is over by
**N ≈ 16–24 events total** — i.e., before the smallest interesting n-body
instance. This is not an implementation problem; it is the output's size, and
only changing the output (Section 5.2) or sampling (Section 6.5) moves it.
Shared-channel matching enumeration dies similarly by the Section 4.3 table:
fine at 2 senders × 8 messages (1.3 × 10⁴), gone at 4 × 4 (6.3 × 10⁷).

---

## 8. Recommendations

Ranked by (impact ÷ effort):

1. **Change the output contract: report happens-before orders, not path sets.**
   One (M, ≺_M) per feasible matching; render ≺_M (transitive reduction, then
   the existing CL-DOT/Graphviz path) or its Foata layers. Justification:
   Section 5.2 — for fixed matching the path set is a single Mazurkiewicz
   class; for the ring, 5 × 10⁹ → 1. Everything else follows from this.
2. **Adopt collapse-then-topo as the core algorithm** (Definitions 3–4,
   Theorem 1): per-channel counters for the forced matching (Lemma 1),
   union–find collapse, Kahn's algorithm. O(N). This *is* the "O(n) TTA for
   n-process CSP" the follow-on is after — with the precise caveat that O(N)
   is achievable for the order-reconstruction problem, and provably not for
   exhaustive enumeration (Sections 3.3–3.4) or shared-channel search
   (Claim 1). A cycle report doubles as a deadlock/truncation diagnostic with
   the offending event cycle as the error message.
3. **Fix `tta.prolog` regardless** (it remains valuable as an executable
   spec): bind `CpuB = CpuA` in rule 1; add full program-order edges (not just
   OUT→IN); add channel labels to `node/3` → `node/4` and constrain rule 3 to
   same-channel, different-core; add a visited list (or better, encode the
   frontier semantics of Section 5.1, which needs no visited list). The
   empirical Section 2 run is the regression test: the example should yield
   feasible linearizations covering all six events, not 2 fragments.
4. **Minimum viable tool** (dedicated-channel MVP, ~500 lines in the existing
   Lisp skeleton): parse → forced matching → collapse → cycle check →
   emit DOT of ≺ + verdict. Complexity O(N). The tta.lisp stubs
   (`find-channels-in`, `link-up-nodes-in-channels`) are precisely the two
   functions this fills in. Add on demand: exact count (ideal-lattice DP,
   ≤ ~30 collapsed nodes or n ≤ ~12), poly-delay enumeration
   (Varol–Rotem / Pruesse–Ruskey), "must e₁ precede e₂?" queries (one BFS).
5. **Shared channels / `select`, when needed**: SMT(IDL) or MonoSAT encoding
   (Section 6.1) with uncrossing symmetry-breaking; AllSAT over matching
   variables to enumerate ≺_M's; #SAT for counts; alternatively the Petri-net
   unfolding (Section 6.3) if a self-contained algorithm is preferred over a
   solver dependency. Before building any of this, weigh Section 4.3's
   tracer-side fix: **one sequence number per channel in the trace format
   deletes this entire problem class.**
6. **For n beyond exact methods**: MCMC sampling of linear extensions +
   hashing-based matching samplers; report statistical confidence rather than
   completeness.

### Completeness/performance tradeoffs

| Mode | Guarantee | Cost | When |
|---|---|---|---|
| Order reconstruction (rec. 2) | complete for "what happened-before what" | O(N) | always; default |
| + exact count (rec. 4) | complete cardinality, no materialization | O(n(k+1)ⁿ) | n ≲ 12 |
| + one-per-trace enumeration (rec. 1) | complete up to Mazurkiewicz ≡ | O(#matchings) | shared channels, small |
| full linearization enumeration | complete, redundant | Θ(output) ≥ exp | N ≲ 20; drill-down only |
| sampling (rec. 6) | probabilistic | poly per sample | everything else |

### The two open lemmas worth writing up

1. NP-completeness of TTA-Exists for shared-channel rendezvous traces
   (Claim 1's clause gadget, verified against the crown criterion).
2. #P-hardness of TTA-Count via a rigorous (Turing) reduction from linear
   extension counting handling the subdivision gadgets (Theorem 2's caveat).

Both are near-misses of published results (Gibbons–Korach; Brightwell–Winkler)
but appear not to have been stated for the synchronous-channel trace model —
they would anchor the follow-on's theory chapter.

---

## References

- G. Brightwell, P. Winkler. *Counting linear extensions is #P-complete.*
  STOC 1991 / Order 8:225–242, 1991.
  [ACM](https://dl.acm.org/doi/10.1145/103418.103441),
  [Order](https://link.springer.com/article/10.1007/BF00383444)
- S. Dittmer, I. Pak. *Counting linear extensions of restricted posets*
  (height 2 is #P-complete). Electron. J. Comb. 27(4), 2020.
  [EJC](https://www.combinatorics.org/ojs/index.php/eljc/article/view/v27i4p48),
  [pdf](https://www.math.ucla.edu/~pak/papers/BruhatPaper2.pdf)
- P. Gibbons, E. Korach. *Testing shared memories.* SIAM J. Comput.
  26(4):1208–1244, 1997 (VSC/VL NP-complete).
  [Semantic Scholar](https://www.semanticscholar.org/paper/4da21090bdfaad245407fe9418119fda931070e6)
- P. Chini et al. *A framework for consistency algorithms.* 2020.
  [arXiv:2007.11398](https://arxiv.org/pdf/2007.11398)
- B. Charron-Bost, F. Mattern, G. Tel. *Synchronous, asynchronous, and
  causally ordered communication.* Distributed Computing 9(4):173–191, 1996
  (crown criterion / RSC).
- L. Valiant. *The complexity of enumeration and reliability problems.*
  SIAM J. Comput. 8(3):410–421, 1979 (#s–t paths is #P-complete).
- G. Pruesse, F. Ruskey. *Generating linear extensions fast.* SIAM J. Comput.
  23(2):373–386, 1994. — Y. Varol, D. Rotem. Comput. J. 24(1), 1981.
- R. Bubley, M. Dyer. *Faster random generation of linear extensions.*
  Discrete Math. 201:81–88, 1999.
- A. Mazurkiewicz. *Trace theory.* In Petri Nets: Applications and
  Relationships to Other Models of Concurrency, LNCS 255, 1987; V. Diekert,
  G. Rozenberg (eds.), *The Book of Traces*, 1995.
- P. Abdulla, S. Aronis, B. Jonsson, K. Sagonas. *Optimal dynamic partial
  order reduction.* POPL 2014; JACM 64(4), 2017 (source sets; one execution
  per Mazurkiewicz trace).
  [POPL'14 pdf](https://user.it.uu.se/~parosha/publications/papers/popl2014.pdf),
  [JACM pdf](https://user.it.uu.se/~parosha/publications/papers/jacm17.pdf)
- K. Kokologiannakis et al. *Truly stateless, optimal dynamic partial order
  reduction.* POPL 2022. [ACM](https://dl.acm.org/doi/10.1145/3498711)
- C. Flanagan, P. Godefroid. *Dynamic partial-order reduction for model
  checking software.* POPL 2005.
- K. McMillan. *Using unfoldings to avoid the state explosion problem…*
  CAV 1992. — J. Esparza, S. Römer, W. Vogler. *An improvement of McMillan's
  unfolding algorithm.* FMSD 20:285–310, 2002. — J. Esparza, K. Heljanko.
  *Unfoldings: A Partial-Order Approach to Model Checking.* Springer, 2008.
- C. Rodríguez, M. Sousa, S. Sharma, D. Kroening. *Unfolding-based partial
  order reduction.* CONCUR 2015.
  [arXiv:1507.00980](https://arxiv.org/pdf/1507.00980)
- J. Burch, E. Clarke, K. McMillan, D. Dill, L. Hwang. *Symbolic model
  checking: 10²⁰ states and beyond.* Inf. & Comp. 98(2), 1992.
- E. Emerson, A. Sistla. *Symmetry and model checking.* FMSD 9:105–131, 1996.
  — C.N. Ip, D. Dill. *Better verification through symmetry.* FMSD 9, 1996.
- L. Lamport. *Time, clocks, and the ordering of events…* CACM 21(7), 1978.
  — F. Mattern. *Virtual time and global states…* 1989; C. Fidge, 1988
  (vector clocks).
- A. Kahn. *Topological sorting of large networks.* CACM 5(11), 1962.

*Computed artifacts (interleaving counts, ring linear-extension DP,
edge-count comparisons, and the SWI-Prolog probe of `tta.prolog`) were
produced during this review; scripts are reproducible from the formulas in
Sections 1.3, 3.3, and 4.*
