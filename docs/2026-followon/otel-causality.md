# OpenTelemetry and the Causality Lattice

## Can OTel's Parenting/Linking Model Substitute for TTA-Style Causal Reconstruction?

*Prepared for the `tta-followon` project, August 2026.*
*Companion to: P. Nathan, "Debugging by Visualizing Communication on a Parallel Embedded System," M.S. report, University of Idaho, 2012 (`masters-work/report.pdf`), which defines the Targeted Trace Algorithm (TTA).*

---

## 0. Executive Summary

The question under examination: **does OpenTelemetry's parenting/linking mechanism "nominally work around" the causality problem that motivates TTA** — namely, that per-core traces without a global clock determine only a partial order of communication events (Lamport 1978), forcing TTA to enumerate the set of feasible linearizations?

The short answer, defended rigorously in §4:

1. **Structurally, yes; semantically and operationally, no.** OTel's data model (spans + one optional parent + arbitrarily many links) is expressive enough to *encode* any finite happens-before relation (Proposition 4.1). But the specification does not *require* — and its standard propagation machinery does not *produce* — a complete encoding. Three independent failures are identified:
   - the **parent-only fragment** (what auto-instrumentation actually emits) can represent only *forest-shaped* causal orders and therefore cannot represent any execution containing a join/fan-in (Theorem 4.2);
   - **link semantics are normatively underspecified** — the spec says links connect "causally related" spans but assigns links no direction and no happens-before force, so even a trace rich in links does not determine a unique partial order (Theorem 4.3, indistinguishability);
   - **one-way context propagation** structurally cannot record the *backward* half of a synchronous rendezvous — the edge from the receiver's history into the sender's continuation (Theorem 4.4).
2. **What OTel actually solves is a *different* TTA problem.** The 2012 report observes (p. 30) that "unless the software environment adds metadata serving as a communication index (e.g., a Lamport clock), it is impossible to disambiguate" which OUT fed a given IN. OTel's `SpanContext` is precisely such a communication index: a globally unique, propagated message tag. OTel therefore eliminates (or can eliminate) TTA's *matching ambiguity* (which OUT pairs with which IN) while leaving TTA's *ordering incompleteness* (which of the unordered events could have interleaved how) fully intact. This distinction — **identification vs. ordering** — is the crux of the whole analysis.
3. **The correct synthesis is complementary, not substitutive.** OTel edges are (when conventions are followed) *sound but incomplete*: every recorded edge is a real happens-before edge, but not every happens-before edge is recorded. TTA is exactly the algorithm that quantifies the residual uncertainty of an incomplete causal order: it enumerates the linear extensions the recorded order fails to exclude. A practical architecture — TTA as an OTLP trace processor, with channel operations encoded as zero-duration linked spans — is given in §5.

---

## 1. The OpenTelemetry Trace Model: A Precise Review

This section states what the specification actually says, distinguishing normative text from folklore. Citations are to the OpenTelemetry specification ("OTel spec": `opentelemetry.io/docs/specs/otel/`, the versioned Markdown under `open-telemetry/opentelemetry-specification`), the OTel semantic conventions ("semconv"), and W3C Trace Context (Recommendation, 23 Nov 2021; Level 2 in Candidate Recommendation as of this writing).

### 1.1 Traces, spans, and span context

OTel has **no first-class trace object**. The Overview is explicit:

> "Traces in OpenTelemetry are defined implicitly by their Spans. In particular, a Trace can be thought of as a directed acyclic graph (DAG) of Spans, where the edges between Spans are defined as parent/child relationship." — OTel spec, Overview, §Traces

A **span** (OTel spec, Trace API, §Span) carries:

| Field | Content | Causal relevance |
|---|---|---|
| Name | operation label | none |
| `SpanContext` | `TraceId` (16 bytes, ≥1 non-zero), `SpanId` (8 bytes, ≥1 non-zero), `TraceFlags` (sampled; random-trace-id in Level 2), `TraceState` (vendor key-values), `IsRemote` | **identity**: a globally unique tag for the span; the unit of propagation |
| Parent | a `Span`, `SpanContext`, or null | one causal in-edge, at most |
| `SpanKind` | `INTERNAL`, `SERVER`, `CLIENT`, `PRODUCER`, `CONSUMER` | *interpretive* hints about synchronicity (see §1.4) |
| Start/End timestamps | wall-clock, nanoseconds since Unix epoch | heuristic ordering only; no clock-synchronization requirement anywhere in the spec |
| Attributes | key-value pairs | free-form; the escape hatch used by every convention |
| Links | zero or more `(SpanContext, Attributes)` pairs | additional causal edges, **direction and semantics unspecified** (§1.3) |
| Events | timestamped `(name, attributes)` records *inside* a span | sub-span granularity; **cannot carry links** (see §3.2) |
| Status | ok/error | none |

Two normative points that matter later:

- **Parent determination is contextual, not structural**: "When a new `Span` is created from a `Context`, the `Context` may contain a `Span` representing the currently active instance, and will be used as parent. If there is no `Span` in the `Context`, the newly created `Span` will be a root span." (Trace API, §Span creation.) A child inherits the parent's `TraceId` and `TraceState`.
- **No containment requirement.** The spec nowhere requires a child span's interval to lie within its parent's. Asynchronous children routinely outlive their parents (this is acknowledged in spec discussions, e.g. opentelemetry-specification issue #3548, and is the designed-for case for `PRODUCER`/`CONSUMER` kinds). Consequently *interval nesting cannot be assumed and cannot be used as a causal inference rule* in general.

### 1.2 Parent-child relationships

The parent edge is a **single, optional, creation-time in-edge**. Its sound causal content is exactly this: the child span was started by a thread of control that possessed the parent's `SpanContext`, hence

> start(parent) **happens-before** start(child).

Nothing more. In particular the parent edge does *not* assert end(child) → end(parent) (no containment, §1.1), and it does not order the child with respect to any sibling.

### 1.3 Span links

From the Trace API (§Specifying links, §Add Links):

- "During `Span` creation, a user MUST have the ability to record links to other `Span`s." Also: "A `Span` MUST have the ability to add `Link`s associated with it after its creation" (added in spec v1.31; links added late "may not be considered by Samplers").
- "Linked `SpanContext`s can be from the same or a different trace."
- A link is a pair: the linked `SpanContext` plus attributes.

The Overview describes the intent:

> "A Span may be linked to zero or more other Spans (defined by SpanContext) that are causally related. Links can point to Spans inside a single Trace or across different Traces. Links can be used to represent batched operations where a Span was initiated by multiple initiating Spans, each representing a single incoming item being processed in the batch." — OTel spec, Overview, §Links between spans

**What the spec does *not* say is decisive.** The word "causally related" is symmetric: the spec assigns links no direction (does the linked span precede or follow?), no transitive force, and no obligation that instrumentation emit a link when a causal dependence exists. Direction is fixed only per-convention: e.g., the messaging semconv has consumers link *backwards* to producer creation contexts, while scatter-gather aggregators link backwards to the scattered operations — but a "continuation" link from an origin span *forward* to follow-up work also appears in the wild. A generic analyzer receiving conformant OTLP data cannot orient an arbitrary link. This is formalized as Theorem 4.3.

**Links vs. parent, precisely:**

| | Parent | Link |
|---|---|---|
| Cardinality | ≤ 1 | 0..* |
| When set | creation only | creation or later |
| Trace scope | same trace (child adopts `TraceId`) | same or different trace |
| Normative causal semantics | creation context ⇒ start(parent) → start(child) | none (attributes may say; spec does not) |
| Effect on sampling | parent-based samplers | creation-time links may inform samplers |

### 1.4 Context propagation

- **In-process**: the Context spec defines an immutable, key-valued `Context` holding *the* current span (one register, not a set). Cross-cutting concerns read/write it; it flows implicitly with execution (thread-locals, async-locals).
- **Cross-process**: `Propagators` inject/extract `SpanContext` into carriers. The default is **W3C Trace Context**: `traceparent: 00-{trace-id,16B}-{parent-id,8B}-{flags,1B}` plus `tracestate` (vendor list, ≥512 chars recommended support). Alternatives: **B3** (Zipkin; `b3` single header or `X-B3-TraceId`/`X-B3-SpanId`/`X-B3-ParentSpanId` multi-header), Jaeger `uber-trace-id`, and W3C Baggage for non-identity key-values.
- **Directionality**: propagation is **one-way, caller → callee**. The Trace Context Recommendation defines request headers only. A `traceresponse` header for callee → caller ("backward") propagation has been discussed in the W3C Distributed Tracing WG for Level 2, but is not part of the Level 1 Recommendation and cannot be relied on. Consequence: **a sender never learns the receiver's `SpanContext`** unless the application returns it in-band; see Theorem 4.4.
- **Messaging** (semconv, §Messaging spans): context is injected into *message headers* ("creation context"). Consumer-side, the semconv makes **links, not parenting, the default**: "It is the only consistent trace structure that can be guaranteed, given the many different messaging systems models available"; and "Exclusively for single-message scenarios, the 'Process' span MAY use the message's creation context as its parent, thus achieving a direct parent-child relationship between producer and consumer(s)." Batch receives produce one consumer span with one link per message creation context — the one place the ecosystem systematically records fan-in.

### 1.5 The causal information a span actually carries

Summing up: a span carries (i) a **globally unique identity** (`SpanContext`); (ii) **at most one explicit causal in-edge** (parent); (iii) **any number of uninterpreted edges** (links); (iv) **two unsynchronized wall-clock timestamps**; (v) **interpretive metadata** (`SpanKind`, attributes) from which conventions — not the data model — let a reader guess further ordering (e.g., a `CLIENT` span "ends after the response was received," implying receipt precedes the client's subsequent work; this is an inference from kind semantics, not a recorded edge).

---

## 2. Does OTel Provide a Complete Causality Lattice?

We now walk the five scenarios. Throughout, "happens-before" (→) is Lamport's relation (formal definitions in §4.1); "captured" means *derivable from the emitted OTel data by a sound rule*.

### 2.1 Parent-child spans

**Partially.** The parent edge soundly captures start(parent) → start(child). It does **not** capture:

- end(child) → *anything* in the parent (no containment guarantee, §1.1);
- any ordering among children;
- the return edge of a synchronous call (that the parent's continuation depends on the child's completion). For sync RPC this is real causality — the caller's next statement is causally after the callee's reply — but OTel records it only implicitly (CLIENT-span end time; kind semantics), never as an edge.

So parent-child is a strict, one-directional *fragment* of happens-before, not the relation itself.

### 2.2 Cross-service causality (A calls B)

**The forward chain, yes; the full chain, no.** `traceparent` flows A → B; B's `SERVER` span becomes a child (or sibling-with-link) of A's `CLIENT` span; nesting continues transitively downstream. So the *request tree* is preserved — this is OTel's home turf, inherited from Dapper (Sigelman et al. 2010). What is not preserved as data:

- the **response edge** B → (rest of A) — recoverable only by interpreting SpanKind + timestamps for the synchronous case, and unrecoverable for async continuations;
- any communication that bypasses instrumented boundaries (shared DB rows, files, side channels): silently absent.

### 2.3 Fan-out/fan-in and inter-sibling causality

**Fan-out: yes. Fan-in: only via deliberate links. Inter-sibling: no.** If A starts B and C, both records show parent A — a tree. If B then sends a message to C:

- if the B→C hop goes through an *instrumented* boundary, C's handler span gets B's context (as parent — but it already has parent A, and "a span can only have a single parent," so the instrumentation must choose one and *may* link the other; the Overview's scatter-gather note recommends not setting a single parent at merge points);
- if the B→C hop is a raw channel, a shared queue in-process, shared memory, or any uninstrumented path, **no edge whatsoever is recorded**. The trace shows A→B, A→C and is bit-for-bit identical to the trace of an execution where B and C never communicated. This is Theorem 4.3's witness, and it is exactly the TTA scenario (threads communicating over XC channels beneath any RPC layer).

Fan-in (join/barrier: D awaits B and C) *can* be expressed — D takes one as parent and links the other, or links both — and the messaging semconv's batch-receive pattern does this properly. But nothing requires it; the default `Context` machinery holds a single current span, so the second incoming context is dropped on the floor unless instrumentation is written to keep it.

### 2.4 Asynchronous communication (Kafka, RabbitMQ, pub/sub, fire-and-forget)

**The best-served case, by convention.** Creation context rides the message headers; consumers link (default) or parent (single-message exception) to it (§1.4). So producer → consumer *is* captured when both ends run conforming instrumentation. Caveats: (i) links are the norm, and links have no spec-level direction — the producer→consumer reading is a semconv convention; (ii) broker-internal causality (partition ordering, consumer-group rebalancing, DLQ hops) is invisible; (iii) fire-and-forget through an uninstrumented medium records nothing.

### 2.5 Concurrent spans (true independence)

**No — and this is a category error, not a bug.** OTel's edge set is **open-world**: absence of an edge means "no edge was recorded," never "these operations were concurrent." An analyzer cannot distinguish unordered-because-independent from unordered-because-unobserved. TTA, by contrast, is **closed-world by construction**: it consumes a *complete* trace of the communication alphabet (every IN/OUT on every channel in the window), so absence of a connecting path is a *positive proof of concurrency* — the very fact TTA exploits when it declares that threads {0,1} vs {2,3} communications "do not depend on each other (by communication), all are feasible" (report, p. 30). Timestamps cannot repair this: they always impose *some* order on concurrent events (falsely suggesting precedence) and, being unsynchronized, may invert real precedence (§4.6).

---

## 3. What Is Missing: The Specific Gaps

### 3.1 Inter-sibling causality

As in §2.3: with B, C children of A and B → C over an uninstrumented path, OTel records A→B and A→C only. The B→C covering edge is representable (a link) but not produced, not required, and not inferable. **The recorded order for {A,B,C} is the tree order; the true order is a strictly larger relation.** Every analysis downstream (critical path, race candidates, latency attribution) is computed against the wrong poset.

### 3.2 Partial ordering within a service

OTel spans model *operations* (request/response scale). Fine-grained intra-process events have two homes, both inadequate:

- **Span events**: timestamped points inside a span — but events **cannot carry links or parents**; they cannot participate in the causal graph at all. Two events in different spans are ordered only by (unsound) timestamps.
- **Zero-duration child spans**: causally linkable but heavyweight (each carries full span overhead through the SDK, wire, and backend), and per-thread *program order* still is not first-class — nothing in the model says "these 40 spans are the sequential execution of one thread, in this total order." Program order must be smuggled in via attributes (`thread.id` + a sequence counter) and reconstructed by a custom analyzer. TTA's input — *the per-core totally-ordered event sequence* — is precisely what OTel does not natively represent.

### 3.3 Channel-level causality

OTel's communication vocabulary is request/response (`CLIENT`/`SERVER`) and produce/consume (`PRODUCER`/`CONSUMER`). CSP-style synchronous channels differ in kind:

- **Rendezvous is symmetric**: an XC `chan` OUT blocks until the IN completes; sender and receiver *synchronize* — each side's continuation causally depends on **both** sides' histories. OTel's propagation is one-way; the receiver→sender half of the rendezvous is structurally unrecordable (Theorem 4.4). A rendezvous is not a message with a header; it is a shared event.
- **No channel identity**: nothing in the data model names the channel; `messaging.destination.name` is the nearest analogue and is convention-only. TTA's feasibility rules ("INs are always preceded by an OUT"; "an IN blocks both threads") are stated *per channel* and need channel identity to apply.
- **No blocking semantics**: OTel cannot say "this span was blocked in a channel operation from t₁ to t₂," which is the load-bearing fact in TTA's rule 2.

### 3.4 Multiple causality paths (join/barrier semantics)

An event caused by *k* independent predecessors needs in-degree *k* in the causal DAG. Parent gives in-degree ≤ 1; links supply the rest **only if instrumentation deliberately records k−1 links and a convention orients them**. The single-slot `Context` abstraction actively works against this: at a join, the runtime is holding one current context, and merging contexts is left entirely to user code. So joins are *expressible* (Prop 4.1) but *exceptional* — the model's grain runs toward trees. (The messaging batch-receive convention is the honorable exception.)

### 3.5 Clock accuracy

OTel timestamps are wall-clock, nanosecond-*resolution*, with **no accuracy or synchronization requirement anywhere in the spec**. NTP-disciplined hosts commonly disagree by 0.5–10 ms — 10³–10⁵× the µs/ns event spacing of interest — and skew is neither bounded nor reported in the data. Formally (§4.6): timestamp order is neither sound (can invert true →) nor complete (orders genuinely concurrent events) as an estimator of causality. Lamport's 1978 point stands unchanged: ordering must come from *communication structure*, not clocks; OTel's own structure (parent/links) is the part of OTel that heeds this, and its timestamps are the part that does not. Any tool that breaks sibling "ties" by timestamp is quietly wrong at exactly the scale where concurrency bugs live.

---

## 4. Theoretical Analysis

### 4.1 Preliminaries: executions, happens-before, and the causality lattice

**Definition 4.1 (Execution).** An *execution* X of a system of n sequential processes P₁,…,Pₙ over a set C of synchronous channels consists of:
- a finite event set E partitioned as E = E₁ ⊎ … ⊎ Eₙ, each Eᵢ totally ordered by *program order* <ᵢ;
- a labeling λ : E → {int} ∪ ({out,in} × C); write OUT(c)/IN(c) events;
- a *matching* μ, a bijection between out-events and in-events with matching channels, such that the induced relation below is acyclic.

Following the report's convention (⊤/⊥ delimit the targeted window; each core's trace is its <ᵢ-sequence of communication events), we model each rendezvous with *split events*: for a matched pair (s, r) = (OUT(c), IN(c)), synchrony means the rendezvous completes only when both sides have arrived, so both continuations depend on both arrivals. Write s → r for the data edge and r → s′ for the release edge, where s′ is the sender's <-successor (equivalently, refine each channel event into begin/end halves with end(s), end(r) each above begin(s), begin(r)). In the CSP reading (Hoare 1978), s and r are simply *one shared event* in the alphabets of both processes; the two formulations generate the same order on the surrounding events.

**Definition 4.2 (Happens-before; Lamport 1978).** →_X is the least strict partial order on E containing (i) each <ᵢ, and (ii) the synchronization edges induced by μ as above. Events e, f are *concurrent*, e ∥ f, iff neither e →_X f nor f →_X e.

**Definition 4.3 (Causality lattice; Mattern 1989).** A *consistent cut* is a downward-closed S ⊆ E (e ∈ S and f →_X e imply f ∈ S). The consistent cuts ordered by inclusion form a **distributive lattice** 𝓛(X) with bottom ∅ (the report's ⊤ start-marker, dually) and top E. Its maximal chains ∅ = S₀ ⊂ S₁ ⊂ … ⊂ S_|E| = E, each step adding one event, correspond **bijectively to the linear extensions of →_X** — i.e., precisely to TTA's *feasible traces*. By Birkhoff duality, 𝓛(X) and →_X are mutually recoverable. Hence:

**Definition 4.4 (Complete causal representation).** A trace representation R (a data format plus instrumentation discipline mapping executions X to data R(X), plus a decoding rule) is *causally complete for a class 𝒳* iff for every X ∈ 𝒳 the relation →_X is recoverable from R(X); equivalently, R(X) determines 𝓛(X); equivalently, R(X) determines the exact set of feasible interleavings. R is *causally sound* iff every edge the decoding rule asserts is in →_X. The pair (sound, complete) is what "provides the causality lattice" must mean: sound-but-incomplete R yields ⊑ ⊆ →_X and therefore admits **spurious linearizations** (LinExt(⊑) ⊇ LinExt(→_X)); unsound R excludes **real** ones.

**Remark (why "lattice" and not just "partial order").** For n processes the concurrency structure can require dimension n: Charron-Bost (1991) showed causal order on n processes is characterized by vector clocks of size n and no smaller per-event timestamp suffices. Any *constant-size* per-event label — such as OTel's 24-byte (`TraceId`,`SpanId`) context — therefore cannot by itself decide e ∥ f; concurrency must be decided by *reachability over the collected edge set*, which is fine post-mortem **iff the edge set is complete**. This observation converts "is the edge set complete?" into the single load-bearing question.

### 4.2 Formalizing the OTel trace model

**Definition 4.5 (OTel trace datum).** A finite set S of spans with: `ctx` : S → ID (injective); `parent` : S ⇀ S; `links` : S → 𝒫(ID × Attr); `kind`, `attrs`; `ts`, `te` : S → ℚ (wall clock, no accuracy axiom). Define the *edge multigraph* G(T) = parent edges ∪ link edges. A *decoding rule* ι orients edges and asserts order; the spec normatively supplies ι only for parent edges: start(parent) → start(child). Call:

- **OTel₀** — the parent-only fragment (spec-guaranteed semantics; what generic auto-instrumentation and generic backends actually rely on);
- **OTel_L(κ)** — spans + links under an out-of-band convention κ that orients links and declares them happens-before edges (κ is *not* part of the spec; e.g. κ_msg = "a consumer's link points at a causal predecessor").

**Definition 4.6 (Propagation-faithful instrumentation).** An instrumentation is *propagation-faithful* if a process can record a parent/link to `ctx`(σ) only if it *possesses* that context, where possession is: (i) a process possesses contexts it generated; (ii) possession transfers only along instrumented communications, in the direction of the message; (iii) the in-process `Context` register holds one span context at a time (others must be explicitly retained by user code). This axiomatizes §1.4 and the Context spec.

### 4.3 Structural adequacy: the impossibility is *not* structural

**Proposition 4.1 (Expressive completeness of OTel_L).** For every finite partial order (E, →) there is an OTel trace datum T and convention κ with derived order equal to →.
*Proof.* One zero-duration span σ_e per event e, all under one `TraceId`; for each covering edge f ⋖ e record on σ_e a link to `ctx`(σ_f) with attribute `dir=predecessor`; κ orients links accordingly; the derived order is the transitive closure of the covering relation, i.e., →. ∎

So OTel's *data model* can carry any causality lattice — including everything TTA reconstructs. Every negative result below is therefore about (a) what the spec *guarantees*, (b) what standard machinery *emits*, and (c) what a decoder can *soundly assume* — not about representational capacity. This is the precise sense in which "OTel nominally works around causality" is nominally true and substantively false.

### 4.4 The parent-only fragment captures exactly forest-shaped causality

**Theorem 4.2 (Forest bound for OTel₀).** The order derivable from OTel₀ data is a forest order (every element has ≤ 1 lower cover). Consequently, no execution X whose →_X has any event with two incomparable immediate predecessors — i.e., **any join, barrier, batch-consume, or rendezvous following independent histories** — is representable in OTel₀: for every propagation-faithful instrumentation, the derived order ⊑ is a *proper* sub-order of →_X.
*Proof.* In G(T) restricted to parent edges, each node has in-degree ≤ 1, so the reachability order's Hasse diagram is a forest; "number of lower covers" is an order invariant, and a forest order has ≤ 1 per element, while →_X by hypothesis has an element with ≥ 2. For the "consequently": soundness of the parent rule gives ⊑ ⊆ →_X; equality would make →_X a forest order. ∎

Interpretation: since virtually every real concurrent execution contains a join (any two-process rendezvous is one: the release edge gives the sender's continuation two incomparable predecessors — its own past and the receiver's arrival), **OTel-as-actually-deployed is incomplete on essentially all inputs of interest**, not on exotic corner cases. The n-fold fan-out/fan-in of `par` blocks, TTA's bread and butter, is the maximal violation.

### 4.5 Underspecified links: the model does not determine the order

**Theorem 4.3 (Indistinguishability).** There exist executions X₁, X₂ over the same processes and channel alphabet with →_{X₁} ≠ →_{X₂} (indeed with different feasible-interleaving sets) whose OTel data under conformant, propagation-faithful instrumentation of the *instrumented* boundaries are identical. Hence no decoder — however clever — computes →_X from OTel data on all executions: causal completeness fails for the specified model.
*Proof (witness).* Processes A, B, C; instrumented boundaries: A's spawns of B and C. X₁: B and C run independently. X₂: identical, except B performs OUT(c) and C the matching IN(c) on an uninstrumented channel c (raw XC channel, shared queue, memory). Both executions emit spans {σ_A; σ_B, σ_C with parent σ_A} with (choosable) identical timestamps: T(X₁) = T(X₂). But →_{X₂} contains OUT→IN (and the release edge) while →_{X₁} does not; LinExt(→_{X₂}) ⊊ LinExt(→_{X₁}). No function of the common datum yields both. ∎

Two distinct morals: (1) *coverage*: OTel edges exist only where instrumentation was placed — the spec imposes no completeness obligation, so absence of an edge is uninformative (open world, §2.5); (2) *semantics*: even where links exist, the spec's refusal to orient them means a κ-free decoder cannot soundly use them, and different κ's in one system (messaging's backward links vs. continuation-style forward links) are indistinguishable in the data unless attributes disambiguate. Completeness, if achieved, is achieved by *discipline external to OTel* — which is Definition 4.4's point: the representation must include the discipline.

### 4.6 One-way propagation cannot record the rendezvous back-edge

**Theorem 4.4 (Back-edge unrecordability).** Under one-way context propagation (W3C Trace Context Level 1: request direction only) and propagation-faithful instrumentation, for any synchronous rendezvous (s, r) the release edge r → s′ (receiver's arrival → sender's continuation) is not recordable at the sender: no span emitted by the sender's process can parent to or link to any receiver-side context.
*Proof.* By induction over the execution: the sender's possession set (Def. 4.6) starts with its own generated contexts and grows only by *incoming* instrumented messages; in one-way propagation the rendezvous transfers context s → r only. Hence no receiver-generated ID is ever in the sender's possession set, and links require possession. ∎
*Remarks.* (i) The receiver *can* record s → r (it possesses the sender's context) — so exactly **half of every rendezvous** is capturable; the causal DAG of a fully instrumented CSP program under one-way OTel is the true DAG minus all release edges, whose linear extensions strictly include impossible schedules (e.g., a sender racing ahead of a rendezvous it is supposedly blocked on). (ii) Synchronous RPC evades this only *interpretively*: a `CLIENT` span "contains" the round-trip, so kind semantics imply the reply preceded the client's next span — an inference from `SpanKind` + intervals, not an edge, and void for `PRODUCER`/`CONSUMER` and one-way messaging. (iii) The cure is response-direction propagation — precisely the `traceresponse` idea floated for W3C Level 2 (still not in the Recommendation) — or an application-level echo of the receiver context, after which a sender-side link closes the rendezvous. TTA needs no cure: it sees IN and OUT as first-class trace records on *both* cores and applies rule 2 ("an IN blocks communication on both execution threads") directly.

### 4.7 Timestamps are neither sound nor complete

**Proposition 4.5.** Let clocks have unbounded (per spec: unspecified) relative skew ε. Define e ⊑_t f iff ts(e) < ts(f). Then (i) ⊑_t is not sound: there are executions with e → f across processes and ts(f) < ts(e) whenever true message latency < skew; (ii) ⊑_t is not complete-in-the-right-way: it is a *total* order, hence asserts an ordering for every concurrent pair, so 𝓛 collapses to a single chain — the analyst sees one interleaving, with no warrant that it occurred. In lattice terms: causal skew *widens* the lattice beyond truth (spurious cuts); timestamp totalization *narrows* it below truth (a single maximal chain, possibly a chain that is not a linear extension of →_X at all, i.e., not even a member of the true lattice). Both failure directions are fatal for debugging inference — the first manufactures impossible schedules, the second hides real ones. This is Lamport's 1978 argument replayed against OTel's own timestamp fields; the spec's structure-bearing parts (parent, links) are its Lamport-compliant parts.

### 4.8 Characterizing the gap exactly

Decompose →_X = (PO ∪ SY_fwd ∪ SY_rel)⁺: program order, forward data edges, release edges. Against this, spec-guaranteed OTel recovers:

| Component | OTel₀ | OTel_L(κ_semconv) | Needed for completeness |
|---|---|---|---|
| PO (per-thread total order) | fragments, via same-thread span nesting; sibling order lost | same | per-thread sequence numbering of *all* events |
| SY_fwd (send→recv) | only where the hop is instrumented **and** wins the single parent slot | instrumented hops, incl. fan-in via links | an oriented edge for **every** matched pair; channel identity where contexts are absent |
| SY_rel (recv→sender's continuation) | never (Thm 4.4); kind-inferred for sync RPC only | same | response-direction propagation, or receiver-context echo + sender link |
| ∥ (independence) | never assertable (open world) | never | closed-world completeness over the communication alphabet in the window |

**Theorem 4.6 (Completion).** Let R⁺ be OTel_L(κ) augmented with the discipline: (a) every event of interest is a (zero-duration) span carrying (`service.instance.id`, `thread.id`, `thread.seq`) with `thread.seq` strictly increasing per thread; (b) every matched communication (s, r) yields a link r ↦ s with `link.dir=predecessor`, and — via context echo or `traceresponse` — a link s′ ↦ r; (c) the window's channel alphabet is declared and completely instrumented (closed world). Then R⁺ is causally sound and complete: the decoded order (union of PO from (a) and oriented links from (b), transitively closed) equals →_X, hence determines 𝓛(X) and the exact feasible set.
*Proof sketch.* Soundness: each decoded edge class is a subset of →_X by construction. Completeness: →_X is generated by PO ∪ SY_fwd ∪ SY_rel; (a) supplies all of PO, (b) supplies both synchronization edge classes for every matched pair (matching is total by (c)); transitive closure does the rest. Independence: closed world (c) makes non-reachability a proof of ∥. ∎

Every ingredient of R⁺ is expressible **today** as attributes and links — no schema change — which yields the paper's central verdict:

> **OTel is a causality *container*, not a causality *calculus*.** Its identifiers and transport can carry a complete Lamport lattice (Prop 4.1, Thm 4.6), but its specification guarantees only a forest fragment (Thm 4.2), leaves link meaning to convention (Thm 4.3), and cannot close rendezvous with standard one-way propagation (Thm 4.4). The "workaround" the parenting/linking mechanism actually delivers is the **communication index** the 2012 report identified as missing (p. 30): globally unique, propagated message identity that collapses TTA's OUT/IN *matching* ambiguity μ. It does not deliver, and was never designed to deliver, the *ordering* completeness that makes the lattice a lattice.

**Corollary (what OTel does to TTA's search space).** TTA's phase-1 possibility graph exists precisely because an IN may link to several candidate OUTs; with contexts on messages, μ is a recorded bijection and the possibility graph loses all matching-induced branching — a potentially exponential pruning of the all-paths enumeration (which, for counting, is #P-hard in general: counting linear extensions, Brightwell–Winkler 1991). The branching that *remains* — genuine concurrency, TTA's actual quarry — is exactly what OTel leaves on the table.

---

## 5. Bridging TTA and OTel

### 5.1 TTA as an OTel trace-analysis tool

Entirely feasible; the natural shape is an **OTLP-consuming processor/backend**:

```
 instrumented system                       analysis plane
┌──────────────────────┐      OTLP      ┌───────────────────┐
│ app + channel shims  │ ─────────────▶ │ OTel Collector    │
│  (zero-duration      │                │  └─ tta-processor │──▶ possible graph
│   IN/OUT spans,      │                │      1. window     │──▶ feasible set
│   thread.seq attrs,  │                │      2. rebuild PO │──▶ DOT / PNG /
│   channel.* attrs,   │                │      3. edges/μ    │    Jaeger DAG
│   links on IN spans) │                │      4. enumerate  │
└──────────────────────┘                └───────────────────┘
```

1. **Ingest & window**: group spans by `TraceId` (or the ⊤/⊥ markers, themselves spans) — the "targeted" window survives intact as a trace.
2. **Rebuild per-thread sequences** from (`service.instance.id`, `thread.id`, `thread.seq`) — reconstructing exactly the Cᵢ = [⊤, …, ⊥] input lists of the 2012 algorithm.
3. **Edge/μ extraction**: where IN spans carry links to OUT spans, μ is given — skip phase-1 candidate generation for those pairs. Where links are absent (partial instrumentation), fall back to the original TTA candidate linking over `channel.id`-compatible OUT/IN pairs; recorded edges *prune* the possibility graph monotonically (each edge removes every path violating it).
4. **Enumerate feasible linearizations** by the report's rules (INs preceded by OUTs; IN blocks both threads; all nodes present), now per reconstructed order — i.e., compute maximal chains of the residual lattice.

The 2012 Lisp phase-2 (`code/development-tracer/tta.lisp`, `tta.prolog`) ports directly: only the parser changes (OTLP protobuf instead of `xsim` output).

### 5.2 Using span links to encode channel-level causality

Yes — this works *today*, spec-conformantly, and is the recommended encoding:

- Each channel operation is a **zero-duration span** (`INTERNAL`), attributes: `channel.id`, `channel.op ∈ {in, out}`, `channel.seq` (per-channel counter), `thread.id`, `thread.seq`.
- The message carries the OUT span's context (in-band piggyback: on HTTP/AMQP/Kafka, headers; on an XC-class channel, one extra word or a side-table keyed by `channel.seq`).
- The **IN span links to the OUT span** with `link.dir = predecessor` — legal since "linked SpanContexts can be from the same or a different trace," and now orientable because the attribute says so.
- For the release edge: the IN side echoes its context (reply path or side-table); the sender's *next* span links back with `link.dir = predecessor`. Where echo is impossible, emit the release edge at analysis time from rule 2 semantics (`channel.op` + blocking semantics of the channel class, declared once per channel).

Span *events* would be the lighter-weight encoding but are disqualified: they cannot carry links (§3.2).

### 5.3 Extensions to the OTel spec that TTA-style analysis would want

1. **Normative link direction/type** — a spec-level `link.kind` (e.g. `follows_from` / `precedes`, shades of OpenTracing's lost `FollowsFrom`) so decoders need no out-of-band κ. (Directly addresses Thm 4.3.)
2. **Links on span events** — event-granularity causal edges without span-per-event overhead. (Addresses §3.2's cost objection.)
3. **Response-direction propagation** — standardize `traceresponse` (W3C Level 2 candidate territory) so SY_rel is recordable. (Addresses Thm 4.4.)
4. **Thread/sequence semantic conventions** — blessed `thread.seq` per-thread counters, making PO first-class. (Logical-clock attributes; also fixes sibling ordering without wall clocks.)
5. **Channel semantic conventions** — `channel.id`, `channel.op`, blocking class; the CSP analogue of the messaging semconv.
6. **Closed-world window marker** — a declarable "within trace X, alphabet A was completely instrumented," licensing independence conclusions (∥ as a theorem, not a shrug).

Notably, items 4–6 need only *semantic-convention* work, not data-model changes — consistent with Prop 4.1.

### 5.4 The practical hybrid

The right division of labor, given the theorems:

- **OTel for the coarse skeleton (sound, incomplete)**: service-to-service parent/link edges, message identity, window delimitation via `TraceId`. Its recorded order ⊑ is a trusted *subset* of →.
- **TTA inside the window (completes and quantifies)**: within a trace, over the channel-instrumented alphabet, TTA enumerates the linear extensions ⊑ fails to exclude — turning OTel's silent incompleteness into an explicit, inspectable *set of feasible executions*. Each OTel edge prunes; each pruning is exponential leverage against the #P-hard enumeration; whatever ambiguity survives is genuine concurrency, displayed to the engineer exactly as the 2012 report's feasible-path PNGs did.
- **Slogan**: *OTel shrinks the lattice; TTA enumerates what remains.* Neither subsumes the other: OTel without TTA silently presents one tree (or one timestamp-sorted chain) as if it were the truth; TTA without OTel drowns in matching ambiguity that span contexts dissolve for free.

---

## 6. Comparison Table

| Aspect | Lamport / TTA (2012 report) | OpenTelemetry (spec as of 2026) |
|---|---|---|
| Causality model | Happens-before partial order from program order + synchronous rendezvous; consistent-cut lattice; feasible set = linear extensions | Implicit DAG: ≤1 parent edge (sound, creation-time) + links ("causally related", direction/semantics unspecified) |
| Primitive unit | Event (single IN/OUT on a channel) | Span (timed operation interval); events exist but are causally inert |
| Channel identification | First-class: every record names its channel/resource; feasibility rules are per-channel | None in data model; `messaging.destination.*` by convention only |
| Message matching (μ) | Ambiguous without metadata — the possibility graph exists to enumerate candidate OUT↔IN matchings (report p. 30) | **Solved**: `SpanContext` is a propagated, globally unique communication index |
| Ordering guarantee | Exact →, given complete channel trace in window; residual ambiguity enumerated, never hidden | start(parent)→start(child) only; links need out-of-band convention; sibling & intra-process order unrecorded |
| Joins / fan-in | Native (multiple predecessors per event) | Parent: impossible (Thm 4.2); links: possible, optional, exceptional |
| Inter-sibling causality | Captured (it is the whole point) | Not captured unless the hop is itself instrumented (Thm 4.3) |
| Rendezvous back-edge (recv → sender's continuation) | Captured (rule 2: IN blocks both threads) | Unrecordable under one-way propagation (Thm 4.4); kind-inferred for sync RPC only |
| Independence (e ∥ f) | Provable: closed world over the traced alphabet | Never assertable: open world; absent edge ≠ no causality |
| Clock reliance | None — cycle counts discarded for ordering; pure structure | Wall-clock timestamps, no sync/accuracy requirement; ubiquitous (unsound) tie-breaker in backends |
| Propagation mechanism | None needed (post-mortem matching over complete traces) | W3C `traceparent`/`tracestate` (one-way), B3, Baggage; messaging headers |
| Completeness obligation | By construction within the targeted window | None; instrumentation coverage is best-effort |
| Analysis output | Possible graph → set of all feasible executions (lattice made explicit) | One tree per trace as rendered by typical backends; a single implied interleaving |
| Ambiguity handling | Enumerated and shown | Silently absent from the data |
| Cost / domain | Heavy per-event tracing; small targeted windows; embedded/multicore | Low per-request overhead; sampling; planet-scale services |

---

## 7. Conclusion

The intuition being tested — *"OTel provides a way to nominally work around the causality questions via its parenting/linking mechanism"* — decomposes under formal analysis into one true claim and one false one.

**True**: OTel's propagated `SpanContext` is exactly the "metadata serving as a communication index" whose absence the 2012 report flagged as the source of matching ambiguity. Where context rides the message, the question *which OUT fed this IN* — the generator of TTA's possibility-graph branching — is answered by construction. In this sense OTel does not merely work around one of TTA's problems; it eliminates it.

**False**: the parenting/linking mechanism does not yield the causality lattice. The spec-guaranteed fragment is a forest and cannot represent a single join (Theorem 4.2); links are semantically unmoored in the spec itself (Theorem 4.3); one-way propagation makes half of every synchronous rendezvous unrecordable (Theorem 4.4); timestamps rescue nothing (Proposition 4.5); and the open-world edge set means concurrency can never be *concluded*, only suspected. What survives is a sound, incomplete sub-order — a lattice strictly larger than the truth, silently presented as if it were the truth.

The constructive result (Theorem 4.6) is the research direction: a modest attribute-and-link discipline — per-thread sequence numbers, oriented links per matched communication, response-direction context echo, a closed-world window declaration — makes an OTel stream causally complete, at which point **TTA becomes the natural decoder**: the algorithm that takes a (possibly still-incomplete) recorded order and renders the residual lattice as an explicit, finite set of feasible executions. Fourteen years on, the division of labor is clean: OpenTelemetry supplies the identity, transport, and ecosystem the 2012 XCore tooling had to hand-build; the Targeted Trace Algorithm supplies the ordering rigor OpenTelemetry never specified.

---

## References

- L. Lamport. "Time, Clocks, and the Ordering of Events in a Distributed System." *CACM* 21(7), 1978.
- F. Mattern. "Virtual Time and Global States of Distributed Systems." *Parallel and Distributed Algorithms*, 1989. (Consistent-cut lattice.)
- C. J. Fidge. "Timestamps in Message-Passing Systems that Preserve the Partial Ordering." *Proc. 11th ACSC*, 1988.
- B. Charron-Bost. "Concerning the Size of Logical Clocks in Distributed Systems." *IPL* 39(1), 1991. (Vector-clock dimension lower bound.)
- R. Schwarz, F. Mattern. "Detecting Causal Relationships in Distributed Computations: In Search of the Holy Grail." *Distributed Computing* 7, 1994.
- G. Brightwell, P. Winkler. "Counting Linear Extensions is #P-complete." *STOC*, 1991.
- C. A. R. Hoare. "Communicating Sequential Processes." *CACM* 21(8), 1978.
- B. H. Sigelman et al. "Dapper, a Large-Scale Distributed Systems Tracing Infrastructure." Google TR, 2010.
- P. Nathan. "Debugging by Visualizing Communication on a Parallel Embedded System." M.S. report, University of Idaho, 2012. (`masters-work/report.pdf`; TTA: ch. 3, esp. §3.4 and pp. 27–32.)
- OpenTelemetry Specification: Overview (§Traces, §Links between spans); Trace API (§SpanContext, §Span creation, §Specifying links, §Add Links); Context spec. `opentelemetry.io/docs/specs/otel/`.
- OpenTelemetry Semantic Conventions: Messaging spans (creation/receive/process/settle spans; links-by-default; single-message parenting exception). `opentelemetry.io/docs/specs/semconv/messaging/messaging-spans/`.
- W3C. *Trace Context*. Recommendation, 23 Nov 2021. `w3.org/TR/trace-context/`. *Trace Context Level 2*, Candidate Recommendation draft (random trace-id flag; response-propagation discussions). `w3.org/TR/trace-context-2/`.
- OpenTelemetry specification issue #3548 ("How to properly link spans that outlive their parent") — evidence on parent/child non-containment in async scenarios.
