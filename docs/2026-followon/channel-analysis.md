# Channel Identification and Naming in the Targeted Trace Algorithm: An Analysis Across Seven Computing Environments

Paul Nathan, 2026

## 1. Introduction

### 1.1 The Original Channel Identification Problem

The Targeted Trace Algorithm (TTA), developed as part of a 2013 Master's
thesis at the University of Idaho, addresses a fundamental question in
debugging concurrent systems: given per-core traces of communication
events (INs and OUTs), determine the set of feasible execution
orderings that could have produced the observed behavior. The algorithm
operates on tuples of the form `(core, comm-sequence, inst)`, where
`core` identifies the execution thread, `comm-sequence` identifies
which channel carried the communication, and `inst` is either IN or
OUT.

The algorithm proceeds in phases: it collects per-core traces, filters
for communication events, partitions those events by channel and then
by core, links each IN to its possible OUTs (forming a lattice between
synthetic TOP and BOTTOM nodes), enumerates all paths through that
lattice, and prunes infeasible paths using causality rules. The three
causality rules are:

1. INs are always preceded by an OUT.
2. An IN blocks communication on both execution threads until its data
   is received.
3. All nodes must be represented in a feasible trace.

This algorithm was validated on the XMOS XS1-G4 architecture, a
four-core processor built around Communicating Sequential Processes
(CSP) principles. In the XS1, all inter-core communication passes
through a central hardware COMM Switch. The XC programming language
provides a `chan` keyword that declares channels, and the `<:` and `:>`
operators perform synchronous, blocking send and receive operations
respectively. At the language level, channels have clear identities: a
programmer writes `chan queue1;` and then uses `queue1 <: data` and
`queue1 :> data` on different cores within a `par` block.

However, the thesis encountered a critical limitation in mapping these
language-level channel names to what was observable in the trace. The
XC compiler establishes channels before execution reaches the
START probe point. The hardware switch does not expose channel identity
in a way that can be straightforwardly captured at trace time. The
implementation extracted channel identity from resource IDs embedded in
the operands of IN and OUT assembly instructions (`in`, `int`, `out`,
`outt`), using a function `channel-of-comms` that parsed the resource
counter from the instruction's resource ID field. As the sim-reader
source comments:

> "it is impossible in the current system to determine channel
> creation/deletion on the fly with a trace"

The generic TTA implementation in `tta.lisp` explicitly deferred the
problem, noting:

> "A key simplification in this code is that I assume any professional
> monitoring solution tells me what core it executed on, along with
> which communication sequence a communication belonged to."

The `find-channels-in` and `link-up-nodes-in-channels` functions were
left incomplete, their bodies empty -- the channel identification
problem was architecturally sidestepped rather than solved.

The desktop tracer implementation (`communicator.lisp`) took a
different approach: since it controlled the channel implementation in
software, it could instrument channel creation directly. The
`new-channel` function generated names using `(gentemp "C" 'KEYWORD)`
combined with sender/receiver metadata, producing structured channel
identifiers like `((:CHANNEL-NAME . :C3) (:SENDER . "node2")
(:RECEIVER . "node3"))`. The `:around` methods on `send` and `receive`
logged these identifiers alongside `:tx`/`:rx` operation types and
per-core sequence counters.

This contrast between the hardware tracer (where channel identification
was opaque and incomplete) and the desktop tracer (where channel
identification was trivially available because the channel
implementation was owned by the tracer) illuminates the central
question of this analysis: **how would channel identification work if
TTA were applied to systems beyond the original XMOS XS1?**

### 1.2 What "Channel" Means in TTA

In TTA, a "channel" is an abstraction over any mechanism by which one
execution context sends data to another. The algorithm does not require
channels to be synchronous in the CSP sense -- that was a property of
the XS1 hardware that enabled stronger causality pruning. What TTA
requires is:

- **Identifiability**: each communication event must be attributable to
  a specific channel.
- **Directionality**: each event on a channel must be classifiable as
  IN or OUT.
- **Per-core ordering**: events on a given core must have a total order.

The original system enjoyed a further property -- synchronous blocking
-- which the causality rules exploit. When channels are asynchronous,
the causality rules weaken: an OUT no longer blocks the sending thread
until the corresponding IN completes. This means the feasible path set
grows, and additional metadata (timestamps, sequence numbers, message
IDs) becomes necessary to prune it.

### 1.3 The Six Dimensions of Analysis

For each computing environment examined below, we address:

1. **Channel Identification**: How are communication endpoints named
   and distinguished? What identifiers are available at trace time?
2. **Channel Multiplexing**: Can multiple logical channels share a
   physical transport? How is demultiplexing observed?
3. **Channel Directionality**: Is directionality inherent in the
   channel mechanism, or must it be imposed by the tracer?
4. **Observability**: What instrumentation is available to capture
   channel events? What is the probe effect?
5. **Temporal Ordering**: What clocks or ordering mechanisms exist?
   How do they map to TTA's requirement for per-core event ordering?
6. **Practical Naming Scheme**: A concrete proposal for TTA channel
   names in this environment.


## 2. Modern Embedded Systems (Non-Linux)

This section addresses real-time operating systems (RTOS),
bare-metal multicore configurations, and domain-specific embedded
platforms such as those found in automotive (AUTOSAR), avionics
(ARINC 653), and industrial control systems.

### 2.1 Channel Identification

Modern embedded systems present a heterogeneous landscape of
communication mechanisms, but they share a common trait: communication
channels are typically configured statically at build time or during
system initialization, and their identities are known to the system
integrator.

**RTOS message queues.** In FreeRTOS, a queue is created by
`xQueueCreate()` and referenced thereafter by an opaque
`QueueHandle_t`. The handle is a pointer to an internal queue control
block. The kernel does not assign a human-readable name unless the
application uses `vQueueAddToRegistry()`, which associates a string
name with the handle. For TTA purposes, the handle address provides a
unique channel identifier, but it is only meaningful within a single
address space. In symmetric multiprocessing (SMP) FreeRTOS variants
(e.g., ESP-IDF's FreeRTOS SMP), queues can be accessed from multiple
cores, and the handle remains valid across cores because they share an
address space.

Zephyr RTOS provides `k_msgq` (message queues), `k_pipe` (byte
streams), and `k_mbox` (mailboxes). Each is a statically or
dynamically allocated kernel object with a pointer-based identity.
Zephyr's object tracing subsystem can enumerate all kernel objects of a
given type, providing a mechanism for discovering channels at trace
time.

**Inter-processor communication (IPC) in multicore MCUs.** On
asymmetric multiprocessing (AMP) systems such as the NXP i.MX RT1170
(Cortex-M7 + Cortex-M4) or STM32MP1 (Cortex-A7 + Cortex-M4), cores do
not share an address space. Communication uses hardware mailboxes,
shared memory regions with hardware semaphores, or vendor-specific IPC
frameworks. NXP's Multicore SDK uses the Multicore Manager (MCMgr) with
named endpoints. STM32's OpenAMP implementation uses RPMsg, which
identifies channels by a `(source-address, destination-address)`
pair on a virtual transport (VirtIO).

In RPMsg, the source and destination addresses are 32-bit integers
assigned during channel creation. A channel is uniquely identified by
the tuple `(vproc-id, src-addr, dst-addr)`, where `vproc-id`
identifies the remote processor. This provides a stable, observable
channel identifier.

**AUTOSAR COM.** The AUTOSAR Classic Platform defines communication
through signal-based and PDU-based (Protocol Data Unit) models. Each
signal has a globally unique Signal ID defined at configuration time.
The COM module multiplexes signals into PDUs, which are carried by the
transport layer (CAN, LIN, FlexRay, Ethernet). For TTA, the Signal ID
provides the channel identifier. Because AUTOSAR configurations are
generated from system description files (ARXML), the complete channel
topology is known statically. The I-PDU ID serves as a channel
identifier at the transport level, while individual Signal IDs provide
finer-grained channel identification at the application level.

**ARINC 653.** The avionics standard defines inter-partition
communication through ports: sampling ports (latest-value semantics)
and queuing ports (FIFO semantics). Each port has a system-unique name
(a string up to 30 characters) assigned in the module configuration
table. Channels connect source ports to destination ports and are named
in the configuration. The port name is the natural TTA channel
identifier.

### 2.2 Channel Multiplexing

Embedded systems exhibit multiplexing at multiple levels.

At the transport level, CAN bus multiplexes all communication onto a
shared differential pair, with message identity carried by the CAN ID
(11-bit standard or 29-bit extended). Multiple logical channels may
share a CAN bus, distinguished by CAN ID or by PDU-level framing. A TTA
tracer on a CAN bus sees a single physical channel with many logical
channels multiplexed onto it. CAN arbitration imposes a partial
ordering (higher-priority messages preempt lower-priority ones), but
concurrent transmissions from different nodes are not temporally
ordered by the bus itself.

At the RTOS level, a single task may service multiple queues using a
`select`-like pattern (e.g., FreeRTOS `xQueueSelectFromSet` or Zephyr
`k_poll`). This is analogous to the XC `select` statement that waits
on multiple channel ends. The TTA tracer must record which queue was
selected on each iteration.

In RPMsg, multiple channels are multiplexed onto a single shared-memory
VirtIO transport. The RPMsg header contains source and destination
addresses, providing channel demultiplexing at the transport level.

### 2.3 Channel Directionality

Most embedded IPC mechanisms are inherently unidirectional. An RTOS
message queue has a single direction of data flow: tasks enqueue with
`xQueueSend` and dequeue with `xQueueReceive`. Bidirectional
communication requires two queues.

ARINC 653 ports are explicitly directional: a port is either SOURCE or
DESTINATION. The channel configuration specifies the direction.

RPMsg channels are bidirectional at the transport level -- either end
can send. However, the typical usage pattern is request-response, where
one end is the initiator and the other is the responder. TTA needs to
observe send and receive operations independently, which is
straightforward because the RPMsg API distinguishes `rpmsg_send()` from
the receive callback.

CAN is inherently broadcast: any node can transmit, and all nodes
receive. A CAN message has a sender (implicit in which node placed it
on the bus) and potentially multiple receivers. This is a 1:N channel
that does not fit the original TTA model of 1:1 CSP channels.
Extending TTA to handle broadcast channels would require treating each
receiver's acceptance of the message as a separate IN event, all
corresponding to the same OUT.

### 2.4 Observability

**RTOS trace tools.** Percepio Tracealyzer, SEGGER SystemView, and
similar tools provide kernel-level trace instrumentation with minimal
probe effect. These tools hook into RTOS kernel instrumentation points
(e.g., FreeRTOS trace macros like `traceBLOCKING_ON_QUEUE_RECEIVE`,
`traceQUEUE_SEND`, `traceQUEUE_RECEIVE`) and record timestamped events
to a circular buffer. The probe effect is typically 1-5 microseconds
per event, which is significant for systems with microsecond-scale
deadlines but acceptable for millisecond-scale ones.

Zephyr's built-in tracing subsystem provides CTF (Common Trace Format)
output compatible with tools like Babeltrace and TraceCompass. This
provides per-event timestamps with cycle-counter precision.

**Hardware trace.** ARM CoreSight provides non-intrusive instruction
and data tracing via ETM (Embedded Trace Macrocell) and ITM
(Instrumentation Trace Macrocell). ETM trace is the embedded analog of
the XS1 simulator trace -- it records every instruction executed,
including load/store operations that implement IPC. The trace data is
streamed off-chip via a TPIU (Trace Port Interface Unit) or stored in
an on-chip ETB (Embedded Trace Buffer). CoreSight provides global
timestamps across cores via the global timestamp generator, offering
cross-core temporal ordering that the XS1 simulator lacked.

The key advantage over the original XS1 approach is that CoreSight
timestamps are non-intrusive: they do not perturb the program's timing.
The probe effect is zero for instruction trace and near-zero for
data trace (a few cycles of stall when the trace buffer fills). This is
a substantial improvement over the XS1 debug-interrupt approach, which
consumed instruction cycles and memory for the trace handler.

**CAN bus analyzers.** External CAN bus analyzers (e.g., Vector
CANalyzer, PEAK PCAN-View) provide complete, non-intrusive observation
of all messages on the bus with microsecond-resolution timestamps.
Since the analyzer is external to all nodes, the probe effect is zero.
However, the analyzer's timestamps are in the analyzer's clock domain,
not the nodes' clock domains, requiring clock synchronization.

### 2.5 Temporal Ordering

Per-core ordering is straightforward: each core executes instructions
sequentially, and trace tools record events in execution order. The
challenge is cross-core ordering.

**Cycle counters.** Most modern MCUs provide a cycle counter (e.g.,
ARM DWT CYCCNT). On a single-chip multicore device with a shared clock,
cycle counters can be synchronized at boot time, providing a global
time reference. However, if cores run at different frequencies (common
in big.LITTLE and heterogeneous configurations), cycle counters are not
directly comparable and must be converted to wall-clock time using
the known frequency ratios.

**Global timestamps.** ARM CoreSight global timestamps and AUTOSAR
Synchronized Time Base Manager provide cross-core time references.
AUTOSAR Global Time Synchronization (StbM) distributes time across the
vehicle network, but its precision is typically limited to microseconds
over CAN and sub-microsecond over Ethernet (using IEEE 802.1AS /
gPTP).

**Sequence numbers.** In systems without synchronized clocks (e.g.,
AMP configurations with independent oscillators), application-level
sequence numbers or Lamport clocks can establish causal ordering. The
RTOS itself does not provide these; they must be implemented in the
application or in the TTA instrumentation layer.

The TTA algorithm's original formulation does not require globally
synchronized time -- it requires only per-core ordering and the ability
to identify which OUT corresponds to which IN. Timestamp information,
when available, strengthens pruning: a candidate trace where an IN
precedes its corresponding OUT in wall-clock time can be eliminated.

### 2.6 Practical Channel Naming Scheme

For TTA applied to modern embedded systems, a hierarchical naming
scheme captures the relevant layers:

```
<system>/<core>/<mechanism>/<id>

Examples:
  stm32mp1/m4/rpmsg/0x400.0x401
  imxrt1170/cm7/freertos-queue/0x2000_1234
  autosar/ecu3/com-signal/BrakePressure_0x1A3
  arinc653/partition2/qport/FuelFlowData
  canbus/node5/canid/0x1A3
```

The naming scheme encodes:
- The system or chip identity (for multi-ECU or multi-chip systems)
- The core identity
- The communication mechanism type
- The mechanism-specific channel identifier

For the TTA tuple, this maps as:
- `core` = `<system>/<core>`
- `comm-sequence` = `<mechanism>/<id>`
- `inst` = IN or OUT, derived from the API call observed (send vs.
  receive, enqueue vs. dequeue)


## 3. Desktop Systems

This section addresses multi-process and multi-threaded applications on
general-purpose operating systems (Linux, Windows, macOS), including
IPC mechanisms, threading primitives, and cross-process communication.

### 3.1 Channel Identification

Desktop operating systems provide a rich and overlapping set of IPC
mechanisms. Each carries its own identification scheme.

**Pipes and FIFOs.** Unix pipes are the closest analog to CSP
channels: unidirectional, point-to-point, blocking. An anonymous pipe
is identified by a pair of file descriptors in the parent process and
inherited by children. It has no system-wide name. Named pipes (FIFOs)
have filesystem paths (e.g., `/tmp/myapp.fifo`). For anonymous pipes,
the channel identifier must be the file descriptor pair plus the PID of
each endpoint process. For named pipes, the filesystem path is the
natural channel name.

**Unix domain sockets.** Identified by filesystem path
(`/var/run/app.sock`) for bound sockets, or by an abstract namespace
address (`\0/app/channel1`) on Linux. A connected socket pair forms a
bidirectional channel. The channel identifier is the socket's address
plus the `(local-fd, remote-fd)` or `(local-pid, remote-pid)` pair
for connected endpoints.

**System V and POSIX IPC.** System V message queues are identified by
a numeric key (from `ftok()`) and a queue ID (from `msgget()`). POSIX
message queues are identified by a name string (e.g., `/myqueue`). POSIX
shared memory segments are identified similarly. For message queues, the
queue name or key is the natural channel identifier. For shared memory
used as a communication channel (with synchronization via semaphores or
futexes), the channel is the shared memory segment plus the
synchronization object, identified by their keys or names.

**TCP/UDP sockets.** Identified by the 5-tuple `(protocol, src-ip,
src-port, dst-ip, dst-port)`. For local inter-process communication
over loopback, this collapses to `(protocol, src-port, dst-port)`.

**D-Bus.** The Linux desktop IPC standard identifies channels by
`(bus-name, object-path, interface, member)`. A D-Bus signal or method
call carries all four components. The bus name (e.g.,
`org.freedesktop.Notifications`) provides service identity, the object
path (e.g., `/org/freedesktop/Notifications`) identifies the target
object, and the interface plus member identify the specific operation.
For TTA, the D-Bus message header provides a complete channel
identifier without additional instrumentation.

**Threads within a process.** Intra-process communication between
threads typically uses shared memory with synchronization primitives
(mutexes, condition variables, channels in languages like Go or Rust).
Go channels are the closest to CSP: `ch := make(chan int)` creates an
anonymous channel identified only by its memory address. Rust's
`std::sync::mpsc` channels are similarly identified by address. For TTA
instrumentation of in-process channels, the desktop tracer approach
from the original thesis -- wrapping channel operations with recording
code -- is the most practical path. The channel name is assigned at
creation time by the instrumentation layer.

### 3.2 Channel Multiplexing

Desktop IPC multiplexing takes several forms.

**File descriptor multiplexing.** A process may multiplex reads from
multiple pipes, sockets, and FIFOs using `select()`, `poll()`,
`epoll()` (Linux), or `kqueue()` (BSD/macOS). This is the operating
system's analog to the XC `select` statement. The TTA tracer must
record which file descriptor was ready and serviced on each call.

**Connection multiplexing.** HTTP/2 multiplexes multiple logical
streams over a single TCP connection. gRPC inherits this. Each stream
has a numeric stream ID. For TTA, the channel identifier becomes
`(connection-id, stream-id)`.

**Thread pool multiplexing.** A thread pool may service requests from
multiple channels. The logical channel is the work queue or the
request's origin, not the servicing thread. TTA must distinguish
between the core (the servicing thread) and the channel (the work
queue or request source).

### 3.3 Channel Directionality

Unix pipes are unidirectional. Sockets (TCP, Unix domain) are
bidirectional. D-Bus distinguishes method calls (request-response),
signals (one-way broadcast), and properties (get/set).

For bidirectional channels, TTA can treat each direction as a separate
logical channel, or it can treat send and receive on the same channel
as OUT and IN respectively. The latter is more natural for
request-response patterns, where a send by the client is an OUT and the
corresponding receive by the server is an IN on the same channel.

The desktop tracer from the original thesis took the
unidirectional approach: `new-channel` created a channel from sender to
receiver, and a reverse channel would be a separate object. This is
clean for the algorithm but requires the tracer to model bidirectional
mechanisms as pairs of unidirectional channels.

### 3.4 Observability

**strace / ltrace / dtrace.** System call tracing (`strace` on Linux,
`dtruss` on macOS) captures all IPC system calls with arguments: which
file descriptor, what data, what flags. This provides complete IPC
observability at the cost of significant probe effect. `strace`
typically slows a process by 10-100x due to the ptrace overhead of
stopping the process on every system call.

**eBPF.** Linux's eBPF (extended Berkeley Packet Filter) provides
programmable kernel-level tracing with minimal probe effect. An eBPF
program attached to the `tracepoint/syscalls/sys_enter_write` and
`sys_enter_read` tracepoints (or to `kprobe/unix_stream_sendmsg` for
socket-specific tracing) can record IPC events with nanosecond
timestamps. The probe effect is typically 100-500 nanoseconds per
event. eBPF programs can extract file descriptor numbers, PIDs, and
even inspect message contents, providing complete channel
identification.

**ETW (Event Tracing for Windows).** Windows provides kernel-level
tracing of IPC operations through the OS kernel provider. Named pipes,
sockets, and ALPC (Advanced Local Procedure Call) operations are all
traceable with microsecond-resolution timestamps.

**Language-level instrumentation.** For Go channels, the Go runtime
trace (`go tool trace`) records goroutine blocking on channel
operations. For Rust, tracing crates can wrap channel operations. For
Java, JFR (Java Flight Recorder) captures concurrent primitive
operations. These provide language-level channel identification with
moderate probe effect.

**Hardware performance counters.** Intel PT (Processor Trace) and ARM
CoreSight (on Apple Silicon) provide non-intrusive instruction trace on
desktop processors. However, at desktop scale (billions of instructions
per second, many active threads), the trace volume is enormous and
filtering for IPC-relevant events is challenging.

### 3.5 Temporal Ordering

Desktop systems provide several ordering mechanisms.

**Per-process/thread ordering.** Within a thread, events are totally
ordered by program execution. Within a process, events across threads
are partially ordered by synchronization operations (mutex
acquire/release, channel send/receive).

**Clock sources.** `clock_gettime(CLOCK_MONOTONIC)` provides
nanosecond-resolution timestamps within a single machine. For
inter-machine communication, NTP provides millisecond-scale
synchronization, PTP (IEEE 1588) provides sub-microsecond, and the
CPU's TSC (Time Stamp Counter) provides cycle-level resolution within
a single machine (but requires calibration across cores on NUMA
systems, mitigated by invariant TSC on modern Intel/AMD).

**Kernel tracing timestamps.** eBPF's `bpf_ktime_get_ns()` provides a
monotonic nanosecond timestamp consistent across all cores on a single
machine. This is the highest-quality time source for TTA on desktop
systems.

### 3.6 Practical Channel Naming Scheme

```
<host>/<pid>/<mechanism>/<id>

Examples:
  localhost/1234/pipe/fd3-fd4
  localhost/1234/unix-socket//var/run/app.sock
  localhost/1234/posix-mq//taskqueue
  localhost/1234/tcp/127.0.0.1:8080-127.0.0.1:45321
  localhost/1234/dbus/org.freedesktop.Notifications/Notify
  localhost/1234/go-chan/0xc0000b6060
  localhost/1234/thread/go-routine-42
```

For the TTA tuple:
- `core` = `<host>/<pid>/<thread-id>`
- `comm-sequence` = `<mechanism>/<id>`
- `inst` = IN or OUT, derived from the syscall or API observed
  (write/send = OUT, read/recv = IN)


## 4. Cloud Computing and Distributed Processes

This section addresses microservice architectures, message queues,
service mesh, and container orchestration platforms.

### 4.1 Channel Identification

Cloud systems introduce a fundamental shift from the original TTA
context: channels are dynamic, ephemeral, and often mediated by
infrastructure components (message brokers, load balancers, service
mesh proxies) that are invisible to the communicating application code.

**HTTP/REST APIs.** In a microservice architecture, a "channel" is an
HTTP endpoint. The channel identity is the URL:
`https://orders-service.prod.internal/api/v2/orders`. This is a
logical channel; the physical path may traverse a load balancer, a
service mesh sidecar (Envoy), a CDN, or multiple layers of proxies.
The channel is inherently request-response (one OUT from the client, one
IN at the server, one OUT from the server, one IN at the client).

**gRPC.** gRPC provides strongly-typed service definitions via Protocol
Buffers. A channel is identified by `(service, method)` -- e.g.,
`OrderService/CreateOrder`. gRPC supports four communication patterns:
unary (request-response), server streaming, client streaming, and
bidirectional streaming. Each pattern maps differently to TTA's IN/OUT
model. Unary is two pairs: client-OUT/server-IN followed by
server-OUT/client-IN. Streaming introduces sequences of OUTs and INs
that may interleave.

**Message queues.** Apache Kafka, RabbitMQ, Amazon SQS, and Google
Pub/Sub provide asynchronous channels that decouple sender and receiver
in time and space.

In Kafka, the channel identity is the `(topic, partition)` pair.
Topics are named strings (e.g., `order-events`). Partitions are
numbered integers within a topic. A producer sends to a topic (possibly
with a partition key), and consumers in a consumer group each read from
a subset of partitions. The consumer group ID further qualifies the
channel: `(topic, partition, consumer-group)` identifies a
unique logical channel from producers to a specific consumer.

In RabbitMQ, the channel identity is more complex: messages are
published to an exchange with a routing key, and the exchange routes to
queues based on bindings. The logical channel is
`(exchange, routing-key, queue)`. A direct exchange with routing
key `order.created` bound to queue `order-processor` forms a named
channel.

In SQS, each queue has an ARN (Amazon Resource Name) that uniquely
identifies it globally: `arn:aws:sqs:us-east-1:123456789:order-queue`.

**Service mesh (Istio/Envoy).** The service mesh interposes a sidecar
proxy between every pair of communicating services. The proxy can
observe and record all communication. From TTA's perspective, the
sidecar is an ideal instrumentation point: it sees every IN and OUT for
its associated service, with complete channel identification
(destination service, port, path). Envoy access logs record timestamps,
upstream/downstream addresses, request/response metadata, and latency
breakdowns.

**Event buses and streaming.** Apache Kafka Streams, AWS EventBridge,
and Google Cloud Eventarc provide event-driven communication where the
channel is an event type or event pattern. EventBridge rules match
events by pattern and route them to targets. The channel identity is
the `(event-bus, event-pattern, target)` tuple.

### 4.2 Channel Multiplexing

Cloud systems aggressively multiplex.

**Connection pooling.** HTTP/2 and gRPC multiplex many logical
channels over a single TCP connection. Individual requests are
identified by stream IDs. A load balancer may fan out requests from one
upstream connection to many downstream connections.

**Topic partitioning.** Kafka partitions multiplex a topic across
multiple brokers and consumer instances. A single producer "channel"
fans out to multiple partition-level channels.

**Pub/sub fan-out.** A single message published to an SNS topic or a
RabbitMQ fanout exchange is delivered to multiple subscribers. This is
1:N communication. Each subscriber's receipt is a separate IN event
corresponding to the same OUT event. TTA must model this as one OUT
linked to N INs.

**Connection sharing.** Multiple microservice instances behind a load
balancer share the same logical endpoint. A client's OUT may be routed
to any instance. The channel identity at the logical level is the
service name; at the physical level, it includes the specific instance.

For TTA, the relevant level depends on the debugging question. If the
question is "did service A communicate with service B?", the logical
channel suffices. If the question is "did this specific request reach
this specific instance?", the physical channel (including instance
identity and request ID) is needed.

### 4.3 Channel Directionality

Request-response APIs (HTTP, gRPC unary) are bidirectional at the
channel level but naturally decompose into two unidirectional exchanges:
request and response. Each has a clear OUT (sender) and IN (receiver).

Message queues are unidirectional: a producer sends (OUT) and a
consumer receives (IN). The reverse direction, if needed, uses a
separate queue (reply queue pattern, as in RabbitMQ's `reply_to`
header).

Streaming (gRPC streams, WebSockets, Kafka Streams) creates
long-lived bidirectional channels where either side can send at any
time. These require treating each message as an independent IN/OUT
event on the same channel, with direction determined by which side sent
it.

### 4.4 Observability

**Distributed tracing (OpenTelemetry).** OpenTelemetry (OTel) provides
a standardized framework for recording spans -- timed operations that
form a directed acyclic graph of causality. A span records a service
name, operation name, start time, end time, and parent span ID. The
trace ID propagated via HTTP headers (W3C Trace Context:
`traceparent`) links spans across service boundaries. An OTel span
with kind=CLIENT records an OUT; a span with kind=SERVER records the
corresponding IN. The span's attributes carry channel identification
(URL, gRPC method, message queue topic).

OTel is the closest existing system to what TTA needs for cloud
environments. The key differences are:

1. OTel traces are rooted at a specific request and capture the
   causal chain for that request. TTA examines all communication in a
   time window across all channels.
2. OTel establishes causality via trace context propagation (a parent
   span ID explicitly links cause and effect). TTA infers causality
   from the possible orderings of INs and OUTs.
3. OTel requires instrumentation (auto-instrumentation via agents or
   manual span creation). TTA could use passive observation.

An OTel-based TTA implementation could ingest spans, extract the IN/OUT
events, and run the TTA algorithm over them. The trace context provides
stronger causality than TTA alone can infer, so the combination would
produce tighter feasible path sets.

**Service mesh telemetry.** Envoy sidecar proxies emit metrics, logs,
and traces for every request. Istio's telemetry provides per-request
attributes including source/destination workload, namespace, and
request metadata. This is non-intrusive to the application code (the
proxy intercepts all traffic), making the probe effect near-zero from
the application's perspective.

**Message queue metrics.** Kafka provides consumer lag, partition
offsets, and consumer group state. The Kafka consumer's committed
offset for a `(topic, partition, consumer-group)` provides a sequence
number that establishes ordering within a partition. Kafka guarantees
ordering within a partition, so partition-level ordering maps directly
to TTA's per-core ordering requirement.

### 4.5 Temporal Ordering

Cloud systems face the most challenging temporal ordering problem of
all the environments considered here.

**No global clock.** Services run on different machines with
independent clocks. NTP synchronization provides millisecond-scale
accuracy, but microservice calls complete in single-digit milliseconds,
so NTP alone cannot order events. Google's TrueTime (used in Spanner)
provides bounded clock uncertainty (typically < 7ms), but it is not
generally available.

**Lamport clocks and vector clocks.** The trace context propagated by
OTel provides a form of Lamport clock: the parent span ID establishes
a happens-before relationship. However, OTel does not implement full
Lamport clocks across all communication -- only within a single trace.

**Kafka offsets.** Within a Kafka partition, offsets provide a total
order. Across partitions, there is no ordering unless the application
imposes one (e.g., by using a single partition or by including
timestamps/sequence numbers in message payloads).

**Causal ordering.** For TTA in cloud environments, the most practical
approach is to combine wall-clock timestamps (for approximate ordering)
with causal metadata (trace IDs, message IDs, consumer offsets) for
precise ordering. TTA's feasibility pruning then eliminates candidate
traces that violate causal constraints even if timestamps are
ambiguous.

### 4.6 Practical Channel Naming Scheme

```
<cluster>/<namespace>/<service>/<mechanism>/<endpoint>

Examples:
  prod-us-east/orders/order-svc/http/POST:/api/v2/orders
  prod-us-east/orders/order-svc/grpc/OrderService/CreateOrder
  prod-us-east/messaging/kafka/topic/order-events/partition/3
  prod-us-east/messaging/rabbitmq/exchange/orders/key/order.created/queue/processor
  aws/us-east-1/sqs/arn:aws:sqs:us-east-1:123456789:order-queue
```

For the TTA tuple:
- `core` = `<cluster>/<namespace>/<service>/<instance-id>`
- `comm-sequence` = `<mechanism>/<endpoint>`
- `inst` = IN or OUT, derived from span kind (CLIENT=OUT, SERVER=IN)
  or queue operation (produce=OUT, consume=IN)


## 5. Edge Computing and Messages

This section addresses IoT device communication, MQTT, CoAP, fog
computing, and edge-cloud interaction patterns.

### 5.1 Channel Identification

Edge computing creates a tiered architecture where devices at the
periphery communicate through intermediaries (gateways, brokers, fog
nodes) to reach cloud services. Channel identification must span these
tiers.

**MQTT.** The dominant IoT messaging protocol identifies channels by
topic strings: hierarchical, slash-delimited, with wildcard support.
A device publishing temperature data might use the topic
`building/floor3/room301/temperature`. The channel identity for TTA is
the `(client-id, topic)` pair for a publisher and the
`(client-id, topic-filter)` pair for a subscriber.

MQTT's topic structure creates a complication for TTA: a subscriber
using a wildcard topic filter (e.g., `building/floor3/+/temperature`)
receives messages from multiple publishers on different specific topics.
The wildcard subscription is a single logical channel that encompasses
multiple specific channels. For TTA, the tracer must record the
specific topic of each received message, not just the subscription
filter.

MQTT provides three QoS levels that affect channel semantics:
- QoS 0 (at most once): fire-and-forget. An OUT may have no
  corresponding IN (message lost).
- QoS 1 (at least once): guaranteed delivery but possible duplicates.
  An OUT may have multiple corresponding INs.
- QoS 2 (exactly once): guaranteed single delivery. OUT and IN are
  1:1.

Only QoS 2 provides the semantics assumed by the original TTA
algorithm. For QoS 0 and 1, the causality rules must be relaxed: an
IN may not have a corresponding OUT (if the message was a duplicate or
if the OUT was lost before tracing began), and an OUT may not have a
corresponding IN.

**CoAP (Constrained Application Protocol).** CoAP is a RESTful
protocol for constrained devices, operating over UDP. Channels are
identified by `(server-address, resource-path)` -- e.g.,
`coap://sensor1.local/temperature`. CoAP supports observation
(RFC 7641), where a client registers interest in a resource and the
server sends notifications on change. This creates a long-lived
channel from server to client, identified by the observation token.

**MQTT-SN.** For extremely constrained devices (8-bit MCUs, sub-kbps
links), MQTT-SN uses numeric topic IDs instead of string topic names,
with a registration mechanism to map IDs to names. The TTA tracer must
record the topic name mapping, not just the numeric ID.

**LwM2M (Lightweight M2M).** OMA's device management protocol
identifies resources by a numeric path: `/{object-id}/{instance-id}/
{resource-id}`. For example, `/3303/0/5700` is the "Sensor Value"
resource of the first instance of the Temperature object. This numeric
path is the channel identifier.

**LoRaWAN.** Long-range, low-power WAN communication identifies
devices by DevEUI (64-bit globally unique), application by AppEUI, and
session by DevAddr (32-bit). Uplink and downlink are separate channels.
A message is identified by `(DevAddr, FCnt, direction)`, where FCnt is
a frame counter providing sequence numbering.

### 5.2 Channel Multiplexing

Edge systems multiplex aggressively due to bandwidth constraints.

**MQTT broker fan-out.** A single MQTT message published to a topic is
delivered to all subscribers matching that topic. The broker is a
multiplexing/demultiplexing point. From TTA's perspective, the broker
is transparent: the OUT is the publish, the INs are the deliveries to
each subscriber.

**Gateway aggregation.** An IoT gateway may aggregate messages from
many devices into fewer, larger messages for upstream transmission.
This creates a many-to-one channel multiplexing that obscures
individual device communication. For TTA to work through a gateway,
the gateway must either pass through device identity metadata or
provide its own trace instrumentation.

**Protocol translation.** Edge gateways often translate between
protocols (e.g., BLE to MQTT, Zigbee to CoAP). The channel identity
changes at the translation point: a BLE characteristic UUID becomes an
MQTT topic. TTA must maintain a channel identity mapping across the
translation boundary.

### 5.3 Channel Directionality

**MQTT.** Publish is OUT; receive (on subscription callback) is IN.
The MQTT model is inherently unidirectional for each topic. A
request-response pattern requires two topics (request topic and
response topic), each unidirectional.

**CoAP.** Request-response is bidirectional on a single resource path.
Observe notifications are unidirectional (server to client). CoAP's
tokens link requests to responses, providing channel identity for the
response direction.

**LoRaWAN.** Uplink and downlink are physically separate channels
(different frequencies, different time slots). They are naturally
modeled as two unidirectional channels for TTA.

### 5.4 Observability

Edge computing presents the most constrained observability environment.
Devices have limited CPU, memory, and power. Trace instrumentation
competes with application code for these resources.

**MQTT broker logs.** The MQTT broker (e.g., Mosquitto, HiveMQ, EMQX)
can log all publish and subscribe events with timestamps. The broker
is the natural TTA instrumentation point for MQTT: it sees all OUTs
(publishes received from clients) and all INs (messages delivered to
subscribers). The broker's log provides a centralized trace without
instrumenting individual devices.

**Device-side constraints.** On an 8-bit MCU with 32KB flash and 2KB
RAM, the probe effect of any trace instrumentation is severe. Logging
a single TTA event (core ID, channel name, direction, sequence number)
requires at minimum 8-16 bytes of RAM per event. A circular buffer of
64 events consumes 0.5-1KB -- a significant fraction of available RAM.
The computational overhead of logging also affects timing, potentially
altering the communication pattern being observed. This is the modern
equivalent of the XS1's probe effect problem.

**Network-level observation.** Packet capture at the gateway provides
non-intrusive observation of all device communication passing through
the gateway. However, encrypted communication (TLS, DTLS) hides
message contents. The packet headers still reveal source/destination
addresses and timing.

**Edge platform telemetry.** AWS IoT Core, Azure IoT Hub, and Google
Cloud IoT Core provide metrics on message delivery, device connections,
and rule evaluations. These cloud-side metrics provide partial
observability of the OUT-to-IN path, including delivery latency and
failure counts.

### 5.5 Temporal Ordering

**No synchronized clocks.** IoT devices typically lack precision time
synchronization. A battery-powered sensor's RTC may drift by seconds
per day. NTP requires network access and computational resources that
constrained devices may not have.

**Broker timestamps.** The MQTT broker can timestamp each message on
arrival (publish) and delivery (to subscriber). These timestamps are
in the broker's clock domain, providing a consistent time reference for
ordering events that pass through the broker.

**Sequence numbers.** MQTT 5.0 does not provide built-in message
sequence numbers; however, the application can include them in the
payload or user properties. LoRaWAN's frame counter (FCnt) provides
per-device sequence numbering. CoAP's message IDs provide per-exchange
ordering.

**Gateway-mediated ordering.** If all communication passes through a
single gateway, the gateway's timestamp provides a total ordering of
events. This is analogous to the XS1's COMM Switch -- a central point
through which all communication flows, providing a natural ordering
point. Unlike the COMM Switch, the gateway can be instrumented to
record this ordering.

### 5.6 Practical Channel Naming Scheme

```
<network>/<device-or-gateway>/<protocol>/<topic-or-resource>

Examples:
  factory-floor/sensor-3a7f/mqtt/building/floor3/room301/temperature
  factory-floor/gateway-01/coap/coap://sensor1.local/temperature
  smartcity/device-0x1234ABCD/lorawan/uplink/port3
  home/thermostat-living/mqtt/home/living-room/hvac/setpoint
  field/gateway-07/mqttsn/topicid/42
```

For the TTA tuple:
- `core` = `<network>/<device-or-gateway>`
- `comm-sequence` = `<protocol>/<topic-or-resource>`
- `inst` = IN or OUT (publish=OUT, subscribe-receive=IN)


## 6. Deep Space Networks

This section addresses NASA's Deep Space Network (DSN), the Delay/
Disruption Tolerant Networking (DTN) architecture, Bundle Protocol, and
the unique challenges of light-speed communication delays.

### 6.1 Channel Identification

Deep space communication operates under constraints so extreme that
they redefine what "channel" means. One-way light time to Mars ranges
from 3 to 22 minutes. A round-trip communication takes 6 to 44
minutes. Communication windows may be limited to hours per day due to
orbital geometry. Links are asymmetric: downlink (spacecraft to Earth)
bandwidth may be megabits per second, while uplink (Earth to
spacecraft) may be kilobits per second.

**DSN link identification.** The Deep Space Network identifies
communication links by spacecraft ID (a numeric identifier, e.g.,
Voyager 1 = 31, Mars Reconnaissance Orbiter = 74), DSN antenna complex
(Goldstone, Canberra, Madrid), and antenna designation (e.g., DSS-14,
the 70-meter antenna at Goldstone). A communication pass is identified
by the tuple `(spacecraft-id, ground-station, antenna, time-window)`.

**CCSDS protocols.** The Consultative Committee for Space Data Systems
(CCSDS) defines the protocol stack for space communication. At the
lowest level, Telemetry (TM) and Telecommand (TC) frames are
identified by Spacecraft ID (SCID) and Virtual Channel ID (VCID).
Multiple virtual channels are multiplexed onto a single physical link,
each carrying different data types (science data, housekeeping
telemetry, commands). The channel identity for TTA is
`(SCID, VCID, direction)`.

**Bundle Protocol (BP).** DTN's Bundle Protocol identifies endpoints
by Endpoint IDs (EIDs), which are URIs. The `dtn` scheme uses a
node-specific syntax: `dtn://mars-rover-7/science-camera`. The `ipn`
scheme uses a numeric encoding: `ipn:14.3` (node 14, service 3). A
bundle is a self-contained message that carries its source EID,
destination EID, and creation timestamp. The channel for TTA is
`(source-EID, destination-EID)`.

**Proximity links.** Communication between co-located space assets
(e.g., a Mars rover communicating with an orbiter for relay) uses
proximity-1 protocols with link-local addressing. These links are
identified by the local and remote asset identifiers.

### 6.2 Channel Multiplexing

**Virtual channels.** CCSDS multiplexes multiple data streams onto a
single physical link using virtual channels (up to 64 per physical
channel). Each virtual channel has its own frame counter, providing
per-channel sequence numbering. This is a clean demultiplexing
mechanism for TTA: the VCID distinguishes channels.

**Store-and-forward.** DTN bundles may traverse multiple relay nodes
(e.g., surface rover -> orbiter -> DSN ground station -> mission
operations). Each relay stores the bundle and forwards it when the next
link is available. From TTA's perspective, each hop is a separate
channel. The bundle's EIDs identify the end-to-end channel, while the
intermediate hops are transport-level channels with their own
identities.

**Priority multiplexing.** Science data, engineering telemetry, and
commands share the same physical link but with different priorities.
Priority multiplexing does not create separate channels for TTA -- it
affects timing but not channel identity.

### 6.3 Channel Directionality

Deep space channels are strongly directional. Uplink and downlink are
physically separate: they use different frequencies, different
modulation schemes, and different data rates. The asymmetry is
extreme -- Mars Reconnaissance Orbiter's downlink is 6 Mbps while its
uplink is 2 kbps.

Commands (uplink) and telemetry (downlink) are naturally modeled as
two separate unidirectional channels for TTA. There is no
request-response pattern in the HTTP sense; commands are fire-and-
eventually-acknowledge, with acknowledgments returning minutes to hours
later.

### 6.4 Observability

Deep space communication is inherently and completely observable at
the ground station. Every bit transmitted and received is logged with
precise timestamps. The DSN provides:

- **Doppler tracking**: measures the spacecraft's radial velocity,
  providing precise timing information.
- **Range measurements**: provides round-trip light time, enabling
  clock synchronization between ground and spacecraft.
- **Frame accounting**: every TM frame received and TC frame
  transmitted is logged with Earth-receive time (ERT) and spacecraft
  event time (SCET).

The probe effect for ground-side observation is zero. On the
spacecraft, telemetry is recorded as part of normal operations;
the overhead is built into the mission design.

**The clock synchronization problem is effectively solved.** SCET is
reconstructed from ERT by subtracting the one-way light time (OWLT),
which is computed from the spacecraft's known trajectory (ephemeris).
The SCET accuracy is typically better than 1 millisecond for orbiters
with continuous tracking and better than 1 second for surface assets
with intermittent contact.

### 6.5 Temporal Ordering

Deep space communication has the most interesting temporal ordering
properties of any environment considered here.

**Known propagation delay.** Unlike terrestrial networks where
propagation delay is small and variable, deep space propagation delay
is large and precisely known (from ephemeris data). This known delay
can be directly incorporated into TTA's causality rules: an IN at the
ground station at time T corresponds to an OUT at the spacecraft at
time T - OWLT. The delay is not uncertainty; it is a physical constant
(modulo the small uncertainty in the ephemeris).

**Total ordering within a link.** Within a single uplink or downlink
session, frames are transmitted sequentially and received in order
(the speed of light does not reorder packets). Frame counters provide
explicit sequence numbering. There is a total order within each
virtual channel, which maps directly to TTA's per-core ordering
requirement.

**No global ordering across links.** Communication between independent
spacecraft (e.g., two Mars rovers) has no direct ordering relationship
unless mediated by a common relay. TTA's approach of considering all
possible orderings and pruning infeasible ones is well-suited to this
scenario.

**Causality cones.** The propagation delay creates a natural causality
cone (the spacetime light cone from special relativity). An event on
Mars at time T can only influence events on Earth at time T + OWLT or
later. TTA's causality rules can incorporate this physical constraint
to prune infeasible orderings that would require faster-than-light
information transfer.

This is a domain where TTA's approach of enumerating possible
orderings and pruning infeasible ones is not just useful but
physically meaningful. The set of feasible orderings corresponds
exactly to the set of orderings consistent with the causal structure
of spacetime.

### 6.6 Practical Channel Naming Scheme

```
<mission>/<asset>/<protocol>/<channel-detail>

Examples:
  msl/curiosity/ccsds/scid-76/vcid-1/downlink
  msl/curiosity/ccsds/scid-76/vcid-0/uplink
  mro/orbiter/dtn/dtn://mro.mars/science-relay
  dsn/goldstone/dss14/pass-2026-214-0800
  artemis/gateway/proximity1/rover-link-3
  mars2020/perseverance/bundle/ipn:168.3
```

For the TTA tuple:
- `core` = `<mission>/<asset>` (e.g., `msl/curiosity`, `dsn/goldstone`)
- `comm-sequence` = `<protocol>/<channel-detail>`
- `inst` = IN or OUT (transmit=OUT, receive=IN)

A notable property of this domain is that the `core` identity is
absolutely unambiguous: there is exactly one Curiosity rover, and
it is on Mars. There is no aliasing, no dynamic
provisioning, no load balancing. Channel identification in deep space
is solved by the extreme cost and planning that goes into space
missions.


## 7. Denied, Degraded, Intermittent, and Limited (DDIL) Environments

This section addresses military tactical networks, ad-hoc mesh
networks, submarine communications, and other environments
characterized by unreliable, constrained, or adversarial communication
conditions.

### 7.1 Channel Identification

DDIL environments share deep space's intermittency but add adversarial
conditions: the network may be actively disrupted (jamming, cyber
attack), and the communicating parties may need to conceal their
identities and communication patterns.

**Military tactical data links.** Link 16 (TADIL J) identifies
participants by Track Numbers (5-digit octal) and communication by
message type (J-series messages, e.g., J2.2 Air Track, J3.2 Land
Point). The channel identity is `(source-track-number,
message-type, network-id)`. Link 16 uses Time Division Multiple Access
(TDMA) with cryptographic network access, providing both channel
identification and temporal ordering (slot numbers).

**MANET (Mobile Ad-hoc Networks).** In mesh networks using protocols
like OLSR (Optimized Link State Routing) or AODV (Ad-hoc On-demand
Distance Vector), nodes are identified by IP addresses or MAC
addresses, but routes change dynamically. A channel between two nodes
may traverse different intermediate hops at different times. The
channel identity at the application level is `(source-node,
destination-node, port)`; the physical path is variable and may not
be observable to any single node.

**DTN in tactical environments.** The same Bundle Protocol used in
deep space is also applicable to tactical networks. Bundles are
store-and-forward, tolerating disruption. EIDs identify endpoints.
The difference from deep space is that propagation delays are short
(milliseconds to seconds), but disruption is unpredictable and
adversarial rather than orbital.

**Submarine communications.** Submarines communicate via Extremely Low
Frequency (ELF, 3-30 Hz) or Very Low Frequency (VLF, 3-30 kHz) radio
for shore-to-submarine broadcasts, and via satellite (when surfaced or
at periscope depth) for bidirectional communication. ELF/VLF is
receive-only for the submarine (the antenna required to transmit at
these frequencies is enormous). The channel is
`(shore-station, frequency-band)` for ELF/VLF downlink, and
`(submarine-id, satellite-link)` for satellite uplink.

**HF radio.** High Frequency (3-30 MHz) radio provides beyond-
line-of-sight communication via ionospheric reflection. Channels are
identified by frequency and call sign. HF links are notoriously
unreliable: signal quality varies with time of day, solar activity,
and atmospheric conditions. Automatic Link Establishment (ALE)
protocols manage link setup and teardown, with link identity carried
in the ALE handshake.

### 7.2 Channel Multiplexing

**TDMA multiplexing.** Link 16 divides time into slots (each 7.8125
ms). Participants are assigned time slots for transmission.
Multiplexing is by time slot, and the slot assignment is the channel
identifier at the physical level. Multiple message types share a
participant's time slots.

**Frequency hopping.** Many military radios use frequency-hopping
spread spectrum (FHSS), changing frequency according to a
pseudo-random sequence shared by all participants. The "channel" is the
hopping sequence, identified by a crypto key or net ID. Individual
frequencies are not meaningful channel identifiers; the net ID is.

**Mesh routing.** In a mesh network, a message from A to D may
traverse A->B->D or A->C->D depending on link conditions. The
intermediate nodes are multiplexing/demultiplexing points, analogous
to MQTT brokers or DTN relays. For TTA, the end-to-end channel
(A to D) is the relevant abstraction; the intermediate hops affect
latency but not channel identity.

### 7.3 Channel Directionality

DDIL channels are often asymmetric in capability.

**One-way links.** ELF/VLF submarine communication is inherently
one-way (shore to submarine). The submarine cannot respond on the same
channel; it must surface (or reach periscope depth) to use a satellite
link. This creates a pair of strongly asymmetric channels: high-
availability, low-bandwidth downlink and low-availability, higher-
bandwidth uplink.

**Asymmetric reliability.** In a jammed environment, one direction may
be more disrupted than the other (e.g., a jammer near one party
affects that party's reception more than its transmission). This
creates asymmetric channel reliability that TTA must account for:
OUTs that never result in INs.

**Broadcast vs. unicast.** Link 16 messages can be broadcast to all
participants in a net or directed to specific participants. Broadcast
messages create 1:N channels. Directed messages create 1:1 channels.
The message's Participating Group (PG) field determines the audience.

### 7.4 Observability

Observability in DDIL environments is severely constrained and often
adversarial.

**OPSEC constraints.** In military operations, trace data is itself
sensitive. Recording communication patterns reveals operational tempo,
force composition, and intent. TTA trace data must be classified and
protected at the same level as the communications it records.

**Node-local tracing.** Each node can trace its own communication
events. Collecting traces from multiple nodes requires a rendezvous
mechanism (which may itself be disrupted). Unlike cloud environments
where a centralized trace collector is always available, DDIL trace
collection may be deferred until the operation concludes and all
nodes can upload their traces.

**Post-hoc analysis.** The natural use case for TTA in DDIL
environments is post-mission analysis: each node records its local
trace, traces are collected after the mission, and TTA reconstructs
the possible communication orderings. This is exactly the post-mortem
debugging scenario TTA was designed for, scaled to a larger and more
challenging environment.

**Electronic warfare.** If the adversary is actively monitoring or
disrupting communications, the TTA trace may include events caused
by adversary action (jamming, spoofing, replay attacks). These
appear as anomalous communication patterns: INs without corresponding
OUTs (replayed messages), OUTs without corresponding INs (jammed
messages), or INs that do not match any legitimate OUT (spoofed
messages). TTA's feasibility pruning would flag these as infeasible
orderings, potentially revealing adversary activity.

### 7.5 Temporal Ordering

**GPS time.** When available, GPS provides nanosecond-accuracy time
synchronization across all nodes. However, GPS may be denied (jammed
or spoofed) in contested environments.

**Link 16 time.** Link 16 requires precise time synchronization among
all participants (within 2 microseconds). TDMA slot boundaries
provide temporal ordering at 7.8125 ms granularity. This is an
excellent time source for TTA when available.

**Crystal oscillator drift.** Without GPS or network time, nodes
maintain time using their local oscillators. Military-grade oscillators
(e.g., oven-controlled crystal oscillators, OCXOs) maintain accuracy
of approximately 1 ppm, drifting roughly 1 second per 11.5 days. For
short missions (hours), this provides sub-millisecond accuracy. For
extended operations without resynchronization, drift accumulates and
temporal ordering becomes uncertain.

**Logical clocks.** In the absence of reliable physical clocks,
Lamport clocks or vector clocks embedded in messages provide causal
ordering. DTN's Bundle Protocol includes a creation timestamp, but
its accuracy depends on the source node's clock. Adding a Lamport
counter to each bundle would provide reliable causal ordering without
depending on clock synchronization.

### 7.6 Practical Channel Naming Scheme

```
<network>/<node>/<protocol>/<link-detail>

Examples:
  tadil-j/net-3/link16/track-01234/j2.2
  mesh/unit-alpha/manet/10.0.1.5:4567
  tactical/fob-bravo/dtn/dtn://fob-bravo.tac/logistics
  subsurface/ssn-785/vlf/naa-cutler/broadcast
  subsurface/ssn-785/satcom/muos/channel-3
  hf-net/callsign-7AB/ale/freq-group-4
```

For the TTA tuple:
- `core` = `<network>/<node>` (e.g., `tadil-j/track-01234`,
  `mesh/unit-alpha`)
- `comm-sequence` = `<protocol>/<link-detail>`
- `inst` = IN or OUT (transmit=OUT, receive=IN)


## 8. Mobile Phones

This section addresses Android and iOS IPC mechanisms, cellular
communication, push notifications, and short-range wireless protocols
(Bluetooth, NFC).

### 8.1 Channel Identification

Mobile platforms combine multiple IPC mechanisms within the device,
between devices, and between devices and cloud services.

**Android IPC.** Android provides several IPC mechanisms, each with
its own identification:

- **Intents**: Identified by action string (e.g.,
  `android.intent.action.SEND`), category, data URI, and component
  name. An explicit intent specifies the target component; an implicit
  intent is resolved by the system based on intent filters. For TTA,
  the explicit intent's component name is the channel identifier. For
  implicit intents, the resolved component (which may vary at runtime)
  is the actual channel.

- **Binder**: Android's primary IPC mechanism. A Binder interface is
  identified by its descriptor string (e.g.,
  `android.app.IActivityManager`) and the Binder token (an opaque
  handle). Binder transactions carry a transaction code (integer)
  identifying the method. The channel identity is
  `(service-descriptor, transaction-code)`. Binder transactions are
  synchronous (the caller blocks until the callee returns), making
  them closely analogous to CSP channel operations.

- **ContentProviders**: Identified by authority URI (e.g.,
  `content://com.example.provider/table`). CRUD operations on a
  content provider are request-response, with the URI as the channel
  identifier.

- **BroadcastReceivers**: Identified by the broadcast Intent's action
  string. Broadcasts can be ordered (delivered sequentially to
  receivers by priority) or unordered (delivered concurrently). An
  ordered broadcast creates a pipeline of IN/OUT events; an unordered
  broadcast creates a 1:N fan-out.

- **AIDL services**: Android Interface Definition Language generates
  Binder stubs. The AIDL interface name is the channel identifier;
  each method is a sub-channel.

**iOS IPC.** iOS is more restrictive about inter-app IPC, but provides:

- **XPC**: macOS's inter-process communication, used in iOS for
  communication between an app and its extensions. Identified by
  service name (a reverse-DNS string, e.g.,
  `com.example.app.extension`). XPC connections are
  bidirectional with reply handlers.

- **URL Schemes / Universal Links**: Inter-app communication via URL
  (e.g., `myapp://action?param=value`). The URL scheme identifies the
  target app; the path and query identify the operation. This is a
  fire-and-forget OUT from the source app and an IN at the target app.

- **App Groups / Shared Containers**: Shared UserDefaults or files in
  a shared container (identified by group ID, e.g.,
  `group.com.example.shared`) enable IPC through shared state. This is
  shared-memory communication, not message-passing, and does not map
  cleanly to TTA's IN/OUT model without additional abstraction.

- **NotificationCenter**: In-process notification broadcast.
  Identified by notification name (e.g., `NSNotification.Name
  .UIApplicationDidBecomeActive`). 1:N fan-out within a process.

**Push notifications.** Apple Push Notification Service (APNs) and
Firebase Cloud Messaging (FCM) identify channels by device token (APNs)
or registration token (FCM), combined with the notification topic or
collapse key. A push notification is an OUT from the server and an
IN at the device. The channel identity is
`(server-app-id, device-token, topic)`.

### 8.2 Channel Multiplexing

**Binder multiplexing.** Android's Binder driver multiplexes all IPC
for a process through a single `/dev/binder` file descriptor. The
Binder driver demultiplexes based on the target Binder node. A single
process may simultaneously communicate with dozens of system services
via Binder, all multiplexed through the same driver. For TTA, the
Binder target and transaction code demultiplex the logical channel.

**Network multiplexing.** A mobile app typically maintains multiple
network connections: a persistent WebSocket for real-time updates, HTTP
connections for API calls, push notification channels, and possibly
peer-to-peer connections (Bluetooth, Wi-Fi Direct). Each is a separate
TTA channel, multiplexed through the device's network stack.

**Intent multiplexing.** An Android Activity or Service may receive
Intents from multiple sources. The Intent's action, data, and extras
distinguish the channel, but the receiving component is the
multiplexing point.

### 8.3 Channel Directionality

**Binder.** Binder transactions are request-response: the client sends
a transaction (OUT), the server receives it (IN), processes it, and
sends a reply (OUT), which the client receives (IN). The two-way
transaction flag (`FLAG_ONEWAY = 0` for synchronous, `FLAG_ONEWAY = 1`
for asynchronous fire-and-forget) determines whether the client blocks.
Synchronous Binder transactions are exactly CSP channels.
Asynchronous Binder transactions (one-way) are fire-and-forget OUTs.

**Push notifications.** Unidirectional: server to device. The device
cannot "reply" via the push channel; any response uses a separate
HTTP channel.

**Bluetooth.** Bluetooth Classic (RFCOMM) provides bidirectional
byte streams, like TCP sockets. BLE (Bluetooth Low Energy) uses a
client-server model with characteristics: the client can read, write,
or subscribe to notifications from characteristics on the server. A
BLE characteristic is identified by its UUID (e.g.,
`00002a37-0000-1000-8000-00805f9b34fb` for Heart Rate Measurement).
Notifications create a server-to-client unidirectional channel; writes
create a client-to-server channel.

**NFC.** Near Field Communication is inherently bidirectional but
short-lived: the channel exists only while the devices are in physical
proximity (< 4 cm). NFC-A/B identifies the channel by the tag's UID
or the handover record. For TTA, an NFC exchange is a brief channel
with a single OUT (the initiator's data) and a single IN (the target's
response or data read).

### 8.4 Observability

**Android.** Comprehensive tracing infrastructure exists:

- **Systrace / Perfetto**: Kernel-level tracing including Binder
  transactions. Perfetto's trace processor can extract Binder
  transaction events with client PID, server PID, interface name,
  transaction code, duration, and timestamps. This provides exactly
  the information TTA needs for Binder channels.

- **`adb logcat`**: Application-level logging of Intent delivery,
  BroadcastReceiver invocations, and ContentProvider operations.

- **Network profiler**: Android Studio's network profiler records all
  HTTP(S) requests with timing.

- **Bluetooth HCI snoop log**: Records all Bluetooth Host Controller
  Interface transactions, providing complete Bluetooth channel
  observability.

**iOS.** More restricted but still available:

- **Instruments / os_signpost**: Apple's profiling tools can trace XPC
  transactions, network requests, and Bluetooth operations.

- **sysdiagnose**: Comprehensive system diagnostic dump including IPC
  logs.

- **PacketLogger**: Apple's Bluetooth packet analysis tool.

**Probe effect on mobile.** Mobile devices are battery-powered, and
trace instrumentation increases CPU usage, which increases power
consumption, which may alter behavior (e.g., the OS may throttle
operations under battery pressure). Perfetto is designed for low-overhead
tracing (< 1% CPU), but extensive tracing can still affect battery
life and thermal behavior.

### 8.5 Temporal Ordering

**Per-device ordering.** Within a single mobile device, Perfetto
provides monotonic nanosecond timestamps across all cores and
processes. This gives a total order of events on the device, which is
stronger than TTA requires (TTA only needs per-core ordering).

**Cross-device ordering.** Communication between mobile devices
(Bluetooth, Wi-Fi Direct, cloud-mediated) faces the same clock
synchronization challenges as distributed systems. NTP over cellular
provides millisecond-scale synchronization. For Bluetooth, the
piconet clock provides local synchronization between connected
devices, but this does not extend to non-piconet interactions.

**Push notification ordering.** The time between server OUT
(notification sent to APNs/FCM) and device IN (notification delivered)
is variable and opaque. APNs and FCM do not guarantee delivery
ordering: notifications sent in order A, B may arrive in order B, A.
There is no propagation delay that can be subtracted (unlike deep
space). For TTA, push notification channels have weak ordering
guarantees, increasing the feasible path set.

### 8.6 Practical Channel Naming Scheme

```
<device>/<process>/<mechanism>/<endpoint>

Examples:
  pixel-7a/com.example.app/binder/android.app.IActivityManager/startActivity
  pixel-7a/com.example.app/intent/action:android.intent.action.SEND
  pixel-7a/com.example.app/fcm/topic:news-updates
  iphone-14/com.example.app/xpc/com.example.app.notification-ext
  pixel-7a/com.example.app/ble/00002a37-0000-1000-8000-00805f9b34fb/notify
  pixel-7a/com.example.app/nfc/tag-uid:04A23B7C
```

For the TTA tuple:
- `core` = `<device>/<process>` (or `<device>/<process>/<thread>` for
  intra-process analysis)
- `comm-sequence` = `<mechanism>/<endpoint>`
- `inst` = IN or OUT (send/write/publish/notify-send = OUT;
  receive/read/subscribe-callback/notify-receive = IN)


## 9. Cross-Cutting Analysis

### 9.1 The Channel Identification Spectrum

The seven environments arrange themselves along a spectrum of
channel identification difficulty:

1. **Deep space** (easiest): Channels are completely static, planned
   years in advance, and identified by internationally-registered
   spacecraft IDs. Every bit is logged. This is the ideal case for
   TTA.

2. **Modern embedded (static config)**: AUTOSAR and ARINC 653 systems
   have statically-configured channels known at build time. RTOS
   queue handles provide runtime identifiers.

3. **DDIL environments**: Military data links have defined message
   formats with explicit channel identification. The challenge is
   collection, not identification.

4. **Mobile phones**: Platform IPC mechanisms (Binder, XPC) have
   well-defined identification. Platform tracing tools (Perfetto,
   Instruments) provide good observability.

5. **Desktop systems**: Rich IPC mechanisms with varied identification
   schemes. Kernel-level tracing (eBPF) provides comprehensive
   observability, but correlating across mechanisms is challenging.

6. **Edge computing**: Channel identification through MQTT topics or
   CoAP resources is straightforward, but device-level observability
   is constrained by resource limitations.

7. **Cloud computing** (hardest): Dynamic channel creation,
   multiplexing through intermediaries, ephemeral instances, and
   multi-layer network abstraction make channel identification
   context-dependent and challenging.

### 9.2 Synchronous vs. Asynchronous Channels

The original TTA's causality rules exploit synchronous, blocking
communication. When TTA is applied to asynchronous channels, the
rules weaken:

- **Rule 1** ("INs are always preceded by an OUT") holds for all
  channels, synchronous or asynchronous.
- **Rule 2** ("An IN blocks communication on both execution threads
  until data is received") fails for asynchronous channels. A
  producer publishing to Kafka or MQTT does not block until the
  consumer reads the message. This means OUTs on the producing core
  can continue without waiting for INs on the consuming core,
  expanding the set of feasible orderings.
- **Rule 3** ("All nodes must be represented in a trace") holds
  regardless of synchrony.

For asynchronous channels, additional pruning criteria are needed:
- **Timestamp-based pruning**: If wall-clock timestamps are available
  (even approximately), candidate orderings where an IN precedes its
  OUT in wall time can be eliminated.
- **Sequence-number pruning**: If messages carry sequence numbers
  (Kafka offsets, CCSDS frame counters, LoRaWAN FCnt), candidate
  orderings that violate sequence order within a channel can be
  eliminated.
- **Causal-context pruning**: If messages carry causal context
  (OTel trace IDs, W3C traceparent), candidate orderings that violate
  the causal chain can be eliminated.

### 9.3 The Probe Effect Across Environments

The probe effect varies dramatically across environments:

| Environment | Typical Probe Effect | Impact |
|---|---|---|
| Deep space | Zero (ground-side) | None |
| AUTOSAR/ARINC 653 | Built into design | Accounted for in WCET |
| RTOS (Tracealyzer) | 1-5 us per event | Significant for us-scale deadlines |
| Desktop (eBPF) | 100-500 ns per event | Negligible |
| Desktop (strace) | 10-100x slowdown | Severe; alters behavior |
| Cloud (OTel) | ~1 ms per span | Comparable to service latency |
| Edge (device-side) | Major (RAM/CPU) | May alter communication patterns |
| Edge (broker-side) | Minimal | Practical approach |
| DDIL (node-local) | Moderate | Acceptable for post-mission analysis |
| Mobile (Perfetto) | < 1% CPU | Acceptable |

The pattern is clear: non-intrusive observation (external analyzers,
broker-side logging, ground-station recording) provides the best
trade-off between observability and probe effect. Where this is not
possible, kernel-level tracing (eBPF, Perfetto) offers low overhead.
Application-level instrumentation (OTel, strace) has higher overhead
but provides richer semantic information.

### 9.4 What the Original XS1 Got Right

The original XS1 implementation, despite its channel identification
limitations, had several properties that are worth preserving across
environments:

1. **The trace was bounded.** START and STOP markers defined a finite
   region of interest. This is essential: TTA's all-paths algorithm has
   factorial complexity in the number of nodes, so unbounded traces are
   computationally infeasible. Every environment needs a bounding
   mechanism.

2. **Per-core ordering was guaranteed.** The simulator recorded
   instructions in execution order. This is the minimum requirement
   for TTA, and it is available in all seven environments (within a
   core/process/node).

3. **Communication was synchronous.** This gave the strongest possible
   causality constraints. While most modern environments use
   asynchronous communication, the insight remains: the more
   constrained the communication model, the more TTA can prune, and
   the more useful the results.

4. **The algorithm was independent of channel naming.** TTA needs
   channel identifiers to partition events, but the algorithm itself
   is agnostic to the naming scheme. This means TTA can be applied to
   any environment that provides identifiable channels, regardless of
   how different the naming scheme is from the original resource-ID
   approach.

### 9.5 Toward a Universal TTA Tuple

The original TTA tuple `(core, comm-sequence, inst)` can be
generalized across all seven environments as:

```
(execution-context, channel-identifier, direction)
```

where:

- **execution-context** is a hierarchical identifier for the thread
  of execution: a CPU core, an OS process, a container, a
  microservice instance, a spacecraft, a tactical radio node. It must
  uniquely identify a sequentially-executing entity.

- **channel-identifier** is a hierarchical identifier for the
  communication path: a hardware register, a file descriptor, a
  message queue name, an MQTT topic, a CCSDS virtual channel, a
  Binder interface. It must uniquely identify a communication path
  such that all INs and OUTs on the same channel are part of the same
  communication sequence.

- **direction** is IN or OUT, derived from the API or protocol
  operation observed. Some mechanisms make this obvious (pipe read vs.
  write); others require interpretation (a Binder transaction is an
  OUT from the client's perspective and an IN from the server's).

The universality of this tuple is the key finding of this analysis.
TTA does not need to know what a channel "is" in hardware or
software terms. It needs only to know that events labeled with the
same channel identifier are part of the same communication sequence,
and that events within a single execution context are totally
ordered. Given these two properties, the algorithm works unchanged.

The channel identification problem that bedeviled the original XS1
implementation was not inherent to TTA. It was a consequence of the
XS1's particular hardware design, where the COMM Switch's internal
state was not exposed and the compiler established channels before the
trace region began. In every other environment examined here, at least
one practical mechanism exists for identifying channels at trace time.
The original problem was the exception, not the rule.


## 10. Recommendations for a Follow-On Implementation

Based on this analysis, a follow-on TTA implementation should:

1. **Define a pluggable channel identification layer.** The TTA core
   algorithm should accept `(execution-context, channel-identifier,
   direction)` tuples without imposing a specific naming scheme.
   Environment-specific adapters should translate from each
   environment's native identifiers to TTA tuples.

2. **Support asynchronous channels.** The causality rules should be
   parameterized: the original synchronous rules should be available
   for environments that support them (Binder, CSP channels), and
   weakened rules should be available for asynchronous channels (MQTT,
   Kafka, push notifications).

3. **Incorporate external ordering information.** Timestamps, sequence
   numbers, and causal context (trace IDs) should be additional inputs
   to the pruning phase, not just IN/OUT events. This enables tighter
   pruning on asynchronous channels.

4. **Support hierarchical execution contexts.** The notion of "core"
   should be hierarchical: a machine contains processes, which contain
   threads. A service mesh contains services, which contain instances,
   which contain request-handling threads. The algorithm should be
   applicable at any level of this hierarchy.

5. **Support 1:N channels.** Broadcast, pub/sub, and fan-out
   communication should be modeled as one OUT linked to N INs. The
   all-paths algorithm generalizes to this case, but the
   implementation must handle the combinatorial expansion.

6. **Bound the trace.** Every environment needs a START/STOP mechanism.
   In cloud environments, this might be a time window. In embedded
   systems, it might be a trigger condition. In DDIL, it might be a
   mission phase. The mechanism varies, but the need is universal.

7. **Minimize probe effect.** Use non-intrusive observation wherever
   possible: broker-side logging for MQTT, sidecar proxies for
   microservices, external bus analyzers for CAN, ground station logs
   for deep space. Fall back to kernel-level tracing (eBPF, Perfetto,
   CoreSight) when non-intrusive options are unavailable. Resort to
   application-level instrumentation only when necessary.

8. **Validate against the desktop tracer.** The existing desktop
   tracer (`communicator.lisp`) provides a controlled environment
   where channel identification is solved and communication is
   synchronous. This should be the validation platform for the
   generalized TTA implementation before applying it to more
   challenging environments.
