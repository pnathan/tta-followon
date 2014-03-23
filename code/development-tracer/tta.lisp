;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tta.lisp
;;;; implements the tta algorithm in a generic fashion.
;;;; derived from Paul Nathan's MS report (<title here>)
;;;;
;;;; A key simplification in this code is that I assume any
;;;; professional monitoring solution tells me what core it executed
;;;; on, along with which communication sequence a communication
;;;; belonged to.

;;;; The algorithm description:
;;;;
;;;; Problem- given a list of data with the following tuple (core,
;;;; comm-sequence, inst), where core denotes the
;;;; cpu/core/thread/host, comm-sequence is a unique identifier
;;;; denoting the exchange of information, and inst is either IN or
;;;; OUT, determine the correct order of events. Comm sequences are
;;;; presumed to be unorderable. The PRESUMPTION is made that
;;;; communications are synchronous: each IN is tied to one or more
;;;; OUTs, and the IN does not move on until the communication
;;;; finishes.
;;;;
;;;; An example of this problem is logfiles on multiple computers
;;;; recording information. They record the sequence of messages and
;;;; what program performed the messages. But it is not
;;;; straightforward to understand if messages crossed in flight and
;;;; what information influenced which future actions. "Who talked to
;;;; who, when" is the question.
;;;;
;;;; The TTA algorithm takes in a sequence of the above tuples and
;;;; partitions them by communication channel and then by core.  Each
;;;; core has a list of IN, OUT nodes in sequence (it is assumed that
;;;; this sequence is what really happened). Each IN node is mapped to
;;;; a list of possible OUT nodes: the next OUT node on the current
;;;; core, and all OUT nodes that occur on the other cores (within the
;;;; communication sequence). This forms a graph (possibly with
;;;; multiple disconnected groups) of possible communications
;;;; histories.
;;;;
;;;; Next, without respecting sequences, a synthetic top and bottom
;;;; node are joined to the graph at the start and end of the sequence
;;;; for each core. Remark: this now forms a lattice. This graph then
;;;; has ALL-PATHS executed on it from top to bottom. ALL-PATHS is
;;;; defined at the routine that takes a graph G, a start vertex S, an
;;;; end vertex E, and returns a set of graphs P, where each graph in
;;;; P is a possible graph of hwo the communication occured.
;;;;
;;;;
;;;; Remark. When a system of synchronous communication is in place
;;;; with only 1:1 comms, we can then speak of linearization of
;;;; communication in this event. However, in the event where we have
;;;; 1:n communication, we can not speak of linearization. Ergp, there
;;;; is no "path", but actually a multipath.Each n paths out
;;;; n-furcates the worldspace of possibility.
;;;;
;;;;
;;;; The system is architected in three phases: read into the internal
;;;; graph format, execute the TTA algorithm on the internal graph,
;;;; render out the final verdict of what is possible.
;;;;


(defclass comm-node ()
  ((core
    :accessor core
    :documentation "The core/thread/cpu this node was executed on")
   (operation
    :accessor operation
    :documentation ":in or :out")
   (out-data
    :documentation "If :out, the data. Nil for no data recorded or
    operation = :in.")

   ;; TBD: if this is required
   (sequence-identifier
    :accessor sequence-identifier
    :documentation "The monotonically increasing number of the
    sequence on this particular core.")

   (u-id
    :accessor u-id
    :documentation "Unique ID of the node")
   (next-node-in-core
    :accessor next-node-in-core
    :documentation "The next node in the core's
    execution.

Precondition: (< (sequence-identifier self) (sequence-identifier
    next-node-in-core)")
   (next-nodes-in-flow
    :accessor next-nodes-in-flow
    :documentation "This node has multiple possibilities of what
    'next' could be; these are represented by next-nodes-in-flow")))

(defclass start-node (comm-node)
  ((core :initform :start))
  (:documentation
   "Start node for all operations; TOP in the lattice of
  communication. Set its list of next-nodes-in-flow to the start of
  each core's execution list."))

(defclass final-node (comm-node)
  ((core :initform :final))
  (:documentation
   "Final node for all operations; BOTTOM in the lattice of
  communication."))

(defclass run ()
  ((start-node
    :accessor start-node)
   (final
    :accessor final-node)))

(defmethod add-core-sequence-to-run ((node comm-node) (run run))
  ;; node must be the starting point in the sequence
  (push node (next-nodes-in-flow (start-node run))))

(defmethod is-incoming ((node comm-node))
  (eq (operation node) :in))

(defmethod is-outgoing ((node comm-node))
  (eq (operation node) :out))
