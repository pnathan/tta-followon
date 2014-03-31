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
;;;; P is a possible graph of how the communication occured.
;;;;
;;;; Remark. When a system of synchronous communication is in place
;;;; with only 1:1 comms, we can then speak of linearization of
;;;; communication in this event. However, in the event where we have
;;;; 1:n communication, we can not speak of linearization. Ergp, there
;;;; is no "path", but actually a multipath.Each n paths out
;;;; n-furcates the worldspace of possibility.

;;;; Each graph g in P is then evaluated according to a condition P. P
;;;; fulfils the following conditions: P : g -> {True | False}. P is
;;;; actually described by P := p[0] * p[1] ... * p[n-1].  P defines
;;;; the conditions for a path to actually have existed in the program
;;;; execution.Each
;;;;
;;;; p[0] each node in g must be exist in G.
 ;;;;
;;;; The system is architected in three phases: read into the internal
;;;; graph format, execute the TTA algorithm on the internal graph,
;;;; render out the final verdict of what is possible.
;;;;

(ql:quickload :alexandria)

(define-condition unimplemented (error)
  ((message :initarg :message
            :reader message
            :initform nil)))

(defmethod print-object ((object unimplemented) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if (message object)
        (format stream "~a" (message object))
        "")))

(defun unimplemented (&optional message)
  (error 'unimplemented :message message))

(let ((counter 1000)
      (sym "SYM"))
 (defun genkeyword ()
   (incf counter)
   (intern (format nil "~a-~a" sym counter) :keyword)))

(defclass comm-node ()
  ((core
    :initform :UNBOUND
    :initarg :core
    :accessor core
    :documentation "The core/thread/cpu this node was executed on")
   (operation
    :initform :UNBOUND
    :initarg :operation
    :accessor operation
    :documentation ":in or :out")

   ;; TBD: remove?
   (out-data
    :initform nil
    :accessor out-data
    :documentation "If :out, the data. Nil for no data recorded or
    operation = :in.")

   (sequence-identifier
    :accessor sequence-identifier
    :initform :unbound
    :initarg :sequence-identifier
    :documentation "The monotonically increasing number of the
    sequence on this particular core.")

   (u-id
    :reader u-id
    :initform (genkeyword)
    :documentation "Unique ID of the node")

   (next-node-in-core
    :initform :unbound
    :initarg  :next-node-in-core
    :accessor next-node-in-core
    :documentation "The next node in the core's
    execution.

Precondition: (< (sequence-identifier self) (sequence-identifier
    next-node-in-core)")
   (next-nodes-in-flow
    :initform nil
    :initarg :next-nodes-in-flow
    :accessor next-nodes-in-flow
    :documentation "This node has multiple possibilities of what
    'next' could be; these are represented by next-nodes-in-flow")))

(defmethod print-object ((object comm-node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a core ~a seq - ~a op ~a"
            (u-id object)
            (core object)
            (sequence-identifier object)
            (operation object)
            )))

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
  ((start
    :initarg :start
    :initform nil
    :accessor start
    :documentation "TOP of the lattice")
   (final
    :initarg :final
    :initform nil
    :accessor final
    :documentation "BOTTOM of the lattice")))

(defmethod last-ins-of ((run run))
  "The final INs conclusively define what happened last"
  (unimplemented))

(defmethod add-core-sequence-to-run ((node comm-node) (run run))
  ;; node must be the starting point in the sequence
  (push node (next-nodes-in-flow (start-node run))))

(defmethod is-incoming ((node comm-node))
  (eq (operation node) :in))

(defmethod is-outgoing ((node comm-node))
  (eq (operation node) :out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-ifc-file (filename)
  (read-from-string
   (alexandria:read-file-into-string filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-ifc-file (filename)
  "Loads a file in the standard TTA trace format"
  (read-ifc-file filename))

(defparameter *read-in-data* nil )

(defun link-up-nodes-in-core (core)
  "Links the nodes in core (presuming core is a vectors of nodes) to
each other. Idempotent on core. Note that the last node in core will
not have a `next node`"
  (loop for i from 1 below (length core)
     do
       (setf (next-node-in-core (aref core (1- i))) (aref core i)))
  t)

(defun link-up-nodes-in-channels (nodes)
  ;; hook up each IN with its corresponding OUT. Somehow.
  )

(defun find-channels-in (nodes)
  "Explodes the list of node vectors (from each core) into channels,
returns a list of channels. The underlying node is referenced, not
copied.

Algorithmic complexity is O(total-number-of-nodes).


"
  (let ((number-of-sequences))

    ))

(defun convert-from-read-to-nodes (read-in-data)
  (flet ((event-core (event)
           (cdar event))
         (event-op (event)
           (cdr (assoc :type (cdr event))))
         (event-seq (event)
           (cdr (assoc :sequence-id (cdr event)))))
    (let ((start-node (make-instance 'start-node))
          (run (make-instance 'run))
          (nodes
           (loop for core in read-in-data
               collect
                 (let ((core-id (car core)))
                   (concatenate
                    'vector
                    (loop for event in (cdr core)
                       collect
                         (make-instance 'comm-node
                                        :sequence-identifier (event-seq event)
                                        :core core-id
                                        :operation (event-op event))))))))
      ;; remark - at this point, we have the graph as a set of
      ;; disconnected nodes (they don't know anything about each
      ;; other). The next step is to link the nodes via the
      ;; next-node-in-core link.

      (loop for core in nodes
         do
         (link-up-nodes-in-core core))

      ;; Form links between ins and outs.
      (loop for channel in (find-channels-in nodes)
         do
           (link-up-nodes-in-channels channel))

      (setf (next-nodes-in-flow start-node)
            nodes)
      (setf (start run) start-node)

      run)))
