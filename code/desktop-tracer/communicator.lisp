;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paul Nathan 2012
;;; Code developed as part of Master's Thesis at U of Idaho
;;; Communicator.lisp
;;;
;;; A message passing paradigm with instrumented communication nodes
;;; and start/stop nodes according to the TTA algorithm.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  for simplicity's sake, we do not implement a multi-process Lisp
;;  system, we use threads.
;; For further simplicity's sake, and for modelling correctly, (and
;; correctness verification), we implement our own channels instead of
;; using the CALISPEL library, which implements CSP around a GIL.


(ql:quickload :bordeaux-threads)        ;standard thread lib for cl
(asdf:load-system :batteries)

;; this adds the sweet BIND macro
(ql:quickload :metabang-bind)
(use-package :metabang-bind)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions

;;; Logging code.

;; A thread-safe queue.
(defclass safe-queue ()
  ((name :accessor name :initarg :name)
   (data :accessor data :initform nil)
   (lock :accessor lock :initform nil)))

(defun make-safe-queue (name)
  (make-instance 'safe-queue :name name))

(defmethod initialize-instance :after ((queue safe-queue) &key)
  (setf (lock queue)
        (bordeaux-threads:make-lock
         (format nil "safe-queue-lock: ~a" (name queue)))))

(defmethod push-queue ((obj safe-queue) data)
  (bordeaux-threads:with-lock-held ((lock obj))
    (setf (data obj)
          (append (data obj) (list (ensure-list data))))))

(defmethod retrieve ((obj safe-queue))
  (bordeaux-threads:with-lock-held ((lock obj))
    (data obj)))

(defmethod print-object ((obj safe-queue) stream)
  (print-unreadable-object (obj stream :identity t)
    (bordeaux-threads:with-lock-held ((lock obj))
      (loop for ele in (data obj) do
           (format stream "~a~&" ele)))))

;;; Utilities
(defun ensure-list (x)
  (if (consp x)
      x
      (list x)))

;;; Hash table printing.
(defmethod print-object ((obj hash-table) stream)
  (if (= (hash-table-count obj) 0)
      (format stream "{}")
      (maphash
       #'(lambda (k v)
           (format stream "~a => ~a~&" k v))
       obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Our specified thread of execution is a core. This represents any
;; line of execution: a processor, a core, a process, a thread, etc.

(defparameter *core-id* nil "This is bound to the core's id on creation")

(defclass core ()
  ;; the string denoting the core
  ((name :accessor name
        :initform nil
        :initarg :name)
   ;; the function to start execution
   (entrypoint :accessor entrypoint
               :initform nil
               :initarg :entrypoint)
   ;; the args that the entrypoint needs to begin
   (args :accessor args
         :initform nil
         :initarg arglist)

   ;; program counter for communication
   (counter :accessor counter
            :initform 0)
   (counter-lock :accessor counter-lock
                 :initform nil)

   ;; funcallable that cleans up the core.
   (cleanup :accessor cleanup
            :initform nil
            :initarg cleanup)))

(defmethod initialize-instance :after ((core core) &key)
  (setf (counter-lock core)
        (bordeaux-threads:make-lock (name core))))


(defmethod print-object ((core core) stream)
  (print-unreadable-object (core stream :identity t)
    (format stream "core: ~a" (name core))))

(defclass recording-core (core)
  ((recording-p :accessor recording-p :initform nil)
   (storage-q :accessor storage-q :initform nil)))

(defmethod initialize-instance :after ((core recording-core) &key)
  (setf (storage-q core) (make-safe-queue (name core)))
  (setf (cleanup core) #'historian))


(defmethod recording-p ((core core))
  "A regular core is never recording"
  nil)

;; needs to be called after each communcation
(defmethod counter-tick ((core core))
  (bordeaux-threads:with-lock-held ((counter-lock core))
    (incf (counter core))))



;;;;
;;; historian/recording focused code.

(defparameter *history* (make-hash-table :test #'equal)
  "The history as a hash table mapping cores to a list of records")

(defun historian (core)
  ;no-op
  nil)

(defun reset-history ()
  (setf *history* (make-hash-table :test #'equal)))

(defmethod add-log ((obj recording-core))
  (setf (gethash (name obj) *history*)
        (data (storage-q obj))))

(defun start-recording ()
  (when *core-id*
    (when (eq (type-of *core-id*) 'recording-core)
      (reset-history)
      (setf (recording-p *core-id*) t))))

(defun stop-recording ()
  (when *core-id*
    (when (eq (type-of *core-id*) 'recording-core)
      (setf (recording-p *core-id*) nil)
      (add-log *core-id*))))

(defmethod append-record ((core recording-core) data)
  (push-queue (storage-q core) data))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The channel code. Derived off of the CSP model. In particular, we
;;; have blocking communications.

;; Our specified communication mode for the TTA is a channel with
;; blocking send/rx. The channel has length 1. A channel is
;; effectively a queue with length 0.
;;
;; A channel is a unidirectional 1:1 communicator.

(defclass channel ()
  ;; string denoting the name of the channel
  ((name :accessor name
         :initarg :name)
   ;; source core
   (source :accessor source
           :initform nil
           :initarg :source)

   ;; dst core
   (dest :accessor dest
         :initform nil
         :initarg :dest)

   ;; This is the data channel
   (data-lock :accessor data-lock
              :initform nil
              :initarg :data-lock )
   (data :accessor channel-data :initform nil)

   ;;Sending is the control channel
   (sending-lock :accessor sending-lock
                 :initform nil
                 :initarg :sending-lock)
   (sending :accessor sending
            :initform nil
            :initarg :sending)))

(defmethod initialize-instance :after ((channel channel) &key)
  "Initializes the locks as soon as we create the instance"

  (setf (data-lock channel)
        (bordeaux-threads:make-lock
         (format nil "data-lock-(~a)" (name channel))))

  (setf (sending-lock channel)
        (bordeaux-threads:make-lock
         (format nil "sending-lock-(~a)" (name channel)))))

;; shorthand constructors
(defun make-channel (name)
  "Makes a channel with `name`"
  (make-instance 'channel :name name))

(defun make-core (name entrypoint &key recording )
  "Makes a 'core' with `name`, using function `entrypoint`"
  (if recording
      (make-instance 'recording-core :name name :entrypoint entrypoint)
      (make-instance 'core :name name :entrypoint entrypoint)))

(defmethod start-core ((core core))
  "Starts a core with the settings defined in the core object"
  (let ((bordeaux-threads:*default-special-bindings* (acons '*core-id* core nil)))
        (bordeaux-threads:make-thread
          #'(lambda ()
              (if (args core)
                  (funcall (entrypoint core) (args core))
                  (funcall (entrypoint core)))
              (when (cleanup core)
                  (funcall (cleanup core) core)))
          :name (name core))))

(defmethod new-channel ((sender core) (receiver core))
  "Creates a channel connecting `sender` and `receiver`"
  (let ((channel (make-channel
                  (format nil "~a ~~> ~a"
                          (name sender)
                          (name receiver)))))
    (configure-channel channel sender receiver)
    channel))

(defmethod configure-channel ((channel channel) (src core) (dest core))
  "Connects `channel` from `src` to `dest`"
  (setf (source channel) src)
  (setf (dest channel) dest)
  channel)


;; Send algorithm:
;;
;; Wait until SENDING is low. then, raise SENDING high and write the data.
;; When SENDING goes low, return.

;; Get algorithm
;;
;; Wait until SENDING is high and read the data. When done reading,
;; deassert SENDING and return.

(defun poll-until(channel value)
  "Blocks until SENDING is eql to `test` on channel"
  ;; implementation detail: we are spinlocking. Really not a great
  ;; idea. Need to figure out how to drop
  (let ((waiting t))
    (loop
       until (eql waiting nil)
       do
         (bordeaux-threads:with-lock-held ((sending-lock channel))
           ;; If it's nil...
           (when (eql value (sending channel))
             (setf waiting nil))))))

(defmethod send ((channel channel) data)
  (poll-until channel nil)
  (bordeaux-threads:with-lock-held ((data-lock channel))
    (setf (channel-data channel) data)
    (setf (sending channel) t)
    (counter-tick (source channel)))
  (poll-until channel nil))

(defmethod receive ((channel channel))
  (let ((incoming))
    (poll-until channel t)
    (bordeaux-threads:with-lock-held ((data-lock channel))
      (setf incoming (channel-data channel))
      (setf (sending channel) nil)
      (counter-tick (dest channel)))
    incoming))

;; Question. Since we aren't specializing here, should we roll this
;; into the normal send/ receive methods?
;;
;; Answer: Probably.
(defmethod send :around ((channel channel) data)
  (let ((core (source channel)))
    (when (recording-p core)
      (append-record core
                     (list *core-id*
                           (name channel)
                           :tx
                           (counter (source channel))))))
    (call-next-method))

(defmethod receive :around  ((channel channel))
  (let ((communication-counter (counter (dest channel))))
    (let ((result (call-next-method))
          (core (dest channel)))
      (when (recording-p core)
        (append-record core
                       (list *core-id*
                             (name channel)
                             :rx
                             communication-counter)))
      result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producer-consumer setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun producer (channel)
  (let ((channel (car channel)))
    (start-recording)
    (loop for i from 0 upto 30 do
         (send channel i))
    (stop-recording)))

(defun consumer (channel)
  (let ((channel (car channel)))
    (start-recording)
    (loop for i from 0 upto 30 do
      (let ((data (receive channel)))
        ;(format t "Got: ~a~%" data)
        ))
    (stop-recording)))

(defun do-pc()
  (let* ((producer (make-core "producer" #'producer :recording t))
         (consumer (make-core "consumer" #'consumer :recording t))
         (channel (new-channel producer consumer)))

    (setf (args producer) (list channel))
    (setf (args consumer) (list channel))

    (start-core consumer)
    (start-core producer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node1 -> Node2 <- Node3

(defun node1 (channel)
  (start-recording)
  (loop for i from 0 to 5 do
       (send channel i))
  (stop-recording))

(defun node2 (channels)
  (bind (((channel1 channel2) channels))
    (start-recording)
    (loop for i from 0 to 5 do
      (cond  ((oddp i)
              (receive channel1)
              (receive channel2))
             (t
              (receive channel2)
              (receive channel1))))
    (stop-recording)
    (format t "All done. ^_^")))

(defun node3 (channel)
  (start-recording)
  (loop for i from 100 to 105 do
       (send channel i))
  (stop-recording))

(defun do-network ()
  (let* ((node1 (make-core "node1" #'node1 :recording t))
         (node2 (make-core "node2" #'node2 :recording t))
         (node3 (make-core "node3" #'node3 :recording t))

         ;; create links
         (channel1 (new-channel node1 node2))
         (channel2 (new-channel node2 node3)))
    ;; initialize the channels for the cores.
    (setf (args node1) channel1)
    (setf (args node2) (list channel1 channel2))
    (setf (args node3) channel2)

    (start-core node1)
    (start-core node2)
    (start-core node3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process the result from the *history*

(defun emit-readable-history (history)
  "History is a hash table containing mappings from cpu id to queues
describing the input and output. Outputs a list of lists suitable for
writing to a file."

  (loop for id in (alexandria:hash-table-keys history)
        collect
        (list
         (cons :thread id)
         (cons :data
               (loop for comm in  (gethash id history)
                  collect
                    (list (cons :name
                                (second comm))
                          (cons :sequence-id
                                (fourth comm))
                          (cons :type
                                (third comm)) ))))))


(defun write-history (history filename)
  (alexandria:write-string-into-file
   (format nil "~s" (emit-readable-history history))
   filename
   :if-exists :overwrite))
