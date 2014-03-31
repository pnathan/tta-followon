;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Simulation reader.
;;;; Paul Nathan's MS Thesis, University of Idaho 2007-2011
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-level discussion of the algorithm.

;;; First, load the data file. This can come in one of two forms:
;;; either the data from the simulator or the data from the
;;; hardware. We read the file and lexically analyze it into a known
;;; form. After lexical analysis, we transform the results into a
;;; semantically meaningful execution trace, which contains the full
;;; information about what can be rendered.

;;; The initial XC code must use some start/end-trace macros, which
;;; are shown in the sample code. This will engender labels in the
;;; disassembled code, which we shall read. These labels map to a
;;; given address in memory. We only use the address ranges we are
;;; given in the simulation, in the actual hardware, the macros would
;;; signify to start/end the instruction tracing.

;;; The final sets of instructions shall be abstracted into the set of
;;; elementary communication primitives. These primitives represent
;;; the partial ordering of the program flow. These primitives and the
;;; program flow are rendered out into a png file.



;;;; Software Maintenance Notes Follow

;;; The software has been organically put together. Most of it was
;;; done late in the evening. The structure is generally a
;;; layer-driven structure.

;;; There is a distinction between the partially-parsed lexical object
;;; and the fully parsed semantic object (called a cpu-step). This
;;; distinction is artificial and probably should be rewritten after
;;; the program is rolling along.

;; An instruction, as parsed from the sim, has op, destination, and
;; arguments.  So we need a "context" & "instruction" &
;; "after-execution state" somehow context denotes a particular cpu.

;; The simulation uses a rising-edge rendering of what happens. So
;; shall we.

(defpackage :sim-reader
  (:use :common-lisp)
  (:export :main))
(in-package :sim-reader)

;;regex lib
(ql:quickload '(:cl-ppcre
                :alexandria
                :cl-dot
                :metabang-bind))


(use-package :metabang-bind)

;;Sets the path to dot correct for the standard install of Graphbiz on
;;OSX
(if (find :darwin *features*)
    (setf cl-dot:*dot-path* "/usr/local/bin/dot"))

;;Personal libraries.
(asdf:load-system :batteries)
(use-package :batteries)
(ql:quickload :defobject)
(use-package :defobject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utils

;; the classic example of a closure
(defun counter-generator ()
  (let ((counter 0))
    (lambda ()
      (incf counter))))

(defparameter *counter-closure* (counter-generator))

;; Some slicker hackery might induce it to not need a function call
(defmacro *counter* ()
  (funcall *counter-closure*))

(defun true-p (var)
  (not (not var)))
;;; Logic

(defmacro gassoc (item alist)
  "Gets the data from assoc"
  `(cdr (assoc ,item ,alist )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric printme (object)
  (:documentation
   "PRINTME is the standard debugging routine used here for the
  objects"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XCORE simulation parsing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-sim-file (filename)
  "Reads file.
File is presumed to be the output from an XCORE simulation.

Returns lines from the simulation as a list of strings

Throw an error if TRAP ET is detected.

Ignored lines:

Event caused | DBG_INT
"
  (let ((file-text-list (cl-ppcre:split "\\n"
                                        (batteries:read-text-file filename))))
    (remove-if #'not
               (loop for line in file-text-list
                  collect
                    (cond
                      ((cl-ppcre:scan "Event caused" line)
                       nil)
                      ((cl-ppcre:scan "DBG_INT" line)
                       nil)
                      ((cl-ppcre:scan "TRAP ET" line)
                       (error "Trap ET detected"))
                      (t
                       ;; There's got to be a cleaner method here.
                       line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collection of the lexical instructions
(defobject lexical-run (inst-list)
  :documentation "Holds the information from the run; not wholly parsed. The
  inst-list is indeed an iterable")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod printme ((obj lexical-run))
  (let ((list (lexical-run-inst-list obj)))
    (loop for var in list do
         (printme var)
         (writeln ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The initial object created by the lexical pass
(defobject lexical-instruction  (core
                                 thread
                                 pc
                                 op
                                 reginfo
                                 cycle)
  :documentation "This is a given instruction's execution in the lexical environment")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod printme ((obj lexical-instruction))
  (print-hash-table-1 (object-to-hash obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod store ((container lexical-run) (instr lexical-instruction))
  "Appends the instruction to the run.
FIFO"
  (setf (lexical-run-inst-list container)
        (append (lexical-run-inst-list container) (list instr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lex-instruction (sim-line)
  "Parses a line from the simulation and transforms it to an
instruction: returns a `lexical-instruction` object"
  (let ((result
         (cl-ppcre:register-groups-bind
                                        ;(core thread)
                                        ;("stdcore\\[(\\d)\\]@(\\d)"
             (core thread  nil pc  op reginfo  cycle)
             ("stdcore\\[(\\d)\\]@(\\d)..+?(\.|-)+?([0-9A-Fa-f]{8}).+?:\\s(\\w+)\\s+(.*?)\\s+@(\\d+)"
              sim-line)
           (make-lexical-instruction
            :core core
            :thread thread
            :pc pc
            :reginfo reginfo
            :cycle cycle
            :op op))))
    (if result
        result
        (error "Unable to lex: ~a" sim-line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod lex-registers ((obj lexical-instruction))
  "Lexically chop up the register string into a list"
  ;;;More useful than Lex Luthor.

  ;;splits it up up by ,
  ;;strips whitespace from each constituent part.
  (loop for var in (cl-ppcre:split "," (lexical-instruction-reginfo obj))
     collect
       (string-trim " " var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lexically-parse-sim (list-of-lines)
  "Returns a run from the list of lines"
  (let ((run (make-lexical-run)))
    (loop for sim-line in list-of-lines do
         (store
          run
          (lex-instruction sim-line)))
    run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulation superclass object
;; the comm node is the arbitrary analysis node.
(defobject:defobject comm-node
    (core
     operation
     out-data
     marked
     u-id
     next-inst
     next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defobject start-node ((core (make-array
                              '(4)
                              :initial-contents
                              #(nil nil nil nil) )))
  :superclasses '(comm-node)
  :undecorated t
  :documentation "Start node for all operations; TOP in the lattice of
  communication")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defobject final-node ()
  :undecorated t
  :superclasses '(comm-node)
  :documentation "Finalnode for all operations; the lattice of
  operations meets here.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simulation object.
;;; This will get rendered out to the debug windows
(defobject cpu-step
    (core    ;core
     thread  ;thread id
     opcode  ;asm op
     cycle   ;cycle (only relevant for sims)
     pc      ;program counter
     lr      ;link register
     sp      ;stack pointer
     cp      ;constant pointer
     dp      ;data pointer
     res-id  ;Resource id.
     (reg
                                        ;simply specifying #(...)
                                        ;yields the same referent in
                                        ;memory.
      (make-array '(12)
                  :initial-contents
                  #(nil nil nil
                    nil nil nil
                    nil nil nil
                    nil nil nil)))
     out      ;Data being sent out
     marked   ;Has this been marked
     u-id     ;Used to id the step for this particular run.

     next-inst ;The next instruction this
                                        ;can points to
     next      ;The set of other
                                        ;instructions that the
                                        ;sequencing can point at.
     )
  :undecorated t
  :superclasses '(comm-node)
  :documentation "The context of a given CPU state at the end of an instruction
  on a given core/cpu. cpu-state denotes the set of user registers
  found at the end of the cycle; ie, those not the pc/lr/cp, etc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod printme ((obj cpu-step))
  (print-hash-table-1 (object-to-hash obj)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone ((obj cpu-step))
  "Duplicates obj into a new slot of memory"
  ;; This can be done with a sufficiently smart macro. Which shall be
  ;; done when crunch is over.
  (let ((new (make-cpu-step)))
    (setf (core new) (core obj))
    (setf (thread new) (thread obj))
    (setf (opcode new) (opcode obj))
    (setf (cycle new) (cycle obj))
    (setf (pc new) (pc obj))
    (setf (lr new) (lr obj))
    (setf (sp new) (sp obj))
    (setf (cp new) (cp obj))
    (setf (dp new) (dp obj))
    (setf (res-id new) (res-id obj))
    ;; This may be a problem: reg is probably a reference
    (setf (reg new) (reg obj))
    (setf (out new) (out obj))
    (setf (marked new) (marked obj))
    (setf (u-id new) (u-id obj))
    (setf (next-inst new) (next-inst obj))
    (setf (next new) (next obj))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod modified-register ((obj cpu-step))
  "Returns the modified register for the operation"
  (position-if #'true-p
               (reg obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod modified-value ((obj cpu-step))
  (let ((register (modified-register obj)))
    (if register
        (elt (reg obj) register)
        nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod register-modifications ((obj cpu-step))
  "Returns a list (register# value) of the registers modified by the
  operation"
  (list (modified-register obj)
        (modified-value obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These variables are partitioned out according to the syntax they
;;; use in the simulation.
;;;
;;; This roughly corresponds to section 19.2 in "The XMOS XS1
;;; Architecture".

;;; See PARSE-INSTRUCTION for more information

;; TODO: Look into these instructions
(defvar *thread-instructions*
  '("init")
  "Has the form t[reg]:(cp|dp|pc) reg")

;; TODO: Look into these instructions
(defparameter *resource-instructions*
  '("freer"
    "getst"
    "eeu")
  "Has the form op res[reg addr]")

(defparameter *resource-register-instructions*
  '("set"
    "setd"
    "setv"
    "out"
    "outt"
    "outct")
  "Has the form op res[reg value] reg value")

(defparameter *register-resource-instructions*
  '("inct"
    "in")
  "Has the form op reg val res[reg val]")

(defvar *resource-immediate-instructions*
  '("setc"
    "setclk"
    "msync"
    "mjoin"
    "chkct"
    )
  "Has the form op res[reg addr] imm")

(defvar *get-instructions*
  '("get")
  "has the form op reg ps[reg addr]")

;;TODO: Examine sssync
(defparameter *no-arg-instructions*
  '("drestsp"
    "dret"
    "dentsp"
    "dcall"
    "ssync"
    "clre"
    "waiteu")
  "Has the form op")

(defvar *immediate-instructions*
  '("bl"
    "bu"
    "extsp")
  "Has the form op imm")

(defvar *register-instructions*
  '("bla"
    "bau")
  "has the form op reg")

(defvar *register-register-immediate-instructions*
  '("eq"
    "ldaw")
  "has the form op reg reg imm")

(defvar *register-immediate-instructions*
  '("ldc"
    "ldap"
    "getr"
    "getst"
    "bt"
    "bf"
    "addi"
    "zext"
    "mkmsk")
  "has the form op r imm")

(defparameter *register-register-register-instructions*
  '("add"
    "sub"
    "lsu"
    "shl"
    "or"
    "shr"
    "lss"
    "and"
    "rems"
    "mul")
  "has the form op reg reg reg")

(defparameter *register-register-register-register-instructions*
  '("maccu"
    "maccs")
  "Has the form op reg reg reg reg")

(defparameter *register-register-instructions*
  '("andnot")
  "has the form op reg reg")

(defvar *memory-instructions*
  '("ldw"
    "stw")
  "Has the form op reg (sp|dp)[address] (L|S)[address]")

(defvar *sliced-memory-instructions*
  '("ld8u"
    "ld16s"
    "st16")
  "op r11 0x00000068 , r11 0x000102a8 [r5 0x00000001 ] L[0x000102a9]")

(defvar *register-register-register-offset*
  '("lda16")
  "lda16   r0 0x000100da , r1 0x000102ac [-r0 0x000000e9 ]")

(defvar *sp-instructions*
  '("entsp"
    "retsp")
  "has the form op imm (L|S)[address]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This regex is used to parse a register with value string
(defparameter *reg-parsing-string* "r(\\w+)\\(0x(\\w+)\\)$"
  "This parses 'r1(0x0000000b)'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-dst-reg (regstring)
  "Expects something of the form denoted by *reg-parsing-string* as a string.

Returns a (VALUES dst-register register-value).

Both dst-register and register-value are a number"
  (let ((result (cl-ppcre:register-groups-bind
                    (reg value)
                    (*reg-parsing-string*
                     regstring)
                  (list reg value)
                  )))
    (if result
        (values (parse-integer (first result))
                (parse-integer (second result) :radix 16))

        (error "Unable to parse: ~a" regstring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *resource-parsing-string*
  "\\w+\\[r\\d+\\(0x(\\w+)\\)\\]"
  "This parses this string: ps[r2(0x10b)]")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-resource-spec (regstring)
  "Takes a register string, presumably from the simulator listing, and
determines which resource ID got specified.

Returns an int.

Used for OUT or SET instructions

Expects something of the form  *resource-parsing-string*
"
  (let ((result
         (cl-ppcre:register-groups-bind
             (value)
             (*resource-parsing-string*
              regstring)
           value)))

    (if result
        (parse-integer result :radix 16)

        (error "Unable to parse: ~a" regstring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-instruction (op list)
  "Takes an operation and a specification of what happened to the CPU's
 registers.  Depending on which bucket the instruction falls, the
 register-string parser reads the string and determines the state of
 the cpu. "

  (let ((register-state (make-cpu-step)))

    (labels ((assign-to-reg-state (list)
               "Used in storing the destination register to the register-state variable"
               (multiple-value-bind (reg value)
                   (parse-dst-reg (first list))
                 (cond ((< reg 12)
                        (setf (aref (reg register-state) reg) value))
                       ;;See Chapter 4 of the XS1 ISA reference.
                       ;;Register 14 is an alias for the SP.
                       ((eql reg 14)
                        (setf (sp register-state) value)))))

             (assign-to-res-state (list)
               "Stores the resource id assigned in an OUT or a SET statement"
               (setf (res-id register-state)
                     (parse-resource-spec (first list)))))

      ;; This is very imperative case-based logic. There're probably
      ;; simplifications possible.

      (cond
        ((find op *register-register-register-register-instructions* :test 'string=)
         (assign-to-reg-state list))
        ((find op *register-register-register-offset* :test 'string=)
         ;;only a load right now
         (assign-to-reg-state list))

        ((find op *sliced-memory-instructions* :test 'string=)
         (if (string= op "ld16s")
             (assign-to-reg-state list)))

        ((find op *register-resource-instructions* :test 'string=)
         ;;e.g., INCT
         ;;get the assigned register
         (assign-to-reg-state list)
         ;;and the resource it came from
         (setf (res-id register-state)
               (parse-resource-spec (second list))))


        ((find op *register-instructions* :test 'string=)
         ;;no value returned
         )
        ((find op *resource-instructions* :test 'string=)
         ;; no value returned
         )
        ((find op *no-arg-instructions* :test 'string=)
         ;;no value returned
         )
        ((find op *resource-immediate-instructions* :test 'string=)
         ;;no value returned
         )
        ((find op *thread-instructions* :test 'string=)
         ;;init not document in the ISA?!
         )

        ((find op *register-register-instructions* :test 'string=)
         ;;   r11(0x10444), r2(0x3)
         (assign-to-reg-state list))

        ((find op *register-register-immediate-instructions* :test 'string=)
         ;;
         (assign-to-reg-state list))

        ((find op *memory-instructions* :test 'string=)
         (if (string= op "ldw")
             (assign-to-reg-state list)))

        ((find op *sp-instructions* :test 'string=)
         ;;TODO: futz with sp/lr
         ;;sp-modifying instructions, possibl lr as well.
         )

        ((find op *register-immediate-instructions* :test 'string=)
         ;;("r1 0x0000000b" "i 0x0000000b")
         (assign-to-reg-state list))

        ((find op *get-instructions* :test 'string=)
         ;;r0 0x00010000 , ps[r1 0x0000000b ] @9
         (assign-to-reg-state list))

        ((find op *register-register-register-instructions* :test 'string=)
         ;;("r0 0x00020000" "r10 0x00010000" "r11 0x00010000")
         (assign-to-reg-state list))

        ((find op *resource-register-instructions* :test 'string=)
         ;;ps[r2 0x0000010b ], r0 0x00010000

         ;;Here we do not edit the state of the CPU proper, but instead
         ;;modify some output or jtag setting.

         ;;Set can do a lot of things. Right now we're not interested
         ;;in set. Just out & its kin.
         (cond
           ((string= op "out")
            (multiple-value-bind (reg value)
                (parse-dst-reg (second list))
              (setf (out register-state)
                    value))))

         (if (not (string= op "set"))
             (assign-to-res-state list)))

        ((find op *immediate-instructions* :test 'string=)
         ;;bl      i 0x0000000f

         ;; These are branch instructions, which solely modify the
         ;; PC. The current PC is discovered from the sim line.

         ;;extsp is also in this group, it modifies the SP
         )


        (t
         (error "Operation not understood: ~a" op)))
      ;;end cond
      ;;set the operation
      (setf (opcode register-state) op)
      register-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod semantic-parse-instruction ((obj lexical-instruction))
  "Takes a lexical instruction and returns a cpu-step"

  (let ((reglist (lex-registers obj))
        (op (lexical-instruction-op obj)))

    (let ((new-cpu-state (parse-instruction op reglist)))

      (setf (pc new-cpu-state)
            (lexical-instruction-pc obj))

      (setf (cycle new-cpu-state)
            (parse-integer (lexical-instruction-cycle obj)))

      (setf (thread new-cpu-state)
            (parse-integer (lexical-instruction-thread obj)))

      (setf (core new-cpu-state)
            (parse-integer (lexical-instruction-core obj)))

      new-cpu-state)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generalized interface


(defobject sexpr-step (channel-id core-id src dst core-op op-id)
  :documentation "One step from the sexpr codes"
  :undecorated t)

(defmethod u-id ((obj sexpr-step))
  (op-id obj))

(defmethod opcode ((obj sexpr-step))
  (core-op obj))

(defun parse-sexpr (form)
  (make-sexpr-step
   :channel-id  (gassoc :channel-name (gassoc :communicators form))
   :core-id (gassoc :id form )
   :src (gassoc :sender (gassoc :communicators form))
   :dst (gassoc :receiver (gassoc :communicators form))
   :op-id (gassoc :sequence-id form)
   :core-op (gassoc :type form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic run.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defobject semantic-run ((container nil)
                         sequence-cache)
  :documentation "An object that handles semantic analysis"
  :undecorated t)

(defobject semantic-xcore-run ((container nil) sequence-cache)
  :documentation "Contains a list of cpu-steps. Each cpu step has an id
  unique to the current run"
  :superclasses '(semantic-run)
  :undecorated t)

(defobject semantic-sexpr-run
    ((container nil)
     core-list
     channels)
  :documentation
  "Contains information on sexpr-based ops, organized into a list of
  vectors, one vector per core.

Each element in core-list is index-linked to the list of vectors.

Further, channels contains a list of channels that were communicated
upon."
  :superclasses '(semantic-run)
  :undecorated t)

(defun parse-sexpr-file (data)
  (declare (optimize (speed 3) (space 0) (debug 1)))
  (let ((run (make-semantic-sexpr-run)))
    (setf (container run)
          (loop for core in data
             collect
               (progn
                 (push (car core) (core-list run))
                 (sort
                 (concatenate
                  'vector
                  (mapcar #'parse-sexpr (cdr core)))
                 #'<
                 :key #'u-id))))
    (setf (core-list run) (reverse (core-list run)))
    (setf (channels run)
          (uniqueize (loop for core in (container run)
                        append
                          (map 'list #'channel-id core))))
    run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod printme ((obj semantic-run))
  (loop for step in (container obj)
       do
         (format t "~%** ~a--->~&" (u-id step))
         (printme step)))

(defmethod semantic-run-sort-container ((self semantic-run))
  "Sorts myself by the order laid out in the sequence"
  (let ((result (sort (container self)
                      #'(lambda (x y)
                          (< (u-id x)
                             (u-id y))))))
    (setf (container self) result))
  self)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod semantic-run-append ((self semantic-run) element)
  "Appends `element` to `self`"
  (push element (container self) )
  (semantic-run-sort-container self)
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod semantic-run-set-container((self semantic-run) (container list))
  "Setter"
  (setf (container self) container)
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone ((self semantic-run))
  (let ((new-run (make-semantic-run)))
    (semantic-run-set-container
     new-run
     (loop for var in (container self)
        collect
          (clone var)))
    new-run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric merge-semantic-runs (&rest objlist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note that if this is used again, it should be refactored to be:
;; defun merge-semantic-runs ( list-of-runs)
;; This allows a (loop collect) mechanism
(defmethod merge-semantic-runs (&rest objlist)
  (let ((newrun (make-semantic-run))
        (step-list (flatten (remove-if 'not (loop for obj in objlist
                                               collect (container obj))))))
    (semantic-run-set-container
     newrun
     step-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod semantic-parse-run ((obj lexical-run))
  "Parse a lexical run and return a semantic-run"

  (let* ((instruction-collection (lexical-run-inst-list obj))
         (parsed-inst-list (loop for step in instruction-collection
                       collect
                         (semantic-parse-instruction step)))
         (instruction-list (make-semantic-run)))
    ;;Now assign each instruction to a given id, counting up from 0.
    (loop
       for id in (range-1 0 (length instruction-collection))
       for inst in parsed-inst-list
       do
         (setf (u-id inst) id)
         (semantic-run-append instruction-list inst))
    instruction-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod semantic-run-count ((self semantic-run))
  "Returns the number of steps contained in this object"
  (list-length (container self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod semantic-run-sequence ((self semantic-run))
  "Returns the sequence describing the IDs in the container"
  ;;Consider caching.
  (sort
   (mapcar #'(lambda (step)
               (u-id step))
           (container self))
   '<))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod semantic-run-get-id ((self semantic-run) id)
  (find-if #'(lambda (step)
               (eql id (u-id step)))
           (container self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod semantic-run-delete-id ((self semantic-run) id)
  (setf (container self)
        (remove-if #'(lambda (step)
                       (eql id (u-id step)))
                   (container self)))
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod semantic-run-delete-multiple-ids ((self semantic-run) id-list)
  (loop for id in id-list
     do
       (semantic-run-delete-id self id))
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod filter-by-core ((obj semantic-run) core)
  "Returns a new semantic-run object containing only the specified
core"

  (let ((filtered-run (make-semantic-run)))
    (semantic-run-set-container
     filtered-run
     (remove-if-not
      #'(lambda (val)
          (eql
           (core val)
           core))
      (container obj)))
    filtered-run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod filter-out-by-core ((object semantic-run) core)
  "Returns a new semantic-run object without the specified core"
  (let ((filtered-run (make-semantic-run)))
    (semantic-run-set-container
     filtered-run
     (remove-if
      #'(lambda (val)
          (eql
           (core val)
           core))
      (container object)))
    filtered-run))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful when looking for an instruction to examine at leisure
(defgeneric find-first-inst-of-type (opcode object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-first-inst-of-type (inst-string (list lexical-run))
  (elt (lexical-run-inst-list list)
       (position-if
        #'(lambda (inst)
            (string= (lexical-instruction-op inst) inst-string))
        (lexical-run-inst-list list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-first-inst-of-type (opcode (list semantic-run))
  (let ((seq (semantic-run-sequence list)))
    (loop for var in seq
         until (string=
                opcode
                (opcode (semantic-run-get-id list var)))
         finally (return (semantic-run-get-id list var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-a-sim-file (filename)
  "Returns a semantic run of the cpu-steps"
  (semantic-parse-run
   (lexically-parse-sim
    (read-sim-file filename))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-ifc-file (filename)
  (read-from-string
   (alexandria:read-file-into-string filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-ifc-file (filename)
  "Loads a file in the standard TTA trace format"
  (read-ifc-file filename))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Probably usable on a lexical-run as well
(defgeneric find-opcodes (obj opcodes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-opcodes ((obj semantic-run) opcodes)
  "Given a semantic-run and a list of opcodes to look for, filters out the
  non-conforming opcodes from the incoming hash and returns a new semantic-run"
  (let ((filtered-run (make-semantic-run)))

    (semantic-run-set-container
     filtered-run
     (remove-if-not #'(lambda (val)
                        (find
                         (opcode val)
                         opcodes
                         :test 'string=))
                    (container obj)))
    filtered-run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are the instructions of interest here.
(defparameter *incoming-ops* '("in" "int" :rx))
(defparameter *outgoing-ops* '("out" "outt" :tx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod filter-for-incoming ((obj semantic-run))
  "Instructions relating to incoming communication"
  ;;difference is data width
  ;;in is a word, int is a byte
  (find-opcodes obj *incoming-ops*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod filter-for-outgoing ((obj semantic-run))
  "Instructions relating to outgoing communication"
  ;;difference is datawidthx
  ;;Out is a word, outt is a byte
  (find-opcodes obj *outgoing-ops*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod filter-for-transfer ((self semantic-run))
  "Return a container with all non-transfer instructions trimmed"
  (find-opcodes self (append *incoming-ops* *outgoing-ops*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-transfer (obj)
  "Is object a transfer instruction?"
  (or (is-incoming obj)
      (is-outgoing obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-outgoing (obj)
  (find
   (opcode obj)
   *outgoing-ops*
   :test 'string=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-incoming (obj)
  (find
   (opcode obj)
   *incoming-ops*
   :test 'string=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In a typical message-sending, INs occur with a (apparently)
;; spurious first IN that gets 0.  By comparing with the master/slave
;; transactions, the 0 is some sort of syncrhonization mechanism.
(defmethod prune-duplicate-ins ((obj semantic-run) &optional transaction)
  (cond
    (transaction
      ;;A transaction only has the 1st
      (semantic-run-delete-id
       obj
       (first
        (semantic-run-sequence (filter-for-incoming obj)))))
    (t
     ;;A non-transaction has every other
     (semantic-run-delete-multiple-ids
      obj
      (every-other (semantic-run-sequence (filter-for-incoming obj)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graph reading layer
;
;; takes a semantic-run and generates 4 lists, one for each cpu, which
;; contain the needed information for the next layer, the renderer.
;;
;; note that channels can be requested and freed. So a
;; channel-existence-id will be created for the lifetime of each
;; channel-request/free, and this will be denoted by some sort of box
;; at the start/end, instead of a circle for a normal in/out.
;;
;; we'll generate a channel-existence-id as a run-unique ID (incrementing integer)
;;
;; (later) Turns out that it is impossible in the current system to
;; determine channel creation/deletion on the fly with a trace. :-(
;;
;; An element in the list looks something like this:
;; channel-existence-id (IN|OUT) channel-id. Possibly including the data
;;
;; The channel IDs will match between each in and each out. so it's a
;; queue for a given channel-existence-id
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *channel-existence-id* 0)       ;this could be anything
(defun get-new-channel-id ()
  (setf *channel-existence-id* (1+  *channel-existence-id*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Conversion routines.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render-label is the routine that takes a node and converts it into
;; a text suitable for rendering on the image
(defgeneric render-label (data) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod render-label ((data cpu-step))
  "Render a text description of the cpu-step"
    (format nil
            "core: ~a pc: ~a op: ~a : ~a on q ~a"
            (core data)
            (pc data)
            (opcode data)
            (cond ((is-incoming data)
                   (register-modifications data))
                  ((is-outgoing data)

                   (out data)))
            (channel-of-comms data)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod render-label ((object list))
  "This is set to empty"
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun connect-cpu-steps (visible-list)
  "Links each cpu step with its next cpu-step

 Precondition: The visible-list points to a line of execution on a
single core."

  ;; This pairs up cpu-steps
  (let ((paired-node-list
           (batteries:gather-generator
            visible-list
            'batteries:sliding-window-2-wide
            'batteries:sliding-chunker)))

      (loop for pair in paired-node-list do
           ;;Set the first of the pairs
           (setf (next-inst (first pair))
                 (adjoin
                  (second pair)
                  (next-inst (first pair))))))
  visible-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun connect-cpu-steps-by-core (run)
  (loop for core in (list-of-cores-in-run run)
     do
       (connect-cpu-steps (container (filter-by-core run core)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod pre-render-semantic-run ((object semantic-run))
  "Loads the sequence of a `semantic-run` and returns a list of
`cpu-step`s"
  (connect-cpu-steps (container object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod cl-dot:object-node ((object cpu-step))
  (make-instance 'cl-dot:node
                 :attributes (list
                              :label (render-label object)
                              :shape :box)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod cl-dot:object-points-to ((object cpu-step))
  "Returns a list of cpu-steps the object points to"
  (append
   (if (cpu-step-next-inst object)
       (loop for ptr in (cpu-step-next-inst object)
          collect
            (make-instance 'cl-dot:attributed
                           :object  ptr
                           :attributes '(:weight 3))))
   (if (cpu-step-next object)
       (loop for ptr in (cpu-step-next object)
          collect
            (make-instance 'cl-dot:attributed
                           :object  ptr
                           :attributes '(:weight 3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layer for the reading of the disassembly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here we read an xobjdump file (see the Makefile for the format
;; expected). The objdump file will have several labels
;; starttrace([1..4]) with corresponding endtrace labels.
;;
;; After the input comes in, we separate by core, find the first
;; occurance of starttrace(core#), then move through the list of data
;; until we hit the address specified by endtrace(core#). This becomes
;; the Traced information for core#.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-address-string (address-string)
  "Parses the address string passed in from the dissassembly"
  (let*
      ;; this cleans some spaces and the pesky newline out
      ((cleaned-address
        (cl-ppcre:regex-replace-all
         "\\s|\\n"
         address-string ""))

       ;;Parse the string
       (result
        (cl-ppcre:register-groups-bind
            (position core address)
            ("(\\w+?)(\\d+)>:0x(\\w+):" cleaned-address)

          ;;Returns this list
          (list
           ;;Allow integer compares of a core
           (parse-integer core)
           ;;Use a symbol here.
           (if (string= "starttrace" position)
               'START
               'END)
           ;;Use the string hex address
           address))))

    (if result
        result
        (error "Unable to parse: ~a~&" cleaned-address))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-xobjdump (filename)
  "Loads the disassembly file `filename` into a collection of cores
and the trace locations they map to."
  (let* ((text (batteries:read-text-file filename))
         ;(error "This needs to be split into the memory maps for each core")
         ;;Suck out the critical parts
         (scanner (cl-ppcre:create-scanner
                   "(start|end)trace(\\d+)>:.*?0x(\\w+):" :single-line-mode t))
         (address-strings (cl-ppcre:all-matches-as-strings
                           scanner
                           text))
         (trace-locations (make-hash-table)))

    (loop for address-string in address-strings
       do
         (let ((record (parse-address-string address-string)))
           ;;Convert this into a data structure that we can use
           (hashset trace-locations
                    (first record)
                    ;;start/end   address
                    (acons (second record) (third record)
                           (hashget trace-locations (first record))))))
    trace-locations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-trace-location (datastore point core)
  "Gets the trace location as a string from datasore on core `core`.

Point := START | END"
  (cdr (assoc point (hashget datastore core))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter-untraced-location (run objdump core)
  "Given a semantic run and a core, return a new semantic run, but
only with instructions between start and end.

The incoming trace is walked until the first time start-address is
found, then the steps are captured until the first time end-address is
found.

If core has no trace on it, then an empty run is returned."
  (unless (= (length (list-of-cores-in-run run) ) 1)
    ;; There's an error!
    (if (= (semantic-run-count run) 0)
        ;; Oh, wait, it's just an empty run
        (return-from filter-untraced-location run)
        ;; Oh, it's not- that's bad.
        (error "Number of cores in trace is not 1: ~a"  (length (list-of-cores-in-run run) ))))

  (assert (= (first (list-of-cores-in-run run) )
             core))
  (let ((start-address (get-trace-location objdump 'START core))
        (end-address   (get-trace-location objdump 'END core))
        (return-run    (make-semantic-run)))

    ;;Early return
    (if (not (and start-address end-address))
        (return-from filter-untraced-location
          return-run))

    ;;actual logic
    (let ((run-sequence  (semantic-run-sequence run))
          (gettingflop))                ;flip-flop
      (loop for key in run-sequence
         do
           (let ((current-step (semantic-run-get-id run key)))
             (cond
               ;;start trace
               ((string=
                 (cpu-step-pc current-step )
               start-address)
                (setf gettingflop t)
                (semantic-run-append return-run current-step))

               ;;stop trace
               ((string=
               (cpu-step-pc current-step)
               end-address)
                (setf gettingflop nil)
                (semantic-run-append  return-run current-step))

               ;;Otherwise...
               (t
                (cond (gettingflop
                       (semantic-run-append return-run current-step))))))))
    return-run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod filter-for-traced-core ((trace semantic-run)  objdump core)
  (filter-untraced-location (filter-by-core  trace core)
                            objdump
                            core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun write-single-core-graph (trace objdump core filename)
;;   "Writes the graph of a core from starttrace to endtrace out to the fire"
;;  (render-cpu-collection
;;    (prune-duplicate-ins
;;     (filter-for-transfer
;;      (filter-untraced-location
;;       (filter-by-core trace core) objdump core))))
;;   filename)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun channel-of-comms (inst)
  "Returns the channel of communication the instruction `step` is
communicating on."
  (assert (find
           (cpu-step-opcode inst)
           '("in" "int" "out" "outt")
           :test 'string=))
  (cdr (assoc 'resource-counter (parse-resource-id (cpu-step-res-id inst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here we figure out how to link this stuff up!
;;;

;; Givens:
;; Any valid linearization routes through all the nodes.
;; An IN follows an OUT.
;; An OUT follows an IN.

;; Several phases must happen.
;; Phase 1: separate by comm channel
;; Phase 2: separate by core.
;; Phase 3: Establish links between INs and /potential/ OUTs.
;; Phase 4: Establish the starting OUT and ending IN.
;; Phase 5: Determine the set of possibible routes between start-OUT and end-IN.
;; Phase 6: Prune routes that do not encompass all notes.

;;This object is used for testing search algorithms.
(defobject search-node (data type ptr)
  :documentation "Sample node for a linearization. Type can be IN or OUT")

(defmethod link ((from-obj search-node) (to-obj search-node))
  (setf (search-node-ptr from-obj) (adjoin to-obj (search-node-ptr from-obj))))

(defmethod printme ((obj search-node))
  (format t "~a~%" (stringme obj)))



(defmethod stringme ((obj search-node))
  (format nil "~a  : -> ~a"
          ;obj
          (search-node-data obj)
          ;(search-node-type obj)
          (loop for next-obj in (search-node-ptr obj) collect (search-node-data next-obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is used for testing
(defparameter *core0* nil)

(defparameter *core1* nil)


;;Resets the graph
(defun load-links()
  (setf *core0*
        (list
         (make-search-node :data "A")        ;1
         (make-search-node :data "B")        ;2
         (make-search-node :data "C")        ;3
         (make-search-node :data "D")        ;4
         (make-search-node :data "E")        ;5
         (make-search-node :data "0")
         (make-search-node :data "1")
         (make-search-node :data "F")))




  (link (first *core0*) (second *core0*)) ;A->B
  (link (fifth *core0*) (sixth *core0*))
  (link (sixth *core0*) (seventh *core0*))
  (link (first *core0*) (third *core0*))  ;A->C
  (link (second *core0*) (fourth *core0*)) ;B->D
  (link (third *core0*) (fifth *core0*))   ;C->E
  (link (fourth *core0*) (final *core0*))  ;D->F
  (link (fifth *core0*) (final *core0*))   ;E->F

  )
;;; end test code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns t or nil
;; Call against nodes.
;; NOTE: this is a bad name. It should be graph-node-equal.
(defgeneric graph-equal (a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; returns a list
(defgeneric graph-next (a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod graph-equal ((obj1 search-node) (obj2 search-node))
  (eql (search-node-data obj1) (search-node-data obj2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod graph-equal ((obj1 cpu-step) (obj2 cpu-step))
  (eql (cpu-step-u-id obj1)
       (cpu-step-u-id obj2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod graph-equal ((o1 t) (o2 t))
  "Most abstract version of equality possible"
  (eq o1 o2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod graph-next ((obj search-node))
  (search-node-ptr obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod graph-next ((object start-node))
  ;;TODO: Does this even get called? It seems like magic.
  (mapcar 'car
          (concatenate 'list (start-node-core object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod graph-next ((object final-node))
  "Final nodes have no list of next-nodes"
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod graph-next ((object cpu-step))
  "generate a list of cpu-steps"
  (append
    (cpu-step-next-inst object)
    (cpu-step-next object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod graph-next ((object list))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic functions to allow all-paths to work correctly.

;;(defmethod graph-data ((o search-node))
;;  (search-node-data o))

(defmethod graph-data ((o cpu-step))
  (cpu-step-u-id o))
(defmethod graph-data ((o start-node))
  "start")
(defmethod graph-data ((o final-node))
  "end")
(defmethod graph-data ((o list))
  (format nil "~s "o))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all-paths (start end)
  (let ((stack (list (list start (list start)))) ; list of (node path)
                                                 ; elements
        (allpaths '()))
    (do  ()
      ((not stack))
      (let ((item (pop stack)))
        ;(format t "~S~%" item)
        (let ((node (first item))
              (path (second item)))
          ;(format t "I: ~a~%" (graph-data node))
          (cond ((graph-equal node end)
                   (push path allpaths)))
                   ;(format t "*Q: ~a~%" path))))
          (loop for next in (graph-next node)
             do
               (cond ((not (in next path :test #'graph-equal))
                      (push (list next (cons next path)) stack)))))))
    ;; Push forms reverse lists
    (mapcar 'reverse allpaths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod filter-by-comm-channel ((obj semantic-run ) channel)
  "Return a run object without non-channel instructions from the run
object"
  (let ((new-run (make-semantic-run)))
    (loop for id in (semantic-run-sequence obj)
         do
         (if (eql
              (channel-of-comms  (semantic-run-get-id obj id))
              channel)
             (semantic-run-append new-run (semantic-run-get-id obj id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod core-starts-comms-p ((obj semantic-run) core)
  "Does this particular core initiate transmission?"
  ;;Pre-conditions:
  ;;assert: all of run belongs to core
  ;;assert: all of run is on the same channel

  ;;We check the first transfer instruction.
  ;; It's either an in or an out. If it's an out, T. Else nil.
  (let* ((id (first (semantic-run-sequence (filter-by-core obj core))))
         (op  (semantic-run-get-id obj id)))
    (cond (id
           ;(format t "~a~%" core)
           ;(format t "~a~%" op)
           (is-outgoing op )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod core-stopped-comms-p ((obj semantic-run) core)
  "Does this particular core initiate transmission?"
  ;;Pre-conditions:
  ;;assert: all of run belongs to core
  ;;assert: all of run is on the same channel

  ;;We check the first transfer instruction.
  ;; It's either an in or an out. If it's an out, T. Else nil.
  (assert (/= (length (semantic-run-sequence obj)) 0))
  (let* ((id (final (semantic-run-sequence (filter-by-core obj core))))
         (op  (semantic-run-get-id obj id)))
    (is-incoming op )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod which-core-started-talking ((obj semantic-run))
  "Returns the # of the core that first started talking on channel X

Expects a semantic-run trimmed down to the objects of interest.
Expects the semantic-run to hold all the cores of interest.
"
  (loop for core from 0 upto 3 do
       (if (core-starts-comms-p obj core)
           (return-from which-core-started-talking core))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod which-core-stopped-talking ((obj semantic-run))
  "Like which-core-started-talking, but stopped"
  (loop for core from 0 upto 3 do
       (if (core-stopped-comms-p obj core)
           (return-from which-core-stopped-talking core))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod final-talkers ((obj semantic-run))
  "Returns the list of cores that terminated their
communications. That is, the IN-residuals."
  (list  (which-core-stopped-talking obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod list-of-cores-in-run ((obj semantic-run))
  "Gives the list of the cores in the run"
  (uniqueize (loop for step in (container obj)
                collect
                  (cpu-step-core step))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod other-core-in-comms ((obj semantic-run) core)
  "Returns the integer id of the other core in the communication
channel"
  (let ((core-list (list-of-cores-in-run obj)))
    (assert (= (length core-list )
               2))
    (assert (in core core-list))
    (car (remove core core-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod next-instruction-naive-core ((obj semantic-run) id)
  "Given an instruction and the run, finds the next instruction in the
  sequence.

Does not filter based on core"

  (let* ((sequence (semantic-run-sequence obj))
        (next-instruction-index (1+
                                 (position id sequence))))
    (if (/= next-instruction-index (length sequence))
        (semantic-run-get-id obj
                             (elt (semantic-run-sequence obj) next-instruction-index))
        nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod next-instruction-on-this-core ((obj semantic-run) (inst cpu-step))
  "Given an instruction and the run, finds the next instruction in the
  sequence.

Returns the cpu-step of the next instruction on this core.

Filters out cores that don't belong."
  (let ((this-core-trace (filter-by-core obj (cpu-step-core inst))))
    (next-instruction-naive-core this-core-trace (cpu-step-u-id inst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun walk-core (obj alpha-seq beta-seq)
  "Side effect, modifies the cpu-step"
  (labels ((walker (look-at-predicate look-for-predicate)

             (let ((seen-list nil))
               ;;Figure out where this channel points
               (loop for step in alpha-seq do
                  ;; Yep we looked at the current node
                    (push step seen-list)
                    (cond
                      ;; If it's an in/outgoing, let's find the node
                      ;; on the other side that might work for us.w
                      ((funcall look-at-predicate (semantic-run-get-id obj step))
                       (let ((found 0))
                         ;;Find first two unseen nodes
                         (loop named finder
                            for index in beta-seq do
                              (cond
                                ;;When it's not seen...
                                ((not (in index
                                          seen-list))

                                 ;; If it's of the Type of Interest...
                                 (if (funcall look-for-predicate
                                              (semantic-run-get-id obj index))
                                     ;; We found something, so add it to the list
                                     (let ((other-step (semantic-run-get-id obj index))
                                           (current-step (semantic-run-get-id obj step)))
                                       (setf  (cpu-step-next current-step)
                                              (adjoin
                                               other-step
                                               (cpu-step-next current-step)))
                                       (incf found)))

                                 ;;Insert it into the seen list
                                 (if (= found 1)
                                     (push index seen-list))

                                 ;; If we found all 2 of them, stop looping
                                 (if (= found 2)
                                       (return-from finder  index))))))))))))

    (walker #'is-outgoing #'is-incoming)
    (walker #'is-incoming #'is-outgoing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod linearize-communication ((obj semantic-run))
  " Given that a set of instructions from 2 different cores on the
same channel are in the OBJ, creates the set of possible edges.

 We know that for a given channel an in is always followed by an out,
followed by an in. Therefore, if we find the first out (or the last
in), we can inductively walk up to the last operation."
  ;;assert: all elements are in the same channel

  (let*
      ;; Core communications started on
      ((starting-core (which-core-started-talking obj))
       (starting-core-seq (semantic-run-sequence (filter-by-core obj starting-core)))
       (other-core-id (other-core-in-comms obj starting-core))
       (other-core-seq (semantic-run-sequence (filter-by-core obj other-core-id))))
    (walk-core obj starting-core-seq other-core-seq)
    (walk-core obj other-core-seq starting-core-seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rendering-oriented routines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod cl-dot:object-node ((object start-node))
  (make-instance 'cl-dot:node
                 :attributes (list
                              :label "start"
                              :shape :diamond)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod cl-dot:object-node ((object list))
  (make-instance 'cl-dot:node
                 :attributes (list
                              :label "null"
                              :shape :circle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod cl-dot:object-node ((object final-node))
  (make-instance 'cl-dot:node
                 :attributes (list
                              :label "end"
                              :shape :diamond)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod cl-dot:object-points-to ((object start-node))
  (loop for core across (start-node-core object)
       collect
       (make-instance 'cl-dot:attributed
                      :object  (car core)
                      :attributes '(:weight 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful for debugging interactively
(defun merge-and-trim-run (trace objdump coreA coreB &optional transaction)
  (merge-semantic-runs
   (prune-duplicate-ins
    (filter-for-transfer
     (filter-for-traced-core  trace objdump  coreA))
    transaction)
   (prune-duplicate-ins
    (filter-for-transfer
     (filter-for-traced-core  trace objdump  coreB))
    transaction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prepare-consolidated-graph (base-trace objdump &optional transaction)
  "Prepare the entire graph from the brought-in trace and the objdump
Returns the starting node to send to the rendering routines"
  (let ((top-node (make-start-node))
        (end-node (make-final-node))
        (trace (clone base-trace)))

    (let ((full-run
           (merge-semantic-runs
            ;; this block could be simplified if merge-semantic-runs
            ;; was modified to take a single list; then a (loop
            ;; ... collect ) could be used.
            (prune-duplicate-ins
             (filter-for-transfer
              (filter-for-traced-core  trace objdump 0))
             transaction)
            (prune-duplicate-ins
             (filter-for-transfer
              (filter-for-traced-core  trace objdump 1))
              transaction)
            (prune-duplicate-ins
             (filter-for-transfer
              (filter-for-traced-core  trace objdump 2))
             transaction)
            (prune-duplicate-ins
             (filter-for-transfer
              (filter-for-traced-core  trace objdump 3))
             transaction))))

      ;;Side-effect!!!!!
      (linearize-communication full-run)

      (loop for core from 0 upto 3 do
        (setf (elt (start-node-core top-node) core)
              (pre-render-semantic-run
               (prune-duplicate-ins
                (filter-for-transfer
                 (filter-for-traced-core trace objdump core))
                transaction))))


    ;;Which cores had channels which were terminations of
    ;;communication? Link them to the end
      (let ((finished-conversation (final-talkers full-run)))
        (loop for core in finished-conversation
           do
             (setf (cpu-step-next
                    (final
                     (elt (start-node-core top-node) core)))
                   (list end-node))))

        (values top-node end-node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generate-consolidated-graph (trace objdump filename &optional transaction)
  "Prints out the entire graph"
  (cl-dot:dot-graph
   (cl-dot:generate-graph
    (prepare-consolidated-graph trace objdump transaction) )
   filename
   :format :png))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod dump-graph ((obj start-node) filename)
  (cl-dot:dot-graph
   (cl-dot:generate-graph
    obj)
   filename
   :Format
   :png))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun path-to-semantic-run (list)
;;   "list ::= ( start-node cpu-step* final-node)"
;;   (let ((new-run (make-semantic-run)))
;;     (semantic-run-set-container new-run (cdr (butlast list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gather-runs (trace objdump &optional transaction)
 (merge-semantic-runs
  (prune-duplicate-ins (filter-for-transfer (filter-for-traced-core trace objdump 0)) transaction)
  (prune-duplicate-ins (filter-for-transfer (filter-for-traced-core trace objdump 1)) transaction)
  (prune-duplicate-ins (filter-for-transfer (filter-for-traced-core trace objdump 2)) transaction)
  (prune-duplicate-ins (filter-for-transfer (filter-for-traced-core trace objdump 3)) transaction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prune-too-short-paths (all-paths trace objdump &optional transaction)
  (let ((actual-length (semantic-run-count (gather-runs trace objdump transaction))))
    (remove-if #'(lambda (list)
                   ;; Remember the passed-in list has pseudo start/end nodes
                   (/= (length list) (+ actual-length 2)))
               all-paths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun path-to-run (list)
 (let ((newrun (make-semantic-run)))
    (semantic-run-set-container newrun (butlast (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-path (list)
  (let ((newrun (make-semantic-run)))
    (semantic-run-set-container newrun (butlast (cdr list)))
    (clean-print newrun)
    (format t "~%~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-all-paths-for-trace  (trace objdump &optional transaction)
  (multiple-value-bind (start end)
      (prepare-consolidated-graph trace objdump transaction)
    (all-paths start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prune-bad-starts (list-of-paths)
  "The instruction list has to start with an out"
  (remove-if-not
   #'(lambda (path)
       (is-outgoing (second path)))
   list-of-paths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prune-incorrect-sequencing (list-of-paths)
  "Ins are followed by outs are followed by ins"
  (remove-if-not
   #'(lambda (path)
       (let ((pairs (batteries:gather-generator
                     (butlast (cdr path))
                     'batteries:sliding-window-2-wide
                     'batteries:sliding-chunker))
             ;; Not bad yet.
             (bad nil))


         (loop for (step next) in pairs
              do
              (cond ((is-outgoing step)
                     (unless (is-incoming next)
                       (setf bad t)))
                    ((is-incoming step)
                     (unless (is-outgoing next)
                       (setf bad t)))
                    (t
                     (error "+++Mr. Jelly! Mr. Jelly!+++" )))
              )
         (not bad)))
   list-of-paths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-feasible-paths-for-trace (trace objdump &optional transaction)
  (prune-incorrect-sequencing
   (prune-bad-starts
    (prune-too-short-paths
     (find-all-paths-for-trace trace objdump transaction)
     trace
    objdump
    transaction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod reset-linearization ((obj semantic-run))
  "Sets all the links for the non-instruction paths to nil"
  (loop for var in (container obj)
     do
       (setf (cpu-step-next var) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-sequence-from-path (list)
  "Returns a list of ids"
  (loop for node in list
     collect (cond ((eq (type-of node) 'START-NODE)
                    'START-NODE)
                   ((eq (type-of node) 'FINAL-NODE)
                    'END-NODE)
                   (t
                    (cpu-step-u-id node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun apply-linearization (list)
  " Presumes the linearization has been run and the list is coming
from all-paths.

Strips out all links except for the ones denoted by the order in the
list.

SIDE EFFECTS.

list ::= ( start-node cpu-step* final-node)
"
  ;;Remove start and end nodes
  (let* ((cpu-step-list  (loop for node in (butlast (cdr list))
                            collect (clone node)))
         (start-node (make-start-node))
         (final-node (make-final-node))
         ;;List of pairs
        (step-pairs
         (batteries:gather-generator
          cpu-step-list
          'batteries:sliding-window-2-wide
          'batteries:sliding-chunker)))


    (loop for (step next) in step-pairs
       do
         ;; Set up the link correctly
         (setf (cpu-step-next step)  (list next))
         (setf (cpu-step-next-inst step) nil))

    (setf (elt (start-node-core start-node) 0)
          (list (car cpu-step-list)))

    (setf (cpu-step-next (final cpu-step-list)) (list final-node))

    (cons start-node (cdr list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top level functions - entry points into the system


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generates the set of linearization graphs
(defun build-all-feasible-path-images (trace objdump png-prefix &optional transaction)
  (loop for path in (mapcar
                     'apply-linearization
                     (find-feasible-paths-for-trace trace objdump transaction))
     do
       (dump-graph (car path)
                   (format nil "~a-path-~a.png"
                           png-prefix
                           (funcall *counter-closure*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun render-linearized-paths (filename-prefix &optional transaction)
  "Only renderes the linearized path"
  (let ((trace (load-a-sim-file (strcat filename-prefix ".trace")))
        (objdump (load-xobjdump (strcat filename-prefix ".objdump"))))
    (build-all-feasible-path-images trace objdump filename-prefix transaction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun render-all-paths (filename-prefix output-png &optional transaction)
  "Only renders all paths"
  (let ((trace (load-a-sim-file (strcat filename-prefix ".trace")))
        (objdump (load-xobjdump (strcat filename-prefix ".objdump"))))
    (generate-consolidated-graph trace objdump output-png transaction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun render-trace (filename-prefix output-png &optional transaction)
  "Renders both the linearized and all paths pngs"
  (let ((trace (load-a-sim-file (strcat filename-prefix ".trace")))
        (objdump (load-xobjdump (strcat filename-prefix ".objdump"))))
    (build-all-feasible-path-images trace objdump output-png transaction)
    (generate-consolidated-graph trace objdump (format nil "~a-all.png" output-png) transaction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use when calling by #!/script
(defun main(args)
  (let ((fileprefix (first args))
        (transaction (second args)))
    (cond ((not fileprefix)
           (format t "Syntax: sim-reader.lisp prefix-of-files [t]~%"))
          (t
           (let ((paths-name (join-paths
                              (getcwd)
                              (strcat
                               (file-namestring fileprefix) ""))))
             (render-trace fileprefix
                           paths-name
                           transaction))))))

;(main (rest (batteries:getargs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging routines; they load data into the globals *trace* and
;;; *objdump* for inspection and analysis.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun load-exchange ()
;;   (setf *trace*
;;      (load-a-sim-file "../../programs_of_interest/one-transfer.trace"))
;;   (setf *objdump*
;;      (load-xobjdump "../../programs_of_interest/one-transfer.objdump")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun load-disjoint ()
;;   (setf *trace*
;;      (load-a-sim-file "../../programs_of_interest/disjoint.trace"))
;;   (setf *objdump*
;;      (load-xobjdump "../../programs_of_interest/disjoint.objdump")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun load-producer-consumer ()
;;   (setf *trace*
;;      (load-a-sim-file "../../programs_of_interest/simple-p-c.trace"))
;;   (setf *objdump*
;;      (load-xobjdump "../../programs_of_interest/simple-p-c.objdump")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun load-three-transfer ()
;;   (setf *trace*
;;      (load-a-sim-file "../../programs_of_interest/three-transfer.trace"))
;;   (setf *objdump*
;;      (load-xobjdump "../../programs_of_interest/three-transfer.objdump")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun load-multi-transfer ()
;;   (setf *trace*
;;      (load-a-sim-file "../../programs_of_interest/multi-transfer.trace"))
;;   (setf *objdump*
;;      (load-xobjdump "../../programs_of_interest/multi-transfer.objdump")))

;; (defun load-file (prefix)
;;   (setf *trace* (load-a-sim-file (strcat prefix ".trace")))
;;   (setf *objdump* (load-xobjdump (strcat prefix ".objdump"))))



;(generate-consolidated-graph *trace* *objdump* "/path/to/a/pngfile.png")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions for repl development


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clean-print ((step cpu-step) &key (all nil) (res-info nil))

  (labels
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((t-a (str)
         "Tab append. For setting the pretty-print stuff"
         (concatenate
          'string
          str
          "~1,7T")))

    (format t (t-a "~a") (u-id step))
    (format t (t-a "~a") (opcode step))
    (format t (t-a "R V ~a") (register-modifications step))

    (if all
        (format t (t-a "PC ~a") (pc step)))

    (format t (t-a "core ~a") (core step))
    (format t (t-a "cycle ~a") (cycle step))
    (if (out step)
        (format t (t-a "out ~a") (out step)))

    (if res-info
        (if (res-id step)
            (format t (t-a "res-id 0x~x") (res-id step))))

    (if (next-inst step)
        (format t (t-a "|> ~a")
                (loop for var in (next-inst step)
                   collect
                     (u-id var))))
    (if (next step)
        (format t (t-a "-> ~a")
                (loop for var in (next step)
                   collect
                     (if (eql (type-of var) 'CPU-STEP)
                       (u-id var)
                       'END-NODE))))

    (format t "~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clean-print ((obj final-node) &key)
  (format t "end~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clean-print ((obj start-node) &key)
  (format t "start~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clean-print ((obj semantic-run) &key (all nil) (res-info nil))
  (loop for id in (semantic-run-sequence obj)
     do
       (let ((step (semantic-run-get-id obj id)))
         (clean-print step :all all :res-info res-info))))
