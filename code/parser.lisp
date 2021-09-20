(in-package #:sheyn)


(defclass fragment ()
  ((parent
     :accessor fragment-parent
     :initarg :parent
     :initform nil)
   (position
     :accessor fragment-position
     :initarg :position
     :initform nil)
   (value
     :accessor fragment-value
     :initarg :value
     :initform nil)
   (status
     :accessor fragment-status
     :initarg :status
     :initform nil)
   (children
     :accessor fragment-children
     :initarg :children
     :initform nil)
   (start
     :accessor fragment-start
     :initarg :start)
   (end
     :accessor fragment-end
     :initarg :end)))


(defclass symbol-name-fragment (fragment)
  ())


(defclass package-name-fragment (fragment)
  ())


(defclass package-marker-fragment (fragment)
  ())


(defclass symbol-fragment (fragment)
  ())


(defclass skipped-fragment (fragment)
  ())


(defmethod initialize-instance :after ((instance fragment) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (trivial-do:dolist* (position child (fragment-children instance))
    (setf (fragment-position child) position)
    (setf (fragment-parent child) instance)))


(defclass tracking-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((stream
     :reader tracking-stream-stream
     :initarg :stream)
   (line
     :accessor tracking-stream-line
     :initform 0)
   (column
     :accessor tracking-stream-column
     :initform 0)
   (previous-column
     :accessor tracking-stream-previous-column
     :initform 0)))


(defmethod trivial-gray-streams:stream-file-position ((stream tracking-stream))
  (file-position (tracking-stream-stream stream)))


(defmethod trivial-gray-streams:stream-listen ((stream tracking-stream))
  (listen (tracking-stream-stream stream)))


(defmethod trivial-gray-streams:stream-read-char ((stream tracking-stream))
  (let ((ch (read-char (tracking-stream-stream stream) nil :eof)))
    (cond
      ((eq :eof ch))
      ((char= ch #\Newline)
        (incf (tracking-stream-line stream))
        (setf (tracking-stream-previous-column stream) (tracking-stream-column stream)
              (tracking-stream-column stream) 0))
      ((char= ch #\Tab)
        (incf (tracking-stream-column stream) 8))
      ((graphic-char-p ch)
        (incf (tracking-stream-column stream))))
    ch))


(defmethod trivial-gray-streams:stream-peek-char ((stream tracking-stream))
  (peek-char nil (tracking-stream-stream stream) nil :eof))


(defmethod trivial-gray-streams:stream-unread-char ((stream tracking-stream) char)
  (cond
    ((char= char #\Newline)
      (decf (tracking-stream-line stream))
      (setf (tracking-stream-column stream) (tracking-stream-previous-column stream)))
    ((char= char #\Tab)
      (decf (tracking-stream-column stream) 8))
    ((graphic-char-p char)
      (decf (tracking-stream-column stream))))
  (unread-char char (tracking-stream-stream stream)))


(defmacro with-tracking-stream ((stream &rest rest) &body body)
  `(let ((,stream (make-instance 'tracking-stream :stream (open ,@rest))))
     (unwind-protect
         (progn
           ,@body)
       (close (tracking-stream-stream ,stream)))))


(defmethod eclector.base:source-position (client (stream tracking-stream))
  (cons (tracking-stream-line stream)
        (tracking-stream-column stream)))


(defclass tracking-client (eclector.parse-result:parse-result-client)
  ())


(defmethod eclector.parse-result:make-expression-result ((client tracking-client) result children source)
  (print source)
  (make-instance 'fragment :value result :start (car source) :end (cdr source) :children children))


(defun fragment-shift (frag offset)
  (setf (fragment-start frag) (cons (+ (car (fragment-start frag)) (car offset))
                                    (+ (cdr (fragment-start frag)) (cdr offset)))
        (fragment-end frag) (cons (+ (car (fragment-end frag)) (car offset))
                                  (+ (cdr (fragment-end frag)) (cdr offset))))
  (dolist (child (fragment-children frag))
    (fragment-shift child offset)))


(defmethod eclector.parse-result:make-expression-result ((client tracking-client) (result fragment) children source)
  (fragment-shift result (car source))
  result)


(defmethod eclector.parse-result:make-skipped-input-result ((client tracking-client) stream reason source)
  (declare (ignore stream))
  (print source)
  (make-instance 'skipped-fragment :status reason :start (car source) :end (cdr source)))


(defmethod eclector.reader:interpret-symbol-token ((client tracking-client) input-stream
                                   token
                                   position-package-marker-1
                                   position-package-marker-2)
  (cond
    (position-package-marker-2
      (make-instance 'symbol-fragment
                     :status :internal
                     :start (cons 0 0)
                     :end (cons 0 (length token))
                     :children (list (make-instance 'package-name-fragment
                                                    :value (subseq token 0 position-package-marker-1)
                                                    :start (cons 0 0)
                                                    :end (cons 0 position-package-marker-1))
                                     (make-instance 'package-marker-fragment
                                                    :value (subseq token position-package-marker-1 (1+ position-package-marker-2))
                                                    :start (cons 0 (1+ position-package-marker-2))
                                                    :end (cons 0 (length token)))
                                     (make-instance 'symbol-name-fragment
                                                    :value (subseq token (1+ position-package-marker-2))
                                                    :start (cons 0 (1+ position-package-marker-2))
                                                    :end (cons 0 (length token))))))
    (position-package-marker-1
      (make-instance 'symbol-fragment
                     :status :external
                     :start (cons 0 0)
                     :end (cons 0 (length token))
                     :children (list (make-instance 'package-name-fragment
                                                    :value (subseq token 0 position-package-marker-1)
                                                    :start (cons 0 0)
                                                    :end (cons 0 position-package-marker-1))
                                     (make-instance 'package-marker-fragment
                                                    :value (subseq token position-package-marker-1 (1+ position-package-marker-1))
                                                    :start (cons 0 position-package-marker-1)
                                                    :end (cons 0 (1+ position-package-marker-1)))
                                     (make-instance 'symbol-name-fragment
                                                    :value (subseq token (1+ position-package-marker-1))
                                                    :start (cons 0 (1+ position-package-marker-1))
                                                    :end (cons 0 (length token))))))
    (t
      (make-instance 'symbol-fragment
                     :status :local
                     :start (cons 0 0)
                     :end (cons 0 (length token))
                     :children (list (make-instance 'symbol-name-fragment
                                                    :value token
                                                    :start (cons 0 0)
                                                    :end (cons 0 (length token))))))))


(defun read-fragment (stream)
  (eclector.parse-result:read (make-instance 'tracking-client)
                              (make-instance 'tracking-stream :stream stream)
                              nil))

        
