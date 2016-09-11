;;; stream.el --- Lazy stream implementation of a stream  -*- lexical-binding: t; -*-
;;
;; Filename: stream.el
;; Description: Just a small stream implementation with a function
;; Author: Francis Murillo
;; Maintainer: Francis Murillo
;; Created: Sun Sep 11 21:54:13 2016 (+0800)
;; Version: 0.10
;; Package-Requires: (dash)
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'dash)


;;* Stream
(defconst stream-start 'stream-start
  "A value signifying the start of a stream.
Not to be used directly by the stream implementation.")

(defconst stream-stop 'stream-stop
  "A value signifying the end of a stream.
Should be used when the stream is done.")

(defun stream-start-value-p (value)
  "Check if VALUE is the start signal of a stream."
  (eq value stream-start))

(defun stream-stop-value-p (value)
  "Check if VALUE is the stop signal of a stream."
  (eq value stream-stop))

(defun stream (fn)
  "A simple stream or generator with FN function.
The function should not be idempotent and return `stream-stop'
when the function stops producing values."
  (lexical-let* ((streamer fn)
      (value stream-start))
    (lambda (&rest args)
      (cond
       ((stream-start-value-p value)
        (setq value (apply streamer args))
        stream-start)
       ((stream-stop-value-p value)
        stream-stop)
       (t
        (prog1
            value
          (setq value (apply streamer args))))))))

(defun stream-from-list (xs)
  "Create a stream from a list XS."
  (lexical-let ((ys xs))
    (stream
     (lambda (&rest _)
       (if (null ys)
           stream-stop
         (prog1
             (car ys)
           (setq ys (cdr ys))))))))

(defun stream-to-list (stream)
  "Unroll a STREAM for convenience."
  (let ((xs (list))
      (value (funcall stream)))
    (while (not (stream-stop-value-p value))
      (unless (stream-start-value-p value)
        (push value xs))
      (setq value (funcall stream)))
    (reverse xs)))


(defconst stream-empty 'stream-empty
  "A value indicating a stream is empty.
Not to be used directly.")

(defun stream-copy (empty-value stream)
  "Returns a cons pair with the car being a wrapped original stream
and the cdr being a stream reflecting the wrapped one."
  (lexical-let* ((stored-value stream-empty)
      (stream-values (list)))
    (cons
     (stream
      (lambda (&rest args)
        (when (not (eq stored-value stream-empty ))
          (setq stream-values (append stream-values (list stored-value))
             stored-value stream-empty))
        (lexical-let* ((value (apply stream args)))
          (when (stream-start-value-p value)
            (setq value (apply stream args)))
          (setq stored-value value)
          (when (stream-stop-value-p value)
            (setq stream-values (append stream-values (list value))))
          value)))
     (stream
      (lambda (&rest _)
        (if (null stream-values)
            empty-value
          (prog1
              (car stream-values)
            (setq stream-values (cdr stream-values)))))))))

(defun stream-stopped ()
  "A stream that always return the end of signal."
  (lambda (&rest _) stream-stop))

(defun stream-cycle (n stream)
  "Repeat the stream values N times of a STREAM."
  (lexical-let ((repeating nil)
      (counter 1)
      (stored-values (list))
      (current-values nil))
    (if (zerop n)
        (stream-stopped)
      (stream
       (lambda (&rest args)
         (if (null repeating)
             (lexical-let ((value (apply stream args)))
               (cond
                ((stream-start-value-p value)
                 (let ((next-value (apply stream args)))
                   (push next-value stored-values)
                   next-value))
                ((stream-stop-value-p value)
                 (setq repeating t
                    counter 2 ; Second cycle
                    stored-values (reverse stored-values)
                    current-values stored-values)
                 (when (>= (1- counter) n) ; Adjust cycle check
                   (setq stored-values nil
                      current-values nil))
                 (if (null current-values)
                     stream-stop
                   (prog1
                       (car current-values)
                     (setq current-values (cdr current-values)))))
                (t
                 (push value stored-values)
                 value)))
           (if (null current-values)
               (if (>= counter n)
                   stream-stop
                 (progn
                   (setq current-values stored-values
                      counter (1+ counter))
                   (prog1
                       (car current-values)
                     (setq current-values (cdr current-values)))))
             (prog1
                 (car current-values)
               (setq current-values (cdr current-values))))))))))

(defun stream-append (&rest streams)
  "Append several STREAMS in execution."
  (lexical-let* ((streams streams)
      (stream (car streams)))
    (stream
     (lambda (&rest args)
       (lexical-let ((value (apply stream args)))
         (while (and (not (null streams))
                   (or (stream-start-value-p value)
                      (stream-stop-value-p value)))
           (cond
            ((stream-start-value-p value)
             (setq value (apply stream args)))
            ((stream-stop-value-p value)
             (setq streams (cdr streams))
             (unless (null streams)
               (setq stream (car streams)
                  value (apply stream args))))))
         value)))))


(provide 'stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stream.el ends here
