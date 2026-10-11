;;;; tests/main.lisp

(defpackage #:mandelbrot-gtk/tests
  (:use #:cl #:parachute #:mandelbrot-gtk))

(in-package #:mandelbrot-gtk/tests)

(define-test mandelbrot-gtk)

(define-test mandelbrot-point-test
    :parent mandelbrot-gtk
  (is = 256 (mandelbrot-point 0d0 0d0 256))
  (is = 256 (mandelbrot-point -1d0 0d0 256))
  (is = 3 (mandelbrot-point 1d0 0d0 256)))

(define-test buffer->surface-test
    :parent mandelbrot-gtk
  (let* ((width 3)
         (height 2)
         (buffer (make-array (* width height) :initial-element 0 :element-type '(unsigned-byte 32)))
         (surface (cairo:image-surface-create :argb32 width height)))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref buffer (+ (* y width) x)) (logior #xFF000000 (+ x (* y width))))))
    (buffer->surface buffer width height surface)
    (let ((data (cairo:image-surface-get-data surface))
          (stride (cairo:image-surface-get-stride surface)))
      (dotimes (x width)
        (dotimes (y height)
          (is = (aref buffer (+ (* y width) x))
              (cffi:mem-ref data :uint32 (+ (* y stride) (* x 4)))
              "pixel( ~d, ~d)" x y))))))

(define-test make-bytes-test
    :parent mandelbrot-gtk
  (is = #xFF123456 (make-pixel #x12 #x34 #x56))
  (is = #xFFFFFFFF (make-pixel 255 255 255))
  (is = #xFF000000 (make-pixel 0 0 0))
  (is = #xFF102030 (make-pixel #x10 #x20 #x30)))