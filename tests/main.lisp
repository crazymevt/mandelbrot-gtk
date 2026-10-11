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