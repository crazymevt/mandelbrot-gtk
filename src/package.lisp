;;;; package.lisp

(defpackage #:mandelbrot-gtk
  (:use #:cl)
  (:export #:mandelbrot-point #:main #:buffer->surface #:make-pixel))
