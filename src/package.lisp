;;;; package.lisp

(defpackage #:mandelbrot-gtk
  (:use #:cl)
  (:export #:mandelbrot-point #:main #:buffer->surface #:make-pixel #:pixel->point #:make-view 
   #:fit-view #:view-scale #:iter->pixel))
