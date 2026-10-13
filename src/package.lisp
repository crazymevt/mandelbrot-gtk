;;;; package.lisp

(defpackage #:mandelbrot-gtk
  (:use #:cl)
  (:export #:mandelbrot-point #:main #:buffer->surface #:make-pixel #:pixel->point #:make-view 
   #:fit-view #:view-scale #:view-cx #:view-cy #:iter->pixel #:zoom-view #:pan-view #:+block-size+ 
   #:render-buffer #:make-render #:render-block-row #:view-max-iter #:render-view #:make-palette #:*palette*
   #:lerp-pixel #:smooth-iter #:smooth-pixel))
