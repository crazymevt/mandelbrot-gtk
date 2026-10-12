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

(define-test make-pixel-test
    :parent mandelbrot-gtk
  (is = #xFF123456 (make-pixel #x12 #x34 #x56))
  (is = #xFFFFFFFF (make-pixel 255 255 255))
  (is = #xFF000000 (make-pixel 0 0 0))
  (is = #xFF102030 (make-pixel #x10 #x20 #x30)))

(define-test pixel->point-test
    :parent mandelbrot-gtk
  (is-values (pixel->point (make-view :scale 0.01d0) 100 100 60 50)
             (= -0.4d0)
             (= 0.0d0))
  (is-values (pixel->point (make-view :scale 0.01d0) 100 100 50 40)
             (= -0.5d0)
             (= 0.1d0))
  (is-values (pixel->point (make-view :scale 0.01d0) 100 100 50 60)
             (= -0.5d0)
             (= -0.1d0))
  (is-values (pixel->point (make-view :scale 0.0034d0) 1024 768 512 384)
             (= -0.5d0)
             (= 0d0)))

(define-test fit-view-test
    :parent mandelbrot-gtk
  (is = 0.00341796875d0 (view-scale (fit-view 1024 768)))
  (is = 0.035d0 (view-scale (fit-view 100 100)))
  
  (let* ((w 100)
         (h 100)
         (view (fit-view w h)))
    (multiple-value-bind (left top) (pixel->point view w h 0 0)
      (true (<= left -2.25d0))
      (true (>= top 1.25d0)))
    (multiple-value-bind (right bottom) (pixel->point view w h w h)
      (true (>= right 1.25d0))
      (true (<= bottom -1.25d0))))
 
  (dolist (size '((100 100) (1024 768) (100 400) (800 100)))
    (destructuring-bind (w h) size
      (multiple-value-bind (left top) (pixel->point (fit-view w h) w h 0 0)
        (true (<= left -2.25d0))
        (true (>= top 1.25d0)))
      (multiple-value-bind (right bottom) (pixel->point (fit-view w h) w h w h)
        (true (>= right 1.25d0))
        (true (<= bottom -1.25d0))))))

(define-test iter->pixel-test
    :parent mandelbrot-gtk
  (is = #xFF000000 (iter->pixel 256 256))
  (is = #xFF000000 (iter->pixel 0 0))
  (is = #xFF020202 (iter->pixel 3 256)))

(define-test zoom-view-test
    :parent mandelbrot-gtk
  (let* ((w 1024)
         (h 768)
         (view (fit-view w h))
         (zoom (zoom-view view w h 300 200 0.5d0)))
    (multiple-value-bind (zoom-x zoom-y) (pixel->point zoom w h 300 200)
      (multiple-value-bind (org-x org-y) (pixel->point view w h 300 200)
      (true (< (abs (- zoom-x org-x)) 1d-12))
      (true (< (abs (- zoom-y org-y)) 1d-12))))))

(define-test pan-view-test
    :parent mandelbrot-gtk
  (let* ((w 100)
         (h 100)
         (x 50)
         (y 50)
         (view (make-view :scale 1d0))
         (panned (pan-view view 10 5)))
    (multiple-value-bind (new-x new-y) (pixel->point panned w h (+ x 10) (+ y 5))
      (multiple-value-bind (org-x org-y) (pixel->point view w h x y)
      (true (< (abs (- new-x org-x)) 1d-12))
      (true (< (abs (- new-y org-y)) 1d-12))))))
