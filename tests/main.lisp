;;;; tests/main.lisp

(defpackage #:mandelbrot-gtk/tests
  (:use #:cl #:parachute #:mandelbrot-gtk))

(in-package #:mandelbrot-gtk/tests)

(define-test mandelbrot-gtk)

(define-test mandelbrot-point-test
    :parent mandelbrot-gtk
  (is = 256 (mandelbrot-point 0d0 0d0 256 :bailout 4d0))
  (is = 256 (mandelbrot-point -1d0 0d0 256 :bailout 4d0))
  (is = 3 (mandelbrot-point 1d0 0d0 256 :bailout 4d0))
  (multiple-value-bind (i z) (mandelbrot-point 0d0 0d0 256)
    (is = 256 i)
    (is = 0d0 z))
  (multiple-value-bind (i z) (mandelbrot-point 1d0 0d0 256)
    (is = 4 i)
    (is = 676d0 z)))

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
  (let* ((stops '((0 0 0 0) (1/2 255 255 255) (1 0 0 0)))
         (palette (make-palette stops 4)))
    (is = #xFF000000 (iter->pixel 256 256 palette))
    (is = #xFF000000 (iter->pixel 0 0 palette))
    (is = #xFF808080 (iter->pixel 3 256 palette))))

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

(defun test-render (w h)
  (make-render :buffer (make-array (* w h) :element-type '(unsigned-byte 32) :initial-element 0)
               :width w :height h
               :view (fit-view w h :max-iter 20)
               :surface nil
               :generation 0))

(defun collect-block-values-at (row render w h)
  "this function reads a block of pixels at row row and returns 
   all the values with duplicates removed."
  (remove-duplicates
   (loop for x from row below (+ row +block-size+)
         when (< x w)
         append (loop for y from row below (+ row +block-size+)
                      when (< y h)
                      collect (aref (render-buffer render) (+ (* y w) x))))))

(define-test render-block-row-test
    :parent mandelbrot-gtk
  (let* ((w 100)
         (h 100)
         (render (test-render w h))
         (max-iter (view-max-iter (render-view render))))
    (render-block-row render 0)
    (render-block-row render 96)
    
    ;; check row 0 blocks
    (multiple-value-bind (cr ci) (pixel->point (render-view render) w h 0 0)
      (let ((block-values (collect-block-values-at 0 render w h))
            (corner-pixel (multiple-value-bind (n mag2) (mandelbrot-point cr ci max-iter)
                            (smooth-pixel n mag2 max-iter palette density))))
        (is equal (list corner-pixel) block-values)))
    
    ;; check that row 8 blocks didn't get written to
    (is equal (list 0) (collect-block-values-at 8 render w h))

    ;; check row 96 blocks
    (multiple-value-bind (cr ci) (pixel->point (render-view render) w h 96 96)
      (let ((block-values (collect-block-values-at 96 render w h))
            (corner-pixel (multiple-value-bind (n mag2) (mandelbrot-point cr ci max-iter)
                            (smooth-pixel n mag2 max-iter palette density))))
        (is = corner-pixel (aref (render-buffer render) (+ (* 99 w) 99)))
        (is equal (list corner-pixel) block-values)))))

(define-test make-palette-test
    :parent mandelbrot-gtk
  (let* ((stops '((0 0 0 0) (1/2 255 255 255) (1 0 0 0)))
         (palette (make-palette stops 4)))
    (is = (make-pixel 0 7 100) (aref *palette* 0)) ; test our current palette first stop
    ;; test palette
    (is = 4 (length palette))
    (is = (make-pixel 255 255 255) (aref palette 2))))

(define-test lerp-pixel-test
    :parent mandelbrot-gtk
  (let ((p0 (make-pixel 0 0 0))
        (p1 (make-pixel 200 100 50))
        (frac 0.5d0))
    (is = (make-pixel 100 50 25) (lerp-pixel p0 p1 frac))
    (is = p0 (lerp-pixel p0 p1 0d0))
    (is = p1 (lerp-pixel p0 p1 1d0))))

(define-test smooth-iter-test
    :parent mandelbrot-gtk
  (true (< (abs (- (smooth-iter 5 256d0)
                   (smooth-iter 6 (* 256d0 256d0))))
           1d-9)))

(define-test smooth-pixel-test
    :parent mandelbrot-gtk
  ;; A uniform palette: any escaped point must get that color, whatever
  ;; its count, magnitude or density, so indexing and wrapping can't go wrong.
  (let* ((color (make-pixel 12 34 56))
         (uniform (make-array 8 :element-type '(unsigned-byte 32) :initial-element color)))
    (dolist (args '((0 300d0 1d0) (1 256.5d0 1d0) (50 1d6 1d0) (50 1d6 3.7d0) (999 1d4 0.25d0)))
      (destructuring-bind (n mag2 density) args
        (is = color (smooth-pixel n mag2 1000 uniform density)
            "n ~d, mag2 ~f, density ~f" n mag2 density)))
    (is = #xFF000000 (smooth-pixel 1000 0d0 1000 uniform 1d0) "inside the set"))
  ;; Blending: pick DENSITY so that idx lands on a known fraction.
  (let* ((c0 (make-pixel 0 0 0))
         (c1 (make-pixel 200 100 48))
         (palette (make-array 2 :element-type '(unsigned-byte 32) :initial-contents (list c0 c1)))
         (smooth (smooth-iter 10 1000d0)))
    ;; idx = 0.5: halfway from entry 0 to entry 1
    (is = (make-pixel 100 50 24) (smooth-pixel 10 1000d0 1000 palette (/ 0.5d0 smooth)))
    ;; idx = 1.25: from entry 1 wrapping to entry 0, a quarter of the way
    (is = (make-pixel 150 75 36) (smooth-pixel 10 1000d0 1000 palette (/ 1.25d0 smooth)))))