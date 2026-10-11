;;;; main.lisp — A Common Lisp project.

(in-package #:mandelbrot-gtk)

(defstruct view
  (cx -0.5d0 :type double-float)
  (cy 0d0 :type double-float)
  (scale (error "A view needs :scale") :type double-float)
  (max-iter 256 :type fixnum))

;;; Handlers are connected by symbol, so redefining ACTIVATE (or a
;;; function it calls) changes the running program.

(defun fit-view (width height)
  (make-view :scale (max (/ 3.5d0 width) (/ 2.5d0 height))))  

(defun pixel-offset (stride x y)
  (+ (* y stride) (* x 4)))
        
(defun buffer->surface (buffer width height surface)
  (cairo:surface-flush surface)
  (let ((stride (cairo:image-surface-get-stride surface))
        (data (cairo:image-surface-get-data surface)))
    (loop for y from 0 below height
          do (loop for x from 0 below width
                   do
                   (setf (cffi:mem-ref data :uint32 (pixel-offset stride x y))
                         (aref buffer (+ (* y width) x)))))
    (cairo:surface-mark-dirty surface)))
  
(declaim (inline make-pixel))
(defun make-pixel (r g b)
  (declare (type (unsigned-byte 8) r g b))
  (logior 
   #xFF000000 ;; this is the alpha
   (ash r 16)
   (ash g 8)
   b))

(defun iter->pixel (n max-iter)
  (let ((grey (if (= n max-iter) 
                  0
                  (floor (* 255 n) max-iter))))
    (make-pixel grey grey grey)))

(declaim (inline pixel->point))
(defun pixel->point (view width height x y)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) width height x y))
  (let* ((scale (view-scale view))
         (center-x (/ width 2d0))
         (center-y (/ height 2d0))
         (shift-x (* scale (- x center-x)))
         (shift-y (* scale (- y center-y))))
    (values (+ (view-cx view) shift-x)
          (- (view-cy view) shift-y))))


(defun render-buffer (view width height)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) width height))
  (let ((buffer (make-array (* width height) :initial-element 0 :element-type '(unsigned-byte 32)))
        (max-iter (view-max-iter view)))
    (loop for y from 0 below height
          do
          (loop for x from 0 below width
                do (multiple-value-bind (cr ci) (pixel->point view width height x y)
                (setf (aref buffer (+ (* y width) x))
                      (iter->pixel (mandelbrot-point cr ci max-iter) max-iter)))))
    buffer))

    
(defun draw (area cr width height)
  (declare (ignore area))
  (let ((surface (cairo:image-surface-create :argb32 width height))
        (view (fit-view width height)))
    (buffer->surface (render-buffer view width height) width height surface)
    (cairo:set-source-surface cr surface 0 0)
    (cairo:paint cr)))

(defun activate (app)
  "Open the main window."
  (let ((window (make-instance 'gtk:application-window
                               :application app :title "mandelbrot-gtk"
                               :default-width 1024 :default-height 768)))
    (let ((area (gtk:drawing-area-new)))
      (gtk:window-set-child window area)
      (gtk:drawing-area-set-draw-func area 'draw))
    (gtk:window-present window)))

(defun main ()
  "Run the application; returns when its last window closes. On macOS it
must run on the first thread: in Cadre, use Run GTK App."
  (let ((app (gtk:application-new "org.example.mandelbrot_gtk" '(:default-flags))))
    (gobject:connect app :activate 'activate)
    (glib:with-gtk-float-traps
      (gio:application-run app nil))))
