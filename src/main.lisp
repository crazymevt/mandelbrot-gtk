;;;; main.lisp — A Common Lisp project.

(in-package #:mandelbrot-gtk)


;;; Handlers are connected by symbol, so redefining ACTIVATE (or a
;;; function it calls) changes the running program.

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

(defun gradient-buffer (width height)
  (let ((buffer (make-array (* width height) :initial-element 0 :element-type '(unsigned-byte 32))))
    (loop for x from 0 below width
          for r = (if (> width 1) (round (* 255 (/ (float x) (1- width)))) 0)
          do (loop for y from 0 below height
                   for b = (if (> height 1) (round (* 255 (/ (float y) (1- height)))) 0)
                   do (setf (aref buffer (+ (* y width) x))
                            (make-pixel r 0 b))))
    buffer))
    
(defun draw (area cr width height)
  (declare (ignore area))
  (let ((surface (cairo:image-surface-create :argb32 width height)))
    (buffer->surface (gradient-buffer width height) width height surface)
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
