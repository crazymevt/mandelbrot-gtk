;;;; main.lisp — A Common Lisp project.

(in-package #:mandelbrot-gtk)

(defvar *view* nil)
(defvar *render* nil)
(sb-ext:defglobal **generation** 0)

(defstruct view
  (cx -0.5d0 :type double-float)
  (cy 0d0 :type double-float)
  (scale (error "A view needs :scale") :type double-float)
  (max-iter 256 :type (integer 1 1000000)))

(defstruct render
  (buffer (error "render needs :buffer") :type (simple-array (unsigned-byte 32) (*)))
  (width (error "render needs :width") :type (unsigned-byte 16))
  (height (error "render needs :height") :type (unsigned-byte 16))
  (view (error "render needs :view") :type view)
  (generation (error "render needs :generation") :type fixnum)
  (next-row 0 :type sb-ext:word))

;;; Handlers are connected by symbol, so redefining ACTIVATE (or a
;;; function it calls) changes the running program.

(defun fit-view (width height)
  (make-view :max-iter 5000 :scale (max (/ 3.5d0 width) (/ 2.5d0 height))))  

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

(declaim (inline iter->pixel))
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

(defun zoom-view (view width height mx my factor)
  (let* ((dx (- mx (/ width 2d0)))
         (dy (- my (/ height 2d0)))
         (shift-mx (* (view-scale view) dx))
         (shift-my (* (view-scale view) dy))
         (cr (+ (view-cx view) shift-mx))
         (ci (- (view-cy view) shift-my))
         (scale (* (view-scale view) factor))
         (cx (- cr (* scale dx)))
         (cy (+ ci (* scale dy)))
         (new-view (copy-view view)))
    (setf (view-cx new-view) cx
          (view-cy new-view) cy
          (view-scale new-view) scale)
    new-view))

(defun render-row (render y)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) y))
  (let* ((width (render-width render))
         (height (render-height render))
         (buffer (render-buffer render))
         (view (render-view render))
         (max-iter (view-max-iter view)))
    (loop for x from 0 below width
          do (multiple-value-bind (cr ci) (pixel->point view width height x y)
               (setf (aref buffer (+ (* y width) x))
                     (iter->pixel (mandelbrot-point cr ci max-iter) max-iter))))))

(defun render-worker (render)
  (let ((height (render-height render)))
    (loop for y = (sb-ext:atomic-incf (render-next-row render))
          until (or (>= y height) (> **generation** (render-generation render)))
          do (render-row render y))))

(defun start-render (view width height threads)
  (declare (type (unsigned-byte 16) width height))
  (incf **generation**)
  (let ((render (make-render :buffer (make-array (* width height) :initial-element 0 :element-type '(unsigned-byte 32))
                             :width width
                             :height height
                             :view view
                             :generation **generation**)))
    (setf *render* render)
    (values render
            (loop for thread from 0 below threads 
                  collect (bt:make-thread (lambda () 
                                            (render-worker render))
                                          :name (format nil "render-~d" thread))))))

(defun render-buffer-single (view width height)
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
  (let ((surface (cairo:image-surface-create :argb32 width height)))
    (or *view* (setf *view* (fit-view width height)))
    (unless (and *render*
                 (= width (render-width *render*))
                 (= height (render-height *render*)))
      (start-render *view* width height 12))
    (buffer->surface (render-buffer *render*) width height surface)
    (cairo:set-source-surface cr surface 0 0)
    (cairo:paint cr)))

(defun activate (app)
  "Open the main window."
  (let ((window (make-instance 'gtk:application-window
                               :application app :title "mandelbrot-gtk"
                               :default-width 1024 :default-height 768))
        (running t))
    (let ((area (gtk:drawing-area-new))
          (scroll-controller (gtk:event-controller-scroll-new '(:vertical)))
          (motion-controller (gtk:event-controller-motion-new))
          (mouse-x nil)
          (mouse-y nil))
      (gtk:window-set-child window area)
      (gtk:widget-add-controller area scroll-controller)
      (gtk:widget-add-controller area motion-controller)
      
      (gtk:drawing-area-set-draw-func area 'draw)
      (glib:timeout-add glib:+priority-default+ 16
                        (lambda () (gtk:widget-queue-draw area) running))
    
      (gobject:connect scroll-controller :scroll
                       (lambda (controller dx dy)
                         (declare (ignore controller dx))
                         (when (and *view* mouse-x mouse-y (not (zerop dy)))
                           (let ((factor (if (< dy 0) 0.8d0 1.25d0))
                                 (width (gtk:widget-get-width area))
                                 (height (gtk:widget-get-height area)))
                               (setf *view* (zoom-view *view* width height mouse-x mouse-y factor))
                               (setf *render* nil)
                               t))))
    
      (gobject:connect motion-controller :motion
                       (lambda (controller x y)
                         (declare (ignore controller))
                         (setf mouse-x x)
                         (setf mouse-y y)))
    
      (gobject:connect window :close-request (lambda (w) 
                                               (declare (ignore w)) 
                                               (setf running nil
                                                     *view* nil
                                                     *render* nil)
                                               nil))
      (gtk:window-present window))))

(defun main ()
  "Run the application; returns when its last window closes. On macOS it
must run on the first thread: in Cadre, use Run GTK App."
  (let ((app (gtk:application-new "org.example.mandelbrot_gtk" '(:default-flags))))
    (gobject:connect app :activate 'activate)
    (glib:with-gtk-float-traps
      (gio:application-run app nil))))
