;;;; main.lisp — A Common Lisp project.

(in-package #:mandelbrot-gtk)

(defvar *view* nil)
(defvar *render* nil)
(sb-ext:defglobal **generation** 0)
(defconstant +block-size+ 8)

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
  (surface (error "render needs a :surface"))
  (drawn-rows 0 :type fixnum)
  (rows-done 0 :type sb-ext:word)
  (next-row 0 :type sb-ext:word))

(defparameter *palette-stops*
  '((0.0    0   7 100)    ; deep blue
    (0.16  32 107 203)
    (0.42 237 255 255)    ; near white
    (0.6425 255 170 0)    ; orange
    (0.8575 0   2   0)    ; almost black
    (1.0    0   7 100)))   ; same as the first: the cycle wraps without a seam


;;; Handlers are connected by symbol, so redefining ACTIVATE (or a
;;; function it calls) changes the running program.

(declaim (inline make-pixel))
(defun make-pixel (r g b)
  (declare (type (unsigned-byte 8) r g b))
  (logior 
   #xFF000000 ;; this is the alpha
   (ash r 16)
   (ash g 8)
   b))

(defun make-palette (stops size)
  (let ((palette (make-array size :element-type '(unsigned-byte 32))))
    (loop for i from 0 below size
          for pos = (/ i size)
          do (loop for (stop-a stop-b) on stops
                   for pos_a = (first stop-a)
                   for pos_b = (first stop-b)
                   while stop-b
                   when (and (>= pos pos_a) (< pos pos_b))
                   do (let* ((frac  (/ (- pos pos_a) (- pos_b pos_a)))
                             (r (round (+ (second stop-a) (* frac (- (second stop-b) (second stop-a))))))
                             (g (round (+ (third stop-a) (* frac (- (third stop-b) (third stop-a))))))
                             (b (round (+ (fourth stop-a) (* frac (- (fourth stop-b) (fourth stop-a)))))))
                        (setf (aref palette i) (make-pixel r g b)))))
    palette))
                                      
(defparameter *palette* (make-palette *palette-stops* 256))                                      
    

(defun fit-view (width height &key (max-iter 5000))
  (make-view :max-iter max-iter :scale (max (/ 3.5d0 width) (/ 2.5d0 height))))  

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
  
(declaim (inline iter->pixel))
(defun iter->pixel (n max-iter palette)
  (if (= n max-iter) 
      #xFF000000
      (aref palette (mod n (length palette)))))

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

(defun pan-view (view dx dy)
  (let* ((scale (view-scale view))
         (cx (- (view-cx view) (* scale dx)))
         (cy (+ (view-cy view) (* scale dy)))
         (new-view (copy-view view)))
    (setf (view-cx new-view) cx
          (view-cy new-view) cy)
    new-view))


(defun render-block-row (render y)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) y))
  (let* ((width (render-width render))
         (height (render-height render))
         (buffer (render-buffer render))
         (view (render-view render))
         (max-iter (view-max-iter view))
         (palette *palette*))
    (declare (type (simple-array (unsigned-byte 32) (*)) palette))
    (loop for x from 0 below width by +block-size+
          do (multiple-value-bind (cr ci) (pixel->point view width height x y)
               (let ((pixel (iter->pixel (mandelbrot-point cr ci max-iter) max-iter palette)))
                 (loop for block-y from 0 below +block-size+
                       for buffer-y = (+ block-y y)
                       when (< buffer-y height)
                       do (loop for block-x from 0 below +block-size+
                                for buffer-x = (+ x block-x)
                                when (< buffer-x width)
                                do (setf (aref buffer (+ (* buffer-y width) buffer-x)) pixel))))))))


(defun render-row (render y)
  (declare (optimize speed))
  (declare (type (unsigned-byte 16) y))
  (let* ((width (render-width render))
         (height (render-height render))
         (buffer (render-buffer render))
         (view (render-view render))
         (max-iter (view-max-iter view))
         (palette *palette*))
    (declare (type (simple-array (unsigned-byte 32) (*)) palette))
    (loop for x from 0 below width
          do (multiple-value-bind (cr ci) (pixel->point view width height x y)
               (setf (aref buffer (+ (* y width) x))
                     (iter->pixel (mandelbrot-point cr ci max-iter) max-iter palette))))))

(defun render-worker (render)
  (let* ((height (render-height render))
         (coarse-height (ceiling height +block-size+)))
    ;; this first loops over course jobs and then over fine jobs
    ;; existing at any point if a new generation has been added 
    ;; aka a user panned or zoomed
    (loop for y = (sb-ext:atomic-incf (render-next-row render))
          until (or (>= y (+ height coarse-height)) (> **generation** (render-generation render)))
          do (if (>= y coarse-height)
                 (progn 
                   ;; only wait if a new generation hasn't been addded
                   ;; or until all the course rows are complete
                   (loop until (or (>= (render-rows-done render) coarse-height) (> **generation** (render-generation render)))
                         do (sb-thread:thread-yield))
                   (render-row render (- y coarse-height)))
                 (render-block-row render (* y +block-size+)))          
          (sb-ext:atomic-incf (render-rows-done render)))))
                 
          

(defun start-render (view width height threads)
  (declare (type (unsigned-byte 16) width height))
  (incf **generation**)
  (let ((render (make-render :buffer (make-array (* width height) :initial-element #xFF000000 :element-type '(unsigned-byte 32))
                             :width width
                             :height height
                             :view view
                             :surface (cairo:image-surface-create :argb32 width height)
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
        (max-iter (view-max-iter view))
        (palette *palette*))
    (declare (type (simple-array (unsigned-byte 32) (*)) palette))
    (loop for y from 0 below height
          do
          (loop for x from 0 below width
                do (multiple-value-bind (cr ci) (pixel->point view width height x y)
                     (setf (aref buffer (+ (* y width) x))
                           (iter->pixel (mandelbrot-point cr ci max-iter) max-iter palette)))))
    buffer))


(defun draw (area cr width height)
  (declare (ignore area))
  (or *view* (setf *view* (fit-view width height)))
  (unless (and *render*
               (= width (render-width *render*))
               (= height (render-height *render*)))
    (start-render *view* width height 12))
  (let ((done (render-rows-done *render*)))
    (buffer->surface (render-buffer *render*) width height (render-surface *render*))
    (setf (render-drawn-rows *render*) done))
  (cairo:set-source-surface cr (render-surface *render*) 0 0)
  (cairo:paint cr))

(defun activate (app)
  "Open the main window."
  (let ((window (make-instance 'gtk:application-window
                               :application app :title "mandelbrot-gtk"
                               :default-width 1024 :default-height 768))
        (running t))
    (let ((area (gtk:drawing-area-new))
          (scroll-controller (gtk:event-controller-scroll-new '(:vertical)))
          (motion-controller (gtk:event-controller-motion-new))
          (drag-controller (gtk:gesture-drag-new))
          (drag-start-view nil)
          (mouse-x nil)
          (mouse-y nil))
      (gtk:window-set-child window area)
      (gtk:widget-add-controller area scroll-controller)
      (gtk:widget-add-controller area motion-controller)
      (gtk:widget-add-controller area drag-controller)
      
      (gtk:drawing-area-set-draw-func area 'draw)
      (glib:timeout-add glib:+priority-default+ 16
                        (lambda () 
                          (when (or (null *render*)
                                    (/= (render-rows-done *render*) (render-drawn-rows *render*)))
                            (gtk:widget-queue-draw area))
                          running))
    
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
      
      (gobject:connect drag-controller :drag-begin
                       (lambda (gesture x y)
                         (declare (ignore gesture x y))
                         (setf drag-start-view *view*)))
      
      (gobject:connect drag-controller :drag-update
                       (lambda (gesture dx dy)
                         (declare (ignore gesture))
                         (when drag-start-view
                           (setf *view* (pan-view drag-start-view dx dy))
                           (setf *render* nil))))

    
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
