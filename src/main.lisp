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
    


(defun activate (app)
  "Open the main window."
  (let ((window (make-instance 'gtk:application-window
                               :application app :title "mandelbrot-gtk"
                               :default-width 1024 :default-height 768)))
    (gtk:window-set-child window (make-instance 'gtk:label :label "test"))
    (gtk:window-present window)))

(defun main ()
  "Run the application; returns when its last window closes. On macOS it
must run on the first thread: in Cadre, use Run GTK App."
  (let ((app (gtk:application-new "org.example.mandelbrot_gtk" '(:default-flags))))
    (gobject:connect app :activate 'activate)
    (gio:application-run app nil)))
