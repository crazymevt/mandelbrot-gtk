;;;; main.lisp — A Common Lisp project.

(in-package #:mandelbrot-gtk)


;;; Handlers are connected by symbol, so redefining ACTIVATE (or a
;;; function it calls) changes the running program.

(defun activate (app)
  "Open the main window."
  (let ((window (make-instance 'gtk:application-window
                               :application app :title "mandelbrot-gtk"
                               :default-width 480 :default-height 320)))
    (gtk:window-set-child window (make-instance 'gtk:label :label "test"))
    (gtk:window-present window)))

(defun main ()
  "Run the application; returns when its last window closes. On macOS it
must run on the first thread: in Cadre, use Run GTK App."
  (let ((app (gtk:application-new "org.example.mandelbrot_gtk" '(:default-flags))))
    (gobject:connect app :activate 'activate)
    (gio:application-run app nil)))
