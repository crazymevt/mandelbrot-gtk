;;;; mandelbrot-gtk.asd

(defsystem "mandelbrot-gtk"
  :description "A Common Lisp project."
  :author "Jessie Hughart"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("gtk4")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "mandelbrot")
                             (:file "main"))))
  :entry-point "mandelbrot-gtk:main"
  :in-order-to ((test-op (test-op "mandelbrot-gtk/tests"))))

(defsystem "mandelbrot-gtk/tests"
  :description "Tests for mandelbrot-gtk."
  :author "Jessie Hughart"
  :license "MIT"
  :depends-on ("mandelbrot-gtk" "parachute")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :mandelbrot-gtk/tests)))
