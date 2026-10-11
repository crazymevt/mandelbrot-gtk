# Makefile for mandelbrot-gtk. Needs Quicklisp, loaded from your Lisp's init file.

LISP ?= sbcl
RUN = $(LISP) --non-interactive --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)'

.PHONY: load test run dev build clean

# Load the system, fetching its dependencies
load:
	$(RUN) --eval '(ql:quickload :mandelbrot-gtk)'

# Run the tests; exits 1 if one fails
test:
	$(RUN) --eval '(ql:quickload :mandelbrot-gtk/tests)' \
		--eval '(uiop:quit (if (eq :passed (parachute:status (parachute:test :mandelbrot-gtk/tests))) 0 1))'

# Run the program (on the first thread, as GTK needs on macOS)
run:
	$(RUN) --eval '(ql:quickload :mandelbrot-gtk)' --eval '(mandelbrot-gtk:main)'

# Run it with a Swank server on port 4005, for an editor's REPL (in Cadre, use Run GTK App)
dev:
	$(LISP) --eval '(require :asdf)' --eval '(push (truename ".") asdf:*central-registry*)' \
		--eval '(ql:quickload (quote (:swank :mandelbrot-gtk)))' --eval '(swank:create-server :port 4005 :dont-close t)' \
		--eval '(mandelbrot-gtk:main)'

# Build the program: bin/mandelbrot-gtk (it needs GTK installed where it runs)
build:
	$(RUN) --eval '(ql:quickload :mandelbrot-gtk)' --eval '(gtk4:save-executable "bin/mandelbrot-gtk" (function mandelbrot-gtk:main))'

clean:
	find . -name '*.fasl' -delete
	rm -rf bin
