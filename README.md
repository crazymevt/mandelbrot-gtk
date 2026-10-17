# mandelbrot-gtk

An interactive Mandelbrot set explorer written in Common Lisp, with a GTK4
interface and Cairo drawing.

## Features

- Scroll to zoom around the mouse pointer; drag to pan.
- Progressive rendering: an 8×8-block preview is refined to full resolution.
- Multithreaded rendering, with generation-based cancellation when the view changes.
- Smooth escape-time coloring and five palette definitions: original, fire
  (the default), ocean, neon, and viridis.

Palettes are currently configured in `src/main.lisp`, rather than through UI controls.
The renderer uses double-float coordinates, so deep zooms are limited by floating-point
precision.

## Requirements

- SBCL (the renderer uses SBCL-specific globals, atomics, and threading operations).
- Quicklisp, loaded from your Lisp initialization file for the Makefile commands.
- Native GTK4 libraries and their dependencies installed and discoverable by Lisp.
- Make for the commands below.

The ASDF system depends on `gtk4` and `bordeaux-threads`; tests also use `parachute`.

## Running

Run these commands from the project root:

```sh
make load     # load the system and its Lisp dependencies
make run      # start the explorer
make dev      # start with a Swank server on port 4005
make build    # build bin/mandelbrot-gtk
```

The built executable still requires native GTK libraries on the machine where it runs.

### In Cadre

On macOS, GTK must run on the process's first thread. Use **Run GTK App** in
Cadre to start it there and keep the REPL available. While it runs, evaluations
run on the GTK thread.

### From Lisp

Make the project discoverable by ASDF, then load and start it:

```lisp
(require :asdf)
(push (truename ".") asdf:*central-registry*) ; from the project root
(ql:quickload :mandelbrot-gtk)
(mandelbrot-gtk:main)
```

On macOS, call `main` on the first thread, not an editor's background REPL thread.
With `make dev`, wrap GTK calls evaluated from another editor's REPL in
`(glib:in-main-thread (:wait t) …)`.

## Controls

| Action | Control |
| --- | --- |
| Zoom in | Scroll up over the image |
| Zoom out | Scroll down over the image |
| Pan | Click and drag |

## Project layout

- `src/package.lisp` — package definition and exports.
- `src/mandelbrot.lisp` — Mandelbrot escape-time calculation.
- `src/main.lisp` — view transforms, palettes, rendering workers, and GTK interface.
- `tests/main.lisp` — Parachute tests for calculation, coloring, coordinates, and rendering helpers.
- `mandelbrot-gtk.asd` — application and test system definitions.

## Tests

```sh
make test
```

Or, after loading the project in Lisp:

```lisp
(asdf:test-system :mandelbrot-gtk)
```

## License

MIT.
