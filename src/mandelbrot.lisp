(in-package #:mandelbrot-gtk)

(declaim (inline mandelbrot-point))
(defun mandelbrot-point (cr ci max-iter)
  (declare (optimize speed)
           (type double-float cr ci)
           (type fixnum max-iter))
  (loop for i from 0 below max-iter
        for zr of-type double-float = 0d0 then (+ (- (* zr zr) (* zi zi)) cr)
        and zi of-type double-float = 0d0 then (+ (* 2 zr zi) ci)
        when (> (+ (* zr zr) (* zi zi)) 4d0) return i
        finally (return max-iter)))
