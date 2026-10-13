(in-package #:mandelbrot-gtk)

(declaim (inline mandelbrot-point))
(defun mandelbrot-point (cr ci max-iter &key (bailout 256d0))
  (declare (optimize speed)
           (type double-float cr ci bailout)
           (type fixnum max-iter))
  (loop for i from 0 below max-iter
        for zr of-type double-float = 0d0 then (+ (- (* zr zr) (* zi zi)) cr)
        and zi of-type double-float = 0d0 then (+ (* 2 zr zi) ci)
        when (> (+ (* zr zr) (* zi zi)) bailout) return (values i (+ (* zr zr) (* zi zi)))
        finally (return (values max-iter 0d0))))
