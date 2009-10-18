; -*- mode: Gimp; -*-
(define (random-in-range a b)
  (let ((r (random (max a b))))
    (if (>= r a)
	r
	(random-in-range a b))))

(define (script-fu-sunray img sun-radius amount start-angle randomize)

  (gimp-undo-push-group-start img)

  (let* ((img-width (car (gimp-image-width img)))
	 (img-height (car (gimp-image-height img)))
	 (radius (sqrt (* (pow (/ img-height 2) 2)
			  (pow (/ img-width 2) 2))))
	 (angle (/ 360 amount 2))
	 (offset-x (/ img-width 2))
	 (offset-y (/ img-height 2)))
    
    (letrec ((deg->rad (lambda (d)
			 (* (/ *pi* 180.0) d)))
	     (point-on-circle (lambda (r angle)
				(let ((angle (deg->rad angle)))
				  (list (round (* r (cos angle)))
					(round (* r (sin angle)))))))
	     (adjust (lambda (point) 
		       (list (+ (car point) offset-x)
			     (+ (cadr point) offset-y))))
	     (make-piece-of-pie (lambda (radius from-angle angle offset-x offset-y)
				  (append (adjust (point-on-circle radius from-angle))
					  (adjust (point-on-circle radius (+ from-angle angle)))
					  (list offset-x offset-y)))))

      (gimp-selection-none img)

      (let loop ((s-a start-angle)
		 (angle angle))
	(draw-figure img (make-piece-of-pie radius
					    s-a 
					    (if (= randomize TRUE) ;yuk yuk yuk! This is NOT why I use Lisp!
						(random-in-range 5 (- (* 2 angle) 5))
						angle)
					    offset-x offset-y))
	(when (< (+ s-a (* 2 angle)) (+ 360 start-angle))
	      (loop (+ s-a (* 2 angle)) angle offset-x offset-y)))
	
      (gimp-ellipse-select img  
			    (- (/ (car (gimp-image-width img))
				  2) 
			       sun-radius)
			    (- (/ (car (gimp-image-height img))
				  2) 
			       sun-radius)
			    (* 2 sun-radius)
			    (* 2 sun-radius)
			    CHANNEL-OP-SUBTRACT
			    FALSE
			    FALSE
			    0))

    (gimp-undo-push-group-end img)))

(script-fu-register "script-fu-sunray"
                    _"Sunray Pattern"
                    "Sets the selection to a sunray-like pattern."
                    "Niels Giesen (niels.giesen@gmail.com)"
                    "Niels Giesen"
                    "2008-04-27"
                    ""
		    SF-IMAGE      "Image to work on" 1
		    SF-ADJUSTMENT "_Radius of the sun (pixels)" '(35 0 1000 1 50 0 0)
		    SF-ADJUSTMENT "_Amount of rays" '(8 2 40 2 6 0 0)
		    SF-ADJUSTMENT "_Start angle degrees" '(0 0 90 1 15 0 0)
		    SF-TOGGLE     "Randomi_ze?" FALSE)

(script-fu-menu-register "script-fu-sunray"
                         _"<Image>/_Select")





