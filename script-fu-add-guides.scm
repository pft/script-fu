;; -*- mode: Gimp; -*-
(define (script-fu-add-guides image drawable horizontal vertical)
  (let* ((dimensions (make-vector 2)))  

    (define (validate s)
      (when (not (re-match "^([ ]*[0-9]+%?[ ]*[, ])*([ ]*[0-9]+%?[ ]*,?)?[ ]*$" s))
	    (error (string-append 
		    "Invalid input: "
		    s
		    "

Only numbers and percentages allowed, separated by commas and/or
spaces (spaces at the sides of commas allowed, but not between a
number and a percentage sign).

Examples of correct input:

 50%, 75%, 200
 3 6 9 12 15 90% "))))

    (map validate (list horizontal vertical))
    
    (define (perc? s)
      (re-match "%$" s))

    (define (split-string s separators)

      (when (string? separators)
	    (set! separators (string->list separators)))

      (let loop ((pending nil)
		 (queue (string->list s))
		 (matches nil))
	(cond 
	 ((null? queue)
	  (if (null? pending)
	      (if (null? matches) matches
		  (nreverse matches))
	      (nreverse (cons (list->string (nreverse pending)) matches))))
	 ((memv (car queue) separators)
	  (loop nil
		(cdr queue)
		(if (null? pending)
		    matches
		    (cons (list->string (nreverse pending)) matches))))
	 (#t 
	  (loop (cons (car queue) pending)
		(cdr queue)
		matches)))))
    
    (define (perc-string->number s)
      (string->number
       (list->string 
	(nreverse (cdr (nreverse (string->list s)))))))

    (define (draw-guide guide direction)
      (let ((guide (string-trim guide))) ;get rid of spaces
	(if (perc? guide)
		(set! guide (/ (* (vector-ref dimensions direction)
				  (perc-string->number guide)) 100))
	    (set! guide (string->number guide)))
	
	(when (<= 0 guide (vector-ref dimensions direction))
	      ((if (= direction 0)
		   gimp-image-add-hguide
		   gimp-image-add-vguide) image guide))))
    
    (vector-set! dimensions 0 (car (gimp-image-height image)))

    (map (lambda (guide)
	   (draw-guide guide 0)) 
	 (split-string vertical ", "))

    (vector-set! dimensions 1 (car (gimp-image-width image)))
    
    (map (lambda (guide)
	   (draw-guide guide 1))
	 (split-string horizontal ", "))

    (gimp-displays-flush)))

 (script-fu-register "script-fu-add-guides"
		     _"Add a bunch of guides"
		     _"Add a bunch of guides input as comma or spaces-separated string"
		     "Niels Giesen (niels.giesen@gmail.com)"
		     "Niels Giesen"
		     "2009-05-27"
		     ""
		     SF-IMAGE	"Image"	1 
		     SF-DRAWABLE	"Drawable"	1 
		     SF-STRING	_"Horizontal guides"	"100,200,33%,66%"
		     SF-STRING	_"Vertical guides"	"25%,50%,75%")

(script-fu-menu-register "script-fu-add-guides"
                         _"<Image>/Image/Guides")