;; -*- mode: Gimp; -*-
(define (script-fu-save-layers-to-files image dont-ask display-images)
  (let ((basename (car (gimp-image-get-filename image))))
    (when (string=? basename "")
	  (set! basename  (string-append 
			   (car (gimp-temp-name ""))
			   (car (gimp-image-get-name image)))))
    (let loop ((layers (vector->list (cadr (gimp-image-get-layers image)))))
      (unless (null? layers)
	      (gimp-edit-copy (car layers))
	      (let ((img  (car (gimp-edit-paste-as-new)))
		    (new-name (string-append 
			       basename
			       (car (gimp-drawable-get-name (car layers)))
			       ".png")))
		(file-png-save dont-ask
			       img
			       (aref (cadr (gimp-image-get-layers img)) 0)
			       new-name
			       new-name
			       TRUE 9 FALSE TRUE FALSE FALSE TRUE)
		(if (= FALSE display-images)
		    ;; clean up afterwards if we are not going to
		    ;; display the images anyway:
		    (gimp-image-delete img)
		    (gimp-display-new img)))
	      (loop (cdr layers))))))

(script-fu-register "script-fu-save-layers-to-files"
                    _"Save all layers to a different file"
                    _"Saves all layers to separate .png files"
                    "Niels Giesen (niels.giesen@gmail.com)"
                    "Niels Giesen"
                    "2008-10-10"
                    ""
		    SF-IMAGE	"Image"	1
		    SF-TOGGLE	_"Don't ask options for each layer" TRUE
		    SF-TOGGLE	_"Display images?" FALSE)

(script-fu-menu-register "script-fu-save-layers-to-files"
                         _"<Image>/Filters/Generic") 
