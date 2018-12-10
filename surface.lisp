(defpackage :surface 
  (:use :cl)
  (:export #:prepare-surface
	   #:visible?
	   #:map-surface
	   #:unmap-surface
	   #:resize-surface
	   #:destroy-surface
	   #:update
	   #:network-update
	   #:local-update
	   #:display
	   #:*available-surfaces*
	   #:get-surface
	   #:get-data
	   #:surface-depth
	   ;#:move is in elements, placement of surface, responsibility of user, not surface.
	   ))
(in-package :surface)

(defparameter *available-surfaces* '() "assoc list of (:KEY . #'fn-that-returns-a-surface)")

(defgeneric prepare-surface (surface)   (:documentation "Prepares a surface to be displayed to"))
(defgeneric visible?        (item)      (:documentation "Returns T if item is visible."))  
(defgeneric map-surface     (surface)   (:documentation "Makes a hidden surface viewable."))
(defgeneric unmap-surface   (surface)   (:documentation "Makes a hidden surface viewable."))
(defgeneric resize-surface  (surface size)(:documentation "Resizes surface if possible, or returns a new one."))
(defgeneric surface-depth   (surface)   (:documentation "Returns depth of surface."))
(defgeneric destroy-surface (surface)   (:documentation "Cleans up everything tied to a surface."))
(defgeneric get-data        (surface)   (:documentation "Returns an array of pixels."))
(defgeneric update          (view)      (:documentation "just a simple updating surface with current data"))
(defgeneric network-update  (item)      (:documentation "Update item with network specific protocols."))
(defgeneric local-update    (item)      (:documentation "Update item with local protocols."))
(defgeneric display    (data surface)   (:documentation "displays/rasterizes data according to defaults onto the given surface"))

;; responsibility of the caller to check if surfaces exist to be gotten.
;; should location be here
(defun get-surface (&key (surface-type nil) (width 10) (height 10) (depth 24) (location '(0 . 0)))
  (let* ((surface-type (if surface-type surface-type (car *available-surfaces*)))
	 (fn (getf *available-surfaces* surface-type))
	 (surface (apply fn `(:width ,width :height ,height :depth ,depth :location ,location))))
    surface))
