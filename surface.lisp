(defpackage :surface 
  (:use :cl)
  (:export #:prepare-surface
	   #:visible?
	   #:map-surface
	   #:unmap-surface
	   #:size
	   #:resize-surface
	   #:location
	   #:move-surface
	   #:destroy-surface
	   #:properties
	   #:update
	   #:network-update
	   #:local-update
	   #:display
	   #:*available-surfaces*
	   #:get-surface
	   #:get-data
	   #:surface-depth
	   #:*network*
	   ;#:move is in ELEMENTS, placement of surface, responsibility of user, not surface.;cop out?
	   ))
(in-package :surface)

(defparameter *available-surfaces* '() "assoc list of (:KEY . #'fn-that-returns-a-surface)")
(defparameter *network* nil "Default to local connections. Bind to T for network connections.")

(defgeneric prepare-surface (surface)   (:documentation "Prepares a surface to be displayed to"))
(defgeneric visible?        (item)      (:documentation "Returns T if item is visible."))  
(defgeneric map-surface     (surface)   (:documentation "Makes a hidden surface viewable."))
(defgeneric unmap-surface   (surface)   (:documentation "Makes a hidden surface viewable."))
(defgeneric location        (surface)   (:documentation "Location of surface on screen."))
(defgeneric move-surface    (surface location)(:documentation "Moves a surface to '(x . y)"))
(defgeneric size            (surface)   (:documentation "returns surface size."))
(defgeneric resize-surface  (surface size)(:documentation "Resizes surface if possible, or returns a new one."))
(defgeneric surface-depth   (surface)   (:documentation "Returns depth of surface."))
(defgeneric destroy-surface (surface)   (:documentation "Cleans up everything tied to a surface."))
(defgeneric get-data        (surface)   (:documentation "Returns an array of pixels."))
(defgeneric update          (surface)   (:documentation "just a simple updating surface with current data"))
(defgeneric network-update  (surface)   (:documentation "Update item with network specific protocols."))
(defgeneric local-update    (surface)   (:documentation "Update item with local protocols."))
(defgeneric display    (data surface pov &key)   (:documentation "displays/rasterizes DATA onto the given SURFACE according to :POV"))
(defgeneric properties      (surface)   (:documentation "Returns interesting info about surface."))
;; responsibility of the caller to check if surfaces exist to be gotten.
;; should location be here
(defun get-surface (&key (type nil) (width 10) (height 10) (depth 24) (location '(0 . 0)) (override :on))
  (let* ((surface-type (or type (car *available-surfaces*)))
	 (fn (getf *available-surfaces* surface-type))
	 (surface (apply fn `(:width ,width :height ,height :depth ,depth :location ,location :override ,override))))
    surface))
(defmethod update (surface)
  (if *network* (network-update surface) (local-update surface)))
