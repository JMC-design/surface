(defpackage :surface 
  (:use :cl)
  (:shadow cl:map)
  (:export #:create
	   #:prepare
           #:destroy
                      
	   #:update
           #:map
	   #:unmap
	   #:visible?
                      
           #:size
	   #:resize
	   #:location
	   #:move

	   #:data
	   #:region

           #:clear
           #:pixel
           #:set-pixel
	   #:blit
		   
	   #:properties
           #:depth	   			
	   #:bpc
           #:bpp
	   
	   #:display
	   #:*available-surfaces*
           #:available?))

(in-package :surface)

(push :generic-surfaces cl:*features*)
(defparameter *available-surfaces* '() "assoc list of (:KEY . #'fn-that-returns-a-surface)")

(defgeneric prepare         (surface))
(defgeneric destroy         (surface))

(defgeneric update          (surface))
(defgeneric map             (surface))
(defgeneric unmap           (surface))
(defgeneric visible?        (surface))

(defgeneric size            (surface))
(defgeneric resize          (surface size))
(defgeneric location        (surface))
(defgeneric move            (surface location))

(defgeneric properties      (surface))
(defgeneric depth           (surface))
(defgeneric bpc             (surface));remove? add to properties?
(defgeneric bpp             (surface));remove? add to properties?

(defgeneric data            (surface &key &allow-other-keys))
(defgeneric region          (start-x start-y width height surface &optional format))

(defgeneric clear           (surface &optional colour))
(defgeneric pixel           (x y surface))
(defgeneric set-pixel       (x y ink surface))

(defgeneric blit            (source surface &optional src-x src-y dest-x dest-y src-width src-height))
(defgeneric display         (data surface pov &key &allow-other-keys))

;(defgeneric attach-glcontext (surface context))

;; responsibility of the caller to check if surfaces exist to be gotten.

(defsetf size resize)
(defparameter *environment->surface-type*
  (list
   '(x11 . :x-window)
   '(console . :fb32)
   '(unknown . :rgba32)))

(defun create (&key (type nil) (width 10) (height 10) (depth 32) (location '(0 . 0)) (override :on))
  (let* ((surface-type (or type (get-default-type)))
	 (fn (getf *available-surfaces* surface-type))
	 (surface (apply fn `(:width ,width :height ,height :depth ,depth :location ,location :override ,override))))
   #+elements(elements:register surface)
    (unless (eql type :x-pixmap) (prepare surface))
    surface))

(defun get-default-type ()
   (cdr (assoc (environment?) *environment->surface-type*)) )

(defun available? (&optional type)
  (if type
      (tag:get type *available-surfaces*)
      (loop :for (a b &rest) :on *available-surfaces* :by #'cddr :collect a)))

(defun environment? () 
  (if (find :linux *features*)
      ;; we're under linux, now distinguish x from console.
      (if (sb-posix:getenv "DISPLAY"); Do we detect a running server? make this more robust?
	  'x11
	  'console)
      'unknown))
