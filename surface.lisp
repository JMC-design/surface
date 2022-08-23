(defpackage :surface 
  (:use :cl)
  (:shadow cl:map)
  (:export #:%create
           #:create
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

           #:buffer
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

           #:attach
           #:detach
           
	   #:display
	   #:*available-surfaces*
           #:available?))

(in-package :surface)

(push :generic-surfaces cl:*features*)
(defparameter *available-surfaces* '() "assoc list of (:KEY . #'fn-that-returns-a-surface)")

(defgeneric %create         (type width height &key depth location &allow-other-keys))
(defgeneric prepare         (surface &key &allow-other-keys))
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

(defgeneric buffer          (surface &key &allow-other-keys))
(defgeneric data            (surface &key &allow-other-keys))
(defgeneric region          (start-x start-y width height surface &optional format type))

(defgeneric clear           (surface &optional colour))
(defgeneric pixel           (x y surface))
(defgeneric set-pixel       (x y ink surface))

(defgeneric blit            (source surface &optional src-x src-y dest-x dest-y src-width src-height))
(defgeneric display         (data surface pov &key &allow-other-keys))

(defgeneric attach (context surface))
(defgeneric detach (surface))

;; responsibility of the caller to check if surfaces exist to be gotten.

(defsetf size resize)
(defparameter *environment->surface-type*
  (list
   '(x11 . :x-window)
   '(console . :fb32)
   '(unknown . :rgba32)))

(defun create (width height &rest rest &key (type nil) (depth 32) (location '(0 . 0)) (override :on))
  (let* ((type (getf rest :type))
         (args (progn (remf rest :type) rest) )
         (surface (apply '%create (or type (get-default-type)) width height args)))
    #+elements(elements:register surface)
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
