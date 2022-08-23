(in-package #:surface)

(defun document (type doc-strings)
  (dolist (doc-string doc-strings)
    (destructuring-bind (thing . doc) doc-string
      (case type
	(:functions  (setf (documentation (find-symbol (symbol-name thing)) 'function) (format nil doc)))
	(:package (setf (documentation (find-package (symbol-name thing)) t) (format nil doc)))))))

(document
 :package
 '((surface .
    "Provides a protocol to create, interact, and query surfaces.  Surfaces are things upon which data can be displayed. They can either be immediate or buffered.")))

(document
 :functions
 '((available? .
    "Checks to see if a given type of surface is available when supplied with a type.  With no arguments returns a list of all types available.")
   
   (create .
    "Creates and returns a surface. :type (:width 10) (:height 10) (:depth 32) :location '(0 . 0) (:override :on) may be specified")
   (prepare .
    "Prepares a surface to be displayed to. Normally called during creation, but can be used to prepare a surface provided by another source to be used ~
     with the protocol. e.g. return a buffered surface from an xlib:window.")
   (destroy .
    "Cleans up everything tied to a surface as well as removing it from display for screen mapped surfaces.")
   
   (map .
    "Maps a given surface to display.")
   (unmap .
    "Unmaps a surface, i.e. makes it invisible.")
   (visible? .
    "Returns T if surface is visible, otherwise nil.")
   (update .
    "Updates surface with current data, i.e. flushes any streams to immediate surfaces and flips backbuffer to front for buffered ones.")

   (size .
    "returns surface size as (cons width height).")
   (resize .
    "Resizes surface if possible, or returns a new one.")
   (location .
    "Location of surface on screen for screen mapped surfaces.") ; relative to screen or parent?
   (move .
    "Moves a surface to '(x . y) on screen for screen mapped surfaces.")

   (properties .
    "Returns interesting info about surface.")
   (depth .
    "Returns bit depth of surface.")
   (bpc .
    "Returns bits per channel.")
   (bpp .
    "Returns bits per pixel.")

   (data .
    "Returns a flat array of pixels.")  ;offer MxN arrays?
   (region .
    "Returns a rectangular region of the surface described by START-X START-Y WIDTH HEIGHT.")

   (clear .
    "Clears surface to a black matt, i.e., alpha 0 for surfaces with alpha, or optionally to supplied rgba colour in surface specific format, ~
     e.g. 8bit for framebuffer surfaces, 16bit for x11 surfaces.")
   (pixel .
    "Returns the pixel values at X Y in a surface specific format. May be set with SETF.")
   (set-pixel .
    "Sets the pixel at X Y with the given INK in surface specific format. e.g. 8bit framebuffer, 16bit X11. INK may also be ~
     A function that returns a colour when called with a given X Y, e.g. (funcall #'gradient 10 12)")
   (blit .
    "Blits the SOURCE to DESTINATION. Defaults to complete SOURCE to (0 . 0) on destination. Optionally specify SOURCE X Y, WIDTH, HEIGHT, and DESTINATION X Y.")

   (display .
    "Displays/rasterizes DATA onto the given SURFACE according to keyword :POV")))
