;;; -*- Mode: Lisp; Package: INSPECT; Log:code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
#+cmu
(ext:file-comment
 "$Header: clx-inspector.lisp,v 1.1 2004/03/12 10:02:30 fmg $")
;;;
;;; **********************************************************************
;;;
;;; An inspector for CMU Common Lisp.
;;; 
;;; Written by Skef Wholey.
;;; Ported to CLX by Christopher Hoover with minor tweaks by Bill Chiles.
;;;
;;; Each Lisp object is displayed in its own X window, and components
;;; of each object are "mouse sensitive" items that may be selected
;;; for further investigation.
;;;
;;; Some cleanup by FMG plus adding dynamic updating of values when
;;; multiprocessing is present. (2000-2002)
;;;
;;; Converted former "home-made object system" to CLOS.  FMG Oct 2002.
;;;
;;; Fix inability to deal with circular lists. Paper over problem with
;;; PCL and uninitialized slots. FMG March 2004.
;;;
;;; Cleanup and minor fixes. FMG 2015. Haha.. ten years.. still works....
;;; Add scroll wheel support. FMG 2015.

(declaim (optimize (speed 2) (safety 3) (debug 3) (space 1.5) (ext:inhibit-warnings 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (provide :clx-inspector))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf lisp::*enable-package-locked-errors* nil))

(in-package "COMMON-LISP-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :clx #+cmu "library:subsystems/clx-library"))

(defpackage "INSPECT"
  (:use "COMMON-LISP" "LISP" "EXTENSIONS" "KERNEL")
  (:export inspect show-object remove-object-display remove-all-displays *interface-style*))

(in-package "INSPECT")


;;;; Parameters and stuff.

(defvar *inspect-result*)

(defparameter *update-interval* .5
  "Seconds between item window background updates.")

;;; CLX specials

(defvar *display* nil)
(defvar *screen* nil)
(defvar *root* nil)
(defvar *gcontext* nil)
(defvar *black-pixel* nil)
(defvar *white-pixel* nil)

;; Inspect-Length is the number of components that will be displayed in a
;; window at any one time.  If an object has more than Inspect-Length 
;; components, we generally put it in a scrolling window.  Inspect-Level
;; might someday correspond to Print-Level, controlling the amount of
;; detail and mouse-sensitivity we get inside components, but for now
;; it's ignored.
(defparameter inspect-length 30)
(defparameter inspect-level 1)

;; Inspect-Print-Level and Inspect-Print-Length are used by
;; IPrin1-To-String to generate the textual representation of
;; components.
(defparameter inspect-print-length 10)
(defparameter inspect-print-level 3)


;; The handler-case is an easy way to handle unbound slots. From what
;; previous versions said, using slot-boundp didn't always work.
(defun iprin1-to-string (object)
  (let ((*print-length* inspect-print-length)
	(*print-level* inspect-print-level)
	(*print-pretty* nil))

    (handler-case (prin1-to-string object)
        (unbound-slot () "Unbound"))))


;;;; Setting up fonts and cursors and stuff.

;; We use Font structures to keep stuff like the character height and
;; width of a font around for quick and easy size calculations. For
;; variable width fonts, the Width slot will be Nil.

(defstruct (font (:constructor make-font (name font height ascent width)))
  name
  font
  height
  ascent
  width)

;; The *Header-Font* is a big font usually used for displaying stuff
;; in the header portion of an object view. *Entry-Font* is used as
;; the main "body font" for an object, and *Italic-Font* is used for
;; special stuff.

;; You can go crazy with fonts here.
;;(defparameter header-font-name "*-*-bold-r-*-sans-14-*-*")
(defparameter header-font-name "-adobe-helvetica-bold-r-*-*-14-*-*")
(defvar *header-font*)

;; XXX You must use a fixed-width font here. Variable-width fonts
;; cause the tracking to fail miserably.
(defparameter entry-font-name "*-courier-medium-r-normal--12-*-*")
(defvar *entry-font*)

;; XXX Better to use a fixed-width font here --- a variable-width font
;; tends to result in bits and pieces of letters getting chopped off.
(defparameter italic-font-name "*-courier-medium-o-normal--12-*-*")
(defvar *italic-font*)

;; The *Cursor* is a normal arrow thing used most of the time. During
;; modification operations, we change the cursor to *Cursor-D* (while
;; the destination for the modification is being chosen) and
;; *Cursor-S* (while the source is being chosen).

(defparameter cursor-name "library:contrib/clx-inspector/inspect11.cursor")
(defvar *cursor*)
(defparameter cursor-d-name "library:contrib/clx-inspector/inspect11-d.cursor")
(defvar *cursor-d*)
(defparameter cursor-s-name "library:contrib/clx-inspector/inspect11-s.cursor")
(defvar *cursor-s*)

;; This file contains the help message for the inspector. The text in
;; the file must not extend past the 72nd column, and any initial
;; whitespace on a line must be built on the space character only. The
;; window that displays this text is too small in height for easy
;; reading of this text.
(defparameter help-file-pathname "library:contrib/clx-inspector/inspector.help")


;;;; CLX stuff

;; Max-Window-Width is used to constrain the width of our views.

(declaim (fixnum max-window-width))
(defparameter max-window-width 1000)

;; Border is the number of pixels between an object view and the box
;; we draw around it. VSP is the number of pixels we leave between
;; lines of text. (We should put VSP in the fonts structure sometime
;; so we can have font-specific vertical spacing.)

(defparameter border 3)
(defparameter vsp 2)

;; The arrow bitmaps are used inside scrollbars.

(defvar *up-arrow*)
(defvar *down-arrow*)
(defvar *up-arrow-i*)
(defvar *down-arrow-i*)

(defparameter arrow-bits
  '(#*0000000000000000
    #*0111111111111110
    #*0100000000000010
    #*0100000110000010
    #*0100001111000010
    #*0100011111100010
    #*0100111111110010
    #*0101111111111010
    #*0100001111000010
    #*0100001111000010
    #*0100001111000010
    #*0100001111000010
    #*0100001111000010
    #*0100000000000010
    #*0111111111111110
    #*0000000000000000))


;; Font and cursor support

(defun open-font (name)
  (let* ((font (xlib:open-font *display* name))
	 (max-width (xlib:max-char-width font))
	 (min-width (xlib:min-char-width font))
	 (width (if (= max-width min-width) max-width nil))
	 (ascent (xlib:max-char-ascent font))
	 (height (+ (xlib:max-char-descent font) ascent)))
    (make-font name font height ascent width)))

(defun get-cursor-pixmap-from-file (name)
  (let ((pathname (probe-file name)))
    (if pathname
	(let* ((image (xlib:read-bitmap-file pathname))
	       (pixmap (xlib:create-pixmap :width 16 :height 16
					   :depth 1 :drawable *root*))
	       (gc (xlib:create-gcontext :drawable pixmap
					 :function boole-1
					 :foreground *black-pixel*
					 :background *white-pixel*)))
	  (xlib:put-image pixmap gc image :x 0 :y 0 :width 16 :height 16)
	  (xlib:free-gcontext gc)
	  (values pixmap (xlib:image-x-hot image) (xlib:image-y-hot image)))
	(values nil nil nil))))

(defun open-cursor (name)
  (multiple-value-bind
      (cursor-pixmap cursor-x-hot cursor-y-hot)
      (get-cursor-pixmap-from-file name)
    (multiple-value-bind
	(mask-pixmap mask-x-hot mask-y-hot)
	(get-cursor-pixmap-from-file (make-pathname :type "mask" :defaults name))
      (declare (ignore mask-x-hot mask-y-hot))
      (let* ((white (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
	     (black (xlib:make-color :red 0.0 :green 0.0 :blue 0.0))
	     (cursor (xlib:create-cursor :source cursor-pixmap :mask mask-pixmap
					 :x cursor-x-hot :y cursor-y-hot
					 :foreground black :background white)))
	(xlib:free-pixmap mask-pixmap)
	(xlib:free-pixmap cursor-pixmap)
	cursor))))

(defun bitvec-list-to-pixmap (bvl width height)
  (let* ((image (apply #'xlib:bitmap-image bvl))
	 (pixmap (xlib:create-pixmap :width width :height height
				     :drawable *root*
				     :depth (xlib:screen-root-depth *screen*)))
	 (gc (xlib:create-gcontext :drawable pixmap
				   :function boole-1
				   :foreground *black-pixel*
				   :background *white-pixel*)))
    (xlib:put-image pixmap gc image :x 0 :y 0 :width 16 :height 16 :bitmap-p t)
    (xlib:free-gcontext gc)
    pixmap))

(defun invert-pixmap (pixmap)
  (let* ((width (xlib:drawable-width pixmap))
	 (height (xlib:drawable-height pixmap))
	 (inv-pixmap (xlib:create-pixmap :width width :height height
					 :drawable *root*
					 :depth (xlib:screen-root-depth *screen*)))
	 (gc (xlib:create-gcontext :drawable inv-pixmap
				   :function boole-c1
				   :foreground *black-pixel*
				   :background *white-pixel*)))
    (xlib:copy-area pixmap gc 0 0 width height inv-pixmap 0 0)
    (xlib:free-gcontext gc)
    inv-pixmap))

;;; Draw-Bitmap, Draw-Box, and Draw-Block --- thin wrapper over X
;;; drawing primitives.

(defun draw-bitmap (window x y pixmap)
  (xlib:copy-area pixmap *gcontext* 0 0 16 16 window x y))

(defun draw-box (window x1 y1 x2 y2)
  (declare (fixnum x1 y1 x2 y2))
  (xlib:draw-rectangle window *gcontext* x1 y1 (- x2 x1) (- y2 y1)))

(defun draw-block (window x1 y1 x2 y2)
  (declare (fixnum x1 y1 x2 y2))
  (xlib:draw-rectangle window *gcontext* x1 y1 (- x2 x1) (- y2 y1) t))

;;; *X-Constraint* is used by Disp-String to truncate long strings so that
;;; they stay inside windows of reasonable width.

(defvar *x-constraint* nil)

;;; Disp-String draws a string in an X window, trying to constrain it
;;; to not run beyond the *X-Constraint*.  For variable width fonts,
;;; we can only guess about the right length...

(defun disp-string (window x y string disp-font)
  (declare (simple-string string))
  (let ((font (font-font disp-font))
	(font-width (font-width disp-font))
	(font-height (font-height disp-font))
	(length (length string))
	(max-width (if *x-constraint* (- *x-constraint* x) max-window-width)))
    (cond (font-width
	   ;; fixed width font
	   (let ((end (if (<= (* length font-width) max-width)
			  length
			  (max 0 (truncate max-width font-width)))))
	     (when window
	       (xlib:with-gcontext (*gcontext* :font font)
		 (xlib:draw-image-glyphs window *gcontext*
					 x (+ y (font-ascent disp-font))
					 string :end end)))
	     (values (* end font-width) (+ font-height vsp))))
	  (t
	   ;; this is hackish...
	   (multiple-value-bind (end width)
	       (do* ((index length (1- index))
		     (width (xlib:text-width font string :end index)
			    (xlib:text-width font string :end index)))
		    ((or (= index 0) (<= width max-width))
		     (values index width)))
	     (when window
	       (xlib:with-gcontext (*gcontext* :font font)
		 (xlib:draw-image-glyphs window *gcontext*
					 x (+ y (font-ascent disp-font))
					 string :end end)))
	     (values width (+ font-height vsp)))))))



;;;; Inspect-Init

;;; Inspect-Init sets all this stuff up, using *Inspect-Initialized* to
;;; know when it's already been done.

(defvar *inspect-initialized* nil)

(defun inspect-init ()
  (unless *inspect-initialized*
    
    (multiple-value-setq (*display* *screen*) (ext:open-clx-display))
    (ext:carefully-add-font-paths
     *display*
     (mapcar #'(lambda (x)
		 (concatenate 'string (namestring x) "fonts/"))
 	     (ext:search-list "library:")))
    (setq *root* (xlib:screen-root *screen*))
    (setq *black-pixel* (xlib:screen-black-pixel *screen*))
    (setq *white-pixel* (xlib:screen-white-pixel *screen*))
    (setq *gcontext* (xlib:create-gcontext :drawable *root* :function boole-1
 					   :foreground *black-pixel*
 					   :background *white-pixel*))
    (setq *cursor* (open-cursor cursor-name))
    (setq *cursor-d* (open-cursor cursor-d-name))
    (setq *cursor-s* (open-cursor cursor-s-name))
    (setq *header-font* (open-font header-font-name))
    (setq *entry-font* (open-font entry-font-name))
    (setq *italic-font* (open-font italic-font-name))
    (setq *up-arrow* (bitvec-list-to-pixmap arrow-bits 16 16))
    (setq *up-arrow-i* (invert-pixmap *up-arrow*))
    (setq *down-arrow* (bitvec-list-to-pixmap (reverse arrow-bits) 16 16))
    (setq *down-arrow-i* (invert-pixmap *down-arrow*))
    (ext:enable-clx-event-handling *display* 'inspector-event-handler)
    (setq *inspect-initialized* t)))

#|
;;; For debugging...
;;; 
(defun inspect-reinit (&optional (host "unix:0.0"))
  (let ((win nil))
    (setq *inspect-initialized* nil)
    (when *display*
      (ext:disable-clx-event-handling *display*)
      (xlib:close-display *display*)))
    (unwind-protect
	(progn
	  (multiple-value-setq
	      (*display* *screen*)
	    (ext:open-clx-display host))
	  (setf (xlib:display-after-function *display*)
		#'xlib:display-finish-output)
	  (setq *root* (xlib:screen-root *screen*))
	  (setq *black-pixel* (xlib:screen-black-pixel *screen*))
	  (setq *white-pixel* (xlib:screen-white-pixel *screen*))
	  (setq *gcontext* (xlib:create-gcontext :drawable *root*
						 :function boole-1
						 :foreground *black-pixel*
						 :background *white-pixel*))
	  (setq *cursor* (open-cursor cursor-name))
	  (setq *cursor-d* (open-cursor cursor-d-name))
	  (setq *cursor-s* (open-cursor cursor-s-name))
	  (setq *header-font* (open-font header-font-name))
	  (setq *entry-font* (open-font entry-font-name))
	  (setq *italic-font* (open-font italic-font-name))
	  (setq *up-arrow* (bitvec-list-to-pixmap arrow-bits 16 16))
	  (setq *up-arrow-i* (invert-pixmap *up-arrow*))
	  (setq *down-arrow* (bitvec-list-to-pixmap (reverse arrow-bits) 16 16))
	  (setq *down-arrow-i* (invert-pixmap *down-arrow*))
	  (setf (xlib:display-after-function *display*) nil)
	  (setf win t))
      (cond (win
	     (ext:enable-clx-event-handling *display* 'inspector-event-handler)
	     (setq *inspect-initialized* t))
	    (*display*
	     (xlib:close-display *display*))))))
|#


;;;; Mid-level interface between inspector and window system.

(defclass view ()
  ((name :initarg :name :accessor name)
   (object :initarg :object :accessor object)
   (view-item :initarg :view-item :accessor view-item)
   (window :initarg :window :accessor window)
   #+:mp (update-process :initarg :update-process :accessor update-process :initform nil)
   (stack :initarg :stack :accessor stack :initform nil))
  (:documentation "We use view classes to associate objects with their
graphical images (View-Items, see below), the X windows that they're
displayed in, and maybe even a user-supplied Name for the whole
thing."))

#+:mp
(defun make-view (name object view-item window)
  (let* ((new-view (make-instance 'view
				  :name name
				  :object object
				  :view-item view-item
				  :window window)))
    ;; Create a background process to update the view once per second.
    (setf (update-process new-view)
	  (mp:make-process
	   #'(lambda ()
	       (loop
		  (update-view-of-object new-view)
		  (sleep *update-interval*)))
	   :name (format nil "Background update process for ~A" name)))
    new-view))

#-:mp
(defun make-view (name object view-item window)
  (make-instance 'view
		 :name name
		 :object object
		 :view-item view-item
		 :window window))


;;; *views* is a list of all the live views of objects.
;;;
(defvar *views* nil)

;;; CLX window to view object mapping.
;;;
(defvar *windows-to-views* (make-hash-table :test #'eq))

(defun add-window-view-mapping (window view)
  (setf (gethash window *windows-to-views*) view))

(defun delete-window-view-mapping (window)
  (remhash window *windows-to-views*))

(defun map-window-to-view (window)
  (multiple-value-bind (view found-p)
      (gethash window *windows-to-views*)
    (unless found-p (error "No such window as ~S in mapping!" window))
    view))

;; *Tracking-Mode* is a kind of hack used so things know what to do
;; during modify operations. If it's :Source, only objects that are
;; really there will be selectable. If it's :Destination, objects that
;; aren't necessarily really there (like the values of unbound
;; symbols) will be selectable.
(declaim (type (member '(:source :destination) *tracking-mode*)))
(defvar *tracking-mode* :source)

;; *Mouse-X* and *Mouse-Y* are a good approximation of where the mouse
;; is in the window that the mouse is in.

(declaim (fixnum *mouse-x* *mouse-y*))
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)


;;;; Event Handling for CLX. Translates events in X windows to
;;;; commands operating on views.

;; We're interested in these events:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant important-xevents
    '(:key-press :button-press :exposure :pointer-motion
		 :enter-window :leave-window #+notready :structure-notify))
  
  (defconstant important-xevents-mask
    (apply #'xlib:make-event-mask important-xevents)))


;; We need to add some mouse key translations to handle the scroll
;; wheel. XXX These should be in CMUCL, not here.

(ext:define-mouse-keysym 4 25607 "Scrollupdown" "Super" :button-press)
(ext:define-mouse-keysym 4 25608 "Scrollupup" "Super" :button-release)

(ext:define-mouse-keysym 5 25609 "Scrolldowndown" "Super" :button-press)
(ext:define-mouse-keysym 5 25610 "Scrolldownup" "Super" :button-release)


(defun inspector-event-handler (display)
  (xlib:event-case (display :discard-p t :force-output-p t :timeout .1)
    ((:exposure) (event-window count)
     (when (zerop (the fixnum count))
       (redisplay-item
	(view-item (map-window-to-view event-window))))
     t)
    ((:key-press) (event-window state code)
     (do-command (map-window-to-view event-window)
		 (ext:translate-key-event display code state))
     t)
    ((:button-press :button-release) (event-key event-window state code)
     (do-command (map-window-to-view event-window)
		 (ext:translate-mouse-key-event code state event-key))
     t)
    ((:enter-notify :motion-notify) (event-window x y)
     (cond ((xlib:event-listen display)
	    ;; if there are other things in the queue, blow this event off...
	    nil)
	   (t
	    ;; This is the alternative to the background update
	    ;; process. When the mouse enters the window, its values
	    ;; get updated.
	    #-:mp (update-view-of-object (map-window-to-view event-window))
	    (setf *mouse-x* x)
	    (setf *mouse-y* y)
	    (tracker (view-item (map-window-to-view event-window)) x y)
	    t)))
    ((:leave-notify) (event-window)
     (tracker (view-item (map-window-to-view event-window)) -1 -1)
     t)

    ((:no-exposure) ()
     ;; just ignore this one
     t)
    ((:client-message) (event-window display data)
     ;; User used the window manager to close a window.
     (when (eq (xlib:atom-name display (aref data 0)) :wm_delete_window)
       ;; Make the program think the user hit the "D" key in the event
       ;; window.
       (do-command (map-window-to-view event-window) #k"D"))
     t)
    (t (event-key)
       (format t "Inspector received unexpected event, ~S, recieved." event-key)
       t)))

#|

;;; Some debugging code...

    (xlib:event-cond (display :timeout 0 :peek-p t)
		     (t (event-key)
			(unless (eq event-key :motion-notify)
			  (format t "Event received: ~S~%" event-key))))

(defun discard-event-on-window (display window type)
  (loop
    (unless (xlib:process-event display :timeout 0
	      :handler #'(lambda (&key event-window event-type &allow-other-keys)
			   (and (eq event-window window)
				(eq event-type type))))
      (return))))

|#
    

;;;; More stuff that interfaces between X and the view stuff.

;; NEXT-WINDOW-POSITION currently uses a very dumb heuristic to decide
;; where the next inspector window ought to go. If there aren't any
;; windows, it puts the view of an object in the upper left hand
;; corner. Otherwise, it'll put it underneath the last one created.
;; When putting the new window below the last one, if it should extend
;; below the bottom of the screen, we position it to just fit on the
;; bottom. Thus, all future windows created in this fashion will "pile
;; up" on the bottom of the screen.
;;
(defun next-window-position (width height)
  (declare (ignore width))
  (if *views*
      (let ((window (window (car *views*))))
	(xlib:with-state (window)
	  (let ((drawable-x (xlib:drawable-x window))
		(drawable-y (xlib:drawable-y window))
		(drawable-height (xlib:drawable-height window))
		(border-width (xlib:drawable-border-width window)))
	    (declare (fixnum drawable-y drawable-height border-width))
	    (multiple-value-bind (children parent root) (xlib:query-tree window)
	      (declare (ignore children))
	      (let ((root-height (xlib:drawable-height root)))
		(declare (fixnum root-height))
		(multiple-value-bind
		    (new-x new-y)
		    (if (eq parent root)
			(values drawable-x (+ drawable-y drawable-height
					      (* 2 border-width)))
			;; Deal with reparented windows...
			(multiple-value-bind (root-x root-y)
					     (xlib:translate-coordinates
					      parent drawable-x drawable-y root)
			  (declare (fixnum root-y))
			  (values root-x (+ root-y drawable-height
					    (* 2 border-width)))))
		  (declare (fixnum new-y))
		  (values new-x
			  (if (> (+ new-y height border-width) root-height)
			      (- root-height height border-width)
			      new-y))))))))
      (values 200 20)))


;;;; View-Item.  A view item is the object that contains the actual
;;;; underlying object being inspected as well as the window being
;;;; used to display it and some other information about the window.

(defclass view-item ()
  ((window :initarg :window :accessor window)
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height))
  (:documentation "View-Items are objects with methods to display
themselves, track the mouse inside their boundries, handle mouse
clicks on themselves, and so on. Everything we put up on the screen is
backed in some way by a View-Item. These are the components of the
total view of an object as described in a view object."))

(defmethod print-object ((item view-item) stream)
  (format stream "#<~S {~8,'0X}>" (type-of item)
	  (kernel:get-lisp-obj-address item)))
	  
(defgeneric view-item-p (item)
  (:method ((item t))
	   nil)
  (:method ((item view-item))
	   t))

;; The following generic functions constitute the interface to the
;; view-item objects. Subclasses of view-item implement behavior by
;; overriding these methods.

(defgeneric display (item window x y))

(defgeneric tracker (item x y)
  (:method ((item view-item) x y)
	   (update-current-item item x y)))

(defgeneric untracker (item)
  (:method ((item view-item))
	   nil))

(defgeneric mouse-handler (item view key-event)
  (:method ((item view-item) view key-event)
	   (declare (ignore view key-event))
	   nil))

(defgeneric walker (item function)
  (:method ((item view-item) function)
	   (declare (ignore function))
	   nil))


;;;; The following are functions that apply to all view-items.

;; The *Current-Item* is the view item that is currently under the
;; mouse, to the best of our knowledge, or Nil if the mouse isn't over
;; an item that does anything with its Tracker method.

(defvar *current-item* nil)

;; Display-Item invokes the Display method of an item to put it up on
;; the specified window. The window, position, and size are all set,
;; and the size is returned.

(defun display-item (item window x y)
  (setf (window item) window
	(x item) x
	(y item) y)
  (multiple-value-bind (width height)
      (display item window x y)
    (setf (width item) width)
    (setf (height item) height)
    (values width height)))

;; Redisplay-Item redraws an item (if, say, it's changed, or if its
;; window has received an exposure event). If the item is the
;; *Current-Item*, we call its tracker method to make sure it gets
;; highlighted if it's supposed to be.

(defun redisplay-item (item)
  (when (window item)
    (xlib:clear-area (window item)
		     :x (x item) :y (y item)
		     :width (width item)
		     :height (height item))
    (multiple-value-bind (width height)
	(display item (window item) (x item) (y item))
      (setf (width item) width)
      (setf (height item) height))
    (xlib:display-force-output *display*)
    (when (and *current-item*
	       (eq (window *current-item*)
		   (window item)))
      (tracker *current-item* *mouse-x* *mouse-y*))))

;; Size-Item uses the Display method to calculate the size of an item
;; once displayed. If the window supplied to View-Item is Nil, all the
;; size calculation will get done, but no graphical output will
;; happen.

(defun size-item (item)
  (if (slot-boundp item 'width)
    (values (width item) (height item))
    (display-item item nil 0 0)))


;;;; Tracking and untracking.

;; Update-Current-Item is used by trackers to figure out if an item is
;; really under the mouse. If it is, and it's not the same as the
;; *Current-Item*, the *Current-Item* gets untracked. If the mouse is
;; inside the current item, Update-Current-Item returns T.

(defun update-current-item (item x0 y0)
  (let ((old-current *current-item*))
    (with-slots (x y width height) item
    (if (and (<= x x0 (+ x width))
	     (<= y y0 (+ y height)))
      (setq *current-item* item)
      (setq *current-item* nil))
    (when (and old-current (not (eq *current-item* old-current)))
      (untracker old-current)))
    (eq item *current-item*)))

;; The Boxifying-Tracker and Boxifying-Untracker highlight and
;; unhighlight an item by drawing or erasing a box around the object.

(defun boxifying-tracker (item x y)
  (when (update-current-item item x y)
    (boxify-item item boole-1)))

(defun boxifying-untracker (item)
  (boxify-item item boole-c1))

(defun boxify-item (item function)
  (when (view-item-p item)
    (with-slots (x y width height window) item
      (xlib:with-gcontext (*gcontext* :function function)
	(xlib:draw-rectangle window *gcontext* (1- x) y (1+ width) (- height 2)))
      (xlib:display-force-output *display*))))

;; Track-In-List tries to track inside of each item in the List.

(defun track-in-list (list x0 y0)
  (dolist (item list)
    (when (view-item-p item)
      (with-slots (x y width height) item
	(when (and (<= x x0 (+ x width))
		   (<= y y0 (+ y height)))
	  (tracker item x0 y0)
	  (return-from track-in-list nil)))))
  (when *current-item*
    (untracker *current-item*)
    (setq *current-item* nil)))


;;;; Specialized View-Item definitions.

(defclass inspection-item (view-item)
  ((objects :initarg :objects :accessor objects)  ; Objects being inspected (for decaching)
   (headers :initarg :headers :accessor headers)  ; List of items in header, may be Nil
   (entries :initarg :entries :accessor entries)) ; List of items below header
  (:documentation "Inspection-Items are used as the `top-level' items
in the display of an object. They've got a list of header items and a
list of entry items."))

(defun make-inspection-item (objects headers entries)
  (make-instance 'inspection-item :objects objects :headers headers :entries entries))

;; Inspection item methods

(defmethod display ((item inspection-item) window x0 y0)
  (let ((y (+ y0 border))
	(x (+ x0 border))
	(max-width 0)
	(max-x 0)
	(first-entry-y nil)
	(header-end-y nil)
	(sb (when (scrolling-inspection-item-p item)
	      (scrollbar item))))
    (when sb
      (funcall (reset-index sb) sb))
    ;; First, header items.
    (when (headers item)
      (dolist (element (headers item))
	(multiple-value-bind (width height)
			     (display-item element window x y)
	  (incf y height)
	  (setq max-width (max max-width width))))
      (setq header-end-y y)
      (incf y vsp))
    (when sb
      (incf x (+ 16 border))
      (funcall (reset-index sb) sb))
    ;; Then do entry items.
    (let ((max-name-width 0))
      (setq first-entry-y y)
      ;; Figure out width of widest entry slot name.
      (dolist (element (entries item))
	(when (slot-item-p element)
	  (setq max-name-width
		(max max-name-width (length (name element))))))
      (dolist (element (entries item))
	(when (slot-item-p element)
	  (unless (slot-boundp element 'max-name-width)
	    (setf (max-name-width element) max-name-width)))
	(multiple-value-bind (width height)
			     (display-item element window x y)
	  (incf y height)
	  (setq max-width (max max-width (+ width (if sb (+ 16 border) 0)))))))
    (setq max-x (+ x0 border max-width border))
    ;; Display scrollbar, if any.
    (when sb
      (setf (bottom sb) y)
      (display-item sb window (+ x0 border) first-entry-y)
      (unless (slot-boundp sb 'window-width)
	(setf (window-width sb) (- max-width 16 border))))
    ;; Finally, draw a box around the whole thing.
    (when window
      (draw-box window x0 y0 max-x y)
      (when header-end-y
	(xlib:draw-line window *gcontext* x0 header-end-y max-x header-end-y)))
    ;; And return size.
    (values (- max-x x0) (- (+ y border) y0))))

(defmethod tracker ((inspection-item inspection-item) x0 y0)
  (dolist (item (headers inspection-item))
    (with-slots (x y width height) item
      (when (and (<= x x0 (+ x width))
		 (<= y y0 (+ y height)))
      (tracker item x0 y0)
      (return-from tracker nil))))
  (track-in-list (entries inspection-item) x0 y0))

(defmethod walker ((item inspection-item) function)
  (flet ((walk-item-list (list function)
	   (dolist (item list)
	     (walker item function))))
    (with-slots (x width) item
      (let ((*x-constraint* (if (slot-boundp item 'width)
			      (+ x width (- border))
			      max-window-width)))
	(walk-item-list (headers item) function)
	(walk-item-list (entries item) function)))))


(defclass scrolling-inspection-item (inspection-item)
  ((scrollbar :initarg :scrollbar :accessor scrollbar) ; Scrollbar display item
   (set-next :initarg :set-next :accessor set-next)    ; To set next state
   (next :initarg :next :accessor next))               ; To get & increment next state
  (:documentation "Scrolling-Inspection-Items are used as the
'top-level' of display of objects that have lots of components and so
have to scroll. In addition to headers and entries, they've got a
scrollbar item and stuff so that the entries can lazily compute where
they are and what they should display."))

(defun make-scrolling-inspection-item (objects headers entries scrollbar)
  (make-instance 'scrolling-inspection-item 
		 :objects objects
		 :headers headers
		 :entries entries
		 :scrollbar scrollbar))

(defgeneric scrolling-inspection-item-p (item)
  (:method ((item t))
	   nil)
  (:method ((item scrolling-inspection-item))
	   t))

;; Scrolling-inspection-item methods.

(defmethod tracker ((item scrolling-inspection-item) x0 y0)
  (dolist (element (headers item))
    (with-slots (x y height width) element
      (when (and (<= x x0 (+ x width))
		 (<= y y0 (+ y height)))
	(tracker element x0 y0)
	(return-from tracker nil))))
  (let ((sb (scrollbar item)))
    (with-slots (x y width height) sb
      (if (and (<= x x0 (+ x width))
	       (<= y y0 (+ y height)))
	(tracker sb x0 y0)
	(track-in-list (entries item) x0 y0)))))



(defclass scrollbar (view-item)
  ((scrollee :initarg :scrollee :accessor scrollee) ; Item for which this guy's a scrollbar
   (bottom :initarg bottom :accessor bottom)        ; Y coordinate of end (hack, hack)
   (active-button :initarg :active-button :accessor active-button :initform nil)
   (first-index :initarg :first-index :accessor first-index)    ; Index of first thing to
					                        ; be displayed
   (next-element :initarg :next-element :accessor next-element) ; Function to extract next 
					                        ; element to be displayed
   (reset-index :initarg :reset-index :accessor reset-index)    ; Function to reset internal
					                        ; index for next-element
   (window-width :initarg :window-width :accessor window-width) ; Max X for scrollees
   (bar-height :initarg :bar-height :accessor bar-height)       ; Height of bar in pixels
   (bar-top :initarg :bar-top :accessor bar-top)
   (bar-bottom :initarg :bar-bottom :accessor bar-bottom)
   (num-elements :initarg :num-elements :accessor num-elements) ; Number of elements in scrollee
   (num-elements-displayed :initarg :num-elements-displayed
			   :accessor num-elements-displayed ))  ; Number of elements displayed
					                        ; at once
  (:documentation "A Scrollbar has buttons and a thumb bar and the
stuff it needs to figure out whatever it needs to figure out."))

(defun make-scrollbar (first-index num-elements num-elements-displayed
			    next-element reset-index)
  (make-instance 'scrollbar
		 :first-index first-index :num-elements num-elements
		 :num-elements-displayed num-elements-displayed
		 :next-element next-element :reset-index reset-index))

;;; Scrollbar methods.

;; Yeah, we use a hard-wired constant 16 here, which is the width and
;; height of the buttons. Grody, yeah, but hey, "16" is only two
;; keystrokes...

(defmethod display ((scrollbar scrollbar) window x y)
  (with-slots (active-button bottom bar-bottom bar-top bar-height
	       first-index num-elements num-elements-displayed)
      scrollbar
    (when window
      (draw-bitmap window x y
		   (if (eq active-button :top)
		     *up-arrow-i* *up-arrow*))
      (draw-bitmap window x (- bottom 16)
		   (if (eq active-button :bottom)
		     *down-arrow-i* *down-arrow*))
      (draw-box window x (+ y 16) (+ x 15) (- bottom 17))
      (setf bar-top (+ y 17)
	    bar-bottom (- bottom 17)
	    bar-height (- bar-bottom bar-top))
      (draw-block window x
		  (+ bar-top (truncate (* first-index bar-height) num-elements))
		  (+ x 16)
		  (- bar-bottom
		     (truncate (* (- num-elements (+ first-index num-elements-displayed))
				  bar-height)
			       num-elements)))
    (xlib:display-force-output *display*))
  (values 16 (- bottom y))))

(defmethod tracker ((scrollbar scrollbar) x0 y0)
  (with-slots (active-button window x y bottom) scrollbar
    (update-current-item scrollbar x0 y0)
    (cond ((<= y y0 (+ y 16))
	   (setf active-button :top)
	   (draw-bitmap window x y *up-arrow-i*))
	  ((<= (- bottom 16) y0 bottom)
	   (setf active-button :bottom)
	   (draw-bitmap window x (- bottom 16) *down-arrow-i*))
	  (t
	   (untracker scrollbar)))
    (xlib:display-force-output *display*)))

(defmethod untracker ((scrollbar scrollbar))
  (with-slots (active-button window x y bottom) scrollbar
    (cond ((eq active-button :top)
	   (draw-bitmap window x y *up-arrow*))
	  ((eq active-button :bottom)
	   (draw-bitmap window x (- bottom 16) *down-arrow*)))
    (xlib:display-force-output *display*)
    (setf active-button nil)))

(defmethod mouse-handler ((scrollbar scrollbar) view key-event)
  (declare (ignore view))
  (with-slots (first-index active-button num-elements num-elements-displayed
	       bar-top bar-bottom bar-height scrollee)
      scrollbar
    (let* ((old-first first-index)
	   (new-first old-first))
      (cond ((or (eq key-event #k"Scrolldowndown") 
		 (eq active-button :bottom))
	     (incf new-first
		   (if (eq key-event #k"Rightdown")
		       num-elements-displayed
		       1)))
	    ((or (eq key-event #k"Scrollupdown")
		 (eq active-button :top))
	     (decf new-first
		   (if (eq key-event #k"Rightdown")
		       num-elements-displayed
		       1)))
	    ((<= bar-top *mouse-y* bar-bottom)
	     (setq new-first
		   (truncate (* (- *mouse-y* bar-top)
				num-elements)
			     bar-height))))
      (setq new-first (max new-first 0))
      (setq new-first (min new-first (- num-elements num-elements-displayed)))
      (unless (= new-first old-first)
	(setf first-index new-first)
	(funcall (reset-index scrollbar) scrollbar)
	(dolist (element (entries scrollee))
	  (redisplay-item element))
	(redisplay-item scrollbar)))))


(defclass scrolling-item (view-item)
  ((scrollbar :initarg :scrollbar :accessor scrollbar)
   (item :initarg :item :accessor item))
  (:documentation "Scrolling-Items are used as the entries in
Scrolling-Inspection-Items. They know the scrollbar that moves them
around so they can lazily do their stuff."))

(defun make-scrolling-item (scrollbar item)
  (make-instance 'scrolling-item :scrollbar scrollbar :item item))

;; Scrolling item methods.

(defmethod display ((item scrolling-item) window x y)
  (with-slots (scrollbar item) item
    (funcall (next-element scrollbar) item)
    (let ((*x-constraint* (if (slot-boundp scrollbar 'window-width)
			    (+ (window-width scrollbar) x)
			    max-window-width)))
      (multiple-value-bind (width height) (display item window x y)
	(values 
	 (or (and (slot-boundp scrollbar 'window-width)
		  (window-width scrollbar))
	     width)
	 height)))))

(defmethod tracker :before ((scrolling-item scrolling-item) x y)
  (update-current-item scrolling-item x y))

(defmethod tracker ((scrolling-item scrolling-item) x y)
  (tracker (item scrolling-item) x y))

(defmethod walker ((scrolling-item scrolling-item) function)
  (walker (item scrolling-item) function))


(defclass string-item (view-item)
  ((item-string :initarg :item-string :accessor item-string) ; String to be displayed
   (font :initarg :font :accessor font))      ; Font in which to display it
  (:documentation "String-Items just have a string of text and a font
that it gets displayed in."))

(defun make-string-item (string &optional (font *entry-font*))
  (make-instance 'string-item :item-string string :font font))

;;; String item method.

(defmethod display ((item string-item) window x y)
  (disp-string window x y (item-string item) (font item)))


(defclass slot-item (view-item)
  ((name :initarg :name :accessor name)              ; String name of slot
   (object :initarg :object :accessor object)        ; Display item for contents of slot
   (max-name-width :initarg :max-name-width 
		   :accessor max-name-width))        ; Length of longest slot name in structure
  (:documentation "Slot-Items have a string name for the slot (e.g.,
structure slot name or vector index) and an object item for the
contents of the slot. The Max-Name-Width is used so that all the slots
in an inspection item can line their objects up nicely in a
left-justified column."))

(defun make-slot-item (name object)
  (make-instance 'slot-item :name name :object object))

(defgeneric slot-item-p (item)
  (:method ((item t))
	   nil)
  (:method ((item slot-item))
	   t))

;;; Slot item methods.

(defmethod display ((item slot-item) window x y)
  (with-slots (name object max-name-width) item
    (let ((name-pixel-width (* (+ 2 max-name-width)
			       (font-width *entry-font*))))
      (disp-string window x y name *entry-font*)
      (multiple-value-bind (width height) (display-item object window (+ x name-pixel-width) y)
	(values (+ name-pixel-width width border)
		(max (+ (font-height *entry-font*) vsp) height))))))

(defmethod tracker ((item slot-item) x y)
  (tracker (object item) x y))

(defmethod walker ((item slot-item) function)
  (with-slots (object max-name-width) item
    (walker object function)
    (setf (width item)
	  (+ (* (+ 2 max-name-width) (font-width *entry-font*))
	     (width object)
	     border))))


(defclass list-item (view-item)
  ((item-list :initarg :item-list :accessor item-list))  ; List of things to be displayed
  (:documentation "List-Items are used to display several things on
the same line, one after the other."))

(defun make-list-item (list)
  (make-instance 'list-item :item-list list))

;;; List item methods.

;; If a thing in the item list is a string, we just Disp-String it.
;; That way, we don't have to cons lots of full string items all the
;; time.
(defmethod display ((item list-item) window x0 y0)
  (let ((x x0)
	(max-height 0))
    (dolist (item (item-list item))
      (multiple-value-bind (width height)
	  (if (stringp item)
	    (disp-string window x y0 item *entry-font*)
	    (display-item item window x y0))
	(incf x width)
	(setq max-height (max max-height height))))
    (values (- x x0) max-height)))

(defmethod tracker ((item list-item) x y)
  (track-in-list (item-list item) x y))

(defmethod walker ((item list-item) function)
  (dolist (element (item-list item))
    (when (view-item-p element)
      (walker element function))))


(defclass object-item (view-item)
  ((object :initarg :object :accessor object)  ; The Lisp object itself
   (item-string :initarg :item-string :accessor item-string) ; String representation cache
   (place :initarg :place :accessor place)     ; Place where it came from
   (index :initarg :index :accessor index)     ; Index into where it came from
   (ref :initarg :ref :accessor ref)           ; Function to get object, given place and index
   (setter :initarg :setter :accessor setter)) ; Function to set object, given place, index 
					       ; and new value
  (:documentation "Object-Items are used to display component Lisp
objects. They know where the object came from and how to get it again
(for decaching) and how to change it (for modification)."))

(defun make-object-item (object place index ref set)
  (make-instance 'object-item :object object :place place :index index :ref ref :setter set))

(defgeneric object-item-p (item)
  (:method ((item t))
	   nil)
  (:method ((item object-item))
	   t))

;;; Object item methods.

(defmethod display ((item object-item) window x y)
  (unless (and (slot-boundp item 'item-string) (item-string item))
    (setf (item-string item) (iprin1-to-string (object item))))
  (disp-string window x y (item-string item) *entry-font*))

(defmethod tracker ((item object-item) x y)
  (when (update-current-item item x y)
    (boxify-item item boole-1)))

(defmethod untracker ((item object-item))
  (boxify-item item boole-c1))

(defmethod mouse-handler ((item object-item) view key-event)
  (cond ((eq key-event #k"Leftdown")
	 ;; Open in current window
	 (push (cons (object view)
		     (view-item view))
	       (stack view))
	 (update-view-of-object view (object item)))

	((eq key-event #k"Rightdown")
	 ;; Open in new window
	 (create-view-of-object (object item) (prin1 (type-of item))))

	((eq key-event #k"Middledown")
	 ;; Return object from inspect
	 (setq *inspect-result* (object item))
	 (try-to-quit))

	((eq key-event #k"Super-Middledown")
	 ;; Return object but leave windows around
	 (setq *inspect-result* (object item))
	 (try-to-proceed))))

(defmethod walker ((item object-item) function)
  (funcall function item))

;;; Object* items.

(defclass object*-item (object-item)
   ((live :initarg :live :accessor live)
    (string* :initarg :string* :accessor string*))
   (:documentation "Object*-Items are like Object-Items except that
sometimes they can be like string items and be not-selectable."))

(defun make-object*-item (string* object live place index ref set)
  (make-instance 'object*-item
		 :string* string* 
		 :object object
		 :live live
		 :place place
		 :index index
		 :ref ref
		 :setter set))

(defgeneric object*-item-p (item)
  (:method ((item t))
	   nil)
  (:method ((item object*-item))
	   t))

;;; Object* item methods.

(defmethod display ((item object*-item) window x y)
  (if (live item)
    (call-next-method)
    (disp-string window x y (string* item) *italic-font*)))

(defmethod tracker ((item object*-item) x y)
  (if (or (live item) (eq *tracking-mode* :destination))
    (boxifying-tracker item x y)
    (update-current-item item x y)))

(defmethod untracker ((item object*-item))
  (when (or (live item) (eq *tracking-mode* :destination))
    (boxifying-untracker item)))

(defmethod mouse-handler ((item object*-item) view key-event)
  (when (live item)
    (call-next-method)))


;;;; Display stuff. This uses the methods defined above to actually
;;;; render the objects onto a visible window.

;; Computing display items for Lisp objects.


(defgeneric plan-view (object &key header stream)
  (:documentation "Plan-View returns a top-level View-Item for the
  given Object."))

(defgeneric replan-view (object plan)
  (:documentation "Replan-view tries to fix up the existing Plan if
possible, but might punt and just return a new View-Item if things
have changed too much."))

(defun replan (plan)
  "Replan is for the update function. It sets up the right calling
  convention for calling the generic replan-view function."
  (let ((object (objects plan)))
    (replan-view object plan)))


(defun replan-object-item (item)
  "Replan-Object-Item is used at the leaves of the replanning walk."
  (if (object*-item-p item)
      (multiple-value-bind (decached-object live)
	  (funcall (ref item) (place item) (index item))
	(unless (and (eq live (live item))
		     (eq decached-object (object item))
		     (or (symbolp decached-object) (numberp decached-object)
			 ;; ...
			 ))
	  (setf (live item) live)
	  (setf (object item) decached-object)
	  (setf (item-string item) nil)
	  (redisplay-item item)))
      (let ((decached-object (funcall (ref item)
				      (place item) (index item))))
	(unless (and (eq decached-object (object item))
		     (or (symbolp decached-object) (numberp decached-object)
			 ;; ... any others that'll be the same?
			 ))
	  (setf (object item) decached-object)
	  (setf (item-string item) nil)
	  (redisplay-item item)))))


;; Figure out how long random list structures are. Deals with dotted
;; lists and circular lists.

;;  This routine is too simple --- I'm not sure it always works. In
;;  particular, I doubt it gives an accurate count for every kind of
;;  circular list.
(defun count-conses (list)
  (if (atom list)
    (values 0 :atom)
    (do ((count 1 (1+ count))
	 (tortoise list)
	 (tortoise-advance nil (not tortoise-advance))
	 (hare (cdr list) (cdr hare)))
	((or (null hare) (not (listp hare)) (eq hare tortoise))
	 (cond ((null hare)
		(values count :proper-list))
	       ((not (listp hare))
		(values count :dotted-list))
	       ((eq hare tortoise)
		(values count :circular-list))))
      (when tortoise-advance
	(setf tortoise (cdr tortoise))))))
     

;; For lists, what we stash in the Inspection-Item-Objects slot is the
;; list of the top level conses, rather than the conses themselves.
;; This lets us detect when conses "in the middle" of the list change.
(defmethod plan-view ((object list) &key &allow-other-keys)
  (cond 
    ;; Display the list object as a "list": ( .... )
    ((or (and (< (size-item (make-string-item (iprin1-to-string object)))
		 (- max-window-width (* 2 border)))
	      (<= (count-conses object) inspect-length))
	 (= (count-conses object) 1))
     (do ((list object (cdr list))
	  (i 0 (1+ i))
	  (items (list "(")))
	 ((or (not (consp (cdr list)))
	      ;; The following covers circular lists.
	      (> i (count-conses object)))
	  (push (make-object-item (car list) list nil 'lref 'lset) items)
	  (when (not (null (cdr list)))
	    (push " . " items)
	    (push (make-object-item (cdr list) list nil 'lref* 'lset*) items))
	  (push ")" items)
	  (make-inspection-item
	   (copy-n-conses object (count-conses object))
	   nil
	   (list (make-list-item (nreverse items)))))
       (push (make-object-item (car list) list nil 'lref 'lset) items)
       (push " " items)))
    
    ((<= (count-conses object) inspect-length)
     (let ((items nil))
       (push (make-list-item (list "("
				   (make-object-item
				    (car object) object nil 'lref 'lset)))
	     items)
       (do ((list (cdr object) (cdr list)))
	   ((not (consp (cdr list)))
	    (cond ((null (cdr list))
		   (push (make-list-item
			  (list " "
				(make-object-item
				 (car list) list nil 'lref 'lset)
				")"))
			 items))
		  (t
		   (push (make-list-item
			  (list " "
				(make-object-item
				 (car list) list nil 'lref 'lset)))
			 items)
		   (push " ." items)
		   (push (make-list-item
			  (list " "
				(make-object-item
				 (cdr list) list nil 'lref* 'lset*)
				")"))
			 items))))
	 (push (make-list-item
		(list " "
		      (make-object-item
		       (car list) list nil 'lref 'lset)))
	       items))
       (make-inspection-item (copy-n-conses object (count-conses object))
			     nil (nreverse items))))

    ;; This list is too long --- use a scrolling view.
    (t
     (let ((scrollbar
	    (let ((index 0)
		  (cons object)
		  (last (last object)))
	      (make-scrollbar
	       0
	       (+ (count-conses object) (if (cdr last) 1 0))
	       inspect-length
	       #'(lambda (item)
		   (setf (item-list item)
			 `(,(cond ((eq cons object) "(")
				  ((not (consp cons)) " . ")
				  (t " "))
			   ,(if (consp cons)
				(make-object-item (car cons) cons nil 'lref 'lset)
				(make-object-item cons last nil 'lref* 'lset*))
			   ,@(if (or (and (eq cons last) (null (cdr cons)))
				     (atom cons))
				 `(")"))))
		   (incf index)
		   (unless (atom cons)
		     (setq cons (cdr cons))))
	       #'(lambda (item)
		   (setq index (first-index item))
		   (setq cons (nthcdr index object)))))))
       (setf (scrollee scrollbar)
	     (make-scrolling-inspection-item
	      (copy-n-conses object (count-conses object))
	      nil
	      (let ((items nil))
		(dotimes (i inspect-length)
		  (push (make-scrolling-item scrollbar (make-list-item nil))
			items))
		(nreverse items))
	      scrollbar)))
	 )))

;; This is kind of like (maplist #'identity list), except that it
;; doesn't choke on non-nil-terminated lists.
(defun copy-conses (list)
  (do ((list list (cdr list))
       (conses nil))
      ((atom list)
       (nreverse conses))
    (push list conses)))


;; This will copy "n" conses; this deals with circular lists.
(defun copy-n-conses (list n)
  (do ((i 1 (1+ i))
       (list list (cdr list))
       (conses nil))
      ((or (atom list) (= i n)) (nreverse conses))
    (push list conses)))


(defmethod replan-view ((object list) plan)
  (cond ((do ((list (car object) (cdr list))
	      (conses object (cdr conses)))
	     ((or (null list) (null conses))
	      (and (null list) (null conses)))
	   (unless (and (eq list (car conses))
			(eq (cdr list) (cadr conses)))
	     (return nil)))
	 (walker plan #'replan-object-item)
	 plan)
	(t
	 (plan-view (car object)))))

(defun lref (object ignore) (declare (ignore ignore))
  (car object))
(defun lref* (object ignore) (declare (ignore ignore))
  (cdr object))
(defun lset (object ignore new) (declare (ignore ignore))
  (setf (car object) new))
(defun lset* (object ignore new) (declare (ignore ignore))
  (setf (cdr object) new))


(defmethod plan-view ((object vector) &key &allow-other-keys)
  (let* ((type (type-of object))
	 (length (array-dimension object 0))
	 (header
	  `(,(make-string-item (format nil "~A" (if (listp type) (car type) type))
			       *header-font*)
	    ,(make-string-item (format nil "Length = ~D" length)
			       *header-font*)
	    ,@(if (array-has-fill-pointer-p object)
		  `(,(make-list-item (list "Fill-Pointer: "
					   (make-object-item
					    (fill-pointer object)
					    object nil 'fpref 'fpset))))))))
     (cond ((<= length inspect-length)
	    (make-inspection-item
	     object
	     header
	     (let ((items nil))
	       (dotimes (i length)
		 (push (make-slot-item (prin1-to-string i)
				       (make-object-item
					(aref object i) object i 'vref 'vset))
		       items))
	       (nreverse items))))
	   (t
	    (let ((scrollbar
		   (let ((index 0))
		     (make-scrollbar
		      0
		      length
		      inspect-length
		      #'(lambda (item)
			  (setf (name item) (prin1-to-string index))
			  (let ((obj (object item)))
			    (setf (object obj) (aref object index))
			    (setf (index obj) index)
			    (setf (item-string obj) nil))
			  (incf index))
		      #'(lambda (item)
			  (setq index (first-index item)))))))
	      (setf (scrollee scrollbar)
		    (make-scrolling-inspection-item
		     object
		     header
		     (let ((items nil)
			   (name-width (length (iprin1-to-string (1- length)))))
		       (dotimes (i inspect-length)
			 (let ((slot
				(make-slot-item
				 nil
				 (make-object-item nil object nil 'vref 'vset))))
			   (setf (max-name-width slot) name-width)
			   (push (make-scrolling-item scrollbar slot) items)))
		       (nreverse items))
		     scrollbar)))))))

(defmethod replan-view ((object vector) plan)
  (cond ((= (length object) (length (objects plan)))
	 (walker plan #'replan-object-item)
	 plan)
	(t
	 (plan-view object))))

(defun vref (object index)
  (aref object index))
(defun vset (object index new)
  (setf (aref object index) new))

(defun fpref (object index)
  (declare (ignore index))
  (fill-pointer object))
(defun fpset (object index new)
  (declare (ignore index))
  (setf (fill-pointer object) new))


(defmethod plan-view ((object array) &key &allow-other-keys)
  (lisp::with-array-data ((data object)
			  (start)
			  (end))
    (let* ((length (- end start))
	   (dimensions (array-dimensions object))
	   (rev-dimensions (reverse dimensions))
	   (header
	    (list (make-string-item
		   (format nil "Array of ~A" (array-element-type object))
		   *header-font*)
		  (make-string-item
		   (format nil "Dimensions = ~S" dimensions)
		   *header-font*))))
      (cond ((<= length inspect-length)
	     (make-inspection-item
	      object
	      header
	      (let ((items nil))
		(dotimes (i length)
		  (push (make-slot-item (index-string i rev-dimensions)
					(make-object-item
					 (aref data (+ start i))
					 object (+ start i) 'vref 'vset))
			items))
		(nreverse items))))
	    (t
	     (let ((scrollbar
		    (let ((index 0))
		      (make-scrollbar
		       0
		       length
		       inspect-length
		       #'(lambda (item)
			   (setf (name item)
				 (index-string index rev-dimensions))
			   (let ((obj (object item)))
			     (setf (object obj)
				   (aref data (+ start index)))
			     (setf (index obj) (+ start index))
			     (setf (item-string obj) nil))
			   (incf index))
		       #'(lambda (item)
			   (setq index (first-index item)))))))
	       (setf (scrollee scrollbar)
		     (make-scrolling-inspection-item
		      object
		      header
		      (let ((items nil)
			    (name-width (length (index-string (1- length)
							      rev-dimensions))))
			(dotimes (i inspect-length)
			  (let ((slot
				 (make-slot-item
				  nil
				  (make-object-item nil data nil 'vref 'vset))))
			    (setf (max-name-width slot) name-width)
			    (push (make-scrolling-item scrollbar slot) items)))
			(nreverse items))
		      scrollbar))))))))

(defun index-string (index rev-dimensions)
  (if (null rev-dimensions)
      "[]"
      (let ((list nil))
	(dolist (dim rev-dimensions)
	  (multiple-value-bind (q r)
			       (floor index dim)
	    (setq index q)
	    (push r list)))
	(format nil "[~D~{,~D~}]" (car list) (cdr list)))))

(defmethod replan-view ((object array) plan)
  (cond ((and (equal (array-dimensions object)
		     (array-dimensions (objects plan)))
	      (lisp::with-array-data ((data1 object)
				      (start1) (end1))
		(lisp::with-array-data ((data2 (objects plan))
					(start2) (end2))
		  (and (eq data1 data2)
		       (= start1 start2)
		       (= end1 end2)))))
	 (walker plan #'replan-object-item)
	 plan)
	(t
	 (plan-view object))))


(defmethod plan-view ((object t) &key &allow-other-keys)
  (make-inspection-item
   object
   nil
   (list (make-object-item object (list object) nil 'lref 'lset))))

(defmethod replan-view ((object t) plan)
  (declare (ignore object))
  (walker plan #'replan-object-item)
  plan)



(defmethod plan-view ((object structure-object) &key &allow-other-keys)
  (let* ((dd (kernel:layout-info (kernel:%instance-layout object)))
	 (dsds (kernel:dd-slots dd)))
    (make-inspection-item
     object
     (list (make-string-item
	    (format nil "~A ~A"
		    (symbol-name (kernel:dd-name dd))
		    object)
	    *header-font*))
     (let ((items nil))
       (dolist (dsd dsds)
	 (push (make-slot-item
		(kernel:dsd-%name dsd)
		(make-object-item
		 (funcall (fdefinition (kernel:dsd-accessor dsd)) object)
		 object (kernel:dsd-index dsd)
		 #'(lambda (str ignore)
		     (declare (ignore ignore))
		     (funcall (fdefinition (kernel:dsd-accessor dsd))
			      str))
		 #'(lambda (str ignore val)
		     (declare (ignore ignore))
		     (funcall (fdefinition `(setf ,(kernel:dsd-accessor dsd)))
			      val str))))
	       items))
       (nreverse items)))))

(defmethod replan-view ((object structure-object) plan)
  (declare (ignore object))
  (walker plan #'replan-object-item)
  plan)



(defmethod plan-view ((object standard-object) &key &allow-other-keys)
  (let ((class (pcl:class-of object)))
    (make-inspection-item
     object
     (list (make-string-item (format nil "~S ~A"
				     (pcl:class-name class)
				     object)
			     *header-font*))
     (let ((slotds (pcl::slots-to-inspect class object))
	   instance-slots class-slots other-slots)
       (dolist (slotd slotds)
	 (with-slots ((slot pcl::name) (allocation pcl::allocation)) slotd
	   (let* ((boundp (slot-boundp object slot))
		  (item (make-slot-item (prin1-to-string slot)
					(make-object*-item
					 "Unbound"
					 (and boundp (slot-value object slot))
					 boundp
					 object
					 slot
					 'ref-slot
					 'set-slot))))
	     (case allocation
	       (:instance (push item instance-slots))
	       (:class (push item class-slots))
	       (otherwise
		(setf (name item)
		      (format nil "~S [~S]" slot allocation))
		(push item other-slots))))))
       (append (unless (null instance-slots)
		 (cons (make-string-item "These slots have :INSTANCE allocation"
					 *entry-font*)
		       (nreverse instance-slots)))
	       (unless (null class-slots)
		 (cons (make-string-item "These slots have :CLASS allocation"
					 *entry-font*)
		       (nreverse class-slots)))
	       (unless (null other-slots)
		 (cons (make-string-item "These slots have allocation as shown"
					 *entry-font*)
		       (nreverse other-slots))))))))


(defun ref-slot (object slot)
  (if (slot-boundp object slot)
    (values (slot-value object slot) t)
    (values nil nil)))

(defun set-slot (object slot val)
  (setf (slot-value object slot) val))

;;; Should check to see if we need to redo the entire plan or not.
(defmethod replan-view ((object standard-object) plan)
  (declare (ignore plan))
  (plan-view object))



(defmethod plan-view ((object symbol) &key &allow-other-keys)
  (make-inspection-item
   object
   (list (make-string-item (format nil "Symbol ~A" object) *header-font*))
   (list (make-slot-item "Value"
			 (make-object*-item
			  "Unbound" (if (boundp object) (symbol-value object))
			  (boundp object) object nil 'valref 'valset))
	 (make-slot-item "Function"
			 (make-object*-item
			  "Undefined" (if (fboundp object) (symbol-function object))
			  (fboundp object) object nil 'defref 'defset))
	 (make-slot-item "Properties"
			 (make-object-item
			  (symbol-plist object) object nil 'plistref 'plistset))
	 (make-slot-item "Package"
			 (make-object-item
			  (symbol-package object) object nil 'packref 'packset)))))

(defmethod replan-view ((object symbol) plan)
  (declare (ignore object))
  (walker plan #'replan-object-item)
  plan)

(defun valref (object ignore) (declare (ignore ignore))
  (if (boundp object)
      (values (symbol-value object) t)
      (values nil nil)))
(defun defref (object ignore) (declare (ignore ignore))
  (if (fboundp object)
      (values (symbol-function object) t)
      (values nil nil)))
(defun plistref (object ignore) (declare (ignore ignore))
  (symbol-plist object))
(defun packref (object ignore) (declare (ignore ignore))
  (symbol-package object))

(defun valset (object ignore new) (declare (ignore ignore))
  (setf (symbol-value object) new))
(defun defset (object ignore new) (declare (ignore ignore))
  (setf (symbol-function object) new))
(defun plistset (object ignore new) (declare (ignore ignore))
  (setf (symbol-plist object) new))
(defun packset (object ignore new) (declare (ignore ignore))
  (lisp::%set-symbol-package object new))


;; This is all very gross and silly now, just so we can get something
;; working quickly. Eventually do this with a special stream that
;; listifies things as it goes along...
(defmethod plan-view ((object function) &key &allow-other-keys)
  (let ((stream (make-string-output-stream)))
    (let ((*standard-output* stream)
	  (ext:*describe-print-level* 30))
      (describe object))
    (close stream)
    (with-input-from-string (in (get-output-stream-string stream))
      (plan-view-text 
       object
       (list
	(make-string-item (format nil "Function ~S" object) *header-font*)
	(make-string-item
	 (format nil "Argument list: ~A" (kernel:%function-arglist object))))
       in))))


(defun plan-view-text (object header stream)
  (let ((list nil))
    (do ((line (read-line stream nil nil) (read-line stream nil nil)))
	((null line))
      (push line list))
    (setq list (nreverse list))
    (if (<= (length list) inspect-length)
	(make-inspection-item
	 object
	 header
	 (mapcar #'make-string-item list))
	(let ((index 0)
	      (vector (coerce list 'vector)))
	  (let ((scrollbar (make-scrollbar
			    0 (length list) inspect-length
			    #'(lambda (item)
				(setf (item-string item)
				      (aref vector index))
				(incf index))
			    #'(lambda (item)
				(setq index
				      (first-index item))))))
	    (setf (scrollee scrollbar)
		  (make-scrolling-inspection-item
		   object
		   header
		   (let ((items nil))
		     (dotimes (i inspect-length)
		       (push 
			(make-scrolling-item 
			 scrollbar
			 ;; This is to ensure that the slots in
			 ;; the string item are bound.
			 (let ((string-item (make-string-item "")))
			   (setf (x string-item) 0
				 (y string-item) 0
				 (width string-item) 0
				 (height string-item) 0)
			   string-item))
			items))
		     (nreverse items))
		   scrollbar)))))))


;;;; Displaying old and new plans in old and new windows.

(defun new-plan-in-new-view (object plan &optional name)
  (multiple-value-bind (width height) (size-item plan)
    ;; add border
    (incf width 10)
    (incf height 10)
    (multiple-value-bind (x y) (next-window-position width height)
      (let* ((window (xlib:create-window :parent *root* :x x :y y
					 :width width :height height
					 :background *white-pixel*
					 :border-width 2))
	     (view (make-view name object plan window)))
	(xlib:set-wm-properties window
				:name "Inspector Window"
				:icon-name "Inspector Display"
				:resource-name "Inspector"
				:x x :y y :width width :height height
				:user-specified-position-p t
				:user-specified-size-p t
				:min-width width :min-height height
				:width-inc nil :height-inc nil)
	(setf (xlib:wm-protocols window) `(:wm_delete_window))
	(add-window-view-mapping window view)
	(xlib:map-window window)
	(xlib:clear-area window)
	(xlib:with-state (window)
	  (setf (xlib:window-event-mask window) important-xevents-mask)
	  (setf (xlib:window-cursor window) *cursor*))
	(xlib:display-finish-output *display*)
	(display-item plan window 5 5)
	(push view *views*)
	(multiple-value-bind
	    (x y same-screen-p child mask root-x root-y root)
	    (xlib:query-pointer window)
	  (declare (ignore same-screen-p child mask root-x root-y root))
	  (when (and (< 0 x (+ width 10)) (< 0 y (+ height 10)))
	    (tracker plan x y)))
	(xlib:display-force-output *display*)
	view))))

(defun create-view-of-object (object &optional name)
  (new-plan-in-new-view object (plan-view object) name))

(defun new-plan-in-old-view (view old new)
  (unless (eq new old)
    (setf (view-item view) new)
    (let ((window (window view)))
      (when (and *current-item*
		 (eql (window *current-item*) window))
	(setq *current-item* nil))
      (multiple-value-bind (width height)
			   (size-item new)
	(xlib:with-state (window)
	  (setf (xlib:drawable-width window) (+ width 10))
	  (setf (xlib:drawable-height window) (+ height 10)))
	(xlib:clear-area window)
	(display-item new window 5 5)
	(setf (window new) window
	      (x new) 5
	      (y new) 5
	      (width new) width
	      (height new) height)
	(xlib:display-force-output *display*)
	(multiple-value-bind
	    (x y same-screen-p child mask root-x root-y root)
	    (xlib:query-pointer window)
	  (declare (ignore same-screen-p child mask root-x root-y root))
	  (when (and (< 0 x (+ width 10)) (< 0 y (+ height 10)))
	    (tracker new x y)))))))

(defun update-view-of-object (view &optional (object (object view)))
  (cond ((eq object (object view))
	 (new-plan-in-old-view view
			       (view-item view)
			       (replan (view-item view))))
	(t
	 (setf (object view) object)
	 (new-plan-in-old-view view (view-item view) (plan-view object))))
  (xlib:display-force-output *display*))


;; DELETING-WINDOW-DROP-EVENT checks for any events on win. If there
;; is one, it is removed from the queue, and t is returned. Otherwise,
;; returns nil.
(defun deleting-window-drop-event (display win)
  (xlib:display-finish-output display)
  (let ((result nil))
    (xlib:process-event
     display :timeout 0
     :handler #'(lambda (&key event-window window &allow-other-keys)
		  (if (or (eq event-window win) (eq window win))
		      (setf result t)
		      nil)))
    result))

(defun remove-view-of-object (view)
  (let (#+:mp (update-process (update-process view))
	(window (window view)))
    #+:mp (mp:destroy-process update-process)
    (setf (xlib:window-event-mask window) #.(xlib:make-event-mask))
    (xlib:display-finish-output *display*)
    (loop (unless (deleting-window-drop-event *display* window) (return)))
    (xlib:destroy-window window)
    (xlib:display-finish-output *display*)
    (delete-window-view-mapping window)
    (setq *views* (delete view *views*))))


;;;; The command interpreter.

(defvar *can-quit* nil)
(defvar *can-proceed* nil)
(defvar *unwinding* t)

(defun try-to-quit ()
  (setq *current-item* nil)
  (when *can-quit*
    (setq *unwinding* nil)
    (ext:flush-display-events *display*)
    (throw 'inspect-exit nil))
  (try-to-proceed))

(defun try-to-proceed ()
  (when *can-proceed*
    (setq *unwinding* nil)
    (ext:flush-display-events *display*)
    (throw 'inspect-proceed nil)))

(defvar *do-command* nil)

(defun do-command (view key-event)
  (cond (*do-command*
	 (funcall *do-command* view key-event))

	;; If we get scrollwheel down key events anywhere in the view,
	;; the scrollbar wants to know about them. Yes, a bit
	;; ad-hoc....
	((and (or (eq key-event #k"Scrollupdown") 
		  (eq key-event #k"Scrolldowndown"))
	      (typep (view-item view) 'scrolling-inspection-item))
	 (dotimes (i 5) ; Simulate multiple clicks.
	   (mouse-handler (scrollbar (view-item view)) view key-event)))

	((or (eq key-event #k"d") (eq key-event #k"D"))
	 ;; Delete current window.
	 (remove-view-of-object view)
	 (setq *current-item* nil)
	 (unless *views*
	   (try-to-quit)
	   (try-to-proceed)))

	((or (eq key-event #k"h") (eq key-event #k"H") (eq key-event #k"?"))
	 (let ((inspect-length (max inspect-length 30)))
	   (with-open-file (stream help-file-pathname :direction :input)
	     (new-plan-in-new-view
	      nil
	      (plan-view-text nil
			      (list (make-string-item "Help" *header-font*))
			      stream)
	      "Help Window"))))

	((or (eq key-event #k"m") (eq key-event #k"M"))
	 ;; Modify something.
	 ;; Since the tracking stuff sets up event handlers that can
	 ;; throw past the CLX event dispatching form in
	 ;; INSPECTOR-EVENT-HANDLER, those handlers are responsible
	 ;; for discarding their events when throwing to this CATCH
	 ;; tag.
	 (catch 'quit-modify
	   (let* ((destination-item (track-for-destination))
		  (source (cond
			   ((eq key-event #k"m")
			    (object (track-for-source)))
			   (t
			    (format *query-io*
				    "~&Form to evaluate for new contents: ")
			    (force-output *query-io*)
			    (eval (read *query-io*))))))
	     (funcall (setter destination-item)
		      (place destination-item)
		      (index destination-item)
		      source)
	     (update-view-of-object view))))

	((or (eq key-event #k"q") (eq key-event #k"Q"))
	 ;; Quit.
	 (try-to-quit))

	((or (eq key-event #k"p") (eq key-event #k"P"))
	 ;; Proceed.
	 (try-to-proceed))

	((or (eq key-event #k"r") (eq key-event #k"R"))
	 ;; Recompute object (decache).
	 (update-view-of-object view))

	((or (eq key-event #k"u") (eq key-event #k"U"))
	 ;; Up (pop history stack).
	 (when (stack view)
	   (let ((parent (pop (stack view))))
	     (setf (object view) (car parent))
	     (new-plan-in-old-view view (view-item view) (cdr parent))
	     (update-view-of-object view))))

	((or (eq key-event #k"Leftdown")
	     (eq key-event #k"Middledown")
	     (eq key-event #k"Rightdown")
	     (eq key-event #k"Super-Leftdown")
	     (eq key-event #k"Super-Middledown")
	     (eq key-event #k"Super-Rightdown")
;;	     (eq key-event #k"Scrollupdown")
;;	     (eq key-event #k"Scrolldowndown")
;;	     (eq key-event #k"Super-Scrollupdown")
;;	     (eq key-event #k"Super-Scrolldowndown")
	     )

	 (when *current-item*
	   (mouse-handler *current-item* view key-event)))))


;;;; Stuff to make modification work.

(defun track-for-destination ()
  (track-for :destination *cursor-d*))

(defun track-for-source ()
  (track-for :source *cursor-s*))

;; TRACK-FOR loops over SYSTEM:SERVE-EVENT waiting for some event
;; handler to throw to this CATCH tag. Since any such handler throws
;; past SYSTEM:SERVE-EVENT, and therefore, past the CLX event
;; dispatching form in INSPECTOR-EVENT-HANDLER, it is that handler's
;; responsibility to discard its event.
(defun track-for (tracking-mode cursor)
  (let ((*tracking-mode* tracking-mode)
	(*do-command* #'track-for-do-command))
    (catch 'track-for
      (unwind-protect
	  (progn
	    (dolist (view *views*)
	      (setf (xlib:window-cursor (window view))
		    cursor))
	    (xlib:display-force-output *display*)
	    (loop
	     (system:serve-event)))
	(dolist (view *views*)
	  (setf (xlib:window-cursor (window view))
		*cursor*))
	(xlib:display-force-output *display*)))))

;; TRACK-FOR-DO-COMMAND is the "DO-COMMAND" executed when tracking.
;; Since this throws past the CLX event handling form in
;; INSPECTOR-EVENT-HANDLER, the responsibility for discarding the
;; current event lies here.
(defun track-for-do-command (view key-event)
  (declare (ignore view))
  (cond
    ((or (eq key-event #k"q") (eq key-event #k"Q"))
     (xlib:discard-current-event *display*)
     (throw 'quit-modify t))
    ((or (eq key-event #k"Leftdown")
	 (eq key-event #k"Middledown")
	 (eq key-event #k"Rightdown"))
     (when (object-item-p *current-item*)
       (throw 'track-for
	      (prog1 *current-item*
		(when (object*-item-p *current-item*)
		  (untracker *current-item*)
		  (setq *current-item* nil))
		(xlib:discard-current-event *display*)))))))



;;;; Top-level program interface.

(defun show-object (object &optional name)
  (inspect-init)
  (dolist (view *views*)
    (when (if name
	      (eq name (name view))
	      (eq object (object view)))
      (update-view-of-object view object)
      (return-from show-object nil)))
  (create-view-of-object object name))

(defun remove-object-view (object &optional name)
  (dolist (view *views*)
    (when (if name
	      (eq name (name view))
	      (eq object (object view)))
      (remove-view-of-object view)
      (return nil))))

(defun remove-all-views ()
  (dolist (view *views*)
    (remove-view-of-object view)))



;;;; Top-level user interface.

(defvar *interface-style* :graphics
  "This specifies the default value for the interface argument to INSPECT.  The
   default value of this is :graphics, indicating when running under X, INSPECT
   should use a graphics interface instead of a command-line oriented one.")

(defun inspect (&optional (object nil object-p)
			  (interface *interface-style*))
  "(inspect <object> <interface>)

Interactively examine Lisp objects.

Arguments:

object: The object to examine.

interface: one of [:window :windows :graphics :graphical :x 
                   :command-line :tty]

Any of [:window :windows :graphics :graphical :x] give a windowing
interface. Once you've got a window, type <h> or <H> to get a help
window explaining how to use it.

Either of [:command-line :tty] gives a pure command-line inspector.

If <interface> is not supplied, the default is to use a windowing
interface if running under X11, and a command-line interface if not.

If neither argument is given, the windowing version of inspect will
resume inspection of items left active from previous uses if there are
any, otherwise give an error. The command-line interface will give an
error."
  (cond ((or (member interface '(:command-line :tty))
	     (not (assoc :display ext:*environment-list*)))
	 (when object-p (tty-inspect object)))
	((not (member interface '(:window :windows :graphics :graphical :x)))
	 (error "Interface must be one of :window, :windows, :graphics, ~
		 :graphical, :x, :command-line, or :tty -- not ~S."
		interface))
	(object-p
	 (inspect-init)
	 (let ((disembodied-views nil)
	       (*inspect-result* object)
	       (*x-constraint* max-window-width)
	       (*can-quit* t)
	       (*can-proceed* t))
	   (let ((*views* nil))
	     (create-view-of-object object "User Supplied Object")
	     (catch 'inspect-proceed
	       (unwind-protect
		   (progn
		     (catch 'inspect-exit
		       (loop
			(system:serve-event)))
		     (setq *unwinding* t))
		 (when *unwinding*
		   (do ((view (pop *views*)
			      (pop *views*)))
		       ((null view))
		     (remove-view-of-object view)))))
	     (setq disembodied-views *views*))
	   (dolist (view (reverse disembodied-views))
	     (push view *views*))
	   *inspect-result*))
	(*views*
	 (inspect-init)
	 (let ((*inspect-result* nil)
	       (*can-quit* t)
	       (*can-proceed* t))
	   (catch 'inspect-proceed
	     (catch 'inspect-exit
	       (loop
		(system:serve-event))))
	   *inspect-result*))
	(t (error "No object supplied for inspection and no previous ~
		   inspection object exists."))))
