;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes;  -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;;; #+ features used in this file
;;;   ansi-common-lisp
;;;   lispm
;;;   genera
;;;   lucid
;;;   lcl3.0
;;;   apollo
;;;   kcl
;;;   ibcl
;;;   excl

#-ansi-common-lisp 
(lisp:in-package :xlib :use '(:lisp))

#+ansi-common-lisp
(common-lisp:in-package :common-lisp-user)

#+ansi-common-lisp
(common-lisp:defpackage xlib
  (:use common-lisp)
  (:size 3000)
  #+Genera
  (:import-from
    (system
      arglist downward-funarg array-register)
    (zwei
      indentation))
  (:export
    *version* access-control access-error access-hosts
    activate-screen-saver add-access-host add-resource add-to-save-set
    alist alloc-color alloc-color-cells alloc-color-planes alloc-error
    allow-events angle arc-seq array-index atom-error atom-name
    bell bit-gravity bitmap bitmap-format bitmap-format-lsb-first-p
    bitmap-format-p bitmap-format-pad bitmap-format-unit bitmap-image
    boole-constant boolean card16 card29 card32 card8
    card8->char change-active-pointer-grab change-keyboard-control
    change-keyboard-mapping change-pointer-control change-property
    char->card8 char-ascent char-attributes char-descent
    char-left-bearing char-right-bearing char-width character->keysyms
    character-in-map-p circulate-window-down circulate-window-up clear-area
    close-display close-down-mode close-font closed-display color
    color-blue color-green color-p color-red color-rgb colormap
    colormap-display colormap-equal colormap-error colormap-id
    colormap-p colormap-visual-info connection-failure convert-selection
    copy-area copy-colormap-and-free copy-gcontext copy-gcontext-components
    copy-image copy-plane create-colormap create-cursor
    create-gcontext create-glyph-cursor create-image create-pixmap
    create-window cursor cursor-display cursor-equal cursor-error
    cursor-id cursor-p cut-buffer declare-event decode-core-error
    default-error-handler default-keysym-index default-keysym-translate
    define-error define-extension define-gcontext-accessor
    define-keysym define-keysym-set delete-property delete-resource
    destroy-subwindows destroy-window device-busy device-event-mask
    device-event-mask-class discard-current-event discard-font-info display
    display-after-function display-authorization-data display-authorization-name
    display-bitmap-format display-byte-order display-default-screen
    display-display display-error-handler display-finish-output
    display-force-output display-host display-image-lsb-first-p
    display-invoke-after-function display-keycode-range display-max-keycode
    display-max-request-length display-min-keycode display-motion-buffer-size
    display-nscreens display-p display-pixmap-formats display-plist
    display-protocol-major-version display-protocol-minor-version
    display-protocol-version display-release-number
    display-report-asynchronous-errors display-resource-id-base
    display-resource-id-mask display-roots display-vendor
    display-vendor-name display-xdefaults display-xid draw-arc
    draw-arcs draw-direction draw-glyph draw-glyphs draw-image-glyph
    draw-image-glyphs draw-line draw-lines draw-point draw-points
    draw-rectangle draw-rectangles draw-segments drawable
    drawable-border-width drawable-depth drawable-display drawable-equal
    drawable-error drawable-height drawable-id drawable-p
    drawable-plist drawable-root drawable-width drawable-x drawable-y
    error-key event-case event-cond event-handler event-key
    event-listen event-mask event-mask-class extension-opcode
    find-atom font font-all-chars-exist-p font-ascent
    font-default-char font-descent font-direction font-display
    font-equal font-error font-id font-max-byte1 font-max-byte2
    font-max-char font-min-byte1 font-min-byte2 font-min-char
    font-name font-p font-path font-plist font-properties
    font-property fontable force-gcontext-changes free-colormap
    free-colors free-cursor free-gcontext free-pixmap gcontext
    gcontext-arc-mode gcontext-background gcontext-cap-style
    gcontext-clip-mask gcontext-clip-ordering gcontext-clip-x
    gcontext-clip-y gcontext-dash-offset gcontext-dashes gcontext-display
    gcontext-equal gcontext-error gcontext-exposures gcontext-fill-rule
    gcontext-fill-style gcontext-font gcontext-foreground gcontext-function
    gcontext-id gcontext-join-style gcontext-key gcontext-line-style
    gcontext-line-width gcontext-p gcontext-plane-mask gcontext-plist
    gcontext-stipple gcontext-subwindow-mode gcontext-tile gcontext-ts-x
    gcontext-ts-y get-external-event-code get-image get-property
    get-raw-image get-resource get-search-resource get-search-table
    get-standard-colormap get-wm-class global-pointer-position grab-button
    grab-key grab-keyboard grab-pointer grab-server grab-status
    icon-sizes iconify-window id-choice-error illegal-request-error
    image image-blue-mask image-depth image-green-mask image-height
    image-name image-pixmap image-plist image-red-mask image-width
    image-x image-x-hot image-x-p image-xy image-xy-bitmap-list
    image-xy-p image-y-hot image-z image-z-bits-per-pixel image-z-p
    image-z-pixarray implementation-error input-focus install-colormap
    installed-colormaps int16 int32 int8 intern-atom invalid-font
    keyboard-control keyboard-mapping keycode->character keycode->keysym
    keysym keysym->character keysym->keycodes keysym-in-map-p
    keysym-set kill-client kill-temporary-clients length-error
    list-extensions list-font-names list-fonts list-properties
    lookup-color lookup-error make-color make-event-handlers
    make-event-keys make-event-mask make-resource-database make-state-keys
    make-state-mask make-wm-hints make-wm-size-hints map-resource
    map-subwindows map-window mapping-notify mask16 mask32
    match-error max-char-ascent max-char-attributes max-char-descent
    max-char-left-bearing max-char-right-bearing max-char-width
    merge-resources min-char-ascent min-char-attributes min-char-descent
    min-char-left-bearing min-char-right-bearing min-char-width
    missing-parameter modifier-key modifier-mapping modifier-mask
    motion-events name-error no-operation open-display open-font
    pixarray pixel pixmap pixmap-display pixmap-equal
    pixmap-error pixmap-format pixmap-format-bits-per-pixel
    pixmap-format-depth pixmap-format-p pixmap-format-scanline-pad
    pixmap-id pixmap-p pixmap-plist point-seq pointer-control
    pointer-event-mask pointer-event-mask-class pointer-mapping
    pointer-position process-event put-image put-raw-image
    query-best-cursor query-best-stipple query-best-tile query-colors
    query-extension query-keymap query-pointer query-tree queue-event
    read-bitmap-file read-resources recolor-cursor rect-seq
    remove-access-host remove-from-save-set reparent-window repeat-seq
    reply-length-error reply-timeout request-error reset-screen-saver
    resource-database resource-database-timestamp resource-error
    resource-id resource-key rgb-colormaps rgb-val root-resources
    rotate-cut-buffers rotate-properties screen screen-backing-stores
    screen-black-pixel screen-default-colormap screen-depths
    screen-event-mask-at-open screen-height screen-height-in-millimeters
    screen-max-installed-maps screen-min-installed-maps screen-p
    screen-plist screen-root screen-root-depth screen-root-visual
    screen-root-visual-info screen-save-unders-p screen-saver
    screen-white-pixel screen-width screen-width-in-millimeters seg-seq
    selection-owner send-event sequence-error set-access-control
    set-close-down-mode set-input-focus set-modifier-mapping
    set-pointer-mapping set-screen-saver set-selection-owner
    set-standard-colormap set-standard-properties set-wm-class
    set-wm-properties set-wm-resources state-keysym-p state-mask-key
    store-color store-colors stringable text-extents text-width
    timestamp transient-for translate-coordinates translate-default
    translation-function undefine-keysym unexpected-reply
    ungrab-button ungrab-key ungrab-keyboard ungrab-pointer
    ungrab-server uninstall-colormap unknown-error unmap-subwindows
    unmap-window value-error visual-info visual-info-bits-per-rgb
    visual-info-blue-mask visual-info-class visual-info-colormap-entries
    visual-info-display visual-info-green-mask visual-info-id visual-info-p
    visual-info-plist visual-info-red-mask warp-pointer
    warp-pointer-if-inside warp-pointer-relative warp-pointer-relative-if-inside
    win-gravity window window-all-event-masks window-background
    window-backing-pixel window-backing-planes window-backing-store
    window-bit-gravity window-border window-class window-colormap
    window-colormap-installed-p window-cursor window-display
    window-do-not-propagate-mask window-equal window-error
    window-event-mask window-gravity window-id window-map-state
    window-override-redirect window-p window-plist window-priority
    window-save-under window-visual window-visual-info with-display
    with-event-queue with-gcontext with-server-grabbed with-state
    withdraw-window wm-client-machine wm-colormap-windows wm-command
    wm-hints wm-hints-flags wm-hints-icon-mask wm-hints-icon-pixmap
    wm-hints-icon-window wm-hints-icon-x wm-hints-icon-y
    wm-hints-initial-state wm-hints-input wm-hints-p wm-hints-window-group
    wm-icon-name wm-name wm-normal-hints wm-protocols wm-resources
    wm-size-hints wm-size-hints-base-height wm-size-hints-base-width
    wm-size-hints-height wm-size-hints-height-inc wm-size-hints-max-aspect
    wm-size-hints-max-height wm-size-hints-max-width wm-size-hints-min-aspect
    wm-size-hints-min-height wm-size-hints-min-width wm-size-hints-p
    wm-size-hints-user-specified-position-p wm-size-hints-user-specified-size-p
    wm-size-hints-width wm-size-hints-width-inc wm-size-hints-win-gravity
    wm-size-hints-x wm-size-hints-y wm-zoom-hints write-bitmap-file
    write-resources xatom))

#+ansi-common-lisp
(common-lisp:in-package :xlib)

#-lispm
(export '(
	  compile-clx
	  load-clx))

#+excl (error "Use excldefsys")


;;;; Lisp Machines

;;; Lisp machines have their own defsystems, so we use them to define
;;; the CLX load.

#+(and lispm (not genera))
(global:defsystem CLX
  (:pathname-default "clx:clx;")
  (:patchable "clx:patch;" clx-ti)
  (:initial-status :experimental)

  (:module depdefs "depdefs")
  (:module clx "clx")
  (:module dependent "dependent")
  (:module macros "macros")
  (:module bufmac "bufmac")
  (:module buffer "buffer")
  (:module display "display")
  (:module gcontext "gcontext")
  (:module requests "requests")
  (:module input "input")
  (:module fonts "fonts")
  (:module graphics "graphics")
  (:module text "text")
  (:module attributes "attributes")
  (:module translate "translate")
  (:module keysyms "keysyms")
  (:module manager "manager")
  (:module image "image")
  (:module resource "resource")
  (:module doc "doc")

  (:compile-load depdefs)
  (:compile-load clx
   (:fasload depdefs))
  (:compile-load dependent
   (:fasload depdefs clx))
  ;; Macros only needed for compilation
  (:skip :compile-load macros
   (:fasload depdefs clx dependent))
  ;; Bufmac only needed for compilation
  (:skip :compile-load bufmac
   (:fasload depdefs clx dependent macros))
  (:compile-load buffer
   (:fasload depdefs clx dependent macros bufmac))
  (:compile-load display
   (:fasload depdefs clx dependent macros bufmac buffer))
  (:compile-load gcontext
   (:fasload depdefs clx dependent macros bufmac buffer display))
  (:compile-load input
   (:fasload depdefs clx dependent macros bufmac buffer display))
  (:compile-load requests
   (:fasload depdefs clx dependent macros bufmac buffer display input))
  (:compile-load fonts
   (:fasload depdefs clx dependent macros bufmac buffer display))
  (:compile-load graphics
   (:fasload depdefs clx dependent macros fonts bufmac buffer display fonts))
  (:compile-load text
   (:fasload depdefs clx dependent macros fonts bufmac buffer display gcontext fonts))
  (:compile-load-init attributes
   (dependent)					;<- There may be other modules needed here.
   (:fasload depdefs clx dependent macros bufmac buffer display))
  (:compile-load translate
   (:fasload depdefs clx dependent macros bufmac buffer display))
  (:compile-load keysyms
   (:fasload depdefs clx dependent macros bufmac buffer display translate))
  (:compile-load manager
   (:fasload depdefs clx dependent macros bufmac buffer display))
  (:compile-load image
   (:fasload depdefs clx dependent macros bufmac buffer display))
  (:compile-load resource)
  (:auxiliary doc)
  )


#+Genera
(scl:defsystem CLX
    (:default-pathname "SYS:X11;CLX;"
     :default-package "XLIB"
     :pretty-name "CLX"
     :maintaining-sites (:scrc)
     :distribute-sources t
     :distribute-binaries t
     :source-category :basic)
  (:module doc ("doc")
	   (:type :lisp-example))
  (:module depdefs ("depdefs" "generalock"))
  (:module clx ("clx")
	   (:uses-definitions-from depdefs))
  (:module dependent ("dependent")
	   (:uses-definitions-from clx))
  (:module macros ("macros")
	   (:uses-definitions-from dependent))
  (:module bufmac ("bufmac")
	   (:uses-definitions-from dependent macros))
  (:module buffer ("buffer")
	   (:uses-definitions-from dependent macros bufmac))
  (:module display ("display")
	   (:uses-definitions-from dependent macros bufmac buffer))
  (:module gcontext ("gcontext")
	   (:uses-definitions-from dependent macros bufmac display))
  (:module input ("input")
	   (:uses-definitions-from dependent macros bufmac display))
  (:module requests ("requests")
	   (:uses-definitions-from dependent macros bufmac display input))
  (:module fonts ("fonts")
	   (:uses-definitions-from dependent macros bufmac display))
  (:module graphics ("graphics")
	   (:uses-definitions-from dependent macros bufmac fonts))
  (:module text ("text")
	   (:uses-definitions-from dependent macros bufmac gcontext fonts))
  (:module attributes ("attributes")
	   (:uses-definitions-from dependent macros bufmac display))
  (:module translate ("translate")
	   (:uses-definitions-from dependent macros bufmac display))
  (:module keysyms ("keysyms")
	   (:uses-definitions-from translate))
  (:module manager ("manager")
	   (:uses-definitions-from dependent macros bufmac display))
  (:module image ("image")
	   (:uses-definitions-from dependent macros bufmac display))
  (:module resource ("resource"))
  )


;;;; Non Lisp Machines

#+lucid
(defvar *foreign-libraries* '("-lc")) ; '("-lresolv" "-lc") for some sites

#+lucid
(defun clx-foreign-files (binary-path)

  ;; apply a patch to 2.0 systems
  #+(and (not lcl3.0) (or mc68000 mc68020))
  (load (merge-pathnames "make-sequence-patch" binary-path))

  ;; Link lisp to the C function connect_to_server
  #+(and apollo (not lcl3.0))
  (lucid::define-foreign-function '(xlib::connect-to-server "connect_to_server")
      '((:val host    :string)
	(:val display :integer32))
    :integer32)
  #+(and (not apollo) (not lcl3.0))
  (lucid::define-c-function xlib::connect-to-server
      (host display)
    :result-type :integer)
  #+lcl3.0
  (lucid::def-foreign-function (xlib::connect-to-server 
				 (:language :c)
				 (:return-type :signed-32bit))
      (host :simple-string) (display :signed-32bit))
  (unintern 'display)

  ;; Load the definition of connect_to_server
  #+apollo
  (lucid::load-foreign-file (merge-pathnames "socket" binary-path)
			    :preserve-pathname t)
  #-apollo
  (lucid::load-foreign-files (list (merge-pathnames "socket.o" binary-path))
			     *foreign-libraries*))


;; socket interface for kcl and ibcl
;;   defines the function (open-socket-stream host display)
;;
;; You must first compile file socket.c
#+(or kcl ibcl)
(defun kcl-socket-init (binary-path)
  (let ((sockcl (namestring (merge-pathnames "sockcl.o" binary-path)))
	(socket (namestring (merge-pathnames "socket.o" binary-path))))
    (si:faslink sockcl (format nil "~a -lc" socket))
    ))


;;;; Compile CLX

;;; COMPILE-CLX compiles the lisp source files and loads the binaries.
;;; It goes to some trouble to let the source files be in one directory
;;; and the binary files in another.  Thus the same set of sources can
;;; be used for different machines and/or lisp systems.  It also allows
;;; you to supply explicit extensions, so source files do not have to
;;; be renamed to fit into the naming conventions of an implementation.

;;; For example,
;;;     (compile-clx "*.lisp" "machine/")
;;; compiles source files from the connected directory and puts them
;;; into the "machine" subdirectory.  You can then load CLX out of the
;;; machine directory.

;;; The code has no knowledge of the source file types (eg, ".l" or
;;; ".lisp") or of the binary file types (eg, ".b" or ".sbin").  Calling
;;; compile-file and load with a file type of NIL usually sorts things
;;; out correctly, but you may have to explicitly give the source and
;;; binary file types.

;;; An attempt at compiling the C language sources is also made,
;;; but you may have to set different compiler switches
;;; should be.  If it doesn't do the right thing, then do
;;;     (compile-clx "" "" :compile-c NIL)
;;; to prevent the compilation.

;;; compilation notes
;;;   lucid2.0/hp9000
;;;     must uudecode the file make-sequence-patch.uu

#-lispm
(defun compile-clx (&optional
		    (source-pathname-defaults "")
		    (binary-pathname-defaults "")
		    &key
		    (compile-c t))

  ;; The pathname-defaults above might only be strings, so coerce them
  ;; to pathnames.  Build a default binary path with every component
  ;; of the source except the file type.  This should prevent
  ;; (compile-clx "*.lisp") from destroying source files.
  (let* ((source-path (pathname source-pathname-defaults))
	 (path        (make-pathname
			:host      (pathname-host      source-path)
			:device    (pathname-device    source-path)
			:directory (pathname-directory source-path)
			:name      (pathname-name      source-path)
			:type      nil
			:version   (pathname-version   source-path)))
	 (binary-path (merge-pathnames binary-pathname-defaults
				       path)))
				       
    ;; Make sure source-path and binary-path file types are distinct so
    ;; we don't accidently overwrite the source files.  NIL should be an
    ;; ok type, but anything else spells trouble.
    (if (and (equal (pathname-type source-path)
		    (pathname-type binary-path))
	     (not (null (pathname-type binary-path))))
	(error "Source and binary pathname defaults have same type ~s ~s"
	       source-path binary-path))

    (format t ";;; Default paths: ~s ~s~%" source-path binary-path)

    ;; In lucid make sure we're using the compiler in production mode.
    #+lcl3.0
    (progn
      #-pqc 
      (cerror "Go ahead anyway."
	      "Lucid's production mode compiler must be loaded to compile CLX.")
      (proclaim '(optimize (speed 3)
			   (safety 1)
			   (space 0)
			   (compilation-speed 0))))

    (flet ((compile-and-load (filename)
	     (let ((source (merge-pathnames filename source-path))
		   (binary (merge-pathnames filename binary-path)))
	       ;; If the source and binary pathnames are the same,
	       ;; then don't supply an output file just to be sure
	       ;; compile-file defaults correctly.
	       #+(or kcl ibcl) (load source)
	       (if (equal source binary)
		   (compile-file source)
		   (compile-file source :output-file binary))
	       (load binary))))

      ;; Now compile and load all the files.
      ;; Defer compiler warnings until everything's compiled, if possible.
      (#.(if (fboundp 'with-compilation-unit)
	     'with-compilation-unit
	   #+lcl3.0 'lucid::with-deferred-warnings
	   #-lcl3.0 'progn)
       ()
       
       #+lucid
       (progn 
	 (when compile-c			; compile the C files
	   #+(and (not lcl3.0) (or mc68000 mc68020))
	   (progn				; sequence patch
	     (format t "You may need to uudecode ms-patch.uu and copy~%")
	     (format t "the result to the binary directory.~%")
	     (format t "You also must rename the file to have the canonical~%")
	     (format t "binary file type in order for lisp to realize it's a~%")
	     (format t "binary file.~%"))
	   ;; compile socket.c
	   (let* ((src  (merge-pathnames "socket.c" source-path))
		  (obj  (merge-pathnames "socket.o" binary-path))
		  (args (list "-c" (namestring src)
			      "-o" (namestring obj)
			      "-DUNIXCONN")))
	     (format t ";;; cc~{ ~a~}~%" args)
	     (multiple-value-bind (iostream estream exitstatus pid)
		 ;; in 2.0, run-program is exported from system:
		 ;; in 3.0, run-program is exported from lcl:
		 ;; system inheirits lcl
		 (system::run-program "cc" :arguments args)
	       (declare (ignore iostream estream pid))
	       (if (/= 0 exitstatus)
		   (error "Exit status of socket.c compile is ~d" exitstatus)))))
	 (format t ";;; Loading foreign files~%")
	 (clx-foreign-files binary-path))

       #+(or kcl ibcl)
       (progn
	 (when compile-c			; compile the C files
	   (let* ((src (merge-pathnames "socket.c" source-path))
		  (obj (merge-pathnames "socket.o" binary-path))
		  (arg (format nil "cc -c ~a -o ~a -DUNIXCONN"
			       (namestring src)
			       (namestring obj))))
	     (format t ";;; ~a~%" arg)
	     (if (/= 0 (system arg))
		 (error "bad exit status for ~s" src))))
	 ;; compile the lisp interface to the c code
	 (let ((src (merge-pathnames "sockcl"   source-path))
	       (obj (merge-pathnames "sockcl.o" binary-path)))
	   (compile-file src :output-file obj))
	 (kcl-socket-init binary-path))

       (compile-and-load "depdefs")
       (compile-and-load "clx")
       (compile-and-load "dependent")
       (compile-and-load "macros")		; these are just macros
       (compile-and-load "bufmac")		; these are just macros
       (compile-and-load "buffer")
       (compile-and-load "display")
       (compile-and-load "gcontext")
       (compile-and-load "input")
       (compile-and-load "requests")
       (compile-and-load "fonts")
       (compile-and-load "graphics")
       (compile-and-load "text")
       (compile-and-load "attributes")
       (compile-and-load "translate")
       (compile-and-load "keysyms")
       (compile-and-load "manager")
       (compile-and-load "image")
       (compile-and-load "resource")
       ))))


;;;; Load CLX

;;; This procedure loads the binaries for CLX.  All of the binaries
;;; should be in the same directory, so setting the default pathname
;;; should point load to the right place.

;;; You should have a module definition somewhere so the require/provide
;;; mechanism can avoid reloading CLX.  In an ideal world, somebody would
;;; just put
;;;		(REQUIRE 'CLX)
;;; in their file (some implementations don't have a central registry for
;;; modules, so a pathname needs to be supplied).

;;; The REQUIRE should find a file that does
;;;		(IN-PACKAGE 'XLIB :USE '(LISP))
;;;		(PROVIDE 'CLX)
;;;		(LOAD <clx-defsystem-file>)
;;;		(LOAD-CLX <binary-specific-clx-directory>)

#-lispm
(defun load-clx (&optional (binary-pathname-defaults "")
		 &key (macros-p nil))

  (let* ((source-path (pathname ""))
	 (path        (make-pathname
			:host      (pathname-host      source-path)
			:device    (pathname-device    source-path)
			:directory (pathname-directory source-path)
			:name      (pathname-name      source-path)
			:type      nil
			:version   (pathname-version   source-path)))
	 (binary-path (merge-pathnames binary-pathname-defaults
				       path)))

    (flet ((load-binary (filename)
	     (let ((binary (merge-pathnames filename binary-path)))
	       (load binary))))

      #+lucid
      (clx-foreign-files binary-path)

      #+(or kcl ibcl)
      (kcl-socket-init binary-path)

      (load-binary "depdefs")
      (load-binary "clx")
      (load-binary "dependent")
      (when macros-p
	(load-binary "macros")
	(load-binary "bufmac"))
      (load-binary "buffer")
      (load-binary "display")
      (load-binary "gcontext")
      (load-binary "input")
      (load-binary "requests")
      (load-binary "fonts")
      (load-binary "graphics")
      (load-binary "text")
      (load-binary "attributes")
      (load-binary "translate")
      (load-binary "keysyms")
      (load-binary "manager")
      (load-binary "image")
      (load-binary "resource")
      )))
