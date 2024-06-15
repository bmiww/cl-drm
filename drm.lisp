
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
;; TODO: The resource stuff could be turned into a smart class with methods interacting with the crtc/connectors blafu
;; NOTE: Some documentation on drm usage:
;; https://landley.net/kdocs/htmldocs/drm.html#idp5032288
;; NOTE: Some virtual KMS tool that could be used for testing DRM integration?
;; https://www.kernel.org/doc/html/v6.8/gpu/drm-uapi.html?highlight=drm+plane#using-vkms-to-test-drm-api
(in-package :drm)

(defvar nodes-max 3)
(defun node-name (node) (if (string= node "") nil node))

(defun get-devices ()
  "Get the devices available on the system"
  ;; NOTE: libdrm mentions a max of 256 devices
  ;; TODO: Make flags settable by the user
  (with-foreign-pointer (array 256 size)
    (let ((result (%get-devices 0 array size))
	  (devices nil))
      (when (< result 0) (error (format nil "Failed to get devices: ~a" result)))
      (setf devices (loop for i from 0 below result
			  collect
			  (let* ((new-struct (make-device!))
				 (structure (mem-ref (mem-aref array '(:pointer (:struct drm-device)) i) '(:struct drm-device)))
				 (nodes (loop for i from 0 below nodes-max
					      collect (mem-aref (getf structure 'nodes) :string i)))
				 (bus-info (getf structure 'bus-info))
				 (dev-info (getf structure 'dev-info)))

			    (setf (device!-dev-info new-struct) (mem-ref (getf dev-info 'pci) '(:struct pci-device)))
			    (setf (device!-bus new-struct)
				  (case (getf structure 'bus-type)
				    (:pci      (mem-ref (getf bus-info 'pci) '(:struct pci-bus)))
				    (:platform (mem-ref (getf bus-info 'platform) '(:struct platform-bus)))
				    (:host1x   (mem-ref (getf bus-info 'host1x) '(:struct host1x-bus)))
				    (t (error (format nil "Unhandled bus type: ~a" (getf structure 'bus-type))))))

			    (setf (device!-primary new-struct) (node-name (car nodes)))
			    (setf (device!-control new-struct) (node-name (cadr nodes)))
			    (setf (device!-render new-struct) (node-name (caddr nodes)))

			    new-struct)))
      (%free-devices array result)
      devices)))


(defun set-crtc (fd crtc-id buffer-id x y connectors mode-ptr &optional (count (length connectors)))
  (with-foreign-objects ((connectors-p :uint32 count))
    (dotimes (i count) (setf (mem-aref connectors-p :uint32 i) (nth i connectors)))
    (mode-set-crtc fd crtc-id buffer-id x y connectors-p count mode-ptr)))

(defun get-crtc (fd crtc-id)
  (mode-get-crtc fd crtc-id))

(defun mk-crtc (c-crtc)
  (let ((de-pointerd (mem-ref c-crtc '(:struct mode-crtc))))
    (make-crtc! :id         (getf de-pointerd 'crtc-id)
		:buffer-id  (getf de-pointerd 'buffer-id)
		:x          (getf de-pointerd 'x)
		:y          (getf de-pointerd 'y)
		:width      (getf de-pointerd 'width)
		:height     (getf de-pointerd 'height)
		:mode-valid (getf de-pointerd 'mode-valid)
		:mode       (mk-mode (getf de-pointerd 'mode) (foreign-slot-pointer c-crtc '(:struct mode-crtc) 'mode))
		:gamma-size (getf de-pointerd 'gamma-size))))

(defun mk-prop (prop value fd)
  (let ((prop-pointer (%mode-get-property fd prop)))
    (if (null-pointer-p prop-pointer)
	nil
	(let ((de-pointerd (mem-ref prop-pointer '(:struct mode-property))))

	  ;; TODO: Could still add values/enums/blob_ids
	  (make-prop :id prop :value value
		     :flags (getf de-pointerd 'flags)
		     :name (foreign-string-to-lisp (getf de-pointerd 'name) :count 32))))))

(defun mk-connector (c-connector fd crtcs)
  (let ((de-pointerd (mem-ref c-connector '(:struct mode-connector))))
    (make-connector!
     :id (getf de-pointerd 'connector-id)
     :encoder-id (getf de-pointerd 'encoder-id)
     :connector-type (getf de-pointerd 'connector-type)
     :connector-type-id (getf de-pointerd 'connector-type-id)
     :possible-crtcs (let* ((possible-crtcs (%connector-get-possible-crtcs fd c-connector))
			    (ids (get-uint32-bit-indices possible-crtcs)))
		       (mapcar (lambda (id) (nth id crtcs)) ids))
     :connection (getf de-pointerd 'connection)
     :mm-width (getf de-pointerd 'mm-width)
     :mm-height (getf de-pointerd 'mm-height)
     :subpixel (getf de-pointerd 'subpixel)
     :modes (let ((count (getf de-pointerd 'count-modes))
		  (modes (getf de-pointerd 'modes)))
	      (loop for i from 0 below count
		    for mode = (let ((ptr (mem-aptr modes '(:struct mode-mode-info) i)))
				 (mk-mode (mem-ref ptr '(:struct mode-mode-info)) ptr))
		    when (and mode (> (mode!-clock mode) 0)) collect mode))
     :props (let ((count (getf de-pointerd 'count-props))
		  (props (getf de-pointerd 'props))
		  (prop-values (getf de-pointerd 'prop-values)))
	      (loop for i from 0 below count
		    for prop = (mk-prop (mem-aref props :uint32 i) (mem-aref prop-values :uint64 i) fd)
		    when prop collect prop))
     :encoders (let ((count (getf de-pointerd 'count-encodes))
		     (encoders (getf de-pointerd 'encoders)))
		 (loop for i from 0 below count
		       collect (mem-aref encoders :uint32 i))))))

(defun mk-encoder (c-encoder)
  (let ((de-pointerd (mem-ref c-encoder '(:struct mode-encoder))))
    (make-encoder! :id (getf de-pointerd 'encoder-id)
		   :encoder-type (getf de-pointerd 'encoder-type)
		   :crtc-id (getf de-pointerd 'crtc-id)
		   :possible-crtcs (getf de-pointerd 'possible-crtcs)
		   :possible-clones (getf de-pointerd 'possible-clones))))

(defun mk-plane (c-plane)
  (let ((plane (mem-ref c-plane '(:struct mode-plane))))
    (make-plane!
     :id (getf plane 'plane-id)
     :crtc-id (getf plane 'crtc-id)
     :fb-id (getf plane 'fb-id)
     :formats (let ((count (getf plane 'count-formats))
		    (formats (getf plane 'formats)))
		(loop for i from 0 below count
		      collect (mem-aref formats :uint32 i)))
     :crtc-x (getf plane 'crtc-x)
     :crtc-y (getf plane 'crtc-y)
     :x (getf plane 'x)
     :y (getf plane 'y)
     :possible-crtcs (getf plane 'possible-crtcs)
     :gamma-size (getf plane 'gamma-size))))

(defun mk-mode (c-mode-info ptr)
  (make-mode!
   :ptr ptr
   :clock (getf c-mode-info 'clock)
   :hdisplay (getf c-mode-info 'hdisplay)
   :hsync-start (getf c-mode-info 'hsync-start)
   :hsync-end (getf c-mode-info 'hsync-end)
   :htotal (getf c-mode-info 'htotal)
   :hskew (getf c-mode-info 'hskew)
   :vdisplay (getf c-mode-info 'vdisplay)
   :vsync-start (getf c-mode-info 'vsync-start)
   :vsync-end (getf c-mode-info 'vsync-end)
   :vtotal (getf c-mode-info 'vtotal)
   :vscan (getf c-mode-info 'vscan)
   :vrefresh (getf c-mode-info 'vrefresh)
   :flags (getf c-mode-info 'flags)
   :type (getf c-mode-info 'type)
   :name (foreign-string-to-lisp (getf c-mode-info 'name) :count 32)))

(defun get-encoder-by-id (resources id)
  (find-if (lambda (encoder) (= id (encoder!-id encoder))) (resources-encoders resources)))

(defun get-resources (fd)
  (let* ((resources (mode-get-resources fd))
	(planes (%get-plane-resources fd))
	(resources-out nil))
    (with-foreign-slots
	((crtcs count-crtcs connectors count-connectors
		fbs count-fbs encoders count-encoders
		min-width max-width min-height max-height)
	 resources (:struct mode-res))
      (let* ((crtcs (loop for i from 0 below count-crtcs
			  collect (mk-crtc (mode-get-crtc fd (mem-aref crtcs :uint32 i)))))
	     (connectors (loop for i from 0 below count-connectors
			       collect (mk-connector (mode-get-connector fd (mem-aref connectors :uint32 i)) fd crtcs)))
	     (encoders (loop for i from 0 below count-encoders
			     collect (mk-encoder (mode-get-encoder fd (mem-aref encoders :uint32 i))))))
	(setf resources-out
	      (make-resources
	       :resources resources
	       ;; :fbs (loop for i from 0 below count-fbs collect (mem-aref fbs i))
	       :crtcs crtcs
	       :connectors connectors
	       :encoders encoders
	       ;; TODO: SBCL Specific
	       :dev-t (sb-posix:stat-rdev (sb-posix:fstat fd))
	       :min-width min-width
	       :max-width max-width
	       :min-height min-height
	       :max-height max-height
	       :planes
	       (with-foreign-slots ((count-planes planes)
				    planes (:struct mode-plane-res))
		 (loop for i from 0 below count-planes
		       collect (mk-plane (%get-plane fd (mem-aref planes :uint32 i)))))
	       ;; TODO: Make this not a default?
	       :capabilities (get-all-capabilities fd)))))
    (mode-free-resources resources)
    resources-out))


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │ └─┐
;; └─┘ └┘ └─┘┘└┘ ┴ └─┘
;; TODO: Should clean up the *event-context* global when done
(defvar *vblank-callback* nil)
(defcallback vblank :void
    ((fd :int) (sequence :uint) (tv-sec :uint) (tv-usec :uint) (data :pointer))
  (when *vblank-callback* (funcall *vblank-callback* fd sequence tv-sec tv-usec data))
  (format t "Vblank arguments: ~a ~a ~a ~a~%" fd sequence tv-sec tv-usec))

(defvar *page-flip-callback* nil)
(defcallback page-flip :void
    ((fd :int) (sequence :uint) (tv-sec :uint) (tv-usec :uint) (data :pointer))
  (when *page-flip-callback* (funcall *page-flip-callback* fd sequence tv-sec tv-usec data)))

(defvar *page-flip2-callback* nil)
(defcallback page-flip2 :void
    ((fd :int) (sequence :uint) (tv-sec :uint) (tv-usec :uint) (crtc-id :uint) (data :pointer))
  (when *page-flip2-callback* (funcall *page-flip2-callback* fd sequence tv-sec tv-usec crtc-id data)))

(defvar *sequence-callback* nil)
(defcallback %sequence :void
    ((fd :int) (sequence :uint64) (ns :uint64) (data :uint64))
  (when *sequence-callback* (funcall *sequence-callback* fd sequence ns data)))

(defvar *event-context* nil)
(defun handle-event (fd &key vblank page-flip page-flip2 sequence)
  "Handle a drm event. Usually paired with polling on the fd.
Sets the callback function whenever invoked. Not thread-safe as far as i assume."
  (unless *event-context*
    (setf *event-context* (foreign-alloc '(:struct event-context)))
    (setf (foreign-slot-value *event-context* '(:struct event-context) 'version) +drm-event-context+)
    (setf (foreign-slot-value *event-context* '(:struct event-context) 'vblank_handler) (callback vblank))
    (setf (foreign-slot-value *event-context* '(:struct event-context) 'page_flip_handler) (callback page-flip))
    (setf (foreign-slot-value *event-context* '(:struct event-context) 'page_flip_handler2) (callback page-flip2))
    (setf (foreign-slot-value *event-context* '(:struct event-context) 'sequence_handler) (callback %sequence)))

  (when vblank (setf *vblank-callback* vblank))
  (when page-flip (setf *page-flip-callback* page-flip))
  (when page-flip2 (setf *page-flip2-callback* page-flip2))
  (when sequence (setf *sequence-callback* sequence))

  (drm-handle-event fd *event-context*))


;; ┌─┐┌─┐┌─┐┌─┐┌┐ ┬┬  ┬┌┬┐┬ ┬  ┌─┐ ┬ ┬┌─┐┬─┐┬ ┬
;; │  ├─┤├─┘├─┤├┴┐││  │ │ └┬┘  │─┼┐│ │├┤ ├┬┘└┬┘
;; └─┘┴ ┴┴  ┴ ┴└─┘┴┴─┘┴ ┴  ┴   └─┘└└─┘└─┘┴└─ ┴
(defvar +capabilities+
  (list
   :dumb-buffer :dumb-preferred-depth :dumb-prefer-shadow
   :vblank-high-crtc :crtc-in-vblank-event
   :prime :timestamp-monotonic :async-page-flip
   :cursor-width :cursor-height
   :addfb2-modifiers
   :page-flip-target
   :syncobj :syncobj-timeline
   :atomic-async-page-flip))

(defvar +client-capabilities+
  (list
   :stereo-3d :atomic
   :universal-planes :aspect-ratio
   :writeback-connectors :cursor-plane-hotspot))

(defvar +cap-type+
  (list
   ;; Regular capabilities for get-cap
   :dumb-buffer :boolean
   :vblank-high-crtc :boolean
   :dumb-prefer-shadow :boolean
   :timestamp-monotonic :boolean
   :async-page-flip :boolean
   :addfb2-modifiers :boolean
   :page-flip-target :boolean
   :crtc-in-vblank-event :boolean
   :syncobj :boolean
   :syncobj-timeline :boolean
   :atomic-async-page-flip :boolean
   ;; Client set capabilities for set-cap
   :stereo-3d :boolean
   :universal-planes :boolean
   :aspect-ratio :boolean
   :writeback-connectors :boolean
   :atomic :boolean
   :cursor-plane-hotspot :boolean))

(defun get-all-capabilities (fd)
  (loop for capability in +capabilities+
	collect capability
	collect (with-foreign-object (value :uint64)
		  (unless (zerop (%get-cap fd capability value))
		    (format t "Failed to get capability ~a~%" capability))

		  (let ((cap-type (getf +cap-type+ capability)) (value (mem-ref value :uint64)))
		    (case cap-type
		      (:boolean (not (zerop value)))
		      (t value))))))

;; TODO: I don't know about cursor-plane-hotspot -
(defun enable-capabilities (fd &rest args)
  (loop for cap in args
	do (progn
	     (unless (member cap +client-capabilities+) (error "Invalid client capability ~a" cap))
	     (set-cap fd cap t))))


(defun set-cap (fd capability value)
  (let ((ret (%set-client-cap
	      fd capability
	      (case (getf +cap-type+ capability)
		(:boolean (if value 1 0))
		(t (error "Capability ~a is not a boolean flag" capability))))))
    (unless (zerop ret) (error "Failed to set capability: ~a. Code: ~a" capability ret))))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun get-uint32-bit-indices (uint32)
  (loop for i from 0 below 32
	for indice = (if (logbitp i uint32) i nil)
	when indice collect indice))
