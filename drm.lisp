
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

;; TODO: Does not need the fancy constructor
(defstruct crtc!
  (id nil)
  (buffer-id nil)
  (x nil)
  (y nil)
  (width nil)
  (height nil)
  (mode-valid nil)
  (mode nil)
  (gamma-size nil))

(defstruct connector!
  (id nil)
  (encoder-id nil)
  (connector-type nil)
  (connector-type-id nil)
  (connection nil)
  (mm-width nil)
  (mm-height nil)
  (subpixel nil)
  (modes nil)
  (props nil)
  (encoders nil))

(defstruct encoder!
  (id nil)
  (encoder-type nil)
  (crtc-id nil)
  (possible-crtcs nil)
  (possible-clones nil))

(defstruct resources
  (resources nil)
  (dev-t nil)
  (fbs nil)
  (crtcs nil)
  (connectors nil)
  (encoders nil)
  (min-width nil)
  (max-width nil)
  (min-height nil)
  (max-height nil))

(defstruct mode!
  (ptr nil)
  (clock nil)
  (hdisplay nil)
  (hsync-start nil)
  (hsync-end nil)
  (htotal nil)
  (hskew nil)
  (vdisplay nil)
  (vsync-start nil)
  (vsync-end nil)
  (vtotal nil)
  (vscan nil)
  (vrefresh nil)
  (flags nil)
  (type nil)
  (name nil))

(defstruct prop id value flags name)

;; ┌─┐┬ ┬┌┐┌┌─┐┌─┐
;; ├┤ │ │││││  └─┐
;; └  └─┘┘└┘└─┘└─┘
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

(defun mk-connector (c-connector fd)
  (let ((de-pointerd (mem-ref c-connector '(:struct mode-connector))))
    (make-connector!
     :id (getf de-pointerd 'connector-id)
     :encoder-id (getf de-pointerd 'encoder-id)
     :connector-type (getf de-pointerd 'connector-type)
     :connector-type-id (getf de-pointerd 'connector-type-id)
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
  (let ((resources (mode-get-resources fd))
	(resources-out nil))
    (with-foreign-slots
	((crtcs count-crtcs connectors count-connectors
		fbs count-fbs encoders count-encoders
		min-width max-width min-height max-height)
	 resources (:struct mode-res))
      (setf resources-out
	    (make-resources
	     :resources resources
	     ;; :fbs (loop for i from 0 below count-fbs collect (mem-aref fbs i))
	     :crtcs      (loop for i from 0 below count-crtcs
			       collect (mk-crtc (mode-get-crtc fd (mem-aref crtcs :uint32 i))))
	     :connectors (loop for i from 0 below count-connectors
			       collect (mk-connector (mode-get-connector fd (mem-aref connectors :uint32 i)) fd))
	     :encoders   (loop for i from 0 below count-encoders
			       collect (mk-encoder (mode-get-encoder fd (mem-aref encoders :uint32 i))))
	     ;; TODO: SBCL Specific
	     :dev-t (sb-posix:stat-rdev (sb-posix:fstat fd))
	     :min-width min-width
	     :max-width max-width
	     :min-height min-height
	     :max-height max-height)))
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
