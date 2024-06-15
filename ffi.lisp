
;; ███████╗███████╗██╗
;; ██╔════╝██╔════╝██║
;; █████╗  █████╗  ██║
;; ██╔══╝  ██╔══╝  ██║
;; ██║     ██║     ██║
;; ╚═╝     ╚═╝     ╚═╝
(in-package :drm)
;; NOTE: Libdrm is the mesa wrapper to the drm API
(define-foreign-library libdrm (t (:default "libdrm")))
(use-foreign-library libdrm)


;; ┌─┐┌─┐┌┐┌┌─┐┌┬┐┌─┐┌┐┌┌┬┐┌─┐
;; │  │ ││││└─┐ │ ├─┤│││ │ └─┐
;; └─┘└─┘┘└┘└─┘ ┴ ┴ ┴┘└┘ ┴ └─┘
(defconstant +drm-event-context+ 3)

(defcenum bus-type
  (:pci 0)
  (:usb 1)
  (:platform 2)
  (:host 3))

(defcenum mode-connection
  (:connected 1)
  (:disconnected 2)
  (:unknown-connection 3))

(defcenum mode-subpixel
  (:unknown 1)
  :horizontal-rgb
  :horizontal-bgr
  :vertical-rgb
  :vertical-bgr
  :none)

(defcenum connector-type
  (:unknown     0)
  (:vga         1)
  (:dvii        2)
  (:dvid        3)
  (:dvia        4)
  (:composite   5)
  (:svideo      6)
  (:lvds        7)
  (:component   8)
  (:9pindin     9)
  (:displayport 10)
  (:hdmia       11)
  (:hdmib       12)
  (:tv	        13)
  (:edp         14)
  (:virtual     15)
  (:dsi         16)
  (:dpi         17)
  (:writeback   18)
  (:spi         19)
  (:usb         20))

(defbitfield (PageFlipFlags :uint32)
  (:page-flip-event 1))

;; NOTE: To extract more meaning out of the flags, you can try to refer to:
;; https://gitlab.freedesktop.org/mesa/drm/-/blob/main/include/drm/drm.h#L623
;; https://dottedmag.net/blog/03-de-drm-ioctl/
(defcenum capabilities
  (:dumb-buffer #x1)
  (:vblank-high-crtc #x2)
  (:dumb-preferred-depth #x3)
  (:dumb-prefer-shadow #x4)
  (:prime #x5)
  (:timestamp-monotonic #x6)
  (:async-page-flip #x7)
  (:cursor-width #x8)
  (:cursor-height #x9)
  (:addfb2-modifiers #x10)
  (:page-flip-target #x11)
  (:crtc-in-vblank-event #x12)
  (:syncobj #x13)
  (:syncobj-timeline #x14)
  (:atomic-async-page-flip #x15))


;; ┌─┐ ┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┌─┐
;; │───└─┐ │ ├┬┘│ ││   │ └─┐
;; └─┘ └─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘
(defcstruct pci-bus
  (domain :uint16)
  (bus :uint8)
  (dev :uint8)
  (func :uint8))

(defcstruct platform-bus
  (full-name :char :count 512))

(defcstruct host1x-bus
  (full-name :char :count 512))

(defcstruct pci-device
  (vendor-id :uint16)
  (device-id :uint16)
  (subvendor-id :uint16)
  (subdevice-id :uint16)
  (revision-id :uint8))

(defcunion bus-info
  (pci (:pointer (:struct pci-bus)))
  (platform (:pointer (:struct platform-bus)))
  (host1x (:pointer (:struct host1x-bus))))

(defcunion dev-info
  (pci (:pointer (:struct pci-device))))

(defcstruct drm-device
  ;; (nodes (:pointer (:pointer :char)))
  (nodes (:pointer :string))
  (available-nodes :int)
  (bus-type bus-type)
  (bus-info (:union bus-info))
  (dev-info (:union dev-info)))


(defcstruct mode-property-enum
  (value :uint64)
  (name :char :count 32))

(defcstruct mode-property
  (id :uint32)
  (flags :uint32)
  (name :char :count 32)
  (count-values :int)
  (values (:pointer :uint64))
  (count-enums :int)
  (enums (:pointer (:struct mode-property-enum)))
  (count-blobs :int)
  (blob_ids (:pointer :uint32)))

(defcstruct mode-res
  (count-fbs :int)
  (fbs (:pointer :uint32))
  (count-crtcs :int)
  (crtcs (:pointer :uint32))
  (count-connectors :int)
  (connectors (:pointer :uint32))
  (count-encoders :int)
  (encoders (:pointer :uint32))
  (min-width :uint32)
  (max-width :uint32)
  (min-height :uint32)
  (max-height :uint32))

(defcstruct mode-mode-info
  (clock :uint32)
  (hdisplay :uint16)
  (hsync-start :uint16)
  (hsync-end :uint16)
  (htotal :uint16)
  (hskew :uint16)
  (vdisplay :uint16)
  (vsync-start :uint16)
  (vsync-end :uint16)
  (vtotal :uint16)
  (vscan :uint16)
  (vrefresh :uint32)
  (flags :uint32)
  (type :uint32)
  (name :char :count 32))

(defcstruct mode-connector
  (connector-id :uint32)
  (encoder-id :uint32)
  (connector-type connector-type)
  (connector-type-id :uint32)
  (connection mode-connection)
  (mm-width :uint32)
  (mm-height :uint32)
  (subpixel mode-subpixel)
  (count-modes :int) ;; defined as just int
  (modes (:pointer (:struct mode-mode-info)))
  (count-props :int) ;; defined as just int
  (props (:pointer :uint32))
  (prop-values (:pointer :uint64))
  (count-encodes :int)
  (encoders (:pointer :uint32)))

(defcstruct mode-encoder
  (encoder-id :uint32)
  (encoder-type :uint32)
  (crtc-id :uint32)
  (possible-crtcs :uint32)
  (possible-clones :uint32))

(defcstruct mode-crtc
  (crtc-id :uint32)
  (buffer-id :uint32)
  (x :uint32)
  (y :uint32)
  (width :uint32)
  (height :uint32)
  (mode-valid :int)
  (mode (:struct mode-mode-info))
  (gamma-size :int))

(defcstruct drm-event
  (type :uint32)
  (length :uint32))

(defcstruct drm-event-vblank
  (base (:struct drm-event))
  (user-data :uint64)
  (tv-sec :uint32)
  (tv-usec :uint32)
  (sequence :uint32)
  (crtc-id :uint32))

(defcstruct event-context
  (version :int)
  (vblank_handler :pointer)
  (page_flip_handler :pointer)
  (page_flip_handler2 :pointer)
  (sequence_handler :pointer))



;; ███████╗██╗   ██╗███╗   ██╗ ██████╗███████╗
;; ██╔════╝██║   ██║████╗  ██║██╔════╝██╔════╝
;; █████╗  ██║   ██║██╔██╗ ██║██║     ███████╗
;; ██╔══╝  ██║   ██║██║╚██╗██║██║     ╚════██║
;; ██║     ╚██████╔╝██║ ╚████║╚██████╗███████║
;; ╚═╝      ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝╚══════╝


;; ┌─┐┌─┐┌┐┌┌┐┌┌─┐┌─┐┌┬┐┌─┐┬─┐
;; │  │ │││││││├┤ │   │ │ │├┬┘
;; └─┘└─┘┘└┘┘└┘└─┘└─┘ ┴ └─┘┴└─
(defcfun ("drmModeGetConnector" mode-get-connector) (:pointer (:struct mode-connector))
  (fd :int)
  (connector-id :uint32))

(defcfun ("drmModeFreeConnector" mode-free-connector) :void
  (connector (:pointer (:struct mode-connector))))

(defcfun ("drmModeConnectorGetPossibleCrtcs" %connector-get-possible-crtcs) :uint32
  (fd :int)
  (connector (:pointer (:struct mode-connector))))

;; ┌┬┐┌─┐┌─┐┌┬┐┌─┐┬─┐
;; │││├─┤└─┐ │ ├┤ ├┬┘
;; ┴ ┴┴ ┴└─┘ ┴ └─┘┴└─
(defcfun ("drmSetMaster" set-master) :int
  (fd :int))

(defcfun ("drmDropMaster" drop-master) :int
  (fd :int))


;; ┌─┐┬─┐┌┬┐┌─┐
;; │  ├┬┘ │ │
;; └─┘┴└─ ┴ └─┘
(defcfun ("drmModeGetCrtc" mode-get-crtc) (:pointer (:struct mode-crtc))
  (fd :int)
  (crtc-id :uint32))

(defcfun ("drmModeFreeCrtc" mode-free-crtc) :void
  (crtc (:pointer (:struct mode-crtc))))

(defcfun ("drmModeSetCrtc" mode-set-crtc) :int
  (fd :int)
  (crtc-id :uint32)
  (buffer-id :uint32)
  (x :uint32)
  (y :uint32)
  (connectors (:pointer :uint32))
  (count :int)
  (mode (:pointer (:struct mode-mode-info))))


;; ┌─┐┬─┐┌─┐┌─┐┌─┐┬─┐┌┬┐┬┌─┐┌─┐
;; ├─┘├┬┘│ │├─┘├┤ ├┬┘ │ │├┤ └─┐
;; ┴  ┴└─└─┘┴  └─┘┴└─ ┴ ┴└─┘└─┘

(defcfun ("drmModeGetProperty" %mode-get-property) (:pointer (:struct mode-property))
  (fd :int)
  (property-id :uint32))


;; ┌─┐┌┐┌┌─┐┌─┐┌┬┐┌─┐┬─┐
;; ├┤ ││││  │ │ ││├┤ ├┬┘
;; └─┘┘└┘└─┘└─┘─┴┘└─┘┴└─
(defcfun ("drmModeGetEncoder" mode-get-encoder) :pointer
  (fd :int)
  (encoder-id :uint32))

(defcfun ("drmModeFreeEncoder" mode-free-encoder) :void
  (encoder (:pointer (:struct mode-encoder))))


;; ┬─┐┌─┐┌─┐┌─┐┬ ┬┬─┐┌─┐┌─┐┌─┐
;; ├┬┘├┤ └─┐│ ││ │├┬┘│  ├┤ └─┐
;; ┴└─└─┘└─┘└─┘└─┘┴└─└─┘└─┘└─┘
(defcfun ("drmModeFreeResources" mode-free-resources) :void
  (resources (:pointer (:struct mode-res))))

(defcfun ("drmModeGetResources" mode-get-resources) (:pointer (:struct mode-res))
  (fd :int))


;; ┌─┐┌┐
;; ├┤ ├┴┐
;; └  └─┘
(defcfun ("drmModeAddFB" mode-add-framebuffer) :int
  (fd :int)
  (width :uint32)
  (height :uint32)
  (depth :uint8)
  (bpp :uint8)
  (pitch :uint32)
  (bo-handle :uint32)
  (buf-id (:pointer :uint32)))

(defcfun ("drmModeRmFB" mode-remove-framebuffer) :int
  (fd :int)
  (buffer-id :uint32))


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │ └─┐
;; └─┘ └┘ └─┘┘└┘ ┴ └─┘
(defcfun ("drmModePageFlip" mode-page-flip) :int
  (fd :int)
  (crtc-id :uint32)
  (fb-id :uint32)
  (flags PageFlipFlags)
  (user-data :pointer))

(defcfun ("drmHandleEvent" drm-handle-event) :int
  (fd :int)
  (event-context (:pointer (:struct event-context))))


;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐┌─┐
;;  ││├┤ └┐┌┘││  ├┤ └─┐
;; ─┴┘└─┘ └┘ ┴└─┘└─┘└─┘
(defcfun ("drmGetCap" %get-cap) :int
  (fd :int)
  (capability capabilities)
  (value (:pointer :uint64)))

(defcfun ("drmGetDevices2" %get-devices) :int
  (flags :uint32)
  (devices (:pointer (:pointer (:struct drm-device))))
  (num-devices :int))

(defcfun ("drmFreeDevices" %free-devices) :void
  (devices (:pointer (:pointer (:struct drm-device))))
  (num-devices :int))
