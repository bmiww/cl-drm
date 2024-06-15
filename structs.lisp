
;; ███████╗████████╗██████╗ ██╗   ██╗ ██████╗████████╗███████╗
;; ██╔════╝╚══██╔══╝██╔══██╗██║   ██║██╔════╝╚══██╔══╝██╔════╝
;; ███████╗   ██║   ██████╔╝██║   ██║██║        ██║   ███████╗
;; ╚════██║   ██║   ██╔══██╗██║   ██║██║        ██║   ╚════██║
;; ███████║   ██║   ██║  ██║╚██████╔╝╚██████╗   ██║   ███████║
;; ╚══════╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝  ╚═════╝   ╚═╝   ╚══════╝
(in-package :drm)

(defstruct crtc!
  (id nil) (buffer-id nil)
  (x nil) (y nil)
  (width nil) (height nil)
  (mode-valid nil) (mode nil)
  (gamma-size nil))

(defstruct connector!
  (id nil) (encoder-id nil)
  (connector-type nil) (connector-type-id nil) (connection nil)
  (possible-crtcs nil)
  (mm-width nil) (mm-height nil) (subpixel nil)
  (modes nil) (props nil) (encoders nil))

(defstruct encoder!
  (id nil) (encoder-type nil) (crtc-id nil)
  (possible-crtcs nil) (possible-clones nil))

(defstruct resources
  (resources nil) (dev-t nil)
  (fbs nil) (crtcs nil)
  (connectors nil) (encoders nil)
  (min-width nil) (max-width nil)
  (min-height nil) (max-height nil)
  (capabilities nil))

(defstruct mode!
  (ptr nil) (clock nil)
  (hdisplay nil) (hsync-start nil) (hsync-end nil) (htotal nil) (hskew nil)
  (vdisplay nil) (vsync-start nil) (vsync-end nil) (vtotal nil) (vscan nil) (vrefresh nil)
  (flags nil) (type nil) (name nil))

(defstruct prop id value flags name)

(defstruct device!
  (primary nil) (control nil) (render nil)
  (bus nil) (dev-info nil))
