
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
(defpackage :drm
  (:use :cl :cffi)
  (:export
   get-resources
   get-devices

   device!-primary device!-render

   resources-crtcs
   resources-connectors
   resources-encoders

   set-crtc

   handle-event


   crtc!-id
   crtc!-buffer-id
   crtc!-x
   crtc!-y
   crtc!-width
   crtc!-height
   crtc!-mode
   crtc!-mode-valid
   crtc!-mode-ptr
   crtc!-gamma-size

   encoder!-id
   encoder!-encoder-type
   encoder!-crtc-id
   encoder!-possible-crtcs
   encoder!-possible-clones

   connector!-id
   connector!-encoders
   connector!-connector-type
   connector!-connector-type-id
   connector!-connection
   connector!-mm-width
   connector!-mm-height
   connector!-subpixel
   connector!-modes
   connector!-props
   connector!-possible-crtcs

   mode!-ptr
   mode!-clock
   mode!-hdisplay
   mode!-hsync-start
   mode!-hsync-end
   mode!-htotal
   mode!-hskew
   mode!-vdisplay
   mode!-vsync-start
   mode!-vsync-end
   mode!-vtotal
   mode!-vscan
   mode!-vrefresh
   mode!-flags
   mode!-type
   mode!-name))
