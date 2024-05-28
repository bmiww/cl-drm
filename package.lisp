
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

   resources-crtcs
   resources-connectors
   resources-encoders

   set-crtc
   free-crtc

   mode-crtc-width
   mode-crtc-height
   handle-event

   mode-hdisplay
   mode-vdisplay

   crtc!-id

   encoder!-id
   encoder!-crtc-id

   connector!-encoders
   connector!-connector-type))
