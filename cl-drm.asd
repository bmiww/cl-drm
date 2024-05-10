
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
;; TODO: Maybe an argument can be made that the original package was trivial enough to not warrant a fork.
;; NOTE: Primarily grabbed from here
;; https://github.com/malcolmstill/cl-drm/blob/master/cl-drm.lisp
(asdf:defsystem #:cl-drm
  :serial t
  :description "Common lisp DRM ffi integration"
  :author "bmiww <bmiww@bky.one>" ;; TODO: Add the original author to the list.
  :license "GPLv3" ;; TODO: Check if the license actually matches somewhat.
  :version "0.0.1"
  :depends-on (#:cffi #:cl-async)
  :components ((:file "package")
	       (:file "fourcc")
	       (:file "ffi")
	       (:file "drm")))
