
;; ███████╗ ██████╗ ██╗   ██╗██████╗  ██████╗ ██████╗
;; ██╔════╝██╔═══██╗██║   ██║██╔══██╗██╔════╝██╔════╝
;; █████╗  ██║   ██║██║   ██║██████╔╝██║     ██║
;; ██╔══╝  ██║   ██║██║   ██║██╔══██╗██║     ██║
;; ██║     ╚██████╔╝╚██████╔╝██║  ██║╚██████╗╚██████╗
;; ╚═╝      ╚═════╝  ╚═════╝ ╚═╝  ╚═╝ ╚═════╝ ╚═════╝
;; NOTE: This keeps enum values for the DRM formats and format modifiers.
;; https://github.com/torvalds/linux/blob/master/include/uapi/drm/drm_fourcc.h
;; Last updated: 09.05.2024 (dd.mm.yyyy).
;; For now - this is a manual process.
(defpackage :drm-fourcc
  (:use :cl :cffi))
(in-package :drm-fourcc)

;; Formats can also be easily parsed from the wayland core protocol xml file.
(defcenum format
  (:argb8888 0)
  (:xrgb8888 1)
  (:c8 #x20203843)
  (:rgb332 #x38424752)
  (:bgr233 #x38524742)
  (:xrgb4444 #x32315258)
  (:xbgr4444 #x32314258)
  (:rgbx4444 #x32315852)
  (:bgrx4444 #x32315842)
  (:argb4444 #x32315241)
  (:abgr4444 #x32314241)
  (:rgba4444 #x32314152)
  (:bgra4444 #x32314142)
  (:xrgb1555 #x35315258)
  (:xbgr1555 #x35314258)
  (:rgbx5551 #x35315852)
  (:bgrx5551 #x35315842)
  (:argb1555 #x35315241)
  (:abgr1555 #x35314241)
  (:rgba5551 #x35314152)
  (:bgra5551 #x35314142)
  (:rgb565 #x36314752)
  (:bgr565 #x36314742)
  (:rgb888 #x34324752)
  (:bgr888 #x34324742)
  (:xbgr8888 #x34324258)
  (:rgbx8888 #x34325852)
  (:bgrx8888 #x34325842)
  (:abgr8888 #x34324241)
  (:rgba8888 #x34324152)
  (:bgra8888 #x34324142)
  (:xrgb2101010 #x30335258)
  (:xbgr2101010 #x30334258)
  (:rgbx1010102 #x30335852)
  (:bgrx1010102 #x30335842)
  (:argb2101010 #x30335241)
  (:abgr2101010 #x30334241)
  (:rgba1010102 #x30334152)
  (:bgra1010102 #x30334142)
  (:yuyv #x56595559)
  (:yvyu #x55595659)
  (:uyvy #x59565955)
  (:vyuy #x59555956)
  (:ayuv #x56555941)
  (:nv12 #x3231564e)
  (:nv21 #x3132564e)
  (:nv16 #x3631564e)
  (:nv61 #x3136564e)
  (:yuv410 #x39565559)
  (:yvu410 #x39555659)
  (:yuv411 #x31315559)
  (:yvu411 #x31315659)
  (:yuv420 #x32315559)
  (:yvu420 #x32315659)
  (:yuv422 #x36315559)
  (:yvu422 #x36315659)
  (:yuv444 #x34325559)
  (:yvu444 #x34325659)
  (:r8 #x20203852)
  (:r16 #x20363152)
  (:rg88 #x38384752)
  (:gr88 #x38385247)
  (:rg1616 #x32334752)
  (:gr1616 #x32335247)
  (:xrgb16161616f #x48345258)
  (:xbgr16161616f #x48344258)
  (:argb16161616f #x48345241)
  (:abgr16161616f #x48344241)
  (:xyuv8888 #x56555958)
  (:vuy888 #x34325556)
  (:vuy101010 #x30335556)
  (:y210 #x30313259)
  (:y212 #x32313259)
  (:y216 #x36313259)
  (:y410 #x30313459)
  (:y412 #x32313459)
  (:y416 #x36313459)
  (:xvyu2101010 #x30335658)
  (:xvyu12_16161616 #x36335658)
  (:xvyu16161616 #x38345658)
  (:y0l0 #x304c3059)
  (:x0l0 #x304c3058)
  (:y0l2 #x324c3059)
  (:x0l2 #x324c3058)
  (:yuv420_8bit #x38305559)
  (:yuv420_10bit #x30315559)
  (:xrgb8888_a8 #x38415258)
  (:xbgr8888_a8 #x38414258)
  (:rgbx8888_a8 #x38415852)
  (:bgrx8888_a8 #x38415842)
  (:rgb888_a8 #x38413852)
  (:bgr888_a8 #x38413842)
  (:rgb565_a8 #x38413552)
  (:bgr565_a8 #x38413542)
  (:nv24 #x3432564e)
  (:nv42 #x3234564e)
  (:p210 #x30313250)
  (:p010 #x30313050)
  (:p012 #x32313050)
  (:p016 #x36313050)
  (:axbxgxrx106106106106 #x30314241)
  (:nv15 #x3531564e)
  (:q410 #x30313451)
  (:q401 #x31303451)
  (:xrgb16161616 #x38345258)
  (:xbgr16161616 #x38344258)
  (:argb16161616 #x38345241)
  (:abgr16161616 #x38344241))


;; For more information on each modifier, please refer to the linked header file
;; TODO: This isn't the best way to do this, since a lot of the values are the same
;; A keyword lookup is problematic and would require a different structure
(defcenum modifier
  (:linear 0))


;; #define AMD_FMT_MOD_TILE_VER_GFX9 1
;; #define AMD_FMT_MOD_TILE_VER_GFX10 2
;; #define AMD_FMT_MOD_TILE_VER_GFX10_RBPLUS 3
;; #define AMD_FMT_MOD_TILE_VER_GFX11 4

;; #define AMD_FMT_MOD_TILE_GFX9_64K_S 9
;; #define AMD_FMT_MOD_TILE_GFX9_64K_D 10
;; #define AMD_FMT_MOD_TILE_GFX9_64K_S_X 25
;; #define AMD_FMT_MOD_TILE_GFX9_64K_D_X 26
;; #define AMD_FMT_MOD_TILE_GFX9_64K_R_X 27
;; #define AMD_FMT_MOD_TILE_GFX11_256K_R_X 31

;; #define AMD_FMT_MOD_DCC_BLOCK_64B 0
;; #define AMD_FMT_MOD_DCC_BLOCK_128B 1
;; #define AMD_FMT_MOD_DCC_BLOCK_256B 2

;; #define AMD_FMT_MOD_TILE_VERSION_SHIFT 0
;; #define AMD_FMT_MOD_TILE_VERSION_MASK 0xFF
;; #define AMD_FMT_MOD_TILE_SHIFT 8
;; #define AMD_FMT_MOD_TILE_MASK 0x1F

;; #define AMD_FMT_MOD_DCC_SHIFT 13
;; #define AMD_FMT_MOD_DCC_MASK 0x1

;; #define AMD_FMT_MOD_DCC_RETILE_SHIFT 14
;; #define AMD_FMT_MOD_DCC_RETILE_MASK 0x1
;; #define AMD_FMT_MOD_DCC_PIPE_ALIGN_SHIFT 15
;; #define AMD_FMT_MOD_DCC_PIPE_ALIGN_MASK 0x1
;; #define AMD_FMT_MOD_DCC_INDEPENDENT_64B_SHIFT 16
;; #define AMD_FMT_MOD_DCC_INDEPENDENT_64B_MASK 0x1
;; #define AMD_FMT_MOD_DCC_INDEPENDENT_128B_SHIFT 17
;; #define AMD_FMT_MOD_DCC_INDEPENDENT_128B_MASK 0x1
;; #define AMD_FMT_MOD_DCC_MAX_COMPRESSED_BLOCK_SHIFT 18
;; #define AMD_FMT_MOD_DCC_MAX_COMPRESSED_BLOCK_MASK 0x3
;; #define AMD_FMT_MOD_DCC_CONSTANT_ENCODE_SHIFT 20
;; #define AMD_FMT_MOD_DCC_CONSTANT_ENCODE_MASK 0x1
;; #define AMD_FMT_MOD_PIPE_XOR_BITS_SHIFT 21
;; #define AMD_FMT_MOD_PIPE_XOR_BITS_MASK 0x7
;; #define AMD_FMT_MOD_BANK_XOR_BITS_SHIFT 24
;; #define AMD_FMT_MOD_BANK_XOR_BITS_MASK 0x7
;; #define AMD_FMT_MOD_PACKERS_SHIFT 27
;; #define AMD_FMT_MOD_PACKERS_MASK 0x7
;; #define AMD_FMT_MOD_RB_SHIFT 30
;; #define AMD_FMT_MOD_RB_MASK 0x7
;; #define AMD_FMT_MOD_PIPE_SHIFT 33
;; #define AMD_FMT_MOD_PIPE_MASK 0x7
