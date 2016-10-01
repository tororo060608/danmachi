(in-package #:lispbuilder-sdl)

(defmethod _render-string-solid_ ((string string) (font ttf-font) (color color) free cache)
  (let ((surf nil))
    (with-foreign-color-copy (col-struct color)
      (setf surf (make-instance 'surface
		    :fp (sdl-ttf-cffi::render-utf8-solid
			 (fp font)
			 string
			 (if (cffi:foreign-symbol-pointer "TTF_glue_RenderText_Solid")
			   col-struct
			   (+ (ash (b color) 16)
			      (ash (g color) 8)
			      (r color)))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

(defmethod _render-string-blended_ ((string string) (color color) (font ttf-font) free cache)
  (let ((surf nil))
    (with-foreign-color-copy (col-struct color)
      (setf surf (make-instance 'surface
		    :fp (sdl-ttf-cffi::render-utf8-blended
			 (fp font) string
			 (if (cffi:foreign-symbol-pointer "TTF_glue_RenderText_Blended")
			   col-struct
			   (+ (ash (b color) 16)
			      (ash (g color) 8)
			      (r color)))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

(defmethod _render-string-shaded_ ((string string) (fg-color color) (bg-color color) (font ttf-font) free cache)
  (let ((surf nil))
    (with-foreign-color-copy (fg-struct fg-color)
      (with-foreign-color-copy (bg-struct bg-color)
        (multiple-value-bind (fg bg)
            (if (cffi:foreign-symbol-pointer "TTF_glue_RenderText_Shaded")
              (values fg-struct bg-struct)
              (values (+ (ash (b fg-color) 16)
                         (ash (g fg-color) 8)
                         (r fg-color))
                      (+ (ash (b bg-color) 16)
                         (ash (g bg-color) 8)
                         (r bg-color))))
          (setf surf (make-instance 'surface
			:fp (sdl-ttf-cffi::render-utf8-shaded
			     (fp font) string fg bg))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

(in-package :danmachi)

(defparameter *ttf-font-misaki*
  (make-instance 'sdl:ttf-font-definition
		 :size 28
		 :filename (lib-path "misaki_ttf/k8x12.ttf")))

