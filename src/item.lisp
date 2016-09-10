(in-package :danmachi)

(define-class item ()
  name
  description
  num)

(define-class equipment (item)
  durability
  )

(define-class weapon (equipment)
  atk
  atk-area)

(define-class protect (equipment)
  df)

(define-class adornment (equipment)
  effect)

(define-class expendables (item)
  effect)

(define-class material (item))

