;;;; package.lisp

(defpackage #:sdlutils
  (:use #:cl #:alexandria #:iterate)
  (:export ROUND-ROBIN 
	   SLOT-LIST 
	   NMAPSLOT 
	   TO-S 
	   MAKE-TIMER
	   CHARGE-TIMER 
	   PRINT-IF 
	   MAKE-PAIRS
	   DEFINE-CLASS 
	   WHENS 
	   DEFINTERACT-METHOD
	   collide
	   DEFCOLLIDE 
	   PMIF 
	   ALAMBDA 
	   LETREC 
	   WHILE 
	   DBIND 
	   KEY-PRESSED-P 
	   KEY-DOWN-P
	   KEY-UP-P 
	   update-key-state
	   next-key-state
	   update-joy-state
	   update-input
	   DEFKEYSTATE 
	   DEFJOYSTICK 
	   AXIS-VALUE-MINUS-P
	   AXIS-VALUE-PLUS-P 
	   AXIS-VALUE-MIDDLE-P 
	   NEW-JOYSTICK 
	   DEFINPUT 
	   RAD
	   DEG
	   VEC-ABS
	   EUC-DIST
	   UNIVEC
	   DIR-UNIVEC
	   A-TO-B-VECTOR
	   DISTANCE
	   UVEC
	   LOAD-PNG-IMAGE
	   LOAD-IMAGE
	   LOAD-ANIMATION
	   LOAD-IMAGES
	   LOAD-ANIMATIONS
	   GET-IMAGE
	   GET-IMAGE-LIST))

