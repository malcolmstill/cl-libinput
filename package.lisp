;;;; package.lisp

(defpackage #:libinput
  (:use #:cl #:cffi)
  (:export
   unref
   path-create-context
   path-add-device
   path-remove-device
   get-fd
   dispatch
   device-ref
   device-unref
   device-cap-keyboard
   device-cap-pointer
   device-cap-touch
   device-cap-tablet-tool
   device-cap-tablet-pad 
   device-cap-gesture
   device-has-capability
   get-event
   event-get-type
   none
   device-added
   device-removed
   keyboard-key
   pointer-motion
   pointer-motion-absolute
   pointer-button
   pointer-axis
   touch-down
   touch-up
   touch-motion
   touch-cancel
   touch-frame
   tablet-tool-axis
   tablet-tool-proximity
   tablet-tool-tip
   tablet-tool-button
   tablet-pad-ring
   tablet-pad-strip
   gesture-swipe-begin
   gesture-swipe-update
   gesture-swipe-end
   gesture-pinch-begin
   gesture-pinch-update
   gesture-pinch-end
   event-destroy
   event-get-keyboard-event
   event-keyboard-get-key
   event-keyboard-get-key-state
   event-keyboard-get-time
   event-get-pointer-event
   event-pointer-get-button
   event-pointer-get-button-state
   event-pointer-get-dx
   event-pointer-get-dy
   event-pointer-get-time
   make-libinput-interface
   with-pointer-motion
   with-pointer-button
   with-keyboard-key))
