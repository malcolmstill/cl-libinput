;;;; cl-libinput.lisp

(in-package #:libinput)

;;; "cl-libinput" goes here. Hacks and glory await!

(define-foreign-library libinput
  (:unix (:or "/usr/lib64/libinput.so" "/usr/lib64/libinput.so.10"))
  (t (:default "libinput")))

(use-foreign-library libinput)

(defcstruct libinput-interface
  (open-restricted :pointer)
  (close-restricted :pointer))

(defcfun ("libinput_unref" unref) :pointer
  (context :pointer))

(defcfun ("libinput_path_create_context" path-create-context) :pointer
  (interface :pointer)
  (user-date :pointer))

(defcfun ("libinput_path_add_device" path-add-device) :pointer
  (context :pointer)
  (path :string))

(defcfun ("libinput_get_fd" get-fd) :int
  (context :pointer))

(defcfun ("libinput_dispatch" dispatch) :int
  (context :pointer))

(defcfun ("libinput_get_event" get-event) :pointer
  (context :pointer))

(defcfun ("libinput_event_get_type" event-get-type) :int
  (event :pointer))

;; Types
(defparameter none 0)
(defparameter device-added 1)
(defparameter device-removed 2)
(defparameter keyboard-key 300)
(defparameter pointer-motion 400)
(defparameter pointer-motion-absolute 401)
(defparameter pointer-button 402)
(defparameter pointer-axis 403)
(defparameter touch-down 500)
(defparameter touch-up 501)
(defparameter touch-motion 502)
(defparameter touch-cancel 503)
(defparameter touch-frame 504)
(defparameter tablet-tool-axis 600)
(defparameter tablet-tool-proximity 601)
(defparameter tablet-tool-tip 602)
(defparameter tablet-tool-button 603)
(defparameter tablet-pad-button 700)
(defparameter tablet-pad-ring 701)
(defparameter tablet-pad-strip 702)
(defparameter gesture-swipe-begin 800)
(defparameter gesture-swipe-update 801)
(defparameter gesture-swipe-end 802)
(defparameter gesture-pinch-begin 803)
(defparameter gesture-pinch-update 804)
(defparameter gesture-pinch-end 805)

(defcfun ("libinput_event_destroy" event-destroy) :void
  (event :pointer))

(defcfun ("libinput_event_get_keyboard_event" event-get-keyboard-event) :pointer
  (event :pointer))

(defcfun ("libinput_event_keyboard_get_time" event-keyboard-get-time) :uint32
  (keyboard-event :pointer))

(defcfun ("libinput_event_keyboard_get_key" event-keyboard-get-key) :uint32
  (keyboard-event :pointer))

(defcfun ("libinput_event_keyboard_get_key_state" event-keyboard-get-key-state) :int
  (keyboard-event :pointer))

(defcfun ("libinput_event_get_pointer_event" event-get-pointer-event) :pointer
  (event :pointer))

(defcfun ("libinput_event_pointer_get_time" event-pointer-get-time) :uint32
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_button" event-pointer-get-button) :uint32
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_button_state" event-pointer-get-button-state) :int
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_dx" event-pointer-get-dx) :double
  (pointer-event :pointer))

(defcfun ("libinput_event_pointer_get_dy" event-pointer-get-dy) :double
  (pointer-event :pointer))

(defcallback open-restricted :int
    ((path :string) (flags :int) (user-data :pointer))
  (format t "Called open-restricted~%")
  (let* ((context user-data)
	 (fd (nix:open path flags)))
    (format t "File descriptor ~A~%" fd)
    (when (< fd 0)
      (error "Failed to open ~A" path))
    fd))

(defcallback close-restricted :void
    ((fd :int) (user-data :pointer))
  (nix:close fd))

(defun make-libinput-interface ()
  (let ((interface (foreign-alloc '(:struct libinput-interface))))
    (setf (foreign-slot-value interface
			      '(:struct libinput-interface)
			      'open-restricted)
	  (callback open-restricted))
    (setf (foreign-slot-value interface
			      '(:struct libinput-interface)
			      'close-restricted)
	  (callback close-restricted))
    interface))

(defmacro with-pointer-motion ((event time dx dy) &body body)
  (let ((pointer-event (gensym "pointer-event")))
    `(let* ((,time (event-pointer-get-time ,event))
	    (,pointer-event (event-get-pointer-event ,event))
	    (,dx (event-pointer-get-dx ,pointer-event))
	    (,dy (event-pointer-get-dy ,pointer-event)))
       ,@body)))

(defmacro with-pointer-button ((event time button state) &body body)
  (let ((pointer-event (gensym "pointer-event")))
    `(let* ((,time (event-pointer-get-time ,event))
	    (,pointer-event (event-get-pointer-event ,event))
	    (,button (event-pointer-get-button ,pointer-event))
	    (,state (event-pointer-get-button-state ,pointer-event)))
       ,@body)))

(defmacro with-keyboard-key ((event time key state) &body body)
  (let ((keyboard-event (gensym "keyboard-event")))
    `(let* ((,time (event-keyboard-get-time ,event))
	    (,keyboard-event (event-get-keyboard-event ,event))
	    (,state (event-keyboard-get-key-state ,keyboard-event))
	    (,key (event-keyboard-get-key ,keyboard-event)))
       ,@body)))

#|
(defun handle-event-context (context event)
  (when (not (null-pointer-p event))
    (let ((type (event-get-type event)))
      (cond
	((= type keyboard-key) (progn
				 (format t "fd: ~A~%" (get-fd context))
				 (handle-keyboard event)))
	((= type pointer-motion) (handle-pointer-motion event))
	((= type pointer-button) (progn
				   (format t "fd: ~A~%" (get-fd context))
				   (handle-pointer-button event)))))
    (event-destroy event)))

(defun handle-keyboard (event)
  (let* ((keyboard-event (event-get-keyboard-event event))
	 (state (event-keyboard-get-key-state keyboard-event))
	 (key (event-keyboard-get-key keyboard-event)))
    (format t "Key: ~A, state: ~A~%" key state)))

(defun handle-pointer-motion (event)
  (let* ((pointer-event (event-get-pointer-event event))
	 (dx (event-pointer-get-dx pointer-event))
	 (dy (event-pointer-get-dy pointer-event)))
    ;;(format t "dx: ~A, dy: ~A~%" dx dy)
    ))

(defun handle-pointer-button (event)
  (let* ((pointer-event (event-get-pointer-event event))
	 (state (event-pointer-get-button-state pointer-event))
	 (button (event-pointer-get-button pointer-event)))
    (format t "Button: ~A, state: ~A~%" button state)))

(defun handle-event (event)
  (when (not (null-pointer-p event))
    (let ((type (event-get-type event)))
      (cond
	((= type keyboard-key) (handle-keyboard event))
	((= type pointer-motion) (handle-pointer-motion event))
	((= type pointer-button) (handle-pointer-button event))))
    (event-destroy event)))

(defun event-loop (context)
  (dispatch context)
  (let ((event (get-event context)))
    (loop :while (not (null-pointer-p event))
       :do (progn
	     (handle-event-context context event)
	     (setf event (get-event context))))))

(defun test-2fd (path-1 path-2)
  (let* ((interface (make-libinput-interface))
	 (context-1 (path-create-context interface (null-pointer)))
	 (fd-1 (get-fd context-1))
	 (context-2 (path-create-context interface (null-pointer)))
	 (fd-2 (get-fd context-2)))
    (path-add-device context-1 path-1)
    (path-add-device context-2 path-2)
    (nix:with-pollfds (pollfds
		       (one fd-1 nix:pollin)
		       (two fd-2 nix:pollin))
      (loop :do
	 (let ((event (nix:poll pollfds 2 -1)))
	   (when event
	     (when (= (nix:poll-return-event one) 1)
	       (event-loop context-1))
	     (when (= (nix:poll-return-event two) 1)
	       (event-loop context-2))))))))

(defun test-alt (&rest paths)
  (let* ((interface (make-libinput-interface))
	 (context (path-create-context interface (null-pointer)))
	 (fd (get-fd context)))
    (mapcar (lambda (path)
	      (path-add-device context path))
	    paths)
    (nix:with-pollfds (pollfds
		       (pollfd fd nix:pollin))
      (loop :with ret = (nix:poll pollfds 1 -1)
	 :do (when ret
	       (event-loop context))))))


|#
