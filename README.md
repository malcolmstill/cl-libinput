# cl-libinput

libinput is a library for handling input devices in Wayland compositors. cl-libinput is a Common Lisp interface to the library.

## Status

cl-libinput is being developed primarily in support of [ulubis](https://github.com/malcolmstill/ulubis) and is therefor feature incomplete. Pull requests adding more of the API are more than welcome.

## Requiremnts

cl-libinput (obiously) requires libinput. It is likely that libinput already exists on your Linux installation if it is recent.

## Installation

```
CL-USER> (ql:quickload :cl-libinput)
```

## Example

```
(in-package :libinput)

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

(defun event-loop (context)
  (dispatch context)
  (let ((event (get-event context)))
    (loop :while (not (null-pointer-p event))
       :do (progn
	     (handle-event-context context event)
	     (setf event (get-event context))))))

(defun test (&rest paths)
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

```