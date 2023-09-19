(in-package :su-real)

(defvar *current-subscriber* nil)

;; @author Luca Krohm
(defmacro wait-for-startsignal (&body body)
  ;; create cram-fluent
  `(let ((fl ,(cpl:make-fluent :name :start-signal-fluent :value nil)))
     ;; create local function 'laser-scan'
     (flet ((laser-scan (msg)
             (roslisp:with-fields
                 ((?ranges
                   (ranges)))
                 msg
               ;; the laserscanner topic has 962 points published on it.
               ;; we filter out the one right in the middle, which is right in front of the robot
               ;; then we wait for it to be further away than 1 meter
               (when (> (nth 481 (coerce ?ranges 'list)) 1.0)
                 ;; pulse fluent
                 (setf (cpl:value fl) t)
                 ;; unsubscribe from topic
                 (roslisp:unsubscribe *current-subscriber*)))))
       (su-demos:call-text-to-speech-action "Waiting for startingsignal.")
       ;; subscribe to laser scan topic and call function 'laser-scan' whenever a value is received
       (setf *current-subscriber* (roslisp:subscribe "hsrb/base_scan" "sensor_msgs/LaserScan" #'laser-scan))
       (roslisp:ros-info (Start-Signal) "Waiting for door to open")
       ;; wait for fluent to pulse, and when pulsed execute the body
       (cpl:wait-for fl)
       ,@body)))



