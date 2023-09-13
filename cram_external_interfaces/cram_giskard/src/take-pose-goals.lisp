;;;
;;; Copyright (c) 2016, Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Institute for Artificial Intelligence/
;;;       Universitaet Bremen nor the names of its contributors may be used to
;;;       endorse or promote products derived from this software without
;;;       specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :giskard)

(defparameter *arm-convergence-delta-xy* 0.025 ;; 0.005
  "in meters")
(defparameter *arm-convergence-delta-theta* 0.5 ;; 0.1
  "in radiants, about 30 degrees")
(defparameter *arm-convergence-delta-joint* 0.17
  "in radiants, about 10 degrees")
(defparameter *arm-max-velocity-slow-xy* 0.1
  "in m/s")
(defparameter *arm-max-velocity-slow-theta* 0.5
  "in rad/s")

(defun make-take-pose-action-goal (collision-mode
                                       &key
                                         pose-keyword
                                         head-pan
                                         head-tilt
                                         arm-lift
                                         arm-flex
                                         arm-roll
                                         wrist-flex
                                         wrist-roll)
  
    (make-giskard-goal
     :constraints (list
                   (make-take-pose-constraint pose-keyword head-pan head-tilt arm-lift arm-flex arm-roll wrist-flex wrist-roll))
     
     :collisions (ecase collision-mode
                   (:avoid-all (make-avoid-all-collision))
                   (:allow-all (make-allow-all-collision))
                   ;; TODO: this should allow collision between attached and environment
                   (:allow-attached (make-avoid-all-collision)))))

(defun ensure-take-pose-goal-input (frame goal-pose arm)
  ;; currently not used, will need to update
  (when goal-pose
    (let* ((tool-pose-in-correct-base-frame
             (cram-tf:ensure-pose-in-frame goal-pose frame))
           (tool-frame
             (if (eq arm :left)
                 cram-tf:*robot-left-tool-frame*
                 cram-tf:*robot-right-tool-frame*))
           (tool-transform-in-correct-base-frame
             (cram-tf:pose-stamped->transform-stamped
              tool-pose-in-correct-base-frame
              tool-frame))
           (wrist-frame
             (if (eq arm :left)
                 cram-tf:*robot-left-wrist-frame*
                 cram-tf:*robot-right-wrist-frame*))
           (ee-P-tcp
             (cut:var-value
              '?ee-P-tcp
              (car
               (prolog:prolog
                `(and (rob-int:robot ?robot-name)
                      (rob-int:tcp-in-ee-pose ?robot-name ?ee-P-tcp))))))
           (tool-T-wrist
             (cl-transforms-stamped:transform->transform-stamped
              tool-frame
              wrist-frame
              0.0
              (cl-transforms:transform-inv
               (cl-transforms:pose->transform ee-P-tcp))))
           (wrist-pose-in-correct-base-frame
             (cram-tf:multiply-transform-stampeds
              frame wrist-frame
              tool-transform-in-correct-base-frame
              tool-T-wrist
              :result-as-pose-or-transform :pose)))
      ;; wrist-pose-in-correct-base-frame
      tool-pose-in-correct-base-frame)))

(defun ensure-take-pose-goal-reached (goal-pose goal-frame)
  ;; currently not used, will need to update
  (when goal-pose
    (multiple-value-bind (converged delta-xy delta-theta)
        (cram-tf:tf-frame-converged
         goal-frame goal-pose
         *arm-convergence-delta-xy* *arm-convergence-delta-theta*)
     (unless converged
       (make-instance 'common-fail:manipulation-goal-not-reached
         :description (format nil "Giskard did not converge to goal:~%~
                                   ~a should have been at ~a.~%
                                   Delta-xy: ~a, delta-theta: ~a."
                              goal-frame goal-pose
                              delta-xy delta-theta))))))

(defun call-take-pose-action (&key
                                action-timeout
                                collision-mode
                                pose-keyword
                                head-pan
                                head-tilt
                                arm-lift
                                arm-flex
                                arm-roll
                                wrist-flex
                                wrist-roll)

  (call-action
   :action-goal (make-take-pose-action-goal
                 collision-mode
                 :pose-keyword pose-keyword
                 :head-pan head-pan
                 :head-tilt head-tilt
                 :arm-lift arm-lift
                 :arm-flex arm-flex
                 :arm-roll arm-roll
                 :wrist-flex wrist-flex
                 :wrist-roll wrist-roll)
   :action-timeout action-timeout
   ;; :check-goal-function (lambda (result status)
   ;;                        (declare (ignore result status))
   ;;                        (or (ensure-take-pose-goal-reached
   ;;                             goal-pose-left cram-tf:*robot-left-tool-frame*)
   ;;                            (ensure-take-pose-goal-reached
   ;;                             goal-pose-right cram-tf:*robot-right-tool-frame*)))
   ))
