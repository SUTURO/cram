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

(defun make-arm-motion-action-goal (collision-mode
                                       &key
                                         collision-object-b
                                         collision-object-b-link
                                         collision-object-a
                                         object-name
                                         action-type
                                         object-size
                                         object-shape
                                         object-height
                                         tip-link
                                         goal-pose
                                         tilt-direction
                                         tilt-angle
                                         context
                                         motion-sequence
                                         gripper-state
                                         distance)
    (make-giskard-goal
     :constraints (list
                   ;; (make-avoid-joint-limits-constraint
                   ;;  :joint-list (append (when left-pose
                   ;;                        (cut:var-value
                   ;;                         '?joints
                   ;;                         (car
                   ;;                          (prolog:prolog
                   ;;                           `(and (rob-int:robot ?robot-name)
                   ;;                                 (rob-int:arm-joints
                   ;;                                  ?robot-name :left ?joints))))))
                   ;;                      (when right-pose
                   ;;                        (cut:var-value
                   ;;                         '?joints
                   ;;                         (car
                   ;;                          (prolog:prolog
                   ;;                           `(and (rob-int:robot ?robot-name)
                   ;;                                 (rob-int:arm-joints
                   ;;                                  ?robot-name :right ?joints))))))))
                   ;;;;
                   ;; Constraints for motions
                   (when (eq action-type 'reach)
                     (make-reach-constraint goal-pose object-name object-size object-shape context))
                   (when (eq action-type 'vertical-motion)
                     (make-vertical-motion-constraint context distance))
                   (when (eq action-type 'retract)
                      (make-retract-constraint "object_name" tip-link))
                   (when (eq action-type 'align-height)
                     (make-align-height-constraint "object_name" goal-pose object-height context))
                   (when (eq action-type 'place)
                     (make-place-constraint goal-pose context))
                   (when (eq action-type 'tilt)
                     (make-tilt-constraint tilt-direction tilt-angle))
                   (when (eq action-type 'sequence)
                     (make-sequence-constraint motion-sequence))
                   (when (eq action-type 'gripper)
                     (make-gripper-constraint gripper-state))

                    )
     :collisions (ecase collision-mode
                   (:avoid-all (make-avoid-all-collision))
                   (:allow-all (make-allow-all-collision))
                   ;; (:allow-hand (alexandria:flatten
                   ;;               (list
                   ;;                (make-allow-hand-collision
                   ;;                 arms collision-object-b
                   ;;                 collision-object-b-link)
                   ;;                (make-allow-hand-collision
                   ;;                 arms (rob-int:get-environment-name)))))
                   ;; (:allow-fingers (alexandria:flatten
                   ;;                  (list
                   ;;                   (make-allow-fingers-collision
                   ;;                    arms collision-object-b
                   ;;                    collision-object-b-link)
                   ;;                   (make-allow-fingers-collision
                   ;;                    arms (rob-int:get-environment-name)))))
                   ;; (:allow-arm (alexandria:flatten
                   ;;              (list
                   ;;               (make-allow-arm-collision
                   ;;                arms collision-object-b
                   ;;                collision-object-b-link)
                   ;;               (make-allow-arm-collision
                   ;;                arms (rob-int:get-environment-name)))))
                   ;; TODO: this should allow collision between attached and environment
                   (:allow-attached (make-avoid-all-collision)))))

(defun ensure-arm-motion-goal-input (frame goal-pose arm)
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

(defun ensure-arm-motion-goal-reached (goal-pose goal-frame)
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

(defun call-arm-motion-action (&key
                                    action-timeout
                                    collision-mode
                                    collision-object-b collision-object-b-link
                                    collision-object-a
                                    action-type
                                    object-size
                                    object-name
                                    object-shape
                                    object-height
                                    tip-link
                                    goal-pose
                                    tilt-direction
                                    tilt-angle
                                    context
                                    motion-sequence
                                    gripper-state
                                    distance)

  ;; EINKOMMENTIEREN wenn goal-pose-left vernuenftig verwendet wird, bzw die target posen eine einheiliche variable haben
  ;; (cram-tf:visualize-marker
  ;;  (list goal-pose-left goal-pose-right)
  ;;  :r-g-b-list '(1 0 1))

  (call-action
   :action-goal (make-arm-motion-action-goal
                 collision-mode
                 :collision-object-b collision-object-b
                 :collision-object-b-link collision-object-b-link
                 :collision-object-a collision-object-a
                 :action-type action-type
                 :object-size object-size
                 :object-name object-name
                 :object-shape object-shape
                 :object-height object-height
                 :tip-link tip-link
                 :goal-pose goal-pose
                 :tilt-direction tilt-direction
                 :tilt-angle tilt-angle
                 :context context
                 :motion-sequence motion-sequence
                 :gripper-state gripper-state
                 :distance distance)
   :action-timeout action-timeout
   :check-goal-function (lambda (result status)
                            (declare (ignore result))
                            (when (or (not status)
                                      (member status '(:preempted :aborted :timeout)))
                              (make-instance
                               'common-fail:environment-manipulation-goal-not-reached
                               :description "Giskard action failed."))))
   ;; :check-goal-function (lambda (result status)
   ;;                        (declare (ignore result status))
   ;;                        (or (ensure-arm-motion-goal-reached
   ;;                             goal-pose-left cram-tf:*robot-left-tool-frame*)
   ;;                            (ensure-arm-motion-goal-reached
   ;;                             goal-pose-right cram-tf:*robot-right-tool-frame*)))
   )
