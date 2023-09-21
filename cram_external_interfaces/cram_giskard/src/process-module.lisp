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

(cpm:def-process-module giskard-pm (motion-designator)
  (destructuring-bind (command argument-1 &rest rest-args)
      (desig:reference motion-designator)
    (ecase command
      (cram-common-designators:move-tcp
       (giskard:call-arm-cartesian-action
        :goal-pose-left argument-1
        :goal-pose-right (first rest-args)
        :collision-mode (second rest-args)
        :collision-object-b (third rest-args)
        :collision-object-b-link (fourth rest-args)
        :collision-object-a (fifth rest-args)
        :move-base (sixth rest-args)
        :prefer-base (seventh rest-args)
        :straight-line (tenth rest-args)
        :align-planes-left (eighth rest-args)
        :align-planes-right (ninth rest-args)
        :precise-tracking (nth 10 rest-args); that's eleventh element
        )) 
      (cram-common-designators:move-joints
       (giskard:call-arm-joint-action
        :goal-configuration-left argument-1
        :goal-configuration-right (first rest-args)
        :align-planes-left (third rest-args)
        :align-planes-right (fourth rest-args)
        :avoid-collisions-not-much (fifth rest-args)))
      (cram-common-designators:move-arm-pull
       (giskard:call-environment-manipulation-action
        :open-or-close :open
        :arm argument-1
        :handle-link (fifth rest-args)
        :joint-angle (second rest-args)
        :prefer-base (eighth rest-args)
        :joint-pose (nth 10 rest-args)))
      (cram-common-designators:move-arm-push
       (giskard:call-environment-manipulation-action
        :open-or-close :close
        :arm argument-1
        :handle-link (fifth rest-args)
        :joint-angle (second rest-args)
        :prefer-base (eighth rest-args)
        :joint-pose (nth 10 rest-args)))
      (cram-common-designators:move-head
       (when argument-1
         (giskard:call-neck-action
          :goal-pose argument-1))
       (when (car rest-args)
         (giskard:call-neck-joint-action
          :goal-configuration (car rest-args))))
      (cram-common-designators:move-base
       (giskard:call-base-action
        :goal-pose argument-1
        :base-velocity (first rest-args)))
      (cram-common-designators:move-torso
       (giskard:call-torso-action
        :goal-joint-state argument-1))
      (cram-common-designators:move-gripper-joint ;; check if used in suturo bc im unsure...i dont think it is though
       (giskard:call-gripper-action
        :action-type-or-position argument-1
        :arm (first rest-args)
        :effort (second rest-args)))
      (cram-common-designators:reach
       (giskard:call-arm-motion-action
        :collision-mode argument-1
        :collision-object-b (first rest-args)
        :collision-object-b-link (second rest-args)
        :collision-object-a (third rest-args)
        :goal-pose (fourth rest-args)
        :object-size (fifth rest-args)
        :object-shape (sixth rest-args)
        :object-name (seventh rest-args)
        :context (eighth rest-args)
        :action-type 'reach
        ))
      (cram-common-designators:vertical-motion
       (giskard:call-arm-motion-action
        :collision-mode argument-1
        :collision-object-b (first rest-args)
        :collision-object-b-link (second rest-args)
        :collision-object-a (third rest-args)
        :context (fourth rest-args)
        :distance (fifth rest-args)
        :action-type 'vertical-motion
        ))
      (cram-common-designators:retract
       (giskard:call-arm-motion-action
        :collision-mode argument-1
        :collision-object-b (first rest-args)
        :collision-object-b-link (second rest-args)
        :collision-object-a (third rest-args)
        :tip-link (fourth rest-args)
        :action-type 'retract
        ))
      (cram-common-designators:align-height
       (giskard:call-arm-motion-action
        :collision-mode argument-1
        :collision-object-b (first rest-args)
        :collision-object-b-link (second rest-args)
        :collision-object-a (third rest-args)
        :goal-pose (fourth rest-args)
        :object-height (fifth rest-args)
        :object-name (sixth rest-args)
        :context (seventh rest-args)
        :action-type 'align-height
        ))
      (cram-common-designators:place
       (giskard:call-arm-motion-action
        :collision-mode argument-1
        :collision-object-b (first rest-args)
        :collision-object-b-link (second rest-args)
        :collision-object-a (third rest-args)
        :goal-pose (fourth rest-args)
        :context (fifth rest-args)
        :action-type 'place
        ))
      (cram-common-designators:tilt
       (giskard:call-arm-motion-action
        :collision-mode argument-1
        :collision-object-b (first rest-args)
        :collision-object-b-link (second rest-args)
        :collision-object-a (third rest-args)
        :direction (fourth rest-args)
        :angle (fifth rest-args)
        :object-name (sixth rest-args)
        :action-type 'tilt
        ))
      (cram-common-designators:sequence
       (giskard:call-arm-motion-action
        :collision-mode argument-1
        :collision-object-b (first rest-args)
        :collision-object-b-link (second rest-args)
        :collision-object-a (third rest-args)
        :motion-sequence (fourth rest-args)
        :action-type 'sequence
        ))
      (cram-common-designators:take-pose
       (giskard:call-take-pose-action
        :collision-mode argument-1
        :pose-keyword (first rest-args)
        :head-pan (second rest-args)
        :head-tilt (third rest-args)
        :arm-lift (fourth rest-args)
        :arm-flex (fifth rest-args)
        :arm-roll (sixth rest-args); that's eleventh element
        :wrist-flex (seventh rest-args)
        :wrist-roll (eighth rest-args)
        ))
      (cram-common-designators:gripper
       (giskard:call-arm-motion-action
        :collision-mode argument-1
        :collision-object-b (first rest-args)
        :collision-object-b-link (second rest-args)
        :collision-object-a (third rest-args)
        :gripper-state (fourth rest-args)
        :action-type 'gripper
        ))
      (cram-common-designators:carry-my-luggage
       (giskard::call-cml-action
        :drive-back argument-1
        :laser-distance-threshold (first rest-args)
        :clear-path (second rest-args)
        :footprint-radius (third rest-args)
        :last-distance-threshold (fourth rest-args)
        :height-for-camera-target (fifth rest-args)
        :laser-avoidance-max-x (sixth rest-args)
        ))
      )))



