;;; Copyright (c) 2018, Christopher Pollok <cpollok@uni-bremen.de>
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

(defsystem cram-pr2-environment-manipulation
    :author "Christopher Pollok"
    :license "BSD"

    :depends-on (roslisp-utilities  ;; for ros-init-function

                 cl-transforms
                 cl-transforms-stamped
                 cl-tf
                 cram-tf

                 cram-language
                 cram-executive
                 cram-designators
                 cram-prolog
                 cram-projection
                 cram-occasions-events
                 cram-utilities ; for EQUALIZE-LISTS-OF-LISTS-LENGTHS

                 cram-common-failures
                 cram-mobile-pick-place-plans

                 cram-knowrob-pick-place
                 cram-robosherlock

                 cram-physics-utils ; for reading "package://" paths
                 cl-bullet ; for handling BOUNDING-BOX datastructures
                 cram-bullet-reasoning
                 cram-bullet-reasoning-belief-state
                 cram-bullet-reasoning-utilities
                 cram-bullet-reasoning-designators

                 cram-semantic-map-costmap
                 cram-robot-pose-gaussian-costmap
                 cram-occupancy-grid-costmap
                 cram-location-costmap

                 cram-pr2-projection      ; for with-simulated-robot
                 cram-pr2-projection-reasoning ; for projection-based reasoning
                 cram-pr2-description
                 cram-process-modules
                 ;; cram-pr2-pick-place-plans
                 )
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "setup" :depends-on ("package"))
             (:file "math" :depends-on ("package"))
             (:file "environment" :depends-on ("package"))
             (:file "costmaps" :depends-on ("package" "math"))
             (:file "grasping" :depends-on ("package"))
             (:file "prolog" :depends-on ("package" "grasping"))
             (:file "plans" :depends-on ("package" "environment"))))))
