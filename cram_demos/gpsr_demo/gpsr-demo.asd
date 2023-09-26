(defsystem gpsr-demo
  :depends-on (roslisp-utilities ; for ros-init-function
               actionlib
               actionlib_msgs-msg
               cram-simple-actionlib-client

               cl-transforms
               cl-transforms-stamped
               cl-tf
	       cl-tf2
	       cl-utils
               cram-tf
               cram-math

               cram-process-modules
               cram-language
               cram-executive
               cram-designators
               cram-prolog
               cram-projection
               cram-occasions-events
               cram-utilities ; for EQUALIZE-LISTS-OF-LISTS-LENGTHS
               

               cram-common-failures
               ;;cram-mobile-pick-place-plans
               cram-object-knowledge

               cram-cloud-logger

               cram-physics-utils     ; for reading "package://" paths
               cl-bullet ; for handling BOUNDING-BOX datastructures
               cram-bullet-reasoning
               cram-bullet-reasoning-belief-state
               cram-bullet-reasoning-utilities

               cram-location-costmap
               cram-btr-visibility-costmap
               cram-btr-spatial-relations-costmap
               cram-robot-pose-gaussian-costmap
               cram-occupancy-grid-costmap
               
               cram-urdf-projection      ; for with-simulated-robot
               cram-urdf-projection-reasoning ; to set projection reasoning to T
               cram-fetch-deliver-plans
               cram-urdf-environment-manipulation

               ;; costmaps
               cram-btr-visibility-costmap
               cram-btr-spatial-relations-costmap

               cram-pr2-description
               cram-boxy-description
               cram-donbot-description
               cram-hsrb-description

               ;;cram-robokudo
               ;;cram-giskard
               robokudo_msgs-msg
               tmc_msgs-msg
               nav_msgs-msg
               manipulation_msgs-msg
	       navigation_msgs-msg
               move_base_msgs-msg
               tmc_control_msgs-msg
               suturo-demos
               gpsr_nlp-msg
               suturo-real-hsr-pm
               )
  
  :components
  ((:module "src"
            :components
            ((:file "package")            
             (:file "utils" :depends-on ("package"))
             (:file "marker-publisher" :depends-on ("package"))
             (:file "gpsr-knowledge" :depends-on ("package"))
             (:file "gpsr-subscribers" :depends-on ("package"))
             (:file "human-perception-interface" :depends-on ("package"))
             (:file "setup" :depends-on ("package"))
             (:file "gpsr-plans" :depends-on ("package"))
             (:file "gpsr-ln" :depends-on ("package"))
             (:file "gpsr-pub" :depends-on ("package"))
             (:file "nlp-testing" :depends-on ("package"))
             (:file "navigation-plans" :depends-on ("package"))
             (:file "searching-plans" :depends-on ("package"))
             (:file "gpsr-clean-plans" :depends-on ("package"))
             (:file "gpsr-demo" :depends-on ("package"))))))
