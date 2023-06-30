(in-package :su-demos)

(defun cml-demo-giskard (&key (step 0) (talk T) (is-start T))
  (let ((percept-pub (roslisp::advertise "/robokudovanessa/query/goal"
                                         "robokudo_msgs/QueryActionGoal"))
        (human-detect-fluent-status
          (cpl:make-fluent :name :human-detect-fluent-status))
        (human-detect-fluent-position
          (cpl:make-fluent :name :human-detect-fluent-position))
        (hsr-pose-fluent
          (cpl:make-fluent :name :hsr-pose-fluent)))
    
    (flet ((perceptresult (msg)
             (roslisp:with-fields
                 ((?result (status status)))
                 msg
               (setf (cpl::value human-detect-fluent-status) ?result)))

           (humanpose (msg)
             (setf (cpl::value human-detect-fluent-position) msg))

           (hsrpose (msg)
             (roslisp:with-fields ((?pose (pose)))
                 msg
               (setf (cpl::value hsr-pose-fluent) ?pose))))

      (vizbox-set-step 0) ;; Vizbox

      (roslisp:subscribe "/hsrb/base_pose"
                         "geometry_msgs/PoseStamped"
                         #'hsrpose)
      (roslisp:subscribe "/robokudovanessa/human_position"
                         "geometry_msgs/PointStamped"
                         #'humanpose)
      (roslisp:subscribe "robokudovanessa/query/result"
                         "robokudo_msgs/QueryActionResult"
                         #'perceptresult)

      (if is-start
          (progn (talk-request "Hey, I am Toya i will help you to carry your Luggage." talk)
                 (talk-request "When we arrived, please shake my hand!" talk)
                 (talk-request "Is philipp finally here?." talk)
                                        ;(park-robot)
                                        ;(call-take-pose-action 0 0 0 0 0 -1.5 -1.5 0)
                 (talk-request "I am now searching for a human" talk))
          (talk-request "I lost you, please stop" talk))

        ;;how to know human
        (loop until (not (or (eq (cpl::value human-detect-fluent-status) 4)
                             (eq (cpl::value human-detect-fluent-status) nil)))
              do
                 (roslisp::publish-msg percept-pub )
                 (talk-request "Please step in front of me so I can recognize you" talk)
                 (sleep 1.5))
        
      (vizbox-set-step 1) ;; Vizbox
      
      (talk-request "I was able to recognize you" talk)
      (when is-start
        (vizbox-set-step 2) ;; Vizbox
        (talk-request "Can you please give me the bag?" talk)
                                        ;(call-take-pose-action 0 0 -0.65 0 -0.43 0 -1.17 -1.62)
                                        ;(human-assist talk))
        
        )

      (vizbox-set-step 3) ;; Vizbox
      (talk-request "I will now follow you, please dont move to fast." talk)
      (talk-request "When we arrived, please shake my hand!" talk)
        (let ((stop-condition t)
              
              (home-pose (cl-tf:make-pose-stamped "map" 0
                                                  (cl-tf:make-3d-vector 0 0 0)
                                                  (cl-tf:make-quaternion 0 0 0 1))))
          (cpl::pursue
            (cpl::seq
              

              (monitoring-mega-function)
              (vizbox-set-step 4) ;; Vizbox
              (talk-request "I think we arrived I hope my service was satisfactory!" talk)
              ;; (exe:perform (desig:a motion
              ;;                       (type gripper-motion)
              ;;                       (:open-close :open)
              ;;                       (effort 0.1)))
              (setf stop-condition t))

            
            (cpl::seq

                (exe:perform (desig:a motion
                                      (type cml)
                                      (laser-distance-threshold 0.5)))

              
              (setf stop-condition nil)))
          
          (unless stop-condition
            (cml-demo :is-start nil))
          (vizbox-set-step 5) ;; Vizbox
          (talk-request "I would now drive back but philipp you have homework to do!" talk)
          (exe:perform (desig:a motion
                                (type cml)
                                (drive-back t)
                                (laser-distance-threshold 0.5)))))
          (vizbox-set-step 6))) ;; Vizbox
                                        ;(call-nav-action-ps home-pose)))))


  
;; (defun cml-demo (&key (step 0) (talk T) (is-start T))
;;   (let ((percept-pub (roslisp::advertise "/robokudovanessa/query/goal"
;;                                          "robokudo_msgs/QueryActionGoal"))
;;         (human-detect-fluent-status
;;           (cpl:make-fluent :name :human-detect-fluent-status))
;;         (human-detect-fluent-position
;;           (cpl:make-fluent :name :human-detect-fluent-position))
;;         (hsr-pose-fluent
;;           (cpl:make-fluent :name :hsr-pose-fluent)))
    
;;     (flet ((perceptresult (msg)
;;              (roslisp:with-fields
;;                  ((?result (status status)))
;;                  msg
;;                (setf (cpl::value human-detect-fluent-status) ?result)))

;;            (humanpose (msg)
;;              (setf (cpl::value human-detect-fluent-position) msg))

;;            (hsrpose (msg)
;;              (roslisp:with-fields ((?pose (pose)))
;;                  msg
;;                (setf (cpl::value hsr-pose-fluent) ?pose))))

;;       (roslisp:subscribe "/hsrb/base_pose"
;;                          "geometry_msgs/PoseStamped"
;;                          #'hsrpose)
;;       (roslisp:subscribe "/robokudovanessa/human_position"
;;                          "geometry_msgs/PointStamped"
;;                          #'humanpose)
;;       (roslisp:subscribe "robokudovanessa/query/result"
;;                          "robokudo_msgs/QueryActionResult"
;;                          #'perceptresult)

;;       (if is-start
;;           (progn (talk-request "Hey, I am Toya i will help you to carry your Luggage." talk)
;;                  (park-robot)
;;                  (talk-request "I am now searching for a human" talk))
;;           (talk-request "I lost you, please stop" talk))
      
;;       (block human-init
;;         ;;how to know human
;;         (loop until (not (or (eq (cpl::value human-detect-fluent-status) 4)
;;                              (eq (cpl::value human-detect-fluent-status) nil)))
;;               do
;;                  (roslisp::publish-msg percept-pub )
;;                  (talk-request "Please step in front of me so I can recognize you" talk)
;;                  (sleep 1.5))
        
;;         (when is-start
;;           (talk-request "I was able to recognize you, Can you please give me the bag?" talk)
;;                                         ;(human-assist talk))
;;           (carry-robot))
        
;;         (talk-request "I will now follow you, please dont move to fast." talk)
;;          (talk-request "When we arrived Please shake my hand" talk)
;;         (let ((stop-condition t)
              
;;               (home-pose (cl-tf:make-pose-stamped "map" 0
;;                                                   (cl-tf:make-3d-vector 0 0 0)
;;                                                   (cl-tf:make-quaternion 0 0 0 1))))
;;           (cpl::pursue
;;             (cpl::seq
              

;;               (monitoring-mega-function)         
;;               (talk-request "I think we arrived I hope my service was satisfactory!" talk)
;;               (exe:perform (desig:a motion
;;                                     (type gripper-motion)
;;                                     (:open-close :open)
;;                                     (effort 0.1)))
;;               (setf stop-condition t))
;;             (cpl::seq
;;               (loop until (or (eq (cpl::value human-detect-fluent-status) 4)
;;                               (eq (cpl::value human-detect-fluent-status) nil))
;;                     do
;;                        (let* ((nav-goal (relative-angle-to (cpl::value human-detect-fluent-position)
;;                                                            (cpl::value hsr-pose-fluent))))

;;                          (cpl::pursue
;;                            (progn (sleep 1)
;;                                   (cpl::wait-for (cpl::pulsed human-detect-fluent-position)))
;;                            (progn
;;                              ;; (par 
;;                              (call-nav-action-ps nav-goal)))))
;;               (setf stop-condition nil)))
          
;;           (unless stop-condition
;;             (cml-demo :is-start nil))
;;           (call-nav-action-ps home-pose))))))

        
(defun monitoring-mega-function (&key (talk T))
  (let((hsr-monitoring-flex-up-bool)
       (hsr-monitoring-flex-down-bool)
       (time-ok))
    
    (loop until (and hsr-monitoring-flex-up-bool
                     hsr-monitoring-flex-down-bool
                     time-ok)
          do
             
               (cpl:seq
                
                 
                 (setf hsr-monitoring-flex-up-bool nil)
                 (setf hsr-monitoring-flex-down-bool nil)
                 
                 (exe:perform
                  (desig:an action
                            (type monitoring-joint-state)
                            ;;todo change joint
                            (comparison :lesser)
                            (joint-name "wrist_flex_joint")))
                 (let ((time-begin (cl::get-universal-time)))
                 (setf hsr-monitoring-flex-up-bool t)

                 (exe:perform
                  (desig:an action
                            (type monitoring-joint-state)
                            (comparison :greater)
                            (joint-name "wrist_flex_joint")))
                 (setf hsr-monitoring-flex-down-bool t)
                 
                 (talk-request "ok thanks" talk)
                 (print "time:")
                 (print  (- (cl::get-universal-time) time-begin))
                 
                 (when (< (- (cl::get-universal-time) time-begin) 2.2)
                   (setf time-ok t)))))))
 
;; (defun cml-follow (&key (talk T))
;;   (cpl::pursue
;;     (cpl::seq
;;       (exe:perform
;;        (desig:an action
;;                  (type monitoring-joint-state)
;;                  (joint-name "wrist_flex_joint")))
;;       (talk-request "I think we arrived I hope my service was satisfactory!" talk)
;;       (exe:perform (desig:a motion
;;                             (type gripper-motion)
;;                             (:open-close :open)
;;                             (effort 0.1)))
;;       t)
;;     (cpl::seq
;;       (loop until (or (eq (cpl::value *human-detect-fluent-status*) 4)
;;                       (eq (cpl::value *human-detect-fluent-status*) nil))
;;             do
;;                (let* ((nav-goal (relative-angle-to (cpl::value *human-detect-fluent-position*)
;;                                                    (cpl::value *hsr-pose-fluent*))))

;;                  (cpl::pursue
;;                    (progn (sleep 1)
;;                           (cpl::wait-for (cpl::pulsed *human-detect-fluent-position*)))
;;                    (progn
;;                      ;; (par 
;;                      (call-nav-action-ps nav-goal)))))
;;       nil)))



 
(defun pose-msg->transform (msg)
  "Returns a transform proxy that allows to transform into the frame
given by x, y, and theta of `msg'."
  (roslisp:with-fields ((?x (x position))(?y (y position))
                        (?qx (x orientation))
                        (?qy (y orientation))
                        (?qz (z orientation))
                        (?qw (w orientation)))
      msg
    (cl-transforms:make-transform
     (cl-transforms:make-3d-vector ?x ?y 0)
     (cl-transforms:make-quaternion ?qx ?qy ?qz ?qw)
     )))
 
(defun relative-angle-to (goal pose-msg)
  "Given a `pose-msg' as a turtlesim-msg:pose and a `goal' as cl-transforms:3d-vector,
calculate the angle by which the pose has to be turned to point toward the goal."

  (roslisp:with-fields ((?x (x Point))(?y (y Point)))
      goal
    (let* ((transformpoint
             (cl-tf::make-point-stamped "map" 0
                                        (cl-tf::make-3d-vector ?x ?y 0)))
           (goalpose
             (cl-tf::transform-point-stamped
              cram-tf::*transformer*
              :point transformpoint :target-frame "/odom")))
      
      (roslisp:with-fields ((?x (x ))(?y (y )))
          
          goalpose
        (let* (
               (point (cl-transforms:make-3d-vector ?x ?y 0))
               (diff-pose (cl-transforms:transform-point
                           (cl-transforms:transform-inv
                            (pose-msg->transform pose-msg))
                           point))
               (atanerin   (/ (atan
                            (cl-transforms:y diff-pose)
                            (cl-transforms:x diff-pose)) 2)))
          (cl-tf::make-pose-stamped "odom" 0
                                    (cl-tf:make-3d-vector ?x ?y 0)
                                    (cl-tf:axis-angle->quaternion
                                     (cl-tf:make-3d-vector 0 0 1) atanerin)))))))



