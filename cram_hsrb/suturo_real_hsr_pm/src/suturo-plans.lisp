(in-package :su-real)

;; @author Luca Krohm
;; @TODO failurehandling
;; has sequence
(defun pick-up (&key
                  ((:collision-mode ?collision-mode))
                  ((:collision-object-b ?collision-object-b))
                  ((:collision-object-b-link ?collision-object-b-link))
                  ((:collision-object-a ?collision-object-a))
                  ((:move-base ?move-base))
                  ((:prefer-base ?prefer-base))
                  ((:straight-line ?straight-line))
                  ((:align-planes-left ?align-planes-left))
                  ((:align-planes-right ?align-planes-right))
                  ((:precise-tracking ?precise-tracking))
                  ((:object-type ?object-type))
                  ((:goal-pose ?goal-pose))
                  ((:object-size ?object-size))
                  ((:object-shape ?object-shape))
                  ((:object-name ?object-name))
                  ((:from-above ?from-above))
                  ((:sequence-goal ?sequence-goal))
                &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"
  (declare (type boolean ?move-base ?prefer-base ?straight-line ?precise-tracking
                 ?align-planes-left ?align-planes-right))
  (cpl:with-retry-counters ((manip-retries 1))
    (cpl:with-failure-handling
        ((common-fail:gripper-closed-completely (e)
           (roslisp:ros-warn (suturo-pickup grasp-object)
                             "Some manipulation failure happened: ~a"
                             e)
           (cpl:do-retry manip-retries
             (roslisp:ros-warn (suturo-pickup grasp-object) "Retrying...")
             (exe:perform (desig:a motion
                                   (type :retracting)
                                   (collision-mode ?collision-mode)
                                   (object-name ?object-name)))
             (su-demos::perc-robot)
             ;; add "looking" to old object-position before perceiving again
             (let* ((?source-object-desig
                      (desig:an object
                                (type ?object-type)))
                    ;; detect object and save the return value
                    (?object-desig
                      (exe:perform (desig:an action
                                             (type detecting)
                                             (object ?source-object-desig)))))
               (roslisp:with-fields 
                   ((?pose
                     (cram-designators::pose cram-designators:data))) 
                   ?object-desig
                 (setf ?goal-pose ?pose)))
             (cpl:retry))))
      
      (unless ?sequence-goal
        (let ((?object-height (cl-transforms:z ?object-size))
              (?context `(("action" . "grasping")
                          ("from_above" . ,?from-above)
                          )))
          (su-demos::pre-align-height-robot)

          (exe:perform (desig:a motion
                                (type aligning-height)
                                (collision-mode ?collision-mode)
                                (collision-object-b ?collision-object-b)
                                (collision-object-b-link ?collision-object-b-link)
                                (collision-object-a ?collision-object-a)
                                (allow-base ?move-base)
                                (prefer-base ?prefer-base)
                                (straight-line ?straight-line)
                                (align-planes-left ?align-planes-left)
                                (align-planes-right ?align-planes-right)
                                (precise-tracking ?precise-tracking)
                                (goal-pose ?goal-pose)
                                (context ?context)
                                (object-height ?object-height)
                                (object-name ?object-name)))
          
          (exe:perform (desig:a motion
                                (type gripper)
                                (gripper-state "open")))
          
          (let ((?context `(("action" . "grasping")
                            ("from_above" . ,?from-above))))
            
            (exe:perform (desig:a motion
                                  (type reaching)
                                  (collision-mode ?collision-mode)
                                  (goal-pose ?goal-pose)
                                  (object-size ?object-size)
                                  (object-shape ?object-shape)
                                  (object-name ?object-name)
                                  (context ?context))))

          (sleep 2)

          (if ?from-above
              (exe:perform (desig:a motion
                                    (type gripper)
                                    (gripper-state "close")))
              (cpl:pursue
                (cpl:seq
                  (exe:perform (desig:a motion
                                        (type gripper)
                                        (gripper-state "close")))
                  (sleep 1)
                  (su-demos::call-text-to-speech-action "I was able to grasp the object"))
                (cpl:seq
                  (exe:perform
                   (desig:an action
                             (type monitoring-joint-state)
                             (joint-name "hand_l_proximal_joint")))
                  (su-demos::call-text-to-speech-action "Failed to grasp the object, retrying")
                  (sleep 1)
                  (cpl:fail 'common-fail:gripper-closed-completely
                            :description "Object slipped"))))
          
          (let ((?context `(("action" . "grasping"))))
            (exe:perform (desig:a motion
                                  (type :lifting)
                                  (collision-mode ?collision-mode)
                                  (collision-object-b ?collision-object-b)
                                  (collision-object-b-link ?collision-object-b-link)
                                  (collision-object-a ?collision-object-a)
                                  (allow-base ?move-base)
                                  (prefer-base ?prefer-base)
                                  (straight-line ?straight-line)
                                  (align-planes-left ?align-planes-left)
                                  (align-planes-right ?align-planes-right)
                                  (precise-tracking ?precise-tracking)
                                  (context ?context))))

          (exe:perform (desig:a motion
                                (type :retracting)
                                (collision-mode ?collision-mode)
                                (collision-object-b ?collision-object-b)
                                (collision-object-b-link ?collision-object-b-link)
                                (collision-object-a ?collision-object-a)
                                (allow-base ?move-base)
                                (prefer-base ?prefer-base)
                                (straight-line ?straight-line)
                                (align-planes-left ?align-planes-left)
                                (align-planes-right ?align-planes-right)
                                (precise-tracking ?precise-tracking)
                                (object-name ?object-name)))))
      
      (when ?sequence-goal
        (exe:perform (desig:a motion
                              (type gripper)
                              (gripper-state "open")))
        (su-demos::pre-align-height-robot)
        (let ((?motions (list :aligning-height :reaching))
              (?object-height (cl-transforms:z ?object-size)))
          (print "sequence1")
          ;;(break)
          (exe:perform
           (desig:an action
                     (type sequence-goal)
                     (action "grasping")
                     (motions ?motions)
                     (goal-pose ?goal-pose)
                     (object-size ?object-size)
                     (from-above ?from-above)
                     (object-height ?object-height)
                     (object-name "test")
                     (gripper-state "neutral"))))
        ;;(break)
        
        (cpl:pursue
          (cpl:seq
            (exe:perform (desig:a motion
                                  (type gripper)
                                  (gripper-state "close")))
            (sleep 1)
            (su-demos::call-text-to-speech-action "Managed to grasp the object"))
          (cpl:seq
            (exe:perform
             (desig:an action
                       (type monitoring-joint-state)
                       (joint-name "hand_l_proximal_joint")))
            (su-demos::call-text-to-speech-action "Failed to grasp the object, retrying")
            (sleep 1)
            ;; (cpl:fail 'common-fail:gripper-closed-completely
            ;;           :description "Object slipped"
            ))
        (print "sequence2")
        ;;(break)
        (let ((?motions (list :vertical-motion :retracting)))
          (exe:perform
           (desig:an action
                     (type sequence-goal)
                     (action "grasping")
                     (motions ?motions)
                     (object-name "test"))))))))

;; @author Luca Krohm
;; @TODO failurehandling
;; has sequence
(defun place (&key
                ((:collision-mode ?collision-mode))
                ((:collision-object-b ?collision-object-b))
                ((:collision-object-b-link ?collision-object-b-link))
                ((:collision-object-a ?collision-object-a))
                ((:move-base ?move-base))
                ((:prefer-base ?prefer-base))
                ((:straight-line ?straight-line))
                ((:align-planes-left ?align-planes-left))
                ((:align-planes-right ?align-planes-right))
                ((:precise-tracking ?precise-tracking))
                ((:goal-pose ?goal-pose))
                ((:object-size ?object-size))
                ((:from-above ?from-above))
                ((:inside ?inside))
                ((:neatly ?neatly))
                ((:sequence-goal ?sequence-goal))
              &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"
  (declare (type boolean ?move-base ?prefer-base ?straight-line ?precise-tracking
                 ?align-planes-left ?align-planes-right))
  (unless ?sequence-goal
    (let ((?object-height (cl-transforms:z ?object-size))
          (?context `(("action" . "placing")
                      ("from_above" . ,?from-above))))
      (exe:perform (desig:a motion
                            (type aligning-height)
                            (collision-mode ?collision-mode)
                            (collision-object-b ?collision-object-b)
                            (collision-object-b-link ?collision-object-b-link)
                            (collision-object-a ?collision-object-a)
                            (allow-base ?move-base)
                            (prefer-base ?prefer-base)
                            (straight-line ?straight-line)
                            (align-planes-left ?align-planes-left)
                            (align-planes-right ?align-planes-right)
                            (precise-tracking ?precise-tracking)
                            (goal-pose ?goal-pose)
                            (object-height ?object-height)
                            (context ?context))))
    
    (let ((?context `(("action" . "placing")
                      ("from_above" . ,?from-above))))

      (when ?inside
        (setf ?object-size (cl-tf:make-3d-vector (cl-tf:z ?object-size)
                                                 (cl-tf:y ?object-size)
                                                 (+ (cl-tf:x ?object-size) 0.05))))
      (exe:perform (desig:a motion
                            (type reaching)
                            (collision-mode ?collision-mode)
                            (goal-pose ?goal-pose)
                            (object-size ?object-size)
                            (from-above ?from-above)
                            (context ?context))))

    (cond
      (?inside ;; inside could be done with cram "location" in the future, but can sth both be a location and an object?
       (let ((?context `(("action" . "placing")))
             (?height (cl-tf:z (su-demos::get-target-size-clean-up ?inside))))
          (exe:perform (desig:a motion
                                (type :vertical-motion)
                                (collision-mode ?collision-mode)
                                (collision-object-b ?collision-object-b)
                                (collision-object-b-link ?collision-object-b-link)
                                (collision-object-a ?collision-object-a)
                                (allow-base ?move-base)
                                (prefer-base ?prefer-base)
                                (straight-line ?straight-line)
                                (align-planes-left ?align-planes-left)
                                (align-planes-right ?align-planes-right)
                                (precise-tracking ?precise-tracking)
                                (distance ?height)
                                (context ?context)))))

      (?neatly
        (exe:perform (desig:a motion
                              (type placing)
                              (collision-mode ?collision-mode)
                              (collision-object-b ?collision-object-b)
                              (collision-object-b-link ?collision-object-b-link)
                              (collision-object-a ?collision-object-a)
                              (allow-base ?move-base)
                              (prefer-base ?prefer-base)
                              (straight-line ?straight-line)
                              (align-planes-left ?align-planes-left)
                              (align-planes-right ?align-planes-right)
                              (precise-tracking ?precise-tracking)
                              (goal-pose ?goal-pose)))))

    (exe:perform (desig:a motion
                          (type gripper)
                          (gripper-state "open")))
    
    (exe:perform (desig:a motion
                          (type :retracting)
                          (collision-mode ?collision-mode)
                          (collision-object-b ?collision-object-b)
                          (collision-object-b-link ?collision-object-b-link)
                          (collision-object-a ?collision-object-a)
                          (allow-base ?move-base)
                          (prefer-base ?prefer-base)
                          (straight-line ?straight-line)
                          (align-planes-left ?align-planes-left)
                          (align-planes-right ?align-planes-right)
                          (precise-tracking ?precise-tracking))))

  (when ?sequence-goal
    (let ((?motions (list :aligning-height :reaching))
          (?object-height (cl-transforms:z ?object-size)))
      (print "sequence1")
      ;;(break)
      (exe:perform
       (desig:an action
                 (type sequence-goal)
                 (action "placing")
                 (motions ?motions)
                 (goal-pose ?goal-pose)
                 (object-size ?object-size)
                 (from-above ?from-above)
                 (object-height ?object-height)
                 (object-name "test"))))
    
    (when ?neatly
      (exe:perform (desig:a motion
                            (type placing)
                            (collision-mode ?collision-mode)
                            (collision-object-b ?collision-object-b)
                            (collision-object-b-link ?collision-object-b-link)
                            (collision-object-a ?collision-object-a)
                            (allow-base ?move-base)
                            (prefer-base ?prefer-base)
                            (straight-line ?straight-line)
                            (align-planes-left ?align-planes-left)
                            (align-planes-right ?align-planes-right)
                            (precise-tracking ?precise-tracking)
                            (goal-pose ?goal-pose))))
    (exe:perform (desig:a motion
                          (type gripper)
                          (gripper-state "open")))
    
    (exe:perform (desig:a motion
                          (type :retracting)
                          (collision-mode ?collision-mode)
                          (collision-object-b ?collision-object-b)
                          (collision-object-b-link ?collision-object-b-link)
                          (collision-object-a ?collision-object-a)
                          (allow-base ?move-base)
                          (prefer-base ?prefer-base)
                          (straight-line ?straight-line)
                          (align-planes-left ?align-planes-left)
                          (align-planes-right ?align-planes-right)
                          (precise-tracking ?precise-tracking)))))


;; @author Felix Krause, Luca Krohm
;; @TODO failurehandling
;; sequence doesnt make sense yet
(defun open-door (&key
                    ((:collision-mode ?collision-mode))
                    ((:collision-object-b ?collision-object-b))
                    ((:collision-object-b-link ?collision-object-b-link))
                    ((:collision-object-a ?collision-object-a))
                    ((:move-base ?move-base))
                    ((:prefer-base ?prefer-base))
                    ((:straight-line ?straight-line))
                    ((:align-planes-left ?align-planes-left))
                    ((:align-planes-right ?align-planes-right))
                    ((:precise-tracking ?precise-tracking))
                    ((:handle-link ?handle-link))
                    ((:handle-pose ?handle-pose))
                    ((:joint-angle ?joint-angle))
                  &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"
  (declare (type boolean ?move-base ?prefer-base ?straight-line ?precise-tracking
                 ?align-planes-left ?align-planes-right))

  (let ((total-retries 0)
        (base-distance 0.05)
        (?reaching-pose ?handle-pose)
        (?collision-mode ?collision-mode))
    
  (exe:perform (desig:a motion
                        (type gripper)
                        (gripper-state "neutral")))


    (cpl:with-retry-counters ((bumping-retries 4))
      (cpl:with-failure-handling
          ((common-fail:environment-manipulation-goal-not-reached (e)
             (roslisp:ros-warn (suturo-reaching reaching)
                               "Some manipulation failure happened: ~a"
                               e)
             (cpl:do-retry bumping-retries
               (print "Bumped into door!")
               (print base-distance)
               (print ?reaching-pose)
               (setf base-distance (get-distance-to-move base-distance :bumping))
               (setf ?reaching-pose (modify-pose ?reaching-pose base-distance :bumping))
               (print ?reaching-pose)
               (print base-distance)
               (exe:perform (desig:a motion
                                     (type :retracting)
                                     (collision-mode ?collision-mode)
                                     (collision-object-b ?collision-object-b)
                                     (collision-object-b-link ?collision-object-b-link)
                                     (collision-object-a ?collision-object-a)
                                     (allow-base ?move-base)
                                     (prefer-base ?prefer-base)
                                     (straight-line ?straight-line)
                                     (align-planes-left ?align-planes-left)
                                     (align-planes-right ?align-planes-right)
                                     (precise-tracking ?precise-tracking)
                                     (tip-link t)))
               ;;Park robot
               (su-demos::prepare-robot)
               
               ;;Move to the original position
               (su-demos::with-knowledge-result (result)
                   `(and ("has_urdf_name" object "shelf:shelf:shelf_base_center")
                         ("object_rel_pose" object "perceive" result))
                 (su-demos::move-hsr (su-demos::make-pose-stamped-from-knowledge-result result)))
               (sleep 2)
               (su-demos::relocalize-robot "shelf:shelf:shelf_base_center")
               (cpl:retry))))
        
        (let ((?context `(("action" . "door-opening"))))
          (exe:perform (desig:a motion
                                (type reaching)
                                (collision-mode ?collision-mode)
                                (collision-object-b ?collision-object-b)
                                (collision-object-b-link ?collision-object-b-link)
                                (collision-object-a ?collision-object-a)
                                (allow-base ?move-base)
                                (prefer-base ?prefer-base)
                                (straight-line ?straight-line)
                                (align-planes-left ?align-planes-left)
                                (align-planes-right ?align-planes-right)
                                (precise-tracking ?precise-tracking)
                                (goal-pose ?reaching-pose)
                                (context ?context))))))
    
    (cpl:with-retry-counters ((grasping-retries 4))
      (cpl:with-failure-handling
          ((common-fail:gripper-closed-completely (e)
             (roslisp:ros-warn (suturo-reaching reaching)
                               "Some manipulation failure happened: ~a"
                               e)
                      (cpl:do-retry grasping-retries
                        (setf ?reaching-pose (modify-pose ?reaching-pose base-distance :bumping)) 
                        (print ?reaching-pose)
                        (su-real::open-gripper :effort 1)
                        (cpl:with-retry-counters ((bumping-retries 4))
                          (cpl:with-failure-handling
                              ((common-fail:environment-manipulation-goal-not-reached (e)
                                 (roslisp:ros-warn (suturo-reaching reaching)
                                                   "Some manipulation failure happened: ~a"
                                                   e)
                                 (cpl:do-retry bumping-retries
                                   (print "Bumped into door!")
                                   (print base-distance)
                                   (print ?reaching-pose)
                                   (setf base-distance (get-distance-to-move base-distance :bumping))
                                   (setf ?reaching-pose (modify-pose ?reaching-pose base-distance :bumping))
                                   (print ?reaching-pose)
                                   (print base-distance)
                                   (exe:perform (desig:a motion
                                                         (type :retracting)
                                                         (collision-mode ?collision-mode)
                                                         (collision-object-b ?collision-object-b)
                                                         (collision-object-b-link ?collision-object-b-link)
                                                         (collision-object-a ?collision-object-a)
                                                         (allow-base ?move-base)
                                                         (prefer-base ?prefer-base)
                                                         (straight-line ?straight-line)
                                                         (align-planes-left ?align-planes-left)
                                                         (align-planes-right ?align-planes-right)
                                                         (precise-tracking ?precise-tracking)
                                                         (tip-link t)))

                                   ;;Park robot
                                   (su-demos::prepare-robot)

                                   ;;Move to the original position
                                   (su-demos::with-knowledge-result (result)
                                       `(and ("has_urdf_name" object "shelf:shelf:shelf_base_center")
                                             ("object_rel_pose" object "perceive" result))
                                     (su-demos::move-hsr (su-demos::make-pose-stamped-from-knowledge-result result)))
                                   (sleep 5)
                                   (su-demos::relocalize-robot "shelf:shelf:shelf_base_center")
                                   (cpl:retry))))



                            (exe:perform (desig:a motion
                                                  (type :retracting)
                                                  (collision-mode ?collision-mode)
                                                  (collision-object-b ?collision-object-b)
                                                  (collision-object-b-link ?collision-object-b-link)
                                                  (collision-object-a ?collision-object-a)
                                                  (allow-base ?move-base)
                                                  (prefer-base ?prefer-base)
                                                  (straight-line ?straight-line)
                                                  (align-planes-left ?align-planes-left)
                                                  (align-planes-right ?align-planes-right)
                                                  (precise-tracking ?precise-tracking)
                                                  (tip-link t)))
               
                            (let ((?context `(("action" . "door-opening"))))
                              (exe:perform (desig:a motion
                                                    (type reaching)
                                                    (collision-mode ?collision-mode)
                                                    (collision-object-b ?collision-object-b)
                                                    (collision-object-b-link ?collision-object-b-link)
                                                    (collision-object-a ?collision-object-a)
                                                    (allow-base ?move-base)
                                                    (prefer-base ?prefer-base)
                                                    (straight-line ?straight-line)
                                                    (align-planes-left ?align-planes-left)
                                                    (align-planes-right ?align-planes-right)
                                                    (precise-tracking ?precise-tracking)
                                                    (goal-pose ?reaching-pose)
                                                    (context ?context))))))

                        (cpl:retry))))
												  
                 (cpl:pursue
                   (cpl:seq
                     (su-real::close-gripper :effort 1)
                     (sleep 3)
                     (cpl:fail 'common-fail:gripper-closed-completely
                               :description "Did not correctly grasp"))
                   (cpl:seq 
                     (exe:perform
                      (desig:an action
                                (type monitoring-joint-state)
                                (joint-angle-threshold -0.9)
                                (joint-name "hand_l_proximal_joint")))))
                 ))

  (cpl:with-retry-counters ((slipping-retries 5))
    (cpl:with-failure-handling
        ((common-fail:environment-manipulation-goal-not-reached (e)
           (roslisp:ros-warn (suturo-open open-door)
                             "Some manipulation failure happened: ~a"
                             e)
           (cpl:do-retry slipping-retries
             
             (print "Slipped while opening the door!")
             ;;Increment tries counter.
             (setf total-retries (1+ total-retries))
             (print ?reaching-pose)
             (print base-distance)
             ;;Define the amount the arm has to be moved & Change pose positive whatever axis that is
             (setf base-distance (get-distance-to-move base-distance :slipping))
             (print base-distance)
             (setf ?reaching-pose (modify-pose ?reaching-pose base-distance :slipping))
             (print ?reaching-pose)
             ;;Open gripper
             (open-gripper :effort 1)

             ;;Park robot
             (su-demos::prepare-robot)
             
             ;;Move to the original position
             (su-demos::with-knowledge-result (result)
                 `(and ("has_urdf_name" object "shelf:shelf:shelf_base_center")
                       ("object_rel_pose" object "perceive" result))
               (su-demos::move-hsr (su-demos::make-pose-stamped-from-knowledge-result result)))

             (su-demos::relocalize-robot "shelf:shelf:shelf_base_center")

             
             ;;Extend arm with failure handling
             (Print "Extending arm")
             
             (cpl:with-retry-counters ((bumping-retries 3))
               (cpl:with-failure-handling
                   ((common-fail:environment-manipulation-goal-not-reached (e)
                      (roslisp:ros-warn (suturo-reaching reaching)
                                        "Some manipulation failure happened: ~a"
                                        e)
                      (cpl:do-retry bumping-retries

                        (print "Bumped into door!")
                        (print ?reaching-pose)
                        (print base-distance)
                        (setf base-distance (get-distance-to-move base-distance :bumping))
                        (print base-distance)
                        (setf ?reaching-pose (modify-pose ?reaching-pose base-distance :bumping))
                        (print ?reaching-pose)
                        (exe:perform (desig:a motion
                                              (type :retracting)
                                              (collision-mode ?collision-mode)
                                              (collision-object-b ?collision-object-b)
                                              (collision-object-b-link ?collision-object-b-link)
                                              (collision-object-a ?collision-object-a)
                                              (allow-base ?move-base)
                                              (prefer-base ?prefer-base)
                                              (straight-line ?straight-line)
                                              (align-planes-left ?align-planes-left)
                                              (align-planes-right ?align-planes-right)
                                              (precise-tracking ?precise-tracking)
                                              (tip-link t)))


                        ;;Park robot
                        (su-demos::prepare-robot)

                        ;;Move to the original position
                        (su-demos::with-knowledge-result (result)
                            `(and ("has_urdf_name" object "shelf:shelf:shelf_base_center")
                                  ("object_rel_pose" object "perceive" result))
                          (su-demos::move-hsr (su-demos::make-pose-stamped-from-knowledge-result result)))
                        (sleep 5)
                        (su-demos::relocalize-robot "shelf:shelf:shelf_base_center")
                        (cpl:retry)
                        )))
                 (let ((?context `(("action" . "door-opening"))))
                   (exe:perform (desig:a motion
                                         (type reaching)
                                         (collision-mode ?collision-mode)
                                         (collision-object-b ?collision-object-b)
                                         (collision-object-b-link ?collision-object-b-link)
                                         (collision-object-a ?collision-object-a)
                                         (allow-base ?move-base)
                                         (prefer-base ?prefer-base)
                                         (straight-line ?straight-line)
                                         (align-planes-left ?align-planes-left)
                                         (align-planes-right ?align-planes-right)
                                         (precise-tracking ?precise-tracking)
                                         (goal-pose ?reaching-pose)
                                         (context ?context))))
             
                 (cpl:with-retry-counters ((grasping-retries 4))
                   (cpl:with-failure-handling
                       ((common-fail:gripper-closed-completely (e)
                          (roslisp:ros-warn (suturo-reaching reaching)
                                            "Some manipulation failure happened: ~a"
                                            e)
                          (cpl:do-retry grasping-retries
                            (setf ?reaching-pose (modify-pose ?reaching-pose base-distance :bumping)) 
                            (print ?reaching-pose)
                            (su-real::open-gripper :effort 1)
                            (cpl:with-retry-counters ((bumping-retries 4))
                              (cpl:with-failure-handling
                                  ((common-fail:environment-manipulation-goal-not-reached (e)
                                     (roslisp:ros-warn (suturo-reaching reaching)
                                                       "Some manipulation failure happened: ~a"
                                                       e)
                                     (cpl:do-retry bumping-retries
                                       (print "Bumped into door!")
                                       (print base-distance)
                                       (print ?reaching-pose)
                                       (setf base-distance (get-distance-to-move base-distance :bumping))
                                       (setf ?reaching-pose (modify-pose ?reaching-pose base-distance :bumping))
                                       (print ?reaching-pose)
                                       (print base-distance)
                                       (exe:perform (desig:a motion
                                                             (type :retracting)
                                                             (collision-mode ?collision-mode)
                                                             (collision-object-b ?collision-object-b)
                                                             (collision-object-b-link ?collision-object-b-link)
                                                             (collision-object-a ?collision-object-a)
                                                             (allow-base ?move-base)
                                                             (prefer-base ?prefer-base)
                                                             (straight-line ?straight-line)
                                                             (align-planes-left ?align-planes-left)
                                                             (align-planes-right ?align-planes-right)
                                                             (precise-tracking ?precise-tracking)
                                                             (tip-link t)))


                                       ;;Park robot
                                       (su-demos::prepare-robot)

                                       ;;Move to the original position
                                       (su-demos::with-knowledge-result (result)
                                           `(and ("has_urdf_name" object "shelf:shelf:shelf_base_center")
                                                 ("object_rel_pose" object "perceive" result))
                                         (su-demos::move-hsr (su-demos::make-pose-stamped-from-knowledge-result result)))
                                       (sleep 5)
                                       (su-demos::relocalize-robot "shelf:shelf:shelf_base_center")
                                       (cpl:retry)
                                       )))


                                (exe:perform (desig:a motion
                                                      (type :retracting)
                                                      (collision-mode ?collision-mode)
                                                      (collision-object-b ?collision-object-b)
                                                      (collision-object-b-link ?collision-object-b-link)
                                                      (collision-object-a ?collision-object-a)
                                                      (allow-base ?move-base)
                                                      (prefer-base ?prefer-base)
                                                      (straight-line ?straight-line)
                                                      (align-planes-left ?align-planes-left)
                                                      (align-planes-right ?align-planes-right)
                                                      (precise-tracking ?precise-tracking)
                                                      (tip-link t)))
               
                            (let ((?context `(("action" . "door-opening"))))
                              (exe:perform (desig:a motion
                                                    (type reaching)
                                                    (collision-mode ?collision-mode)
                                                    (collision-object-b ?collision-object-b)
                                                    (collision-object-b-link ?collision-object-b-link)
                                                    (collision-object-a ?collision-object-a)
                                                    (allow-base ?move-base)
                                                    (prefer-base ?prefer-base)
                                                    (straight-line ?straight-line)
                                                    (align-planes-left ?align-planes-left)
                                                    (align-planes-right ?align-planes-right)
                                                    (precise-tracking ?precise-tracking)
                                                    (goal-pose ?reaching-pose)
                                                    (context ?context)))))) (cpl:retry))))
												                     (cpl:pursue
                   (cpl:seq
                     (su-real::close-gripper :effort 1)
                     (sleep 3)
                     (cpl:fail 'common-fail:gripper-closed-completely
                               :description "Did not correctly grasp"))
                   (cpl:seq 
                     (exe:perform
                      (desig:an action
                                (type monitoring-joint-state)
                                (joint-angle-threshold -0.9)
                                (joint-name "hand_l_proximal_joint")))))




       ))))
             (cpl:retry))))
      
      (exe:perform (desig:a motion
                            (type pulling)
                            (arm :left)
                            (collision-mode :allow-all)
                            (collision-object-b-link ?handle-link)
                            (joint-angle ?joint-angle)))))

    (exe:perform (desig:a motion
                          (type gripper)
                          (gripper-state "neutral")))


    (exe:perform (desig:a motion
                          (type :retracting)
                          (collision-mode ?collision-mode)
                          (collision-object-b ?collision-object-b)
                          (collision-object-b-link ?collision-object-b-link)
                          (collision-object-a ?collision-object-a)
                          (allow-base ?move-base)
                          (reference-frame t)
                          (prefer-base ?prefer-base)
                          (straight-line ?straight-line)
                          (align-planes-left ?align-planes-left)
                          (align-planes-right ?align-planes-right)
                          (precise-tracking ?precise-tracking)
                          (tip-link t)))





    
  
 ))


;; @author Felix Krause
(defun modify-pose (pose distance case)
  (case case
    (:gripping 
     (cl-tf::copy-pose-stamped
      pose
      :origin
      (let ((vector (cl-tf::origin pose)))
        (cl-tf::copy-3d-vector
         vector
         :x (- (cl-tf::x vector) 0.01)
         :y (cl-tf::y vector) 
         :z (cl-tf::z vector)))
      :orientation
      (cl-tf::orientation pose)))
    (:slipping 
     (cl-tf::copy-pose-stamped
      pose
      :origin
      (let ((vector (cl-tf::origin pose)))
        (cl-tf::copy-3d-vector
         vector
         :x (+ (cl-tf::x vector) distance)
         :y (cl-tf::y vector) 
         :z (cl-tf::z vector)))
      :orientation
      (cl-tf::orientation pose)))
    (:bumping
     (cl-tf::copy-pose-stamped
      pose
      :origin
      (let ((vector (cl-tf::origin pose)))
        (cl-tf::copy-3d-vector
         vector
         :x (- (cl-tf::x vector) distance)
         :y (cl-tf::y vector) 
         :z (cl-tf::z vector)))
      :orientation
      (cl-tf::orientation pose)))))


          
;; @author Felix Krause
(defun get-distance-to-move (current-distance case)
  (case case
    (:slipping
     (/ current-distance 1.1)
     )
    (:gripping
     (/ current-distance 1.1)
     )
    (:bumping
     (/ current-distance 1.05)
     )))

;; @author Luca Krohm
(defun open-gripper (&key
                       ((:effort ?effort))
                    &allow-other-keys)
  (call-gripper-action (abs ?effort)))

;; @author Luca Krohm
(defun close-gripper (&key
                        ((:effort ?effort))
                      &allow-other-keys)
  (call-gripper-action (* -1 (abs ?effort))))

;; @author Luca Krohm
;; @TODO failurehandling
;; @TODO put the transforms etc into the designator, like its done in cram
(defun su-pour (&key
                  ((:collision-mode ?collision-mode))
                  ((:collision-object-b ?collision-object-b))
                  ((:collision-object-b-link ?collision-object-b-link))
                  ((:collision-object-a ?collision-object-a))
                  ((:object-size ?object-size))
                  ((:target-object ?target-object))
                  ((:target-size ?target-size))
                  ((:target-name ?target-name))
                  ((:sequence-goal ?sequence-goal))
                &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"

  (let* ((?object-transform (cl-tf:make-transform-stamped "base_footprint" ?target-object 0
                                                          (cl-tf:translation (cl-tf:lookup-transform cram-tf:*transformer* "base_footprint" ?target-object))
                                                          (cl-tf:make-quaternion 0 0 0 1)))
         ;; rel pose to rel transform
         (?rel-pose-transform (cl-tf2::make-pose-stamped
                               "base_footprint" 0
                               (cl-transforms:make-3d-vector
                                0
                                (/ (+ (cl-transforms:y ?target-size)
                                      (cl-transforms:y ?object-size))
                                   2)
                                (/ (+ (cl-transforms:z ?target-size)
                                      (cl-transforms:z ?object-size))
                                   2))
                               (cl-tf2::make-quaternion 0 0 0 1)))
         ;; moves the bowlpose like specified in ?relative-pour-pose, creating ?pour-pose-transform
         (?pour-pose-transform (cram-tf:apply-transform
                                (cl-tf:lookup-transform cram-tf:*transformer* "map" "base_footprint")
                                (cram-tf:apply-transform ?object-transform
                                                         (cram-tf:pose-stamped->transform-stamped
                                                          ?rel-pose-transform
                                                          "base_footprint"))))
         ;; pour transform to pour pose
         (?pour-pose (cram-tf:transform->pose-stamped
                      "map" 0
                      ?pour-pose-transform)))
    (unless ?sequence-goal
      (let ((?height 0.2215)
            (?context `(("action" . "pouring"))))
        (exe:perform (desig:a motion
                              (type aligning-height)
                              (collision-mode ?collision-mode)
                              (goal-pose ?pour-pose)
                              (object-height ?height)
                              (object-name ?target-name))))

      (let ((?context `(("action" . "pouring"))))
        (exe:perform (desig:a motion
                              (type reaching)
                              (collision-mode ?collision-mode)
                              (goal-pose ?pour-pose)
                              (object-size ?object-size)
                              (object-name ?target-name)
                              (context ?context))))

      (exe:perform (desig:a motion
                            (type tilting)
                            (tilt-direction "right")
                            (collision-mode ?collision-mode)))
      

      (exe:perform (desig:a motion
                            (type tilting)
                            (tilt-angle 0.0d0)
                            (collision-mode ?collision-mode)))

      
      (exe:perform (desig:a motion
                            (type :retracting)
                            (reference-frame "hand_gripper_tool_frame")
                            (collision-mode ?collision-mode)
                            (collision-object-b ?collision-object-b)
                            (collision-object-b-link ?collision-object-b-link)
                            (collision-object-a ?collision-object-a))))

    (when ?sequence-goal
       (let ((?motions (list :aligning-height :reaching :tilting))
             (?object-height (cl-transforms:z ?object-size)))
         (print "sequence1")
      ;;(break)
         (exe:perform
          (desig:an action
                    (type sequence-goal)
                    (action "pouring")
                    (motions ?motions)
                    (goal-pose ?pour-pose)
                    (object-size ?object-size)
                    (object-height ?object-height)
                    (tilt-direction "right")
                    (object-name "test"))))

       (let ((?motions (list :tilting :retracting)))
         (print "sequence2")
         ;;(break)
         (exe:perform
          (desig:an action
                    (type sequence-goal)
                    (action "pouring")
                    (motions ?motions)
                    (reference-frame "hand_gripper_tool_frame")
                    (tilt-angle 0.0d0)
                    (object-name "test")))))
 ))

(defun sequence-goal (&key
                        ((:action ?action))
                        ((:motions ?motions))
                        ((:object-type ?object-type))
                        ((:goal-pose ?goal-pose))
                        ((:object-height ?object-height))
                        ((:object-size ?object-size))
                        ((:object-shape ?object-shape))
                        ((:object-name ?object-name))
                        ((:from-above ?from-above))
                        ((:target-object ?target-object))
                        ((:target-size ?target-size))
                        ((:target-name ?target-name))
                        ((:tilt-direction ?tilt-direction))
                        ((:tilt-angle ?tilt-angle))
                        ((:reference-frame ?reference-frame))
                        ((:gripper-state ?gripper-state))
                        ((:pose-keyword ?pose-keyword))
                        ((:distance ?distance))
                      &allow-other-keys)
  (let ((?motion-sequence          
          (mapcar (lambda (motion)
                    (let ((attribs (get-attributes motion))
                          (attr-list nil))

                      (setf attr-list (remove nil (mapcar (lambda (attr)
                                                            (case attr
                                                              (:object-type (when ?object-type `("object_type" . ,?object-type)))
                                                              (:goal-pose (when ?goal-pose `("goal_pose" . (("message_type" . "geometry_msgs/PoseStamped")
                                                                                                            ("message" . ,(giskard::to-hash-table ?goal-pose))))))
                                                              (:object-height (when ?object-height `("object_height" . ,?object-height)))
                                                              (:object-size (when ?object-size `("object_size" . (("message_type" . "geometry_msgs/Vector3")
                                                                                                                  ("message" . ,(giskard::to-hash-table ?object-size))))))
                                                              (:object-shape (when ?object-shape `("object_shape" . ,?object-shape)))
                                                              (:object-name (when ?object-name `("object_name". ,?object-name)))
                                                              (:action (when ?action `("context" . ,(generate-context2 ?action `(from-above ,?from-above)))))
                                                              (:target-object (when ?target-object `("target_object" . ,?target-object)))
                                                              (:target-size (when ?target-size `("target_size" . (("message_type" . "geometry_msgs/Vector3")
                                                                                                                  ("message" . ,(giskard::to-hash-table ?target-size))))))
                                                              (:target-name (when ?target-name `("target_name" . ,?target-name)))
                                                              (:tilt-direction (when ?tilt-direction `("tilt_direction" . ,?tilt-direction)))
                                                              (:tilt-angle (when ?tilt-angle `("tilt_angle" . ,?tilt-angle)))
                                                              (:reference-frame (when ?reference-frame `("reference_frame" . ,?reference-frame)))
                                                              (:gripper-state (when ?gripper-state `("gripper_state" . ,?gripper-state)))
                                                              (:pose-keyword (when ?pose-keyword `("pose_keyword" . ,?pose-keyword)))
                                                              (:distance (when ?distance `("distance" . ,?distance)))))
                                                          attribs)))
               

                      (case motion
                        (:aligning-height `("AlignHeight" . ,attr-list))
                        (:reaching `("Reaching" . ,attr-list))
                        (:vertical-motion `("VerticalMotion" . ,attr-list))
                        (:retracting `("Retracting" . ,attr-list))
                        (:tilting `("Tilting" . ,attr-list))
                        (:gripper `("MoveGripper" . ,attr-list))
                        (:taking-pose `("TakePose" . ,attr-list)))))
                  
                  ?motions)))
    
    (print ?motion-sequence)
    (print "------------------")
    (print  (giskard::alist->json-string ?motion-sequence))
    ;;(break)
    (exe:perform (desig:a motion
                          (type :sequence-goal)
                          (collision-mode :allow-all)
                          (motion-sequence ?motion-sequence)))))

(defun take-pose (&key
                    ((:pose-keyword ?pose-keyword))
                    ((:head-pan ?head-pan))
                    ((:head-tilt ?head-tilt))
                    ((:arm-lift ?arm-lift))
                    ((:arm-flex ?arm-flex))
                    ((:arm-roll ?arm-roll))
                    ((:wrist-flex ?wrist-flex))
                    ((:wrist-roll ?wrist-roll))
                  &allow-other-keys)

  ;; example call
  ;; current pose-keywords: "park", "perceive", "assistance"
  ;; (exe:perform (desig:an action
  ;;                       (type taking-pose)
  ;;                       (pose-keyword STRING-OR-NIL)
  ;;                       (head-pan 0)
  ;;                       (head-tilt 0)
  ;;                       (arm-lift 0)
  ;;                       (arm-flex 0)
  ;;                       (arm-roll -1.5)
  ;;                       (wrist-flex -1.5)
  ;;                       (wrist-roll 0)))

  
  
  ;;added action just in case we want failurehandling later


  
  (exe:perform (desig:a motion
                        (type :taking-pose)
                        (collision-mode :allow-all)
                        (pose-keyword ?pose-keyword)
                        (head-pan ?head-pan)
                        (head-tilt ?head-tilt)
                        (arm-lift ?arm-lift)
                        (arm-flex ?arm-flex)
                        (arm-roll ?arm-roll)
                        (wrist-flex ?wrist-flex)
                        (wrist-roll ?wrist-roll))))





;;;;;;;;;;;;;;;;;;;;; HELPER FUNCTIONS

(defun get-attributes (motion)
  (case motion
    (:aligning-height (list :action :goal-pose :object-height :object-name))
    (:reaching (list :action :goal-pose :object-size :object-name))
    (:vertical-motion (list :action :distance))
    (:retracting (list :object-name :reference-frame))
    (:tilting (list :tilt-direction :tilt-angle))
    (:gripper (list :gripper-state))
    (:taking-pose (list :pose-keyword))))




(defun generate-context (action &key from-above)
  (print "context")
  ;;(break)
  (let ((attr-list `(("action" . ,action))))
    (when from-above
      (setf attr-list (reverse (acons "from_above" from-above attr-list))))
    (print attr-list)))

(defun generate-context2 (action &rest key-tuple-list)
  (let ((attr-list `(("action" . ,(keyword-to-giskard-param (intern (string action)))))))
    (mapc (lambda (key-tuple)
            (when (second key-tuple)
              (setf attr-list (acons
                               (keyword-to-giskard-param (first key-tuple))
                               (second key-tuple)
                               attr-list))))
            key-tuple-list)
    (print (reverse attr-list))))



(defun keyword-to-giskard-param (key)
  (let ((str (string-downcase (symbol-name key))))
    (loop while (search "-" str)
          do
             (setf str (replace str "_" :start1 (search "-" str))))
    str))


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Robocup

;; @author Luca Krohm
;; @TODO failurehandling
;; sequence doesnt make sense yet
;; (defun robo-open-door (&key
;;                          ((:collision-mode ?collision-mode))
;;                          ((:collision-object-b ?collision-object-b))
;;                          ((:collision-object-b-link ?collision-object-b-link))
;;                          ((:collision-object-a ?collision-object-a))
;;                          ((:object-name ?object-name))

;;                        &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"
  ;; (declare (type boolean ?move-base ?prefer-base ?straight-line ?precise-tracking
  ;;                ?align-planes-left ?align-planes-right))

  ;; (exe:perform (desig:a motion
  ;;                       (type gripper)
  ;;                       (gripper-state "close")))

  ;; (let ((?context `(("action" . "approach-door"))))
  ;;   (exe:perform (desig:a motion
  ;;                         (type reaching)
  ;;                         (collision-mode ?collision-mode)
  ;;                         (collision-object-b ?collision-object-b)
  ;;                         (collision-object-b-link ?collision-object-b-link)
  ;;                         (collision-object-a ?collision-object-a)
  ;;                         (allow-base ?move-base)
  ;;                         (prefer-base ?prefer-base)
  ;;                         (straight-line ?straight-line)
  ;;                         (align-planes-left ?align-planes-left)
  ;;                         (align-planes-right ?align-planes-right)
  ;;                         (precise-tracking ?precise-tracking)
  ;;                         (object-name ?handle-link)
  ;;                         (goal-pose ?handle-pose)
  ;;                         (context ?context))))
  
  ;; (exe:perform (desig:a motion
  ;;                       (type su-open)
  ;;                       (object-name ?object-name)))
  
  ;; (exe:perform (desig:a motion
  ;;                       (type :retracting)
  ;;                       (collision-mode ?collision-mode)
  ;;                       (collision-object-b ?collision-object-b)
  ;;                       (collision-object-b-link ?collision-object-b-link)
  ;;                       (collision-object-a ?collision-object-a)
  ;;                       (allow-base ?move-base)
  ;;                       (prefer-base ?prefer-base)
  ;;                       (straight-line ?straight-line)
  ;;                       (align-planes-left ?align-planes-left)
  ;;                       (align-planes-right ?align-planes-right)
  ;;                       (precise-tracking ?precise-tracking)
  ;;                       (tip-link t))))
