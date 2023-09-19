(in-package :su-real)

;; @author Luca Krohm
;; @TODO failurehandling
;; has sequence
(defun pick-up (&key
                  ((:collision-mode ?collision-mode))
                  ((:collision-object-b ?collision-object-b))
                  ((:collision-object-b-link ?collision-object-b-link))
                  ((:collision-object-a ?collision-object-a))
                  ((:object-type ?object-type))
                  ((:goal-pose ?goal-pose))
                  ((:object-size ?object-size))
                  ((:object-name ?object-name))
                  ((:context ?context))
                  ((:sequence-goal ?sequence-goal))
                &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"
  ;; (declare (type boolean ?move-base ?prefer-base ?straight-line ?precise-tracking
  ;;                ?align-planes-left ?align-planes-right))
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
        (let ((?object-height (cl-transforms:z ?object-size)))
          (su-demos::pre-align-height-robot)

          (exe:perform (desig:a motion
                                (type aligning-height)
                                (collision-mode ?collision-mode)
                                (goal-pose ?goal-pose)
                                (context ?context)
                                (object-height ?object-height)
                                (object-name ?object-name)))
          
          (exe:perform (desig:a motion
                                (type gripper)
                                (gripper-state "open")))
          
          

          (exe:perform (desig:a motion
                                (type reaching)
                                (collision-mode ?collision-mode)
                                (goal-pose ?goal-pose)
                                (object-size ?object-size)
                                (object-name ?object-name)
                                (context ?context)))

          (sleep 2)

          ;; so far we only ever grasp from above when picking up a very small object, ie a spoon
          ;; in which case the joint-state failurehandling fails. to prevent that case I added
          ;; the following if statement
          (if (assoc "from_above" ?context :test #'string=)
              (exe:perform (desig:a motion
                                    (type gripper)
                                    (gripper-state "close")))
              (cpl:pursue
                (cpl:seq
                  (exe:perform (desig:a motion
                                        (type gripper)
                                        (gripper-state "close")))
                  ;; sleep to give the failurehandling a chance to abort
                  (sleep 3)
                  (su-demos::call-text-to-speech-action "I was able to grasp the object"))
                (cpl:seq
                  (exe:perform
                   (desig:an action
                             (type monitoring-joint-state)
                             (joint-name "hand_l_proximal_joint")))
                  (su-demos::call-text-to-speech-action "Failed to grasp the object, retrying")
                  ;; sleep to make sure toya finishes her sentence
                  (sleep 1)
                  (cpl:fail 'common-fail:gripper-closed-completely
                            :description "Object slipped"))))
          
          
          (exe:perform (desig:a motion
                                (type :lifting)
                                (collision-mode ?collision-mode)
                                (context ?context)))

          (exe:perform (desig:a motion
                                (type :retracting)
                                (collision-mode ?collision-mode)
                                (object-name ?object-name)))))
      
      (when ?sequence-goal
        (exe:perform (desig:a motion
                              (type gripper)
                              (gripper-state "open")))
        (su-demos::pre-align-height-robot)
        (let ((?motions (list :aligning-height :reaching)))
          ;;(break)
          (exe:perform
           (desig:an action
                     (type sequence-goal)
                     (action "grasping")
                     (motions ?motions)
                     (goal-pose ?goal-pose)
                     (object-size ?object-size)
                     (object-name ?object-name)
                     (gripper-state "neutral"))))
        ;;(break)


        ;; current failurehandling does not work great with sequencegoals yet
        ;; (cpl:pursue
        ;;   (cpl:seq
        ;;     (exe:perform (desig:a motion
        ;;                           (type gripper)
        ;;                           (gripper-state "close")))
        ;;     (sleep 1)
        ;;     (su-demos::call-text-to-speech-action "Managed to grasp the object"))
        ;;   (cpl:seq
        ;;     (exe:perform
        ;;      (desig:an action
        ;;                (type monitoring-joint-state)
        ;;                (joint-name "hand_l_proximal_joint")))
        ;;     (su-demos::call-text-to-speech-action "Failed to grasp the object, retrying")
        ;;     (sleep 1)
        ;;     (cpl:fail 'common-fail:gripper-closed-completely
        ;;               :description "Object slipped"
        ;;     )))

            (exe:perform (desig:a motion
                                  (type gripper)
                                  (gripper-state "close")))
        
        (print "sequence2")
        ;;(break)
        (let ((?motions (list :vertical-motion :retracting)))
          (exe:perform
           (desig:an action
                     (type sequence-goal)
                     (action "grasping")
                     (motions ?motions)
                     (object-name ?object-name))))))))

;; @author Luca Krohm
;; @TODO failurehandling
;; has sequence
(defun place (&key
                ((:collision-mode ?collision-mode))
                ((:collision-object-b ?collision-object-b))
                ((:collision-object-b-link ?collision-object-b-link))
                ((:collision-object-a ?collision-object-a))
                ((:goal-pose ?goal-pose))
                ((:object-size ?object-size))
                ((:context ?context))
                ((:inside ?inside))
                ((:sequence-goal ?sequence-goal))
              &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"
  ;; (declare (type boolean ?move-base ?prefer-base ?straight-line ?precise-tracking
  ;;                ?align-planes-left ?align-planes-right))
  (unless ?sequence-goal
    (let ((?object-height (cl-transforms:z ?object-size)))
      (exe:perform (desig:a motion
                            (type aligning-height)
                            (collision-mode ?collision-mode)
                            (goal-pose ?goal-pose)
                            (object-height ?object-height)
                            (context ?context))))
    
    ;; this was added because we planned on putting a spoon inside a cup to transport it.
    ;; for this to work, the size of the object had to be rotated, to represent the correct height
    ;; in this situation
    (when ?inside
      (setf ?object-size (cl-tf:make-3d-vector (cl-tf:z ?object-size)
                                               (cl-tf:y ?object-size)
                                               (+ (cl-tf:x ?object-size) 0.05))))
    (exe:perform (desig:a motion
                          (type reaching)
                          (collision-mode ?collision-mode)
                          (goal-pose ?goal-pose)
                          (object-size ?object-size)
                          (context ?context)))

    (cond
      (?inside ;; inside could be done with cram "location" in the future, but can sth both be a
       ;; location and an object? (because of the cup mentioned earlier, which then would function
       ;; as an object as well as a location)
       (let ((?height (cl-tf:z (su-demos::get-target-size-clean-up ?inside))))
          (exe:perform (desig:a motion
                                (type :vertical-motion)
                                (collision-mode ?collision-mode)
                                (distance ?height)
                                (context ?context)))))

      ((assoc "neatly" ?context :test #'string=)
        (exe:perform (desig:a motion
                              (type placing)
                              (context ?context)
                              (collision-mode ?collision-mode)
                              (goal-pose ?goal-pose)))))

    (exe:perform (desig:a motion
                          (type gripper)
                          (gripper-state "open")))
    
    (exe:perform (desig:a motion
                          (type :retracting)
                          (collision-mode ?collision-mode))))

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
                 (object-name "test"))))

    ;; ?neatly *does not* work directly within sequence goals, because of the force monitor
    (when (assoc "neatly" ?context :test #'string=)
      (exe:perform (desig:a motion
                            (type placing)
                            (context ?context)
                            (object-size ?object-size)
                            (collision-mode ?collision-mode)
                            (goal-pose ?goal-pose))))
    
    (exe:perform (desig:a motion
                          (type gripper)
                          (gripper-state "open")))
    
    (exe:perform (desig:a motion
                          (type :retracting)
                          (collision-mode ?collision-mode)))))


;; @author Felix Krause, Luca Krohm
;; @TODO failurehandling
;; Failure Handling technische Präsentation Felix.
;; sequence doesnt make sense yet
(defun open-door (&key
                    ((:collision-mode ?collision-mode))
                    ((:collision-object-b ?collision-object-b))
                    ((:collision-object-b-link ?collision-object-b-link))
                    ((:collision-object-a ?collision-object-a))
                    ((:handle-link ?handle-link))
                    ((:handle-pose ?handle-pose))
                    ((:joint-angle ?joint-angle))
                  &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"
  ;; (declare (type boolean ?move-base ?prefer-base ?straight-line ?precise-tracking
  ;;                ?align-planes-left ?align-planes-right))

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
                                                 
                                                  (tip-link t)))
               
                            (let ((?context `(("action" . "door-opening"))))
                              (exe:perform (desig:a motion
                                                    (type reaching)
                                                    (collision-mode ?collision-mode)
                                                    (collision-object-b ?collision-object-b)
                                                    (collision-object-b-link ?collision-object-b-link)
                                                    (collision-object-a ?collision-object-a)
                                                    (goal-pose ?reaching-pose)
                                                    (context ?context))))))

                        (cpl:retry))))
												  
                     (cpl:pursue
                       (cpl:seq
                         (su-real::close-gripper :effort 1)
                         (sleep 4)
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
                                                      (tip-link t)))
               
                            (let ((?context `(("action" . "door-opening"))))
                              (exe:perform (desig:a motion
                                                    (type reaching)
                                                    (collision-mode ?collision-mode)
                                                    (collision-object-b ?collision-object-b)
                                                    (collision-object-b-link ?collision-object-b-link)
                                                    (collision-object-a ?collision-object-a)
                                                    (goal-pose ?reaching-pose)
                                                    (context ?context)))))) (cpl:retry))))
                     (cpl:pursue
                       (cpl:seq
                         (su-real::close-gripper :effort 1)
                         (sleep 4)
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
                          (object-name ?handle-link)
                          (goal-pose ?handle-pose)))
  
  (exe:perform (desig:a motion
                        (type gripper)
                        (gripper-state "close")))
  
  (exe:perform (desig:a motion
                        (type pulling)
                        (arm :left)
                        (collision-object-b-link ?handle-link)
                        (joint-angle ?joint-angle)))

  (exe:perform (desig:a motion
                        (type gripper)
                        (gripper-state "neutral")))

  ;; tiplink t because we dont want to retract the whole robot and risk getting stuck on the door.
  ;; instead we just want to retract the hand from the door
  (exe:perform (desig:a motion
                        (type :retracting)
                        (collision-mode ?collision-mode)
                        (tip-link t)))))



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
         :x (- (cl-tf::x vector) 0.015)
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
                  ((:context ?context))
                  ((:sequence-goal ?sequence-goal))
                &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"

  (let* (;; get transform of the object in base footprint
         (?object-transform (cram-tf:copy-transform-stamped
                             (cl-tf:lookup-transform cram-tf:*transformer*
                                                     "base_footprint"
                                                     ?target-object)
                             :rotation (cl-tf:make-quaternion 0 0 0 1)))

         
         
         ;; calculate relative pour pose, but in base_footprint to make sure we
         ;; have the correct rotation
         (?rel-pose-transform (cl-tf2::make-transform-stamped
                               "base_footprint" "base_footprint" 0
                               (cl-transforms:make-3d-vector
                                0
                                (/ (+ (cl-transforms:y ?target-size)
                                      (cl-transforms:y ?object-size))
                                   2)
                                (/ (+ (cl-transforms:z ?target-size)
                                      (cl-transforms:z ?object-size))
                                   2))
                               (cl-tf2::make-quaternion 0 0 0 1)))
         ;; applies the relative pour pose transform to the object transform, and
         ;; then get that pose relative to the map
         (?pour-pose-transform (cram-tf:apply-transform
                                (cl-tf:lookup-transform cram-tf:*transformer* "map" "base_footprint")
                                (cram-tf:apply-transform ?object-transform
                                                         ?rel-pose-transform)))
         ;; pour transform to pour pose
         (?pour-pose (cram-tf:transform->pose-stamped
                      "map" 0
                      ?pour-pose-transform)))
    (unless ?sequence-goal
      (let ((?height 0.2215))
        (exe:perform (desig:a motion
                              (type aligning-height)
                              (collision-mode ?collision-mode)
                              (goal-pose ?pour-pose)
                              (object-height ?height)
                              (object-name ?target-name))))


      (exe:perform (desig:a motion
                            (type reaching)
                            (collision-mode ?collision-mode)
                            (goal-pose ?pour-pose)
                            (object-size ?object-size)
                            (object-name ?target-name)
                            (context ?context)))

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
                            (collision-mode ?collision-mode))))

    (when ?sequence-goal
       (let ((?motions (list :aligning-height :reaching :tilting)))
         (print "sequence1")
         ;;(break)
         (exe:perform
          (desig:an action
                    (type sequence-goal)
                    (action "pouring")
                    (motions ?motions)
                    (goal-pose ?pour-pose)
                    (object-size ?object-size)
                    (tilt-direction "right")
                    (object-name ?target-name))))

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
                    (object-name ?target-name)))))))


;; @author Luca Krohm
(defun sequence-goal (&key
                        ((:motion-sequence ?motion-sequence))
                        ((:collision-mode ?collision-mode))
                      &allow-other-keys)
  
  (exe:perform (desig:a motion
                        (type :sequence-goal)
                        (collision-mode ?collision-mode)
                        (motion-sequence ?motion-sequence))))

;; @author Luca Krohm
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
;; @author Luca Krohm
(defun get-attributes (motion)
  (case motion
    (:aligning-height (list :context :goal-pose :object-name))
    (:reaching (list :context :goal-pose :object-name :object-size))
    (:placing (list :context :goal-pose :object-name :object-size))
    (:vertical-motion (list :context :distance))
    (:retracting (list :object-name :reference-frame))
    (:tilting (list :tilt-direction :tilt-angle))
    (:gripper (list :gripper-state))
    (:taking-pose (list :pose-keyword))
    (otherwise (error "~a is not a valid keyword" motion))))

;; @author Luca Krohm
(defun rel-context (motion)
  (case motion
    (:aligning-height (list :from-above :vertical-align))
    (:reaching (list :object-type :object-shape :from-above :vertical-align))
    (:placing (list :from-above :vertical-align))
    (:vertical-motion (list :from-above))
    (:retracting (list ))
    (:tilting (list ))
    (:gripper (list ))
    (:taking-pose (list ))
    (otherwise (error "~a is not a valid keyword" motion))))

;; @author Luca Krohm
(defun get-all-attributes (motion-list)
  (remove-duplicates (alexandria:flatten (mapcar #'get-attributes motion-list))))

;; @author Luca Krohm
(defun generate-motion-sequence (motions context &rest args &key
                                                              goal-pose
                                                              object-name
                                                              object-size
                                                              tilt-direction
                                                              tilt-angle
                                                              reference-frame
                                                              gripper-state
                                                              pose-keyword
                                                              distance)

  (declare (ignore goal-pose object-name object-size tilt-direction tilt-angle reference-frame gripper-state pose-keyword distance))
  (setf args (remove-nil args))
  (let (?motion-sequence)
    
    (dolist (motion motions ?motion-sequence)
      (let* ((motion-name (case motion
                            (:aligning-height "AlignHeight")
                            (:reaching "Reaching")
                            (:placing "Placing")
                            (:vertical-motion "VerticalMotion")
                            (:retracting "Retracting")
                            (:tilting "Tilting")
                            (:gripper "MoveGripper")
                            (:taking-pose "TakePose")))
             (motion-attributes (remove :context (get-attributes motion)))
             (attr-list (mapcar
                         (lambda (attr)
                           (generate-alist (assoc attr args)))
                         motion-attributes))
             (motion-context (cdr (assoc motion context))))
        (when motion-context
          (push (generate-alist `(context . ,motion-context)) attr-list))
        (push (intern (giskard::alist->json-string `((,motion-name . ,attr-list)))) ?motion-sequence)))
    (setf ?motion-sequence (coerce (reverse ?motion-sequence) 'vector))))

;; @author Luca Krohm
(defun keyword-to-giskard-param (key)
  (let ((str (string-downcase key)))
    (setf str (substitute #\_ #\- str))
    str))

;; @author Luca Krohm
(defun generate-context (action motions &rest arguments &key
                                                          from-above
                                                          neatly
                                                          object-type
                                                          object-shape
                                                          vertical-align)
  (declare (ignore from-above neatly object-type object-shape vertical-align))
  (setf arguments (remove-nil arguments))
  (labels ((parse-content (cont)
             (etypecase cont
               (cl-tf:3d-vector (giskard::to-hash-table cont))
               (cl-tf:pose-stamped (giskard::to-hash-table cont))
               (t cont)))
           (generate-msg (msg-type content)
             `(("message_type" . ,msg-type)
               ("message" . (("content" . ,content)))))
           (gen-context (act args)
             (let ((attr-list `(("action" . ,(generate-msg "manipulation_msgs/ContextAction"
                                                           (keyword-to-giskard-param act))))))
               (dolist (arg args attr-list)
                 (push (cons (keyword-to-giskard-param (car arg))
                             (generate-msg (to-giskard-msg-type (car arg))
                                           (parse-content (cdr arg))))
                       attr-list)))))

    (typecase motions
      (list (remove nil (mapcar
                         (lambda (motion)
                           (let ((args (remove-if (lambda (arg)
                                                    (not (member (car arg) (rel-context motion))))
                                                  arguments)))
                             (when (rel-context motion)
                               (cons motion (reverse (gen-context action args))))))
                         motions)))
      (keyword (reverse (gen-context action arguments))))))
  
;; @author Luca Krohm
(defun to-giskard-msg-type (param)
  (case param
    (:from-above "manipulation_msgs/ContextFromAbove")
    (:neatly "manipulation_msgs/ContextNeatly")
    (:object-type "manipulation_msgs/ContextObjectType")
    (:object-shape "manipulation_msgs/ContextObjectShape")
    (:vertical-align "manipulation_msgs/ContextVerticalAlign")
    (otherwise (error "Unknown parameter: ~a" param))))

;; @author Luca Krohm
(defun remove-nil (list)
  "Remove elements with nil second element and convert to cons cells."
  (loop for (key value) on list by #'cddr
        when value
          collect (cons key value)))

;; @author Luca Krohm
(defun generate-alist (alist)
  (typecase (cdr alist)
    (cl-tf:3d-vector `(,(keyword-to-giskard-param (car alist))
                        . (("message_type" . "geometry_msgs/Vector3")
                           ("message" . ,(giskard::to-hash-table (cdr alist))))))
    (cl-tf:pose-stamped `(,(keyword-to-giskard-param (car alist))
                            . (("message_type" . "geometry_msgs/PoseStamped")
                               ("message" . ,(giskard::to-hash-table (cdr alist))))))
    (t `(,(keyword-to-giskard-param (car alist)) . ,(cdr alist)))))

;; @author Luca Krohm
(defmethod yason::encode ((symbol symbol) &optional (stream *standard-output*))
  (let ((string (string symbol)))
    (loop for char across string do
         (write-char char stream))
    string))

(defun get-object-type (name)
  
  "cup")

(defun get-goal-pose (obj-name)
  (print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

    (print "This the the get-goal-pose function in suturo-plans, which was called because you did not specify a goal pose")
  
  (print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  (break)
  (cond
      ((search "Cereal" obj-name)  (cl-tf2::make-pose-stamped
                                    "map" 0
                                    (cl-tf2::make-3d-vector 2.0 -0.25 0.7)
                                    (cl-tf2::make-quaternion 0 0 0 1)))

      ((search "Milk" obj-name)  (cl-tf2::make-pose-stamped
                                    "map" 0
                                    (cl-tf2::make-3d-vector 2.0  -0.1 0.7)
                                    (cl-tf2::make-quaternion 0 0 0 1)))

      ((or (search "Spoon" obj-name)
           (search "Fork" obj-name))
      (cl-tf2::make-pose-stamped
        "map" 0
        (cl-tf2::make-3d-vector 2.05 0.3 0.75)
        (cl-tf2::make-quaternion 0 0 0 1)))

      ((search "Bowl" obj-name)  (cl-tf2::make-pose-stamped
                                    "map" 0
                                    (cl-tf2::make-3d-vector 2.0 0.15 0.775)
                                    (cl-tf2::make-quaternion 0 0 0 1)))))

(defun get-object-size (name)
  (or (su-demos::with-knowledge-result (shape)
          `("object_shape_workaround" ,name _ shape _ _)
        (when (second shape)
          (cl-tf:make-3d-vector (second shape)
                                (third shape)
                                (fourth shape))))
      (cond
        ((search "Cereal" name) (cl-tf2::make-3d-vector 0.14 0.06 0.225))
        ((search "Milk" name) (cl-tf2::make-3d-vector 0.09 0.06 0.2))
        ((or (search "Spoon" name)
             (search "Fork" name))
         (cl-tf2::make-3d-vector 0.19 0.02 0.01))
        ((search "Bowl" name) (cl-tf2::make-3d-vector 0.16 0.16 0.05)))))

(defun get-object-shape (name)
  "cube")

(defun get-from-above (name)
  (cond
    ((search "Cereal" name) nil)
    ((search "Milk" name) nil)
    ((or (search "Spoon" name)
         (search "Fork" name))
     T)
    ((search "Bowl" name) T)))

(defun get-neatly (name)
  (break)
  nil)

(defun get-frame (name)
  (su-demos::with-knowledge-result (frame)
      `("object_shape_workaround" ,name frame _ _ _)
    frame))


