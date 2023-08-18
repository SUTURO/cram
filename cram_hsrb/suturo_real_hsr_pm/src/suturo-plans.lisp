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
                  ((:object-shape ?object-shape))
                  ((:object-name ?object-name))
                  ((:from-above ?from-above))
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
        (let ((?object-height (cl-transforms:z ?object-size))
              (?context `(("action" . "grasping")
                          ("from_above" . ,?from-above))))
          (su-demos::pre-align-height-robot)

          (exe:perform (desig:a motion
                                (type aligning-height)
                                (collision-mode ?collision-mode)
                                (collision-object-b ?collision-object-b)
                                (collision-object-b-link ?collision-object-b-link)
                                (collision-object-a ?collision-object-a)
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
                                  (context ?context))))

          (exe:perform (desig:a motion
                                (type :retracting)
                                (collision-mode ?collision-mode)
                                (collision-object-b ?collision-object-b)
                                (collision-object-b-link ?collision-object-b-link)
                                (collision-object-a ?collision-object-a)
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
                ((:goal-pose ?goal-pose))
                ((:object-size ?object-size))
                ((:from-above ?from-above))
                ((:inside ?inside))
                ((:neatly ?neatly))
                ((:sequence-goal ?sequence-goal))
              &allow-other-keys)
  "Receives parameters from action-designator, and then executes the corresponding motions"
  ;; (declare (type boolean ?move-base ?prefer-base ?straight-line ?precise-tracking
  ;;                ?align-planes-left ?align-planes-right))
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
                                (distance ?height)
                                (context ?context)))))

      (?neatly
        (exe:perform (desig:a motion
                              (type placing)
                              (collision-mode ?collision-mode)
                              (collision-object-b ?collision-object-b)
                              (collision-object-b-link ?collision-object-b-link)
                              (collision-object-a ?collision-object-a)
                              (goal-pose ?goal-pose)))))

    (exe:perform (desig:a motion
                          (type gripper)
                          (gripper-state "open")))
    
    (exe:perform (desig:a motion
                          (type :retracting)
                          (collision-mode ?collision-mode)
                          (collision-object-b ?collision-object-b)
                          (collision-object-b-link ?collision-object-b-link)
                          (collision-object-a ?collision-object-a))))

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
                            (goal-pose ?goal-pose))))
    (exe:perform (desig:a motion
                          (type gripper)
                          (gripper-state "open")))
    
    (exe:perform (desig:a motion
                          (type :retracting)
                          (collision-mode ?collision-mode)
                          (collision-object-b ?collision-object-b)
                          (collision-object-b-link ?collision-object-b-link)
                          (collision-object-a ?collision-object-a)))))


;; @author Luca Krohm
;; @TODO failurehandling
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

  (exe:perform (desig:a motion
                        (type gripper)
                        (gripper-state "neutral")))

  (let ((?context `(("action" . "door-opening"))))
    (exe:perform (desig:a motion
                          (type reaching)
                          (collision-mode ?collision-mode)
                          (collision-object-b ?collision-object-b)
                          (collision-object-b-link ?collision-object-b-link)
                          (collision-object-a ?collision-object-a)
                          (object-name ?handle-link)
                          (goal-pose ?handle-pose)
                          (context ?context))))
  
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
  
  (exe:perform (desig:a motion
                        (type :retracting)
                        (collision-mode ?collision-mode)
                        (collision-object-b ?collision-object-b)
                        (collision-object-b-link ?collision-object-b-link)
                        (collision-object-a ?collision-object-a)
                        (tip-link t))))

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

(defun sequence-goal-original (&key
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

(defun sequence-goal (&key
                        ((:motion-sequence ?motion-sequence))
                        ((:collision-mode ?collision-mode))
                      &allow-other-keys)
  
  (exe:perform (desig:a motion
                        (type :sequence-goal)
                        (collision-mode ?collision-mode)
                        (motion-sequence ?motion-sequence))))

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
                        (collision-mode :allow-arm)
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
    (:aligning-height (list :context :goal-pose :object-name))
    (:reaching (list :context :goal-pose :object-name :object-size))
    (:vertical-motion (list :context :distance))
    (:retracting (list :object-name :reference-frame))
    (:tilting (list :tilt-direction :tilt-angle))
    (:gripper (list :gripper-state))
    (:taking-pose (list :pose-keyword))
    (otherwise (error "~a is not a valid keyword" motion))))

(defun rel-context (motion)
  (case motion
    (:aligning-height (list :from-above))
    (:reaching (list :object-type :object-shape :from-above))
    (:vertical-motion (list :from-above))
    (:retracting (list ))
    (:tilting (list ))
    (:gripper (list ))
    (:taking-pose (list ))
    (otherwise (error "~a is not a valid keyword" motion))))

(defun get-all-attributes (motion-list)
  (remove-duplicates (alexandria:flatten (mapcar #'get-attributes motion-list))))

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


(defun keyword-to-giskard-param (key)
  (let ((str (string-downcase key)))
    (setf str (substitute #\_ #\- str))
    str))

(defun generate-context (action motions &rest arguments &key
                                                          from-above
                                                          neatly
                                                          object-type
                                                          object-shape)
  (declare (ignore from-above neatly object-type object-shape))
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
    
    (remove nil (mapcar (lambda (motion)
                          (let ((args (remove-if (lambda (arg)
                                                   (not (member (car arg) (rel-context motion))))
                                                 arguments)))
                            (when (rel-context motion)
                              (cons motion (reverse (gen-context action args))))))
                        motions))))

(defun to-giskard-msg-type (param)
  (case param
    (:from-above "manipulation_msgs/ContextFromAbove")
    (:neatly "manipulation_msgs/ContextNeatly")
    (:object-type "manipulation_msgs/ContextObjectType")
    (:object-shape "manipulation_msgs/ContextObjectShape")
    (otherwise (error "Unknown parameter: ~a" param))))

(defun remove-nil (list)
  "Remove elements with nil second element and convert to cons cells."
  (loop for (key value) on list by #'cddr
        when value
        collect (cons key value)))

(defun generate-alist (alist)
  (typecase (cdr alist)
    (cl-tf:3d-vector `(,(keyword-to-giskard-param (car alist))
                        . (("message_type" . "geometry_msgs/Vector3")
                           ("message" . ,(giskard::to-hash-table (cdr alist))))))
    (cl-tf:pose-stamped `(,(keyword-to-giskard-param (car alist))
                            . (("message_type" . "geometry_msgs/PoseStamped")
                               ("message" . ,(giskard::to-hash-table (cdr alist))))))
    (t `(,(keyword-to-giskard-param (car alist)) . ,(cdr alist)))))


(defmethod yason::encode ((symbol symbol) &optional (stream *standard-output*))
  (let ((string (string symbol)))
    (loop for char across string do
         (write-char char stream))
    string))

(defun get-object-type (name)
  "cup")

(defun get-goal-pose (name)
  (cl-tf2::make-pose-stamped
   "map" 0
   (cl-tf2::make-3d-vector 2.0 0.5 0.75)
   (cl-tf2::make-quaternion 0 0 0 1)))

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
  T)

(defun get-neatly (name)
  T)



(defun motions->sequence-desig (motions)
  (let* ((attribs (get-all-attributes motions))
         (motion-string (string-downcase (format nil ":~{~a~^ :~}" motions)))
         (info "Copy and paste the following designator into your code and adjust the variables where needed:")
         (des-str '("(exe:perform"
                    "(desig:an action"
                    "(type sequence-goal)"
                    "(action ?action)"))
         (att-str (mapcar (lambda (attr)
                             (string-downcase (format nil "(~a ?~a)" attr attr)))
                          (remove :context attribs)))
         (mot-str (push (format nil "(motions (~a))" motion-string) att-str)))
    (format t "~a~%" info)
    (format t "~{~%~a~^ ~}" des-str)
    (format t "~{~%~a~^ ~}))~%~%" mot-str)))

(defun attrib->desig-rule (attrib &optional (type :inference))

  (case type
      (:inference
       (let* ((info "Copy and paste the following inference rule into your designator and adjust the variables where needed:")
              (att-str (string-downcase attrib)))
         (format t "~a~%~%" info)
         (format t "(-> (member :~a ?attributes)~%" att-str)
         (format t "(once (or (desig:desig-prop ?designator (:~a ?~a))~%" att-str att-str)
         (format t "(lisp-fun su-real::get-~a ?ADJUST-INFERENCE-VAR-HERE~%" att-str)
         (format t "?~a)))~%" att-str)
         (format t "(equal ?~a nil))~%" att-str)))
      (:mandatory
       (let* ((info "Copy and paste the following inference rule into your designator and adjust the variables where needed:")
              (att-str (string-downcase attrib)))
         (format t "~a~%~%" info)
         (format t "(-> (member :~a ?attributes)~%" att-str)
         (format t "(or (desig:desig-prop ?designator (:~a ?~a))~%" att-str att-str)
         (format t "(and (format \"WARNING: Please specify the ~a.\")~%" att-str)
         (format t "(fail)))~%")
         (format t "(equal ?~a nil))~%" att-str)))
      (:context
       (let* ((info "Copy and paste the following inference rule into your designator and adjust the variables where needed:")
              (att-str (string-downcase attrib)))
         (format t "~a~%~%" info)
         (format t "(once (or (desig:desig-prop ?designator (:~a ?~a))~%" att-str att-str)
         (format t "(lisp-fun su-real::get-~a ?ADJUST-INFERENCE-VAR-HERE~%" att-str)
         (format t "?~a)))~%" att-str)))))
