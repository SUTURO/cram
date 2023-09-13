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
                            (collision-mode ?collision-mode)
                            (goal-pose ?goal-pose))))
    
    (exe:perform (desig:a motion
                          (type gripper)
                          (gripper-state "open")))
    
    (exe:perform (desig:a motion
                          (type :retracting)
                          (collision-mode ?collision-mode)))))


;; @author Luca Krohm
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

  ;; tiplink t because we dont want to retract the whole robot and risk getting stuck on the door.
  ;; instead we just want to retract the hand from the door
  (exe:perform (desig:a motion
                        (type :retracting)
                        (collision-mode ?collision-mode)
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

    (typecase motions
      (list (remove nil (mapcar (lambda (motion)
                                   (let ((args (remove-if (lambda (arg)
                                                            (not (member (car arg) (rel-context motion))))
                                                          arguments)))
                                     (when (rel-context motion)
                                       (cons motion (reverse (gen-context action args))))))
                                 motions)))
      (keyword (reverse (gen-context action arguments))))))
  

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

(defun get-goal-pose (obj-name)
  (print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

    (print "This the the get-goal-pose function in suturo-plans, which was called because you did not specify a goal pose")
  
  (print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
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
  "bowl_frame")



(defun motions->sequence-desig (motions)
  "Receives a motion list in the form of '(:some :motion :keywords :like :reaching), then generates a sequence goal designator which is ready to be copy and pasted"
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
 "Receives a rule in the form of am :keyword and a optional type :inference, :mandatory or :context, then generates a designator rule which is ready to be copy and pasted"
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
