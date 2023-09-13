(in-package :su-demos)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LUCA TODO
;; rewrite~/SUTURO/SUTURO_WSS/planning_ws/src/cram/cram_external_interfaces/cram_giskard/src/collision-scene.lisp to function without using the bulletworld as reasoning tool, but rather use knowledge as reasoning tool. For example "update-object-pose-in-collision-scene"

;; Rewrite or duplicate and change the following functions (in order to preserve the original implementation in case its vital to other plans):
;; make-giskard-environment-request, uses btr in on the very bottom

;; reset-collision-scene

;; update-object-pose-in-collision-scene

;; add-object-to-collision-scene

;; detach-object-in-collision-scene

;; attach-object-to-arm-in-collision-scene

;; full-update-collision-scene

;; (cram-occasions-events:on-event
;;      (make-instance 'cram-plan-occasions-events:object-perceived-event
;;                          :object-designator desig
;;                          :perception-source :whatever))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *objects* '(:CerealBox :milk :spoon :bowl))

(defun serve-breakfast-demo(&key (step 0) (talk t) (break nil) (?sequence-goals nil))
  
  ;; Calls knowledge to receive coordinates of the shelf pose, then relays that pose to navigation
  (with-knowledge-result (tableleft tableright)
      `(and ("reset_user_data")
            ("init_serve_breakfast")
            ;;adapt the following two lines to change the pickup location
            ("has_urdf_name" object1 "left_table:table:table_front_edge_center")
            ("object_rel_pose" object1 "perceive" (list ("direction" "-x")) tableleft)
            ;;adapt the following two lines to change the placing location
            ("has_urdf_name" object2 "left_table:table:table_front_edge_center")
            ("object_rel_pose" object2 "perceive" (list ("direction" "-x")) tableright))
    (print "Serving Breakfast plan started.")
    (park-robot)
    (talk-request "Hello I am Toya, i will now serve breakfast!" talk)
    (when (<= step 1)
      ;; move to pickup location
      (move-hsr (make-pose-stamped-from-knowledge-result-table-left tableleft) talk)
      (sleep 1))

    ;;(break)
    ;; perceive objects
    (let* ((?current-object nil)
           (?found-cereal nil)
           (?source-object-desig
             (desig:all object
                        (type :breakfast))))
      (park-robot)
      (talk-request "I am now perceiving!" talk)
      (exe:perform (desig:an action
                             (type detecting)
                             (object ?source-object-desig)))

      ;; get next object to handle
      (with-knowledge-result (nextobject)
          `("next_object" nextobject)
        (print "next object:")
        (print nextobject)
        (break)
        ;; loop until there is no new object and the cereal is handled
        (loop until (and (string= nextobject nil)
                         (eq ?found-cereal nil))
              do (let* ((?target-pose (get-target-pos nextobject)))
                   (with-knowledge-result (result)
                       `("next_object" result)
                     (setf ?current-object nextobject)
                     (setf nextobject result))
                   (print ?current-object)
                   (print nextobject)
                   ;; (break)
                   (progn
                     (move-hsr (make-pose-stamped-from-knowledge-result-table-right tableright) talk)
                     (cond
                       ((search "CerealBox" ?current-object) (setf ?found-cereal ?current-object))
                       (t
                        (when (and (eq nextobject nil)
                                   (eq ?current-object nil)
                                   (not (eq ?found-cereal nil)))
                          (print "Inside found-cereal to current-object")
                          (setf ?current-object ?found-cereal)
                          (setf ?found-cereal nil))
                        
                        (with-knowledge-result (pose)
                            `("object_pose" ,?current-object pose)                       
                          
                          ;; picks up the object by executing the following motions:
                          ;; - opening the gripper
                          ;; - reaching for the object
                          ;; - closing the gripper, thus gripping the object
                          ;; - lifting the object
                          ;; - retracting the arm to retrieve the object from, for example, a shelf
                          (let ((?object-size (get-target-size ?current-object))
                                (?object-pose nil)
                                (?from-above (get-frontal-placing ?current-object))
                                (?small-object-case (or (search "Spoon" ?current-object)
                                                        (search "Fork" ?current-object)
                                                        )))
                            (cond
                              ((search "Bowl" ?current-object)
                               ;;bowl i moved to the y side to be able to grasp
                               (setf ?object-pose
                                     (make-pose-stamped-from-knowledge-result-for-bowl-breakfast pose)))
                              (?small-object-case
                               (setf ?object-pose
                                     (make-pose-stamped-from-knowledge-result-for-smallies-breakfast pose)))
                              (t
                               (setf ?object-pose
                                     (make-pose-stamped-from-knowledge-result pose))))
                            (talk-request "I will now Pick up :" talk :current-knowledge-object ?current-object)
                            (when break (break))
                            ;; (cond
                            ;;   ((?object-pose 
                                (exe:perform (desig:an action
                                                                    (type picking-up)
                                                                    (goal-pose ?object-pose)
                                                                    (object-size ?object-size)
                                                                    (from-above ?from-above)
                                                                    (sequence-goal ?sequence-goals)
                                                                    (collision-mode :allow-all)))))
                              ;; (t t))
                       ))
                       
                        (park-robot)
                        
                        ;;(call-text-to-speech-action "Moving to target location")
                        ;; Calls knowledge to receive coordinates of the dinner table pose, then relays that pose to navigation
                        (move-hsr (make-pose-stamped-from-knowledge-result-table-left tableleft) talk)
                        
                        ;; places the object by executing the following motions:
                        ;; - preparing to place the object, by lifting the arm to an appropriate ?object
                        ;; - placing the object
                        ;; - opening the gripper, thus releasing the object
                        (unless (search "CerealBox" ?current-object)
                          (let ((?object-size (get-target-size ?current-object))
                                (?from-above (get-frontal-placing ?current-object))
                                (?neatly (get-neatly-placing ?current-object)))
                            ;;(call-text-to-speech-action "Placing object Cereal-Box")
                            ;; ?frontal-placing and ?neatly are currently the same for each object, thats why i just use the same function until after the milestone
                            (talk-request "I will now place: " talk :current-knowledge-object ?current-object)
                            (when break (break))
                            (exe:perform (desig:an action
                                                   (type :placing)
                                                   (goal-pose ?target-pose)
                                                   (object-size ?object-size)
                                                   (from-above ?from-above)
                                                   (sequence-goal ?sequence-goals)
                                                   (neatly ?neatly)
                                                   (collision-mode :allow-all)))
                            (talk-request "I placed the Object!" talk)
                            (with-knowledge-result ()
                                `("object_pose" ,?current-object ,(reformat-stamped-pose-for-knowledge (get-object-pos ?current-object)))
                              (talk-request "I will now perceive the object again!" talk)
                              (perc-robot)
                              (exe:perform (desig:an action
                                                     (type detecting)
                                                     (object ?source-object-desig)))
                              (park-robot))))))))

  (with-knowledge-result (bowlframe)
      `(and ("has_type" bowlname ,(transform-key-to-string :bowl))
            ("object_shape_workaround" bowlname bowlframe _ _ _))
    (let ((?object-size (get-target-size ?current-object))
          (?bowl-size (get-target-size bowlframe))
          (?cereal-target-pose (get-target-pos ?current-object))
          (?milk-target-pose (get-object-pos "Milk"))
          (?bowl-frame bowlframe))

      (talk-request "I will now pour:" talk :current-knowledge-object ?current-object)
      (when break (break))
      (exe:perform (desig:an action
                             (type su-pouring)
                             (target-object ?bowl-frame)
                             (object-size ?object-size)
                             (target-size ?bowl-size)
                             (collision-mode :allow-all)))
      (park-robot)
      (move-hsr (make-pose-stamped-from-knowledge-result-table-left tableleft) talk)

      (talk-request "I will now place: " talk :current-knowledge-object ?current-object)
      (when break (break))
      (exe:perform (desig:an action
                             (type :placing)
                             (goal-pose ?cereal-target-pose)
                             (object-size ?object-size)
                             (sequence-goal ?sequence-goals)
                             (neatly T)
                             (collision-mode :allow-all)))
      (talk-request "I placed the Object! " talk)

      (talk-request "I will now perceive the object again!" talk)
      (perc-robot)
      (exe:perform (desig:an action
                             (type detecting)
                             (object ?source-object-desig)))
      (park-robot)
      ;; Calls knowledge to receive coordinates of the dinner table pose, then relays that pose to navigation
      (move-hsr (make-pose-stamped-from-knowledge-result-table-left tableleft) talk)

      
      (let ((?object-size (get-target-size "Milk")))
        (talk-request "I will now Pick up :" talk :current-knowledge-object "MilkPack")
        (when break (break))
        (exe:perform (desig:an action
                               (type picking-up)
                               (goal-pose ?milk-target-pose)
                               (sequence-goal ?sequence-goals)
                               (object-size ?object-size)
                               (collision-mode :allow-all)))
        (park-robot)
        
        ;; Calls knowledge to receive coordinates of the dinner table pose, then relays that pose to navigation
        (move-hsr (make-pose-stamped-from-knowledge-result-table-left tableleft) talk)
        (talk-request "I will now pour:" talk :current-knowledge-object "MilkPack")
        (when break (break))
        
        (exe:perform (desig:an action
                               (type su-pouring)
                               (target-object ?bowl-frame)
                               (object-size ?object-size)
                               (target-size ?bowl-size)
                               (collision-mode :allow-all)))
        
        (park-robot)
        (move-hsr (make-pose-stamped-from-knowledge-result-table-left tableleft) talk)
        (talk-request "I will now place: " talk :current-knowledge-object "MilkPack")
        (when break (break))
        (exe:perform (desig:an action
                               (type :placing)
                               (goal-pose ?milk-target-pose)
                               (object-size ?object-size)
                               (sequence-goal ?sequence-goals)
                               (neatly T)
                               (collision-mode :allow-all)))
        (talk-request "I placed the Object! " talk)
        (park-robot)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Hardcoded stuff for debugging ;;;;;;;;;;;;


(defun park-robot ()
  "Default pose"
  (exe:perform (desig:an action
                         (type taking-pose)
                         (pose-keyword "park"))))

(defun perc-robot ()
  "Default pose"
  (exe:perform (desig:an action
                         (type taking-pose)
                         (pose-keyword "perceive"))))

(defun wait-robot ()
  "Default pose"
  (exe:perform (desig:an action
                         (type taking-pose)
                         (pose-keyword "assistance"))))


(defun nav-zero-pos ()
  "Starting pose in IAI office lab"
  (let ((vector (cl-tf2::make-3d-vector 0 0 0))
        (rotation (cl-tf2::make-quaternion 0 0 0 1)))
    (move-hsr (cl-tf2::make-pose-stamped "map" 0 vector rotation))))

(defun get-shelf-pos ()
  (cl-tf2::make-pose-stamped
   "map" 0
   (cl-tf2::make-3d-vector 0.01 0.95 0)
   (cl-tf2::make-quaternion 0 0 1 1)))

(defun get-table-pos ()
  (cl-tf2::make-pose-stamped
   "map" 0
   (cl-tf2::make-3d-vector 0.7 -0.95 0)
   (cl-tf2::make-quaternion 0 0 0 1)))

(defun get-target-pos (obj-name)
  (cond
      ((search "Cereal" obj-name)  (cl-tf2::make-pose-stamped
                                    "map" 0
                                    (cl-tf2::make-3d-vector 1.95 1.25 0.7)
                                    (cl-tf2::make-quaternion 0 0 0 1)))

      ((search "Milk" obj-name)  (cl-tf2::make-pose-stamped
                                    "map" 0
                                    (cl-tf2::make-3d-vector 1.73  1.25 0.7)
                                    (cl-tf2::make-quaternion 0 0 0 1)))

      ((or (search "Spoon" obj-name)
           (search "Fork" obj-name))
      (cl-tf2::make-pose-stamped
        "map" 0
        (cl-tf2::make-3d-vector 2.3 1.27 0.75)
        (cl-tf2::make-quaternion 0 0 0 1)))

      ((search "Bowl" obj-name)  (cl-tf2::make-pose-stamped
                                    "map" 0
                                    (cl-tf2::make-3d-vector 2.2 1.25 0.775)
                                    (cl-tf2::make-quaternion 0 0 0 1)))))

(defun get-object-pos (obj-name)
  (cond
      ((search "Cereal" obj-name)  (cl-tf2::make-pose-stamped
                                    "map" 0
                                    (cl-tf2::make-3d-vector 2.0 -0.25 0.81)
                                    (cl-tf2::make-quaternion 0 0 0 1)))

      ((search "Milk" obj-name)  (cl-tf2::make-pose-stamped
                                    "map" 0
                                    (cl-tf2::make-3d-vector 2.0 -0.1 0.8)
                                    (cl-tf2::make-quaternion 0 0 0 1)))

      ((or (search "Spoon" obj-name)
           (search "Fork" obj-name))
       (cl-tf2::make-pose-stamped
        "map" 0
        (cl-tf2::make-3d-vector 2.05 0.3 0.7)
        (cl-tf2::make-quaternion 0 0 0 1)))

      ((search "Bowl" obj-name)  (cl-tf2::make-pose-stamped
                                    "map" 0
                                    (cl-tf2::make-3d-vector 2.0 0.15 0.75)
                                    (cl-tf2::make-quaternion 0 0 0 1)))))


(defun get-target-size (obj-name)
  (cond
      ((search "Cereal" obj-name) (cl-tf2::make-3d-vector 0.14 0.06 0.225))
      ((search "Milk" obj-name) (cl-tf2::make-3d-vector 0.09 0.06 0.2))
      ((or (search "Spoon" obj-name)
           (search "Fork" obj-name))
       (cl-tf2::make-3d-vector 0.19 0.02 0.01))
      ((search "Bowl" obj-name) (cl-tf2::make-3d-vector 0.16 0.16 0.05))))
      
       
(defun get-frontal-placing (obj-name)
  (cond
      ((search "Cereal" obj-name) nil)
      ((search "Milk" obj-name) nil)
      ((or (search "Spoon" obj-name)
           (search "Fork" obj-name))
       T)
      ((search "Bowl" obj-name) T)))

(defun get-neatly-placing (obj-name)
  (cond
      ((search "Cereal" obj-name) T)
      ((search "Milk" obj-name) T)
      ((or (search "Spoon" obj-name)
           (search "Fork" obj-name))
       nil)
      ((search "Bowl" obj-name) nil)))




    
  
(defun pouring-test ()
  (let* ((?source-object-desig
           (desig:an object
                     (type bowl)))
         (?object-desig
           (exe:perform (desig:an action
                                  (type detecting)
                                  (object ?source-object-desig))))
         (?object-size1 (cl-tf2::make-3d-vector 0.16 0.16 0.05))
         (?object-size2 (cl-tf2::make-3d-vector 0.06 0.12 0.22))
         (?new-origin (cl-transforms:make-3d-vector
                       (/ (+ (cl-transforms:x ?object-size1)
                             (cl-transforms:x ?object-size2))
                          -2)
                       0
                       (/ (+ (cl-transforms:z ?object-size1)
                             (cl-transforms:z ?object-size2))
                          2)))
         (?object-transform (man-int::get-object-transform ?object-desig))
         (?temp-transform (cl-tf2::make-pose-stamped
                           "base_footprint" 0
                           ?new-origin
                           (cl-tf2::make-quaternion 0 0 0 1)))
         (?reach-transform (cram-tf:apply-transform
                            (cl-tf:lookup-transform cram-tf:*transformer* "map" "base_footprint")
                            (cram-tf:apply-transform ?object-transform
                                                    (cram-tf:pose-stamped->transform-stamped
                                                     ?temp-transform
                                                     "base_footprint"))))
         (?reach-pose (cram-tf:transform->pose-stamped
                       "map" 0
                       ?reach-transform)))
    ?reach-pose))



(defun wait-for-human-signal ()
  (cpl:seq
    (exe:perform (desig:a motion
                          (type gripper-motion)
                          (:open-close :open)
                          (effort 0.1)))
    (wait-robot)
    (call-text-to-speech-action "Please give me the object")
    (exe:perform
             (desig:an action
                       (type monitoring-joint-state)
                       (joint-name "wrist_flex_joint")))
    (call-text-to-speech-action "Thank you")
    (exe:perform (desig:a motion
                          (type gripper-motion)
                          (:open-close :close)
                          (effort 0.1)))))

(defun luca-test ()
  (let ((str (string-downcase (symbol-name :test-test-test-tes-tes-tes-tes-te-s-tes))))
    (loop while (search "-" str)
          do
             (setf str (replace str "_" :start1 (search "-" str))))
    str))

  ;; (cram-occasions-events:on-event
  ;;                (make-instance 'cram-plan-occasions-events:object-detached-robot-knowrob
  ;;                  :name name
  ;;                  :pose pose)))


