(in-package :su-demos)


(defun opening-popcorn-demo (&key max-objects skip-open-shelf skip-shelf-perception joint-angle collision-mode talk)

  (let ((popcorndrawer "fake_dishwasher_tray:dishwasher:drawer_bottom")
        (handle-link "iai_kitchen/fake_dishwasher_tray:dishwasher:drawer_handle"))

    
    ;;Resets all Knowledge data.
    (with-knowledge-result ()
        `("reset_user_data"))
    
    
    ;;Init query for Knowledge.
    (with-knowledge-result ()
        `("init_clean_the_table_no_dishwasher"))

  
    (park-robot)

   
    (cond ((equal skip-open-shelf NIL)

           ;;Move to the shelf to a perceive pose.
           (with-knowledge-result (result)
               `(and ("has_urdf_name" object ,popcorndrawer)
                     ("object_rel_pose" object "perceive" result))
             (move-hsr (make-pose-stamped-from-knowledge-result result) talk))
           
         
         (print "Performing sequence, door will be opened.")
         ;;Open the shelf.
         (let ((?handle-link handle-link)
               (?joint-angle joint-angle)
               (?collision-mode collision-mode))

           
           (exe:perform (desig:an action
                                  (type opening-door)
                                  (handle-link ?handle-link)
                                  (joint-angle ?joint-angle)
                                  (tip-link t)
                                  (collision-mode ?collision-mode))))
           (park-robot))
          ((equal skip-open-shelf T) 
           (print "Skipping sequence, shelf door wont be opened.")))))


    
; Perceives and picks up items from a table and places them inside a diswasher object in the urdf file.
;; Thereafter, it places the dishwasher tab inside as well.
;;
(defun clean-the-table-demo (&key (step 0) (talk T) (break nil) (?sequence-goals nil))
                                        ;(when (<= step 0)
  (with-knowledge-result ()
      `("reset_user_data"))

  (with-knowledge-result ()
      `("init_clean_the_table_no_dishwasher"))
  (with-knowledge-result (table popcorntable)
      `(and ("has_urdf_name" object1 "left_table:table:table_front_edge_center")
            ("object_rel_pose" object1 "perceive" table)
            ("has_urdf_name" object2 "kitchen_table:table:table_front_edge_center")
            ("object_rel_pose" object2 "perceive" popcorntable))
    (talk-request "Hello I am Toya, i will now clean the table!" talk)
    ;;alwas park the robot before anything, so arm is not in the way
    (park-robot)
    (let* ((?source-object-desig nil)
           (?object-desig nil)
           (?current-object nil)
           (?current-object-from-above nil)
           (mug nil))
      ;;move to table
      (when (<= step 1)
        (move-hsr (make-pose-stamped-from-knowledge-result table) talk))
      
      ;;perceive  and set 
      (when (<= step 2)
        (perc-robot)
        (talk-request "I am now perceiving!" talk)
        (setf ?source-object-desig
              (desig:all object
                         (type :everything)))
        (setf ?object-desig
              (exe:perform (desig:an action
                                     (type detecting)
                                     (object ?source-object-desig)))))
      (when (<= step 3)
        (with-knowledge-result (nextobject)
            `("next_object" nextobject)
          (when break (break))
          
          (loop until (and (eq nextobject nil)
                           (eq mug nil))
                do
                    (move-hsr (make-pose-stamped-from-knowledge-result table) talk)
                    ;;set next object and current object
                    ;;own clean up hardcoded
                   (let* ((?target-pose (get-target-pos-clean-up nextobject)))
                     (with-knowledge-result (result)
                         `("next_object" result)
                       (setf ?current-object nextobject)
                       (setf nextobject result))
                     
                     (when (eq ?current-object nil)
                       (setf ?current-object mug))

                     (when (and (eq mug nil)
                                (search "Mug" ?current-object))
                       (setf mug ?current-object)
                       (setf ?current-object nil))
                     
                     (when break (break))
                     (when ?current-object
                       (with-knowledge-result (pose)
                           `("object_pose" ,?current-object pose)
                         
                         ;;get object size (hardcoded) and object pose
                         ;;get target size comes from serve-breakfast...
                         (let ((?object-size (get-target-size-clean-up ?current-object))
                               (?object-pose nil)
                               (?from-above nil)
                               (?small-object-case (or (search "Fork" ?current-object)
                                                       (search "Spoon" ?current-object)
                                                       (search "PlasticKnife" ?current-object)
                                                       (search "Knife" ?current-object)
                                                       (search "Bowl" ?current-object))))
                           ;;but tricky trick, set bowl pose different and smallies ;;hardcoded stuff
                           (cond
                             ((search "Bowl" ?current-object)
                              ;;bowl i moved to the y side to be able to grasp
                              (setf ?object-pose
                                    (make-pose-stamped-from-knowledge-result-for-bowl pose)))
                             ((search "Mug" ?current-object)
                              ;;bowl i moved to the y side to be able to grasp
                              (setf ?object-pose
                                    (make-pose-stamped-from-knowledge-result-for-mug pose)))
                             (?small-object-case
                              (setf ?object-pose
                                    (make-pose-stamped-from-knowledge-result-for-smallies pose)))
                             (t
                              (setf ?object-pose
                                    (make-pose-stamped-from-knowledge-result pose))))
                           ;;true or nil depending on our let above
                           (setf ?current-object-from-above ?small-object-case)

                           (cond
                             ((search "MetalPlate" ?current-object)
                              ;; (setf ?plate ?current-object)
                              ;;maybe put this into a better function
                              (human-assist talk)
                              ;; Calls knowledge to receive coordinates of the dinner table pose, then relays that pose to navigation
                              (move-hsr (make-pose-stamped-from-knowledge-result popcorntable) talk)
                              
                              (talk-request "I will now place: " talk :current-knowledge-object ?current-object)

                              (when break (break))

                              (setf ?from-above (get-above-placing-clean-up "Plate"))

                              (exe:perform (desig:an action
                                                     (type :placing)
                                                     (goal-pose ?target-pose)
                                                     (object-size ?object-size)
                                                     (from-above ?from-above)
                                                     (sequence-goal ?sequence-goals)
                                                     (neatly nil)
                                                     (collision-mode :allow-all)))

                              (talk-request "I placed the Object!" talk)
                              (park-robot))
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ((and (search "Mug" ?current-object)
                                   (search "Mug" mug))
                              (break)
                              (let ((?target-pose (get-target-pos-clean-up "Mug")))
                                (when break (break))
                                (talk-request "I will now Pick up :" talk :current-knowledge-object ?current-object)                                    
                                (when break (break))
                                
                                (exe:perform (desig:an action
                                                       (type picking-up)
                                                       (goal-pose ?object-pose)
                                                       (object-size ?object-size)
                                                       (sequence-goal ?sequence-goals)
                                                       (from-above ?current-object-from-above)
                                                       (collision-mode :allow-all)))
                                ;; Calls knowledge to receive coordinates of the dinner table pose, then relays that pose to navigation
                                (move-hsr (make-pose-stamped-from-knowledge-result popcorntable) talk)
                                
                                (talk-request "I will now place: " talk :current-knowledge-object ?current-object)

                                (when break (break))
                                (setf ?from-above (get-above-placing-clean-up "Mug"))
                                (exe:perform (desig:an action
                                                       (type :placing)
                                                       (goal-pose ?target-pose)
                                                       (object-size ?object-size)
                                                       (from-above ?from-above)
                                                       (sequence-goal ?sequence-goals)
                                                       (neatly nil)
                                                       (collision-mode :allow-all)))

                                (talk-request "I placed the Object!" talk)
                                (park-robot))
                              (setf mug nil))
                                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ((not (eq ?current-object nil))
                              (with-knowledge-result (mugpose)
                                  `(and ("has_type" mug ,(transform-key-to-string :MetalMug))
                                        ("object_pose" mug mugpose))
                                (let ((?target-pose (cond
                                                      (mugpose
                                                       (make-pose-stamped-from-knowledge-result-for-mug mugpose))
                                                      (t (get-target-pos-clean-up nextobject)))))
                                  
                                  ;;pick up object
                                  ;;care from my site atm no failure handling failure handling should only be disabled when from-above
                                  (when break (break))
                                  (talk-request "I will now Pick up :" talk :current-knowledge-object ?current-object)
                                        ;(print (concatenate 'string "object-pose" ?object-pose))
                                        ;(print (concatenate 'string "object-size" ?object-size))
                                        ;(print (concatenate 'string "current-object-from-above" ?current-object-from-above))
                                  
                                  (when break (break))
                                  
                                  (exe:perform (desig:an action
                                                         (type picking-up)
                                                         (goal-pose ?object-pose)
                                                         (object-size ?object-size)
                                                         (sequence-goal ?sequence-goals)
                                                         (from-above ?current-object-from-above)
                                                         (collision-mode :allow-all)))
                                  (park-robot)

                                  (move-hsr (make-pose-stamped-from-knowledge-result table) talk)
                                  
                                  (talk-request "I will now place: " talk :current-knowledge-object ?current-object)

                                  (when break (break))
                                  (setf ?from-above (get-above-placing-clean-up ?current-object))
                                  (let ((?inside (when (get-inside-clean-up ?current-object) mug)))
                                    (exe:perform (desig:an action
                                                           (type :placing)
                                                           (goal-pose ?target-pose)
                                                           (object-size ?object-size)
                                                           (from-above ?from-above)
                                                           (sequence-goal ?sequence-goals)
                                                           (inside ?inside) 
                                                           (neatly nil)
                                                           (collision-mode :allow-all))))

                                  (talk-request "I placed the Object!" talk)
                                  (park-robot)))))))))))))))




(defun get-target-pos-clean-up (obj-name)
  ;;todo delete bowl out of here i think idk hehe
  (if (search "Bowl" obj-name)
     (cl-tf2::make-pose-stamped "map" 02
                               (cl-tf2::make-3d-vector 0.20 0.8 0.85)
                               (cl-tf2::make-quaternion 0 0 0 1)))
  ;;           (search "Spoon" obj-name)
  ;;           (search "PlasticKnife" obj-name)
  ;;           (search "Knife" obj-name)
  ;;           (search "Bowl" obj-name))
    (cl-tf2::make-pose-stamped "map" 0
                               (cl-tf2::make-3d-vector 0.20 0.8 0.75)
                               (cl-tf2::make-quaternion 0 0 0 1)))

(defun get-target-size-clean-up (obj-name)
  (cond
      ((search "Cereal" obj-name) (cl-tf2::make-3d-vector 0.14 0.06 0.225))
      ((search "Milk" obj-name) (cl-tf2::make-3d-vector 0.09 0.06 0.2))
      ((search "Spoon" obj-name) (cl-tf2::make-3d-vector 0.16 0.06 0.155)) ;;actually 1,2 1,3
      ((search "Fork" obj-name) (cl-tf2::make-3d-vector 0.16 0.06 0.155)) 
      ((search "PlasticKnife" obj-name) (cl-tf2::make-3d-vector 0.16 0.06 0.125))
      ((search "Bowl" obj-name) (cl-tf2::make-3d-vector 0.8 0.16 0.05)) ;;8 was 16 doing hacky stuff
      ((search "Mug" obj-name) (cl-tf2::make-3d-vector 0.9 0.9 0.08))
      ((search "MetalPlate" obj-name) (cl-tf2::make-3d-vector 0.26 0.26 0.0125))))

(defun get-inside-clean-up (obj-name)
  (cond
      ((search "Cereal" obj-name) nil)
      ((search "Milk" obj-name) nil)
      ((search "Spoon" obj-name) t) ;;actually 1,2 1,3
      ((search "Fork" obj-name) t) 
      ((search "PlasticKnife" obj-name) t)
      ((search "Bowl" obj-name) nil) ;;8 was 16 doing hacky stuff
      ((search "Mug" obj-name) nil)
      ((search "MetalPlate" obj-name) nil)))

(defun get-above-placing-clean-up (obj-name)
  (cond
      ((search "Cereal" obj-name) nil)
      ((search "Milk" obj-name) nil)
      ((search "Spoon" obj-name) nil) ;;actually 1,2 1,3
      ((search "Fork" obj-name) nil) 
      ((search "PlasticKnife" obj-name) nil)
      ((search "Bowl" obj-name) t) ;;8 was 16 doing hacky stuff
      ((search "Mug" obj-name) nil)
      ((search "MetalPlate" obj-name) t)))


;;@author Felix Krause
;; Extracts the pose from an Object Designator.
(defun extract-pose (object)
  (roslisp:with-fields 
      ((?pose
        (cram-designators::pose cram-designators:data))) 
      object    
    ?pose))

;;@author Felix Krause
(defun extract-type (object)
  (roslisp:with-fields 
      ((?type
        (cram-designators::object-identifier cram-designators:data))) 
      object    
     (intern (string-trim "-1" ?type) :keyword)))

;;@author Tim Rienits
;; The hardcoded poses, where objects are to be placed into the dishwasher.
(defun get-object-types ()
  (list :spoon :fork :knife))

;;@author Tim Rienits
;; The hardcoded pose where the dishwasher tab should be placed.
(defun get-hardcoded-tab-pose ()
  (cl-tf:make-pose-stamped "map" 0.0  (cl-tf:make-3d-vector 1.2 1.3 0.2) (cl-tf:make-quaternion 0 0 0 1)))


;;@author Tim Rienits
;; Moves the HSR to urdf-object by querying the pose of it in Knowledge.
(defun urdf-move-to (urdf-object)
  
 (with-knowledge-result (result)
        `(and ("has_urdf_name" object ,urdf-object)
              ("object_rel_pose" object "perceive" result))
      (move-hsr (make-pose-stamped-from-knowledge-result result)))
  )

;;@author Tim Rienits
;; Moves the HSR to urdf-object by querying the pose of it in Knowledge.
(defun create-knowledge-object (?type ?pose)
  
 (with-knowledge-result (name)
          `("create_object" name ,(transform-key-to-string ?type)  ;;TODO Extract keyword
                            ,(reformat-stamped-pose-for-knowledge ?pose)
                            (list ("shape" ("box" 0.145 0.06 0.22)))))
  )

;;@author Tim Rienits
(defun get-next-object-clean-the-table ()
  (with-knowledge-result (result)
      `("next_object" result)
    result))



  
