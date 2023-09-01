(in-package :su-demos)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Robocup test functions:

(defun robocup-par-test ()
  (cpl::par
    (park-robot)
    (move-hsr (cl-tf::make-pose-stamped "map" 0
                                        (cl-tf:make-3d-vector 0 0 0)
                                        (cl-tf:make-quaternion 0 0 0 1)))))

(defun robocup-door-opening-test ()
  (with-knowledge-result (placeshelf pickuptable shelftf)
      `(and ("reset_user_data")
            ("init_storing_groceries")
            ("has_urdf_name" object1 "pantry:pantry:shelf_base_center")
            ("object_rel_pose" object1 "perceive" placeshelf)
            ("has_tf_frame" "pantry:pantry:shelf_base_center" shelftf)            
            ("has_urdf_name" object2 "storing_groceries_table:storing_groceries_table:table_center")
            ("object_rel_pose" object2 "perceive" pickuptable))

    (move-hsr (make-pose-stamped-from-knowledge-result placeshelf))

    (let ((?shelf-tf shelftf))
      
      (exe:perform (desig:an action
                             (type robo-opening-door)
                             (object-name ?shelf-tf)
                             ;;(sequence-goal ?sequence-goals)
                             (collision-mode :allow-all))))))

(defun robocup-pick-test ()

  (with-knowledge-result (placeshelf pickuptable)
      `(and ("reset_user_data")
            ("init_storing_groceries")
            ("has_urdf_name" object1 "pantry:pantry:shelf_base_center")
            ("object_rel_pose" object1 "perceive" placeshelf)
            ("has_urdf_name" object2 "storing_groceries_table:storing_groceries_table:table_center")
            ("object_rel_pose" object2 "perceive" pickuptable))

    (loop
      (park-robot)
      (move-hsr (make-pose-stamped-from-knowledge-result pickuptable))
      (perc-robot)

      (let* ((?source-object-desig
               (desig:an object
                         (type :everything)))
             (?source-perceived-object-desig
               (exe:perform (desig:an action
                                      (type detecting)
                                      (object ?source-object-desig)))))
        
             ;;Extracts pose from the return value of the detecting Designator.
             (roslisp:with-fields 
                 ((?pose
                   (cram-designators::pose cram-designators:data)))
                 ?source-perceived-object-desig

                  (su-demos::with-knowledge-result (frame)
                      `(and ("next_object" nextobject)
                            ("object_shape_workaround" nextobject frame _ _ _))
                            
                    (let* ((?object-size (get-robo-object-size frame)))
                      
                      
                      (exe:perform (desig:an action
                                             (type :picking-up)
                                             (goal-pose ?pose)
                                             (object-size ?object-size)
                                            ;; (sequence-goal ?sequence-goals)
                                             (collision-mode :allow-all)))))

                 (move-hsr (make-pose-stamped-from-knowledge-result pickuptable))
                 (park-robot)
                 (call-text-to-speech-action "Please catch the object")
                 (sleep 2)
                 (exe:perform (desig:a motion
                                       (type gripper)
                                       (gripper-state "open"))))))))

(defun get-robo-object-size (name)
  (cond
    ((search "Sponge" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Cleanser" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "JuicePack" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Cola" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Tropical" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Milk" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "IcedTea" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "OrangeJuice" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Tuna" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "TomatoSoup" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Spam" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Mustard" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "StrawberryJello" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "ChocolateJello" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "CoffeeGrounds" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Sugar" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Pear" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Plum" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Peach" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Lemon" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Orange" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Strawberry" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Banana" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Apple" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "TennisBall" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "RubiksCube" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "BaseBall" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Pringles" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Cornflakes" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Cheezit" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Spoon" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Plate" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Cup" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Fork" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Bowl" name) (cl-tf:make-3d-vector 1 2 3))
    ((search "Knife" name) (cl-tf:make-3d-vector 1 2 3))))

    
    
      
    

    





(defun sg-demo(&key (step 0) (talk t) (break nil) (?sequence-goals nil))
  
  ;; Calls knowledge to receive coordinates of the shelf pose, then relays that pose to navigation
  (with-knowledge-result (placeshelf pickuptable)
      `(and ("reset_user_data")
            ("init_storing_groceries")
            ("has_urdf_name" object1 "pantry:pantry:shelf_base_center")
            ("object_rel_pose" object1 "perceive" placeshelf)
            ("has_urdf_name" object2 "storing_groceries_table:storing_groceries_table:table_center")
            ("object_rel_pose" object2 "perceive" pickuptable))
    (print "Storing-groceries plan started.")
    (park-robot)

     ;; (move-hsr (make-pose-stamped-from-knowledge-result placeshelf))
    
    ;; (perc-robot)

    ;; (talk-request "I will now perceive the contents of the shelf!" talk)
    
    ;; (let* ((?source-object-desig-shelf (desig:all object (type :everything)))
    ;;        (?object-desig-list-shelf
    ;;          (exe:perform (desig:all action
    ;;                                  (type detecting)
    ;;                                  (object ?source-object-desig-shelf)))))
    
    (talk-request "Hello I am Toya, i will now store those groceries!" talk)

    (move-hsr (make-pose-stamped-from-knowledge-result pickuptable))
    
    (perc-robot)

    (talk-request "I will now perceive the groceries!" talk)
    
    (let* ((?source-object-desig-shelf (desig:all object (type :everything)))
           (?object-desig-list-shelf
             (exe:perform (desig:all action
                                     (type detecting)
                                     (object ?source-object-desig-shelf)))))
      
      (with-knowledge-result (nextobject)
          `("next_object" nextobject)
        (print "next object:")
        (print nextobject)
        ;; (break)
        (loop until (eq nextobject nil)
              do
                 (with-knowledge-result (pickpose placepose)
                     `(and ("object_rel_pose" ,nextobject "destination" (list) placepose)
                           ("object_pose" ,nextobject pickpose))

                   (let ((?pick-pose (get-table-pos-sg nextobject pickpose))
                         (?object-size (get-object-size nextobject)))


                     (talk-request "I will now Pick up: " talk :current-knowledge-object nextobject)
                     
                     (exe:perform (desig:an action
                                            (type :picking-up)
                                            (goal-pose ?pick-pose)
                                            (object-size ?object-size)
                                            (sequence-goal ?sequence-goals)
                                            (collision-mode ?collision-mode)))
                     
                     
                     (park-robot)

                     (move-hsr (make-pose-stamped-from-knowledge-result placeshelf))

                     (let ((?place-pose placepose)
                           (?neatly (get-neatly-placing-sg nextobject))
                           (?from-above (get-from-above-sg nextobject)))

                       (talk-request "I will now place: " talk :current-knowledge-object nextobject)

                       (exe:perform (desig:an action
                                              (type :placing)
                                              (goal-pose ?place-pose)
                                              (object-size ?object-size)
                                              (sequence-goal ?sequence-goals)
                                              (from-above NIL)
                                              (neatly ?neatly)
                                              (collision-mode ?collision-mode)))
                       (talk-request "I placed the Object!" talk)
                       (update-object-pose nextobject ?place-pose)

                       (park-robot)
                       (move-hsr (make-pose-stamped-from-knowledge-result pickuptable))))))))))
      


;;@author Felix Krause
;;max-objects dictates how many times the main loop from the table to the shelf should be performed.
;;open-shelf dictates if the shelf door should be opened. open-shelf = 0 -> skip door, open-shelf > 0 -> open door.
;;skip-shelf-perception dictates if the contents of the shelf should be perceived. Useful for testing. skip-shelf-perception = T -> skip perception of shelf contents, skip-shelf-perception = NIL -> perceive shelf contents.
;;joint-angle: Dictates how far the door will be opened, this value MUST ALWAYS be positive.
;;WARNING: JOINT ANGLE MUST BE POSITIVE FLOAT!
;;collision-mode: Different options, most common is :allow-all or :avoid-all
(defun storing-groceries-demo (&key (max-objects 5) skip-open-shelf (skip-shelf-perception NIL) (joint-angle 0.4) (use-localization T) (what-door :both) (neatly NIL) (collision-mode :allow-all) (talk T) (?sequence-goals nil))

  ;;Shelf, table, handle-link-left and handle-link-right have to be set or checked in a new arena!
  (let* ((shelf "open_shelf:shelf:shelf_base_center") ;;"shelf:shelf:shelf_base_center")
         (table "left_table:table:table_front_edge_center")
         (handle-link-left "iai_kitchen/shelf:shelf:shelf_door_left:handle")
         (handle-link-right "iai_kitchen/shelf:shelf:shelf_door_right:handle")
         (all-designator (desig:all object (type :everything)))
         (?neatly neatly)
         (?joint-angle-left joint-angle)
         (?joint-angle-right (* joint-angle -1)))

    ;;open_shelf:shelf:shelf_base_center
    ;;shelf:shelf:shelf_base_center


    ;;    (setf (btr:joint-state (btr:get-environment-object)
    ;;                        "cabinet1_door_top_left_joint")
    ;;       0.0
    ;;       (btr:joint-state (btr:get-environment-object)
    ;;                        "cabinet7_door_bottom_left_joint")
    ;;       0.025
    ;;       (btr:joint-state (btr:get-environment-object)
    ;;                        "dishwasher_drawer_middle_joint")
    ;;       0.0)
    ;; (btr-belief::publish-environment-joint-state
    ;;  (btr:joint-states (btr:get-environment-object)))


    
    ;;Resets all Knowledge data.
    (with-knowledge-result ()
        `("reset_user_data")
      (print "Storing groceries plan reset."))
    
    
    ;;Init query for Knowledge.
    (with-knowledge-result ()
        `("init_storing_groceries")
      (print "Storing groceries plan started."))

    
    (park-robot)

    (talk-request "Hello I am Toya, i will now store the groceries!" talk)
    
    
    (cond ((equal skip-open-shelf NIL)

           (talk-request "I will now move to the shelf to open it!" talk)

           ;;Move to the shelf to a perceive pose.
           (with-knowledge-result (result)
               `(and ("has_urdf_name" object ,shelf)
                     ("object_rel_pose" object "perceive" result))
             (move-hsr (make-pose-stamped-from-knowledge-result result)))
           
           (print "Performing sequence, door will be opened.")


           ;;If joint-angle is not set manually filter for the handle.
           (cond ((equal use-localization NIL)

                  (print "Opening the door with Perception data of the handle.")

                  (print "Unsupported action."))

                 ((equal use-localization T)

                  (print "Opening the door with the tf-frame of the handle.")
                  
                  (let* ((?collision-mode collision-mode)
                         (?handle-link-left handle-link-left)
                         (?handle-link-right handle-link-right))
                    
                    (case what-door
                      ;;TODO: UPDATE JOINT STATE OF THE SHELF DOORS!
                      (:left
                       (talk-request "I will now open the left door of the shelf!" talk)
                       ;;Open the shelf.
                       (exe:perform (desig:an action
                                              (type opening-door)
                                              (joint-angle ?joint-angle-left)
                                              (handle-link ?handle-link-left)
                                              (tip-link t)
                                              (collision-mode ?collision-mode)))
                       (park-robot))
                      (:right
                       (talk-request "I will now open the right door of the shelf!" talk)
                       ;;Open the shelf.
                       (exe:perform (desig:an action
                                              (type opening-door)
                                              (joint-angle ?joint-angle-right)
                                              (handle-link ?handle-link-right)
                                              (tip-link t)
                                              (collision-mode ?collision-mode)))
                       (park-robot))
                      (:both
                       (talk-request "I will now open both doors of the shelf!" talk)
                       (talk-request "I will now open the left door of the shelf!" talk)
                       ;;Open the left door.
                       (exe:perform (desig:an action
                                              (type opening-door)
                                              (joint-angle ?joint-angle-left)
                                              (handle-link ?handle-link-left)
                                              (tip-link t)
                                              (collision-mode ?collision-mode)))
                       (park-robot)

                       ;;Reposition in front of the shelf.
                       (with-knowledge-result (result)
                           `(and ("has_urdf_name" object ,shelf)
                                 ("object_rel_pose" object "perceive" result))
                         (move-hsr (make-pose-stamped-from-knowledge-result result)))

                       (talk-request "I will now open the right door of the shelf!" talk)
                       ;;Open the right door.
                       (exe:perform (desig:an action
                                              (type opening-door)
                                              (joint-angle ?joint-angle-right)
                                              (handle-link ?handle-link-right)
                                              (tip-link t)
                                              (collision-mode ?collision-mode)))
                       (park-robot)
                       )))))
           
           (park-robot))
          ((equal skip-open-shelf T) 
           (print "Skipping sequence, shelf door wont be opened.")))
    ;;(break)
    
    (cond ((equal skip-shelf-perception NIL)
           ;;Perceive the contents of the shelf.
           ;;Saves all possible objects.
           ;;Objects are then created in Knowledge.

           
           ;;Move to the shelf.
           (with-knowledge-result (result)
               `(and ("has_urdf_name" object ,shelf)
                     ("object_rel_pose" object "perceive" result))
             (move-hsr (make-pose-stamped-from-knowledge-result result)))
           
           (perc-robot)

           (talk-request "I will now perceive the contents of the shelf!" talk)
           
           (let* ((?source-object-desig-shelf all-designator)
                  (?object-desig-list-shelf
                    (exe:perform (desig:all action
                                            (type detecting)
                                            (object ?source-object-desig-shelf)))))

             
             (park-robot)
             
             
             (print ?object-desig-list-shelf)))
          ((equal skip-shelf-perception T)
           (print "Skipping sequence, shelf contents wont be perceived.")))


    ;;Move to the table to a perceive pose.
    (with-knowledge-result (result)
        `(and ("has_urdf_name" object ,table)
              ("object_rel_pose" object "perceive" result))
      (move-hsr (make-pose-stamped-from-knowledge-result result)))

    (perc-robot)

    (talk-request "I will now perceive the objects that are standing on the table!" talk)

    ;;Perceive the objects on the table. Put all objects into a list. 
    (let* ((?source-object-desig all-designator)
           (?list-of-objects
             (exe:perform (desig:all action
                                     (type detecting)
                                     (object ?source-object-desig)))))
      

      ;;=======================================MAIN=LOOP========================================================================
      
      (let* ((?place-poses (get-hardcoded-place-poses)))
        
        ;;Perform this loop max-objects amount of times.
        (dotimes (n max-objects)
          ;;Pick up the next best object in the list.
          (let*  ((?collision-mode collision-mode)
                  ;;HARDCODED
                  (?object-size (cl-tf2::make-3d-vector 0.06 0.145 0.215));;(extract-size ?current-object))
                  (?object-height 0.23)
                  ;;DYNAMIC Elements
                  (?next-object (get-next-object-storing-groceries))
                  (?next-pick-up-pose (get-pick-up-pose ?next-object))
                  (?next-place-pose (get-place-pose-in-shelf ?next-object))
                  ;;HARDCODED/OLD PLACE POSES
                  ;; (?place-pose (pop ?place-poses))
                  ;; (?current-object (pop ?list-of-objects))
                  ;; (?current-object-pose (extract-pose ?current-object))
                  )

            ;;(talk-request "I will now Pick up: " talk :current-knowledge-object ?next-object)

            ;;Pick up the object.
            (exe:perform (desig:an action
                                   (type :picking-up)
                                   (goal-pose ?next-pick-up-pose)
                                   (object-size ?object-size)
                                   (sequence-goal ?sequence-goals)
                                   (collision-mode ?collision-mode)))
            
            
            (park-robot)

            ;;Move to the shelf
            (with-knowledge-result (result)
                `(and ("has_urdf_name" object ,shelf)
                      ("object_rel_pose" object "perceive" result))
              (move-hsr (make-pose-stamped-from-knowledge-result result)))

            ;;(talk-request "I will now place: " talk :current-knowledge-object ?current-object)
            
            ;;Places the object currently held.
            (exe:perform (desig:an action
                                   (type :placing)
                                   (goal-pose ?next-place-pose)
                                   (object-height ?object-height)
                                   (object-size ?object-size)
                                   (sequence-goal ?sequence-goals)
                                   (from-above NIL)
                                   (neatly ?neatly)
                                   (collision-mode ?collision-mode)))
            
            (talk-request "I placed the Object!" talk)
            

            ;;Update the location of the Object in Knowledge.
            (update-object-pose ?next-object ?next-place-pose)

            
            (park-robot)

            ;;Move to the table to a perceive pose.         
            (with-knowledge-result (result)
                `(and ("has_urdf_name" object ,table)
                      ("object_rel_pose" object "perceive" result))
              (move-hsr (make-pose-stamped-from-knowledge-result result)))
            (print "Loop finished."))))

      (print "Demo finished."))))



(defun open-drawer-test () ;;2.92 // 3.26
  (let ((?pose (cl-tf:make-pose-stamped "map" 0.0  (cl-tf:make-3d-vector 3.26 0.24 0.84) (cl-tf:make-quaternion 0 0 0 1))))

    ;; (with-knowledge-result (?result)
    ;;     `(and ("has_urdf_name" object "shelf:shelf:shelf_base_center")
    ;;           ("object_rel_pose" object "perceive" result))
    ;;         (exe:perform
    ;;          (desig:an action
    ;;                    (type going)
    ;;                    (target (desig:a location (make-pose-stamped-from-knowledge-result ?result)))
                      ;; )))

    (exe:perform (desig:an action
                           (type opening-door)
                           (joint-angle -1.1)
                           (handle-pose ?pose)
                           (handle-link "iai_kitchen/shelf:shelf:shelf_door_left:handle")
                           (tip-link t)
                           (collision-mode :allow-all)))))


      ;; (exe:perform
      ;;  (desig:an action
      ;;            (type going)
      ;;            (target (desig:a location (make-pose-stamped-from-knowledge-result result)))))


;; :check-goal-function (lambda (result status)
;;                             (declare (ignore result))
;;                             (when (or (not status)
;;                                       (member status '(:preempted :aborted :timeout)))
;;                               (make-instance
;;                                   'common-fail:environment-manipulation-goal-not-reached
;;                                 :description "Giskard action failed.")))))




;;@author Felix Krause
(defun extract-pose (object)
  (roslisp:with-fields 
      ((?pose
        (cram-designators::pose cram-designators:data))) 
      object    
    ?pose))

;;@author Felix Krause
;;Dont use this for now.
(defun extract-type (object)
  (roslisp:with-fields 
      ((?type
        (cram-designators::object-identifier cram-designators:data))) 
      object    
     (intern (string-trim "-1" ?type) :keyword)))

;;@author Felix Krause
;;Doesnt work for now.
(defun extract-size (object)
  (roslisp:with-fields 
      ((?size
        (cram-designators::size cram-designators:description))) 
      object    
    ?size))


;;@author Felix Krause
(defun move-to-table (side table)
    (with-knowledge-result (result)
        `(and ("has_urdf_name" object ,table)
              ("object_rel_pose" object "perceive" result))
      (move-hsr (make-pose-stamped-from-knowledge-result result))))

;;@author Felix Krause
(defun move-to-shelf (side shelf)
  (with-knowledge-result (result)
      `(and ("has_urdf_name" object ,shelf)
            ("object_rel_pose" object "perceive" result))
    (move-hsr (make-pose-stamped-from-knowledge-result result))))

;;@author Felix Krause
(defun get-next-object-storing-groceries ()
  (with-knowledge-result (result)
      `("next_object" result)
    result))

;;@author Felix Krause
(defun get-place-pose-in-shelf (object)
    (with-knowledge-result (result)
      `("object_rel_pose" ,object "destination" (list) result)
      (make-pose-stamped-from-knowledge-result result)))

;;@author Felix Krause
(defun get-pick-up-pose (object)
  (with-knowledge-result (result)
      `("object_pose" ,object result)
    (make-pose-stamped-from-knowledge-result result)))

;;@author Felix Krause
(defun update-object-pose (object pose)
  (with-knowledge-result ()
      `("object_pose" ,object ,(reformat-stamped-pose-for-knowledge pose))
    (print "Object Pose updated !")))

;;@author Felix Krause
(defun get-handles ()
  (with-knowledge-result (result)
      `("findall" x ("has_type" x ,(transform-key-to-string :designedhandle)) result)
    result))

;;@author Felix Krause
(defun sort-handles (handles)
  (with-knowledge-result (result)
      `("sort_right_to_left" "base_footprint" ,(cons 'list (cons "base_footprint" handles)) result)
    result))


;;@author Felix Krause
(defun choose-handle ()


  (let*((?handle-transform (cl-tf:lookup-transform cram-tf:*transformer* "map" "DesignedHandle_LOQFJHIG"))
                          (?base-transform (cl-tf:lookup-transform cram-tf:*transformer* "map" "base_footprint")))

    
    ;; (publish-marker-pose (cl-tf:transform->pose (cl-tf:transform* (cl-tf:transform-inv ?base-transform) ?handle-transform)) :parent "base_footprint")
    (print (cl-tf:transform->pose (cl-tf:transform* (cl-tf:transform-inv ?base-transform) ?handle-transform)) )

    ))
  ;; ;;Base
  ;; (cl-tf:lookup-transform cram-tf:*transformer* "map" "base_footprint")

  ;; ;;Handle
  ;; (cl-tf:lookup-transform cram-tf:*transformer* "map" "DesignedHandle_IJCYOUGM")

  


;;@author Felix Krause
(defun get-hardcoded-place-poses ()
  (list (cl-tf:make-pose-stamped "map" 0.0  (cl-tf:make-3d-vector 2.15 2.55 0.72) (cl-tf:make-quaternion 0 0 0 1))
          (cl-tf:make-pose-stamped "map" 0.0  (cl-tf:make-3d-vector 1.95 2.55 0.72) (cl-tf:make-quaternion 0 0 0 1))
    (cl-tf:make-pose-stamped "map" 0.0  (cl-tf:make-3d-vector 1.95 2.55 0.48) (cl-tf:make-quaternion 0 0 0 1))
    (cl-tf:make-pose-stamped "map" 0.0  (cl-tf:make-3d-vector 2.1 2.55 0.48) (cl-tf:make-quaternion 0 0 0 1))
    (cl-tf:make-pose-stamped "map" 0.0  (cl-tf:make-3d-vector 1.9 2.55 0.48) (cl-tf:make-quaternion 0 0 0 1))))







;;@author Felix Krause
(defun test-place ()

  (park-robot)
  
  (let* ((?collision-mode :allow-all)
         (?object-height 0.2)
         (?place-pose (first (get-hardcoded-place-poses))))
    (print ?place-pose)
  
    
    (exe:perform (desig:an action
                           (type :placing)
                           (target-pose ?place-pose)
                           (object-height ?object-height)
                           (frontal-placing T)
                           (collision-mode ?collision-mode)))))


(defun test-perception ()

  ;;(park-robot)


  ;; (with-knowledge-result (result)
  ;;     `(and ("has_urdf_name" object ,"left_table:table:table_front_edge_center")
  ;;           ("object_rel_pose" object "perceive" result))
  ;;   (move-hsr (make-pose-stamped-from-knowledge-result result)))


  
  (let* ((?source-object-desig
           (desig:all object
                     (type :everything)))
         (?object-desig
           (exe:perform (desig:all action
                                  (type detecting)
                                  (object ?source-object-desig)))))
    

      (roslisp:with-fields 
        ((?pose
          (cram-designators::pose cram-designators:data))) 
          ?object-desig

        (print ?pose)))

      ;; (let ((?object-size
      ;;         (cl-tf2::make-3d-vector 0.06 0.145 0.215)))
      ;;   (exe:perform (desig:an action
      ;;                          (type picking-up)
      ;;                          (object-pose ?pose)
      ;;                          (object-size ?object-size)
      ;;                          (collision-mode :allow-all))))))




  )

;;Archived code for door opening with Perception data.
;; (talk-request "I will now open the door of the shelf!" talk)
           
;;                   ;;Perceiving the door handles and filter them. 
;;                   (print "Perceiving door handles.")
           
;;                   ;;Perceiving the handles to 
;;                   (let* ((?source-object-desig-handles (desig:all object (type :designedhandles)))
;;                          (?object-desig-list-handles
;;                            (exe:perform (desig:all action
;;                                           (type detecting)
;;                                           (object ?source-object-desig-handles))))))
                  

;;                   (let* ((?sorted-handles (reverse (sort-handles (get-handles))))
;;                          (?handle-link-left handle-link-left)
;;                          (?handle-link-right handle-link-right)
;;                          (?collision-mode collision-mode))
                    

;;                     (cond ((equal (length ?sorted-handles) 3)

;;                            (print "Opening both doors.")

;;                            (let* ((?goal-pose-left (get-pick-up-pose (first ?sorted-handles)))
;;                                   (?goal-pose-right (get-pick-up-pose (third ?sorted-handles))))


;;                              ;; Open the shelf.
;;                              (exe:perform (desig:an action
;;                                                     (type opening-door)
;;                                                     (joint-angle ?joint-angle-left)
;;                                                     (goal-pose ?goal-pose-left)
;;                                                     (handle-link ?handle-link-left)
;;                                                     (tip-link t)
;;                                                     (collision-mode ?collision-mode)))
                             
;;                              (park-robot)

;;                              ;;Reposition in front of the shelf.
;;                              (with-knowledge-result (result)
;;                                  `(and ("has_urdf_name" object ,shelf)
;;                                        ("object_rel_pose" object "perceive" result))
;;                                (move-hsr (make-pose-stamped-from-knowledge-result result)))


;;                              ;; Open the shelf.
;;                              (exe:perform (desig:an action
;;                                                     (type opening-door)
;;                                                     (joint-angle ?joint-angle-right)
;;                                                     (goal-pose ?goal-pose-right)
;;                                                     (handle-link ?handle-link-right)
;;                                                     (tip-link t)
;;                                                     (collision-mode ?collision-mode)))
;;                              (park-robot)))

                           
;;                           ((equal (length ?sorted-handles) 2)

;;                            (print "Opening only one door.")


;;                            (cond ((equal (first ?sorted-handles) "base_footprint")

;;                                   (print "Opening the right door.")

;;                                   (let* ((?goal-pose (get-pick-up-pose (second ?sorted-handles))))
                                               
;;                                     ;; Open the shelf.
;;                                     (exe:perform (desig:an action
;;                                                      (type opening-door)
;;                                                      (joint-angle ?joint-angle-right)
;;                                                      (goal-pose ?goal-pose)
;;                                                      (handle-link ?handle-link-right)
;;                                                      (tip-link t)
;;                                                      (collision-mode ?collision-mode)))
;;                                      (park-robot)))



;;                                  ((equal (second ?sorted-handles) "base_footprint")

;;                                   (let* ((?goal-pose (get-pick-up-pose (first ?sorted-handles))))

;;                                     (print "Opening the left door.")
                                          
;;                                     ;; Open the shelf.
;;                                     (exe:perform (desig:an action
;;                                                            (type opening-door)
;;                                                            (joint-angle ?joint-angle-left)
;;                                                            (goal-pose ?goal-pose)
;;                                                            (handle-link ?handle-link-left)
;;                                                            (tip-link t)
;;                                                            (collision-mode ?collision-mode)))
;;                                     (park-robot))))))))
