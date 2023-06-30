(in-package :su-demos)







;;@author Felix Krause
;;max-objects dictates how many times the main loop from the table to the shelf should be performed.
;;open-shelf dictates if the shelf door should be opened. open-shelf = 0 -> skip door, open-shelf > 0 -> open door.
;;skip-shelf-perception dictates if the contents of the shelf should be perceived. Useful for testing. skip-shelf-perception = T -> skip perception of shelf contents, skip-shelf-perception = NIL -> perceive shelf contents.
;;joint-angle: Dictates how far the door will be opened, this value MUST ALWAYS be positive.
;;WARNING: JOINT ANGLE MUST BE POSITIVE FLOAT!
;;collision-mode: Different options, most common is :allow-all or :avoid-all
(defun storing-groceries-demo (&key (max-objects 5) skip-open-shelf (skip-shelf-perception NIL) (joint-angle 0.4) (use-localization T) (what-door :both) (neatly NIL) (collision-mode :allow-all) (talk T) (?sequence-goals nil))

  ;;Shelf, table, handle-link-left and handle-link-right have to be set or checked in a new arena!
  (let* ((shelf "shelf:shelf:shelf_base_center")
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
             (vizbox-set-step 0) ;; Vizbox
             (move-hsr (make-pose-stamped-from-knowledge-result result)))
           
           (print "Performing sequence, door will be opened.")


           ;;If joint-angle is not set manually filter for the handle.
           (cond ((equal use-localization NIL)

                  (print "Opening the door with Perception data of the handle.")

                  (print "Unsupported action."))

                 ((equal use-localization T)

                  (print "Opening the door with the tf-frame of the handle.")

                  (vizbox-set-step 1) ;; Vizbox
     
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
    
   
  (cond ((equal skip-shelf-perception NIL)
         ;;Perceive the contents of the shelf.
         ;;Saves all possible objects.
         ;;Objects are then created in Knowledge.

         
         ;;Move to the shelf.
         (with-knowledge-result (result)
             `(and ("has_urdf_name" object ,shelf)
                   ("object_rel_pose" object "perceive" result))
           (vizbox-set-step 0) ;; Vizbox
           (move-hsr (make-pose-stamped-from-knowledge-result result)))

         (vizbox-set-step 2) ;; Vizbox
         (perc-robot)

         (talk-request "I will now perceive the contents of the shelf!" talk)
         
         (let* ((?source-object-desig-shelf all-designator)
                (?object-desig-list-shelf
                  (exe:perform (desig:all action
                                          (type detecting)
                                          (object ?source-object-desig-shelf)))))


           (vizbox-set-step 3) ;; Vizbox
           (park-robot)
           
           
           (print ?object-desig-list-shelf)))
        ((equal skip-shelf-perception T)
         (print "Skipping sequence, shelf contents wont be perceived.")))


    ;;Move to the table to a perceive pose.
    (with-knowledge-result (result)
        `(and ("has_urdf_name" object ,table)
              ("object_rel_pose" object "perceive" result))
      (vizbox-set-step 4) ;; Vizbox
      (move-hsr (make-pose-stamped-from-knowledge-result result)))

    (perc-robot)

    (talk-request "I will now perceive the objects that are standing on the table!" talk)

    ;;Perceive the objects on the table. Put all objects into a list. 
    (let* ((?source-object-desig all-designator)
           (?list-of-objects
             (exe:perform (desig:all action
                                     (type detecting)
                                     (object ?source-object-desig)))))
    (vizbox-set-step 5) ;; Vizbox
        

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
            (vizbox-set-step 6) ;; Vizbox
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
              (vizbox-set-step 7) ;; Vizbox
              (move-hsr (make-pose-stamped-from-knowledge-result result)))

            ;;(talk-request "I will now place: " talk :current-knowledge-object ?current-object)
            
            ;;Places the object currently held.
            (vizbox-set-step 8) ;; Vizbox
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

        (vizbox-set-step 9) ;; Vizbox

        (print "Demo finished."))))

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
