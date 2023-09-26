(in-package :su-demos)

;;@author Felix Krause
;;Launches the storing groceries demo.
;;Simply use (storing-groceris-demo) to start the demo if everything is set up properly.
;;max-objects: Determines the amount of objects that ist stored in the shelf from the table.
;;skip-open-shelf: T = Door opening will be skiped. NIL = Door opening wont be skiped.
;;skip-shelf-perception: T = shelf contents wont be perceived. NIL = shelf contents will be perceived.
;;joint-angle: Determines the angle at which the door is opened. Only change if the test environment is safe.
;;use-localization: T = opens the shelf with the handle from the semantic map. NIL = Currently not implemented.
;;what-door: :both = opens both doors :left = opens the left door. :right = opens the right door.
;;neatly: T = objects will be placed neatly. NIL = objects wont be placed neatly.
;;collision-mode: Determines the collision mode. :allow-all is recommended.
;;talk: T = talk requests will be executed. NIL = talk requests wont be executed.
;;sequence-goals: Currently just NIL.
(defun storing-groceries-demo (&key (max-objects 5) (skip-open-shelf NIL) (skip-shelf-perception NIL) (joint-angle 0.7) (use-localization T) (what-door :both) (neatly NIL) (collision-mode :allow-all) (talk T) (?sequence-goals nil))

  ;;Shelf, table, handle-link-left and handle-link-right have to be set or checked in a new arena!
  (let* ((shelf "shelf:shelf:shelf_base_center")
         (table "left_table:table:table_front_edge_center")
         (handle-link-left "iai_kitchen/shelf:shelf:shelf_door_left:handle")
         (handle-link-right "iai_kitchen/shelf:shelf:shelf_door_right:handle")
         (all-designator (desig:all object (type :everything)))
         (?neatly neatly)
         (?joint-angle-left (* joint-angle -1))
         (?joint-angle-right joint-angle))

 
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
                       (park-robot))))))
           
           (park-robot))
          ((equal skip-open-shelf T) 
           (print "Skipping sequence, shelf door wont be opened.")))
    
    
    (cond ((equal skip-shelf-perception NIL)           
         

           ;;Move to the shelf.
           (with-knowledge-result (result)
               `(and ("has_urdf_name" object ,shelf)
                     ("object_rel_pose" object "perceive" result))
             (move-hsr (make-pose-stamped-from-knowledge-result result)))
           
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

    (talk-request "I will now perceive the objects that are standing on the table!" talk)

    ;;Perceive the objects on the table. Puts all objects into a list. Objects are also automaticall saved into Knowledge by the Designator. The variable ?source-object-desig is not used anywhere right now but can be used for testing. 
    (let* ((?source-object-desig all-designator)
           (?list-of-objects
             (exe:perform (desig:all action
                                     (type detecting)
                                     (object ?source-object-desig)))))
      

;;=======================================MAIN=LOOP========================================================================
      
      (let* ((?place-poses NIL))
        
        ;;Perform this loop max-objects amount of times.
        (dotimes (n max-objects)

          ;;Definition of a lot of the variables needed for the loop.
          (let*  ((?collision-mode collision-mode)
                  
                  ;;Object size is currently fixed as there is no viable way to extract actual object size.
                  (?object-size (cl-tf2::make-3d-vector 0.06 0.145 0.25));;(extract-size ?current-object))
                  (?object-height 0.23)
                  (?next-object (get-next-object-storing-groceries))
                  (?next-pick-up-pose (get-pick-up-pose ?next-object))
                  (?next-place-pose (get-place-pose-in-shelf ?next-object))
                  (?from-above NIL))

            (talk-request "I will now pick-up the object!" talk)

            ;;Pick up the object.
            (exe:perform (desig:an action
                                   (type :picking-up)
                                   (goal-pose ?next-pick-up-pose)
                                   (object-size ?object-size)
                                   (object-name ?next-object)
                                   (sequence-goal ?sequence-goals)
                                   (collision-mode ?collision-mode)))
            
            
            (park-robot)

            ;;Move to the shelf
            (with-knowledge-result (result)
                `(and ("has_urdf_name" object ,shelf)
                      ("object_rel_pose" object "perceive" result))
              (move-hsr (make-pose-stamped-from-knowledge-result result)))

            (talk-request "I will now place the object!" talk)

            
            ;;Places the object currently held.
            (exe:perform (desig:an action
                                   (type :placing)
                                   (goal-pose ?next-place-pose)
                                   (object-height ?object-height)
                                   (object-size ?object-size)
                                   (sequence-goal ?sequence-goals)
                                   (from-above NIL)
                                   (neatly T)
                                   (object-name ?next-object)
                                   (collision-mode ?collision-mode)))
            
            (talk-request "I placed the object!" talk)
            

            ;;Update the location of the object in Knowledge that was placed.
            (update-object-pose ?next-object ?next-place-pose)

            
            (park-robot)

            ;;Move to the table to a perceive pose.         
            (with-knowledge-result (result)
                `(and ("has_urdf_name" object ,table)
                      ("object_rel_pose" object "perceive" result))
              (move-hsr (make-pose-stamped-from-knowledge-result result)))
            (print "Loop finished."))))

      (print "Demo finished."))))


;; @author Felix Krause
;; Test function for open dorr failure handling.
(defun open-drawer-test () ;;(cl-tf:make-3d-vector 3.20 0.21 0.84)
  (let ((?pose (make-pose-stamped-from-knowledge-result (with-knowledge-result (result)
      `(and ("has_urdf_name" object "shelf:shelf:shelf_door_left:handle")
            ("object_pose" object result))
    result))))

          ;; (cl-tf:make-pose-stamped "map" 0.0  (cl-tf:make-3d-vector 3.25 0.21 0.84) (cl-tf:make-quaternion 0 0 0 1))))

  (prepare-robot)
    (with-knowledge-result (result)
        `(and ("has_urdf_name" object "shelf:shelf:shelf_base_center")
              ("object_rel_pose" object "perceive" result))
            (move-hsr (make-pose-stamped-from-knowledge-result result)))

    (exe:perform (desig:an action
                           (type opening-door)
                           (joint-angle -0.7)
                           (handle-pose ?pose)
                           (handle-link "iai_kitchen/shelf:shelf:shelf_door_left:handle")
                           (tip-link t)
                           (collision-mode :allow-all)))))




