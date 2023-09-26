(in-package :gpsr-demo)
;;;;;;;;;;;;;;;;;;;;;;;;; HSR GPSR PLANS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;----->>>>>>> ALL inputs must be given in the form of keywords and these keywords are resolved by the gpsr-knowledge <<<<<----------

;;*****************************************************************************************************************************************
;;; NAVIGATION (?location ?location)
;;; TODO NAVIGATION TO THE PERSON
(defun navigate-to-location(?location-nr-furt ?room) ;;; input keywords... give atleast one input and set other :nil (room or location-nr-furt) e.g (navigate-to-location :nil :kitchen) or (navigate-to-location :side-table :nil)
  (let* ((?navigation-pose nil)) ;;; get pose 
    (if (eq ?location-nr-furt :nil) ;;; if location of the furniture/object not given then take the room  location
        (setf ?navigation-pose (get-navigation-pose ?room)))
    (if (eq ?room :nil) ;;;; if room is not given then take the location of the furniture/object
        (setf ?navigation-pose (get-navigation-location-near-furniture ?location-nr-furt)))
    
    (call-text-to-speech-action "Going to the location")
    (cpl:with-failure-handling
        ((common-fail:navigation-low-level-failure (e)
           (roslisp:ros-warn (pp-plans navigate)
                             "Low-level navigation failed: ~a~%.Ignoring anyway." e)
           (return-from navigate-to-location "fail")))    
      (let* ((?pose ?navigation-pose))
        (exe:perform (desig:an action
                               (type going)
                               (target (desig:a location
                                                (pose ?pose)))))))
    (return-from navigate-to-location "navigate")))

;; (defun navigate-to-room(?pose) ;;; input keywords... give atleast one input and set other :nil (room or location-nr-furt) e.g (navigate-to-location :nil :kitchen) or (navigate-to-location :side-table :nil)
;;   (cpl:with-failure-handling
;;       ((common-fail:navigation-low-level-failure (e)
;;          (roslisp:ros-warn (pp-plans navigate)
;;                            "Low-level navigation failed: ~a~%.Ignoring anyway." e)
;;          (return-from navigate-to-location "fail")))    
;;        (exe:perform (desig:an action
;;                      (type going)
;;                      (target (desig:a location
;;                               (pose ?pose))))))
;;   (return-from navigate-to-location "navigate"))
;;***************************************************************************************************************************************************
;;; SEARCHING-object or person  (?object ?person ?location ?location)
;; plan depends on  navigation-to-location 
(defun searching-object (?object ?pr-name ?pr-type ?pr-action ?location-nr-furt ?room) ;;;give object/person and give one location at least -object :bottle :nil :couch :nil) or (searching-object :nil :alex :couch :nil) 
  (let* ((?perceived-object nil)
         (?object-looking-check nil)
         (?person-looking-check nil))
    
    ;; (setf *personname*  :mehreen)
    ;; (setf *personaction* :sitting)

    ;;if looking for object
    (if (not (eq ?object :nil))
        (setf ?object-looking-check T) ;;(if object check is T else nil)
        (setf ?object-looking-check nil))

    ;;if looking for person

    (if (not (eq (get-any-person-feature ?pr-name ?pr-type ?pr-action) :nil))
        (setf ?person-looking-check T) ;;(if person check is T else nil)
        (setf ?person-looking-check nil))


;;; go to the location
    
    (if (and (eq ?location-nr-furt :nil) (eq ?room :nil))
        (navigate-to-location (get-specific-info-word ?object :default-location-in-room *gpsr-objects*) :nil) ;;; if no location is given get it from gpsr-knowledge where the object is supposed to be
        (navigate-to-location ?location-nr-furt ?room)) ;;;; else go to the location
    
    (call-text-to-speech-action "Trying to perceive the object or person")

    (let* ((possible-look-directions `(,*forward-upward*
                                       ,*left-downward*
                                       ,*left-downward*
                                       ,*forward-downward*
                                       ,*right-downward*
                                       ,*right-downward*
                                       ,*forward-downward*))
           (?looking-direction (first possible-look-directions)))
      (setf possible-look-directions (cdr possible-look-directions))
      (cpl:with-failure-handling
          (((or common-fail:perception-object-not-found
                common-fail:perception-low-level-failure
                desig:designator-error) (e)
             (when possible-look-directions
               (roslisp:ros-warn  (perception-failure) "Searching messed up: ~a~%Retring by turning head..." e)
               (setf ?looking-direction (first possible-look-directions))
               (setf possible-look-directions (cdr possible-look-directions))
               (exe:perform (desig:an action 
                                      (type looking)
                                      (target (desig:a location
                                                       (pose ?looking-direction)))))
               (cpl:retry))
             
             (roslisp:ros-warn (pp-plans pick-up) "No more retries left..... going back ")
             (call-text-to-speech-action "Plan fails")
             (return-from searching-object "fail")))
		;;; for ?object
        (when (eq ?object-looking-check T)
          (print "...................Looking for object..........................")
          ;;(call-text-to-speech-action "Looking for object")
          
          (let ((?looking-for (object-to-be ?object)))
            (setf ?perceived-object (exe:perform (desig:an action ;;; save the object designator in global variable
                                                            (type detecting)
                                                            (object (desig:an object
                                                                              (type ?looking-for))))))
            (call-text-to-speech-action "Successfully perceived object"))) 
        
               ;;;; for person    
        (when (eq ?person-looking-check T)
          (print "...................Looking for Human..........................")
          ;;(call-text-to-speech-action "Looking for Human")
          
          (let ((?human-name (string-upcase ?pr-name)) (?human-action (string-upcase ?pr-action)))              	
            (setf ?perceived-object (exe:perform (desig:an action
                                                           (type detecting)
                                                           (object (desig:an object
                                                                             (type :HUMAN)
                                                                             (desig:when ?human-name
                                                                               (size ?pr-name))
                                                                             (desig:when ?human-action
                                                                               (location ?pr-action))
                                                                             )))))
            (call-text-to-speech-action "Successfully perceived person"))                
          
          
          (return-from searching-object "search"))
        
        ))))


;;; ************************************************************************************************************************************************
;;; FETCH the object (?object ?location)
;; plan depends on searching plan-->>  navigation-to-location 
(defun fetching-object (?object ?object-type ?object-specification ?location-nr-furt ?room) ;;; 
  (let* ((?perceived-object-desig nil)
         (?object-size nil))
    (searching-object ?object :nil :nil :nil ?location-nr-furt ?room)
    ;;--->>>>> ADD pickup plan with failure handling and (searching plan save the object designator in global variable ) use  *perceived-object* to get object designator
    (roslisp:with-fields
        ((?pose
          (cram-designators::pose cram-designators:data)))
        ?perceived-object-desig
      (setf ?object-size (cl-tf2::make-3d-vector 0.05 0.05 0.2))   ;;TO-DO: espicify the object size for each different object   
		 ;;;; Failure handlers
      (cpl:with-retry-counters ((grasp-retries 2))   
        (cpl:with-failure-handling
            (((or common-fail:navigation-high-level-failure
                  common-fail:manipulation-low-level-failure
                  common-fail:object-unreachable
                  common-fail:navigation-low-level-failure
                  common-fail:navigation-goal-in-collision
                CRAM-COMMON-FAILURES:GRIPPER-CLOSED-COMPLETELY
                desig:designator-error) (e)
             (print "I couldn't pick it up yet")
             (roslisp:ros-warn (pp-plans pick-up)
                               "Manipulation messed up: ~a~%Retring..."
                               e)
             (cpl:do-retry grasp-retries
               (cpl:retry))
             (roslisp:ros-warn (pp-plans pick-up) "No more retries left..... going back ")
			;;;; TODO add navigation plan for going back to starting point
             (return-from fetching-object "fail")))
        
        (exe:perform (desig:an action
                               (type picking-up)
                               (object-pose ?pose)
                               (object-size ?object-size)
                               (collision-mode :allow-all)))
        ))) ;;;TODO ADD parking arm
		;;;; check object in hand
  ;;(when (prolog:prolog `(cpoe:object-in-hand ?object :right ?_ ?_))
  ;;            (progn (print "Yes, I grasps the Object")
  ;;                  (setf grasped-object T))
  ;;                  (return-from fetching-object "fetch"))
  (print "fetching plan")
  (return-from fetching-object "fetch")))



;;; TODO replace dummy plans with real plans (check with gpsr-ln what they should do
(defun delivering-object (?object-name ?fur-location ?room ?person-feature)
  (print "delivering-object-plan"))

(defun transporting-object (?object-name ?object-type ?attribute ?source-room
                            ?fur-location1 ?goal-room ?fur-location2
                            ?personname ?persontype ?personaction)
  (print "transporting-object-plan"))

(defun guide-people (?personname ?room1 ?fur-location1)
  (print "guide-people-plan"))

(defun follow-people (?person-name ?person-type ?person-action ?room ?fur-location1)
  (print "follow-people"))

(defun count-object-people (?object-name ?object-type ?attribute ?person-name
                            ?person-type ?person-action ?room1 ?fur-location1)
  (print "coint-object-people-plan"))


(defun describe-something (?object-name ?object-type ?attribute ?person-name ?person-type ?person-action ?room ?fur-location1)
  (print "describe something plan"))

(defun greet-person (?person-name ?person-type ?person-action ?room1 ?fur-location1)
  (print "greet person plan"))
