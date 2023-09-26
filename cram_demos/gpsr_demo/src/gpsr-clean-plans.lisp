(in-package :gpsr-demo)

(defun fetch-plan (?plan-details)
  (roslisp:ros-info (fetch-plan) "Start fetching plan")
  (say "I am sorry, I haven't learned how to  do this yet."))

(defun deliver-plan (?plan-details)
  (roslisp:ros-info (deliver-plan) "Start deliver plan")
  (say "I am sorry, I haven't learned how to  do this yet."))

(defun search-plan (?plan-details)
  (roslisp:ros-info (search-plan) "Start searching plan")
  (say "I am sorry, I haven't learned how to  do this yet."))

;;XXX "go from the kitchen to the living room"
(defun navigate-plan (?plan-details)
  "go to a room, location, or location inside of a room"
  (let* ((?location-key (plan-details-to-location ?plan-details))
         (?room-key (plan-details-to-room ?plan-details)))
    (roslisp:ros-info (navigate-plan) "Start navigating plan")
    ;;choose plan depending on if goal is a room or person or another location
    
    ;;go-to-location-in-room
    (if (and (not (eq :NIL ?room-key)) (not (eq :NIL ?location-key)))
        (go-to-location-in-room ?location-key ?room-key))
    
    ;;go-to-room
    (if (not (eq :NIL ?room-key))
        (go-to-room ?room-key))
    
    ;;go-to-location (furniture)
    (if (not (eq :NIL ?location-key))
        (go-to-location ?location-key))))

(defun transport-plan (?plan-details)
  "bring an object from A to B"
  (roslisp:ros-info (transport-plan) "Start transport plan")
  (say "I am sorry, I haven't learned how to  do this yet."))

(defun guide-plan (?plan-details)
  (roslisp:ros-info (guide-plan) "Start guide plan")
  (say "I am sorry, I haven't learned how to  do this yet."))

(defun count-plan (?plan-details)
  (roslisp:ros-info (count-plan) "Start counting plan")
  (say "I am sorry, I haven't learned how to  do this yet."))

(defun follow-plan (?plan-details)
  (roslisp:ros-info (follow-plan) "Start follow plan")
  (say "I am sorry, I haven't learned how to  do this yet."))

(defun describe-plan (?plan-details)
  (roslisp:ros-info (describe-plan) "Start describe plan")
  (say "I am sorry, I haven't learned how to  do this yet."))

;; rotations
;; w is forwward into arena
(defun greet-plan (?plan-details)
  (roslisp:ros-info (greet-plan) "Start greet plan")
  (let* ((?name (plan-details-person-name ?plan-details))
         (?from-location (plan-details-from-location ?plan-details))
         (?humans))
    ;;parse ?from-location to knowrob
    ;;navigate to ?from-location (room middle)
    ;;look for person -> rotate and send query to perception
    ;;(suturo-demos:call-text-to-speech-action (string ?name))
    (go-to-location ?from-location) ;;includes talking
    ;;look for person
    
    (loop for x in *rotation-poses-list*
          do (setf ?humans (get-all-humans))
             (if ?humans
                 (progn
                   (say "hello " (string (human-uid (car ?humans)))
                        (return)))
                 (progn
                   (say "rotating")
                   (su-demos::move-hsr x))
                 ))))

(defun nlu-fallback-plan (?plan-details)
  (roslisp:ros-info (fallback-plan) "I'm sorry. I have no plan for this task.")
  (say "I am sorry, I haven't learned how to  do this yet."))

