(in-package :gpsr-demo)

;; contains all the plans related to navigation


;;go to the kitchen/living-room
(defun go-to-room (?room-keyword)
  "navigates the robot to the given room.
   Room-name is given as a keyword.
   Navigation-location is coming from knowrob"
  ;;check knowledge for room-iri
  (let* ((?knowledge-room-iri (check-if-room-exists ?room-keyword)))
    
    (say (concatenate 'string "I will try to go to the " (string ?room-keyword) ". Please stay clear."))
    (if ?knowledge-room-iri
        (su-demos::move-hsr (pose-of-knowrob-object ?knowledge-room-iri))
        (say (concatenate 'string "I'm sorry I don't know where " (string ?room-keyword) "is.")))))

;;go to the cupboard/(table)
(defun go-to-location (?location-keyword)
  "navigates the robot to the given location.
   location is given as a keyword.
   Navigation-location is coming from knowrob"
  ;;check knowledge for room-iri
  (let* ((?knowledge-obj-iri (obj-of-type ?location-keyword)))
    
    (say (concatenate 'string "I will try to go to the " (string ?location-keyword) ". Please stay clear."))
    (if ?knowledge-obj-iri
        (su-demos::move-hsr (pose-for-perceiving-object ?knowledge-obj-iri))
        (say (concatenate 'string "I'm sorry I don't know where " (string ?location-keyword) "is.")))))

(defun go-to-location-in-room(?location-keyword ?room-keyword)
  (let* ((?knowledge-location-iri (obj-of-type ?location-keyword))
         (?pose-obj-in-room (get-obj-in-room ?location-keyword ?room-keyword)))
    
    (say (concatenate 'string "I will try to go to the " (string ?location-keyword) ". Please stay clear."))
    (if ?pose-obj-in-room
        (su-demos::move-hsr ?pose-obj-in-room)
        (say (concatenate 'string "I'm sorry I don't know where " (string ?location-keyword) "is.")))))
  
;;go-to-person
(defun go-to-person-in-room(?location-keyword ?room-keyword ?person-name))
