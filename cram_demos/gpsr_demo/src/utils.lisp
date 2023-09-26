(in-package :gpsr-demo)

;;(defun parse-msgs)

(defparameter *silent* nil)

;;plan-details contains the information we obtain from NLP in a more conveniant format
(defstruct (plan-details
            (:constructor create-plan-details
                (plan object-name object-type person-name person-type
                 object-attribute ;; smth like right-most object
                 person-action color number from-location to-location
                 from-room to-room)))
  plan
  object-name
  object-type
  person-name
  person-type
  object-attribute ;; smth like right-most object
  person-action
  color
  number
  from-location
  to-location
  from-room
  to-room)


(defun plan-details-string-list (?plan-details-struct)
  (concatenate 'string "~% plan: " (string (plan-details-plan ?plan-details-struct))
               "~% object-name: " (string (plan-details-object-name ?plan-details-struct))
               "~% object-type: " (string (plan-details-object-type ?plan-details-struct))
               "~% person-name: " (string (plan-details-person-name ?plan-details-struct))
               "~% person-type: " (string (plan-details-person-type ?plan-details-struct))
               "~% object-attribute: " (string (plan-details-object-attribute ?plan-details-struct))
               "~% person-action: " (string (plan-details-person-action ?plan-details-struct))
               "~% color: " (string (plan-details-color ?plan-details-struct))
               "~% number:" (string (plan-details-number ?plan-details-struct))
               "~% from-location: " (string (plan-details-from-location ?plan-details-struct))
               "~% to-location: "(string (plan-details-to-location ?plan-details-struct))
               "~% from-room: "(string (plan-details-from-room ?plan-details-struct))
               "~% to-room: " (string (plan-details-to-room ?plan-details-struct))))
        
;;; lazyness functions (don't judge me)
(defun say (?string &optional (?silent *silent*))
  (if ?silent
      (format t "say: ~a" ?string)
      (su-demos::call-text-to-speech-action ?string)))


(defun prologify (?keyword)
  "re-format the given keyword into a prolog-friendly string. e.g. CamelCase.
:CUPBOARD-TABLE -> CupboardTable"
  (remove #\- (string-capitalize (string ?keyword))))

;; ----------------perception result strunct ----------------------------------
(defstruct human uid
  type
  shape
  shape-size
  color
  location
  size
  pose
  pose-source
  attribute
  in-description)

(defstruct object uid
  type
  shape
  shape-size
  color
  location
  size
  pose
  pose-source
  attribute
  in-description)



;;debugging info

(defparameter *plan-details-example* (create-plan-details
                                      :NAVIGATE :NIL :NIL :ALEX :PERSON
                                                :LEFT :NIL :GREEN :NIL :NIL :CUPBOARD
                                                :NIL :NIL))

(defvar *perc-msgs '((:type :detect)
                     (:description nil)
                     (:obj (:uid :test-user)
                      (:type :human)
                      (:shape "boxshape")
                      (:shape_size nil)
                      (:color :green)
                      (:location nil)
                      (:size nil)
                      (:pose nil)
                      (:pose_source nil)
                      (:attribute nil)
                      (:description nil))))

;;; rotation poses
(defparameter *face-forward* (cl-tf:make-pose-stamped "base_footprint" 0.0
                                                      (cl-tf:make-identity-vector)
                                                      (cl-tf:make-quaternion 1.0 0.0 0.0 0.0)))

(defparameter *face-backwards* (cl-tf:make-pose-stamped "base_footprint" 0.0
                                                      (cl-tf:make-identity-vector)
                                                      (cl-tf:make-quaternion 0.0 1.0 0.0 0.0)))

(defparameter *face-left* (cl-tf:make-pose-stamped "base_footprint" 0.0
                                                      (cl-tf:make-identity-vector)
                                                      (cl-tf:make-quaternion 0.0 0.0 0.7071 0.7071)))

(defparameter *face-right* (cl-tf:make-pose-stamped "base_footprint" 0.0
                                                      (cl-tf:make-identity-vector)
                                                      (cl-tf:make-quaternion 0.0 0.0 -0.7017 0.7071)))

(defparameter *rotation-poses-list* (list *face-forward* *face-right* *face-backwards* *face-left*))


