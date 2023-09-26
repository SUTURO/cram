(in-package :gpsr-demo)

;;look for a person in a room
(defun perceive-human ()
  (let* ((?object-desig (desig:an object
                                  (obj-type :everything)))
         
         (?action-desig (desig:an action
                                  (type detecting)
                                  (object ?object-desig))))
    (exe:perform ?action-desig)))


;;(defparameter *robokudo-publisher* (roslisp::advertise "/robokudoalina/query/goal"
;;                                                         "robokudo_msgs/QueryActionGoal"))
(defparameter *human-pose-subscriber* nil)
(defparameter *robokudo-data-subscriber* nil)

(defparameter test-subscriber nil)

(defun init-test-subscriber()
  (roslisp:subscribe "/robokudoalina/query/goal"
                     "robokudo_msgs/QueryActionGoal"
                     #'test-cb))
(defun test-cb(?message)
  (format t "[test-msg:] ~a" ?message))

(defun init-robokudo-human-pose-subscriber()
  (setf *human-pose-subscriber* (roslisp:subscribe "/robokudoalina/human_position"
                                                   "geometry_msgs/PointStamped"
                                                   #'human-pose-cb))
  (roslisp:ros-info (robokudo-human) "Robokudo human pose subscriber created."))

(defun init-robokudo-data-subscriber()
  (setf *robokudo-data-subscriber* (roslisp:subscribe "robokudoalina/query/result"
                                                      "robokudo_msgs/QueryActionResult"
                                                      #'robokudo-data-cb))
  (roslisp:ros-info (robokudo-data) "robokudo-data subscriber created."))

(defun human-pose-cb(?message)
  (format t "human pose value: ~a ~%" ?message))

(defun robokudo-data-cb(?message)
  (format t "robokudo data: ~a ~%" ?message))


;;; the working stuff


(defun make-human-msgs()
  (roslisp:make-message
   'robokudo_msgs-msg:querygoal
   :type "detect"
   :description ""
   :obj (roslisp:make-message
         'robokudo_msgs-msg:objectdesignator
         :uid ""
         :type "human"
         :shape  (map 'vector #'identity "")
         :shape_size  (map 'vector #'identity "")
         :color  (map 'vector #'identity "")
         :location ""
         :size ""
         :pose  (map 'vector #'identity "")
         :pose_source  (map 'vector #'identity "")
         :attribute  (map 'vector #'identity "")
         :description  (map 'vector #'identity ""))))

;;TODO needs more testing
(defun make-rename-human-msgs(?name ?id) ;;as strings
  (roslisp:make-message
   'robokudo_msgs-msg:querygoal
   :type "rename"
   :description ""
   :obj (roslisp:make-message
         'robokudo_msgs-msg:objectdesignator
         :uid ?id
         :type ?name
         :shape  (map 'vector #'identity "")
         :shape_size  (map 'vector #'identity "")
         :color  (map 'vector #'identity "")
         :location ""
         :size ""
         :pose  (map 'vector #'identity "")
         :pose_source  (map 'vector #'identity "")
         :attribute  (map 'vector #'identity "")
         :description  (map 'vector #'identity ""))))

;,works like this test with long bag from sorin
(defun make-everything-msgs()
  (roslisp:make-message
   'robokudo_msgs-msg:querygoal
   :type ""
   :description ""
   :obj (roslisp:make-message
         'robokudo_msgs-msg:objectdesignator
         :uid ""
         :type "Everything"
         :shape  (map 'vector #'identity "")
         :shape_size  (map 'vector #'identity "")
         :color  (map 'vector #'identity "")
         :location ""
         :size ""
         :pose  (map 'vector #'identity "")
         :pose_source  (map 'vector #'identity "")
         :attribute  (map 'vector #'identity "")
         :description  (map 'vector #'identity ""))))

(defun make-obj-msgs(?obj-type-as-string)
  (roslisp:make-message
   'robokudo_msgs-msg:querygoal
   :type ""
   :description ""
   :obj (roslisp:make-message
         'robokudo_msgs-msg:objectdesignator
         :uid ""
         :type ?obj-type-as-string
         :shape  (map 'vector #'identity "")
         :shape_size  (map 'vector #'identity "")
         :color  (map 'vector #'identity "")
         :location ""
         :size ""
         :pose  (map 'vector #'identity "")
         :pose_source  (map 'vector #'identity "")
         :attribute  (map 'vector #'identity "")
         :description  (map 'vector #'identity ""))))

;;TODO needs special treatment
(defun make-track-human-msgs()
  (roslisp:make-message
   'robokudo_msgs-msg:querygoal
   :type "track"
   :description ""
   :obj (roslisp:make-message
         'robokudo_msgs-msg:objectdesignator
         :uid ""
         :type "human"
         :shape  (map 'vector #'identity "")
         :shape_size  (map 'vector #'identity "")
         :color  (map 'vector #'identity "")
         :location ""
         :size ""
         :pose  (map 'vector #'identity "")
         :pose_source  (map 'vector #'identity "")
         :attribute  (map 'vector #'identity "")
         :description  (map 'vector #'identity ""))))

;;; DEBUGGING
;; (su-real::with-hsr-process-modules (perceive-human))
(defparameter *robokudo-publisher* nil)


(defun make-robokudo-publisher()
  (setf *robokudo-publisher* (roslisp::advertise "/robokudo/query"
                                                 "robokudo_msgs/QueryGoal")))
;;this works!!! --------------------------------------

(defparameter *ros-action* "robokudo/query")

(defun make-robokudo-action-client ()
  (actionlib-client:make-simple-action-client
   'robokudo-action
   *ros-action* "robokudo_msgs/QueryAction"
   120))

;;simplified-version
(defun call-simple-robokudo-human-action(?robokudo-msgs)
  (actionlib-client:call-simple-action-client
   'robokudo-action
   :action-goal ?robokudo-msgs))

;;setup perception: make node, make publisher

;;gives the reulst as array
;;DONE convert array to a struct of type human
(defun get-human-result (?rk-result)
  ;;process msgs by extracting the important info so that it can be used in plans
  ;;unless specified otherwise expect the values to contain strings
  (let* (;(?rk-result (aref (roslisp:msg-slot-value ?result :RES) 0))
         ;;the list under RES
         (?uid (roslisp:msg-slot-value ?rk-result :uid))
         (?type (roslisp:msg-slot-value ?rk-result :type))
         (?shape (roslisp:msg-slot-value ?rk-result :shape))
         (?shape-size (roslisp:msg-slot-value ?rk-result :shape_size))
         (?color (roslisp:msg-slot-value ?rk-result :color))
         (?location (roslisp:msg-slot-value ?rk-result :location))
         (?size (roslisp:msg-slot-value ?rk-result :size))
         (?pose (cl-tf:from-msg (aref (roslisp:msg-slot-value ?rk-result :pose) 0)))
         (?pose-source (roslisp:msg-slot-value ?rk-result :pose_source))
         (?attribute (roslisp:msg-slot-value ?rk-result :attribute)) ;;is a vector
         (?in-description (roslisp:msg-slot-value ?rk-result :description)) ;;in list aka. second
         (?human nil))

    (setf ?human (make-human :uid ?uid
                             :type ?type
                             :shape ?shape
                             :shape-size ?shape-size
                             :color ?color
                             :location ?location
                             :size ?size
                             :pose ?pose
                             :pose-source ?pose-source
                             :attribute ?attribute
                             :in-description ?in-description))
    ;;do stuff / make output
    (format t "?human: ~a " ?human)
    ?human))
(defun get-object-result (?rk-result)
  ;;process msgs by extracting the important info so that it can be used in plans
  ;;unless specified otherwise expect the values to contain strings
  (let* (;(?rk-result (aref (roslisp:msg-slot-value ?result :RES) 0))
         ;;the list under RES
         (?uid (roslisp:msg-slot-value ?rk-result :uid))
         (?type (roslisp:msg-slot-value ?rk-result :type))
         (?shape (roslisp:msg-slot-value ?rk-result :shape))
         (?shape-size (roslisp:msg-slot-value ?rk-result :shape_size))
         (?color (roslisp:msg-slot-value ?rk-result :color))
         (?location (roslisp:msg-slot-value ?rk-result :location))
         (?size (roslisp:msg-slot-value ?rk-result :size))
         (?pose (cl-tf:from-msg (aref (roslisp:msg-slot-value ?rk-result :pose) 0)))
         (?pose-source (roslisp:msg-slot-value ?rk-result :pose_source))
         (?attribute (roslisp:msg-slot-value ?rk-result :attribute)) ;;is a vector
         (?in-description (roslisp:msg-slot-value ?rk-result :description)) ;;in list aka. second
         (?object nil))

    (setf ?object (make-object :uid ?uid
                              :type ?type
                              :shape ?shape
                              :shape-size ?shape-size
                              :color ?color
                              :location ?location
                              :size ?size
                              :pose ?pose
                              :pose-source ?pose-source
                              :attribute ?attribute
                              :in-description ?in-description))
    ;;do stuff / make output
    (format t "?object: ~a " ?object)
    ?object))

(defun get-all-humans () ;;works with one person TODO test with more bags
  (let* ((?perception-result (call-simple-robokudo-human-action (make-human-msgs)))
         (?all-humans (roslisp:msg-slot-value ?perception-result :RES)) ;;list of pre-procesed humans
         (?humans-amount (length ?all-humans)))
    ;;iterate through all humans, and put them into the human struct
    (map 'list #'get-human-result ?all-humans);;process all humans
    ))

(defun get-all-objects () ;;returns list of structs of objects
  (let* ((?perception-result (call-simple-robokudo-human-action (make-everything-msgs)))
         (?all-objects (roslisp:msg-slot-value ?perception-result :RES)) ;;list of pre-procesed humans
         (?objects-amount (length ?all-objects)))
    ;;iterate through all humans, and put them into the human struct
    (map 'list #'get-object-result ?all-objects);;process all humans
    ))

;;TODO potentially do the same for the humans to get standing human etc.
(defun get-one-object (?obj-type-key) ;;works with one person TODO test with more bags
  "given a keyword returns object which was perceived"
  (let* ((?obj-type (prologify ?obj-type-key)) ;;convert key to string
         (?perception-result (call-simple-robokudo-human-action (make-obj-msgs ?obj-type)))
         (?all-objects (roslisp:msg-slot-value ?perception-result :RES)) ;;list of pre-procesed humans
         (?objects-amount (length ?all-objects))
         (?all-obj-struct-list (map 'list #'get-object-result ?all-objects)))
    ;;match to find the object you are looking for TODO could be prettier
    (format t "looking for: ~a " ?obj-type)
    (loop for ?object in ?all-obj-struct-list
            do (if (string-equal (object-type ?object) ?obj-type)
                   (return ?object)

                   ))))


