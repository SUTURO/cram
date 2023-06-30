(in-package :su-demos)


;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;time
(defmacro measure-time (&body body)
  `(let ((start (get-universal-time)))
     ,@body
     (- (get-universal-time) start)))
;;;;;;;;;;;;;;;;

;;@author Felix Krause
(defun start-ros () (roslisp-utilities:startup-ros))

;;@author Felix Krause
(defun transform-key-to-string (the-key)
  
  (case the-key
    (:trashbin
      "http://www.ease-crc.org/ont/SUTURO.owl#TrashBin")
    (:plasticknife
      "http://www.ease-crc.org/ont/SUTURO.owl#PlasticKnife")
    (:designedchair
      "http://www.ease-crc.org/ont/SOMA-HOME.owl#DesignedChair")
    (:cupboard
     "http://www.ease-crc.org/ont/SOMA.owl#Cupboard")
    (:foambrick
     "http://www.ease-crc.org/ont/SUTURO.owl#FoamBrick")
    (:fork
     "http://www.ease-crc.org/ont/SOMA.owl#Fork")
    (:spatula
     "http://www.ease-crc.org/ont/SOMA.owl#Spatula")
    (:pitcher
     "http://www.ease-crc.org/ont/SUTURO.owl#Pitcher")
    (:bleachcleanserbottle
     "http://www.ease-crc.org/ont/SUTURO.owl#BleachCleanserBottle")
    (:kitchencabinet
     "http://www.ease-crc.org/ont/SOMA-HOME.owl#KitchenCabinet")
    (:crackerbox
     "http://www.ease-crc.org/ont/SUTURO.owl#CrackerBox")
    (:minisoccerball
     "http://www.ease-crc.org/ont/SUTURO.owl#MiniSoccerBall")
    (:baseball
     "http://www.ease-crc.org/ont/SUTURO.owl#Baseball")
    (:tablecloth
     "http://www.ease-crc.org/ont/SUTURO.owl#TableCloth")
    (:washer
     "http://www.ease-crc.org/ont/SUTURO.owl#Washer")
    (:designedhandle
     "http://www.ease-crc.org/ont/SOMA.owl#DesignedHandle")
    (:pot
     "http://www.ease-crc.org/ont/SOMA.owl#Pot")
    (:napkin
     "http://www.ease-crc.org/ont/SUTURO.owl#Napkin")
    (:mustardbottle
     "http://www.ease-crc.org/ont/SUTURO.owl#MustardBottle")
    (:puddingbox
     "http://www.ease-crc.org/ont/SUTURO.owl#PuddingBox")
    (:door
     "http://www.ease-crc.org/ont/SOMA.owl#Door")
    (:servingmat
     "http://www.ease-crc.org/ont/SUTURO.owl#ServingMat")
    (:dishwasher
     "http://www.ease-crc.org/ont/SOMA.owl#Dishwasher")
    (:naturalperson
     "http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#NaturalPerson")
    (:wineglass
     "http://www.ease-crc.org/ont/SOMA.owl#WineGlass")
    (:shelf
     "http://www.ease-crc.org/ont/SUTURO.owl#Shelf")
    (:tray
     "http://www.ease-crc.org/ont/SUTURO.owl#Tray")
    (:orange
     "http://www.ease-crc.org/ont/SUTURO.owl#Orange")
    (:table
     "http://www.ease-crc.org/ont/SOMA.owl#Table")
    (:bowl
     "http://www.ease-crc.org/ont/SOMA.owl#Bowl")
    (:glass
     "http://www.ease-crc.org/ont/SOMA.owl#Glass")
    (:coffeepack
     "http://www.ease-crc.org/ont/SUTURO.owl#CoffeePack")
    (:golfball
     "http://www.ease-crc.org/ont/SUTURO.owl#GolfBall")
    (:softball
     "http://www.ease-crc.org/ont/SUTURO.owl#SoftBall")
    (:saltshaker
     "http://www.ease-crc.org/ont/SOMA.owl#SaltShaker")
    (:metalplate
     "http://www.ease-crc.org/ont/SUTURO.owl#MetalPlate")
    (:pringleschipscan
     "http://www.ease-crc.org/ont/SUTURO.owl#PringlesChipsCan")
    (:strawberry
     "http://www.ease-crc.org/ont/SUTURO.owl#Strawberry")
    (:glasscleanerspraybottle
     "http://www.ease-crc.org/ont/SUTURO.owl#GlassCleanerSprayBottle")
    (:tennisball
     "http://www.ease-crc.org/ont/SUTURO.owl#TennisBall")
    (:spoon
     "http://www.ease-crc.org/ont/SOMA.owl#Spoon")
    (:marble
     "http://www.ease-crc.org/ont/SUTURO.owl#Marble")
    (:bottle
     "http://www.ease-crc.org/ont/SOMA.owl#Bottle")
    (:metalmug
     "http://www.ease-crc.org/ont/SUTURO.owl#MetalMug")
    (:rope
     "http://www.ease-crc.org/ont/SUTURO.owl#Rope")
    (:stackingblocks
     "http://www.ease-crc.org/ont/SUTURO.owl#StackingBlocks")
    (:abrasivesponge
     "http://www.ease-crc.org/ont/SUTURO.owl#AbrasiveSponge")
    (:racquetball
     "http://www.ease-crc.org/ont/SUTURO.owl#Racquetball")
    (:jellobox
     "http://www.ease-crc.org/ont/SUTURO.owl#JelloBox")
    (:dishwashertab
     "http://www.ease-crc.org/ont/SOMA.owl#DishwasherTab")
    (:knife
     "http://www.ease-crc.org/ont/SOMA.owl#Knife")
    (:plasticchain
     "http://www.ease-crc.org/ont/SUTURO.owl#PlasticChain")
    (:plate
     "http://www.ease-crc.org/ont/SOMA.owl#Plate")
    (:cup
     "http://www.ease-crc.org/ont/SOMA.owl#Cup")
    (:dice
     "http://www.ease-crc.org/ont/SUTURO.owl#Dice")
    (:cerealbox
     "http://www.ease-crc.org/ont/SOMA.owl#CerealBox")
    (:metalbowl
     "http://www.ease-crc.org/ont/SUTURO.owl#MetalBowl")
    (:sugarbox
     "http://www.ease-crc.org/ont/SUTURO.owl#SugarBox")
    (:skillet
     "http://www.ease-crc.org/ont/SUTURO.owl#Skillet")
    (:kiwi
     "http://www.ease-crc.org/ont/SUTURO.owl#Kiwi")
    (:drawer
     "http://www.ease-crc.org/ont/SOMA.owl#Drawer")
    (:toyotahsr
     "http://www.ease-crc.org/ont/SUTURO.owl#ToyotaHSR")
    (:coffeecan
     "http://www.ease-crc.org/ont/SUTURO.owl#CoffeeCan")
    (:milkpack
     "http://www.ease-crc.org/ont/SOMA.owl#MilkPack")
    (:toothpaste
     "http://www.ease-crc.org/ont/SUTURO.owl#Toothpaste")
    (:oreganoshaker
     "http://www.ease-crc.org/ont/SUTURO.owl#OreganoShaker")
    (:apple
     "http://www.ease-crc.org/ont/SUTURO.owl#Apple")
    (:tomatosoupcan
     "http://www.ease-crc.org/ont/SUTURO.owl#TomatoSoupCan")
    (:tunafishcan
     "http://www.ease-crc.org/ont/SUTURO.owl#TunaFishCan")
    (:bag
     "http://www.ease-crc.org/ont/SUTURO.owl#Bag")
    (:juicebox
     "http://www.ease-crc.org/ont/SUTURO.owl#JuiceBox")
    (:pancakemixbottle
     "http://www.ease-crc.org/ont/SUTURO.owl#PancakeMixBottle")
    (:jellostrawberrybox
     "http://www.ease-crc.org/ont/SUTURO.owl#JellOStrawberryBox")
    (:teabagbox
     "http://www.ease-crc.org/ont/SUTURO.owl#TeaBagBox")
    (:pastrybox
     "http://www.ease-crc.org/ont/SUTURO.owl#PastryBox")
    (:pear
     "http://www.ease-crc.org/ont/SUTURO.owl#Pear")
    (:lemon
     "http://www.ease-crc.org/ont/SUTURO.owl#Lemon")
    (:banana
     "http://www.ease-crc.org/ont/SUTURO.owl#Banana")
    (:pottedmeatcan
     "http://www.ease-crc.org/ont/SUTURO.owl#PottedMeatCan")
    (:peach
     "http://www.ease-crc.org/ont/SUTURO.owl#Peach")
    (:plum
     "http://www.ease-crc.org/ont/SUTURO.owl#Plum")))




;;OLD KEYS

;; (:KoellnMuesliKnusperHonigNuss
;;                :breakfast-cereal)
;;               (:muesli
;;                :breakfast-cereal)
;;               (:cerealbox
;;                :cerealbox)
;;               (:milk
;;                :milk)
;;               (:TunaFishCan
;;                :TunaFishCan)
;;               (:everything
;;                :everything)
;;               (:CupEcoOrange
;;                :cup)
;;               (:EdekaRedBowl
;;                :bowl)
;;               (:IkeaRedBowl
;;                :bowl)
;;               (:SoupSpoon
;;                :spoon)
;;               (:apple
;;                :apple)
;;               (:spoon
;;                :spoon)
;;               (:IkeaRedCup
;;                :cup)
;;               (:bowl
;;                :bowl)
;;               (:mustard-bottle
;;                :mustard-bottle)
;;               (:cup
;;                :cup)
;;               (:mug
;;                :cup)
;;               (:WeideMilchSmall
;;                :milk)
;;               (:BLUEPLASTICSPOON
;;                :spoon)
;;               (:BALEAREINIGUNGSMILCHVITAL
;;                :balea-bottle)
;;               (:DENKMITGESCHIRRREINIGERNATURE
;;                :dish-washer-tabs)
;;               (:GarnierMineralUltraDry
;;                :deodorant)
;;               (:DMRoteBeteSaftBio
;;                :juice-box)
;;               (:JeroenCup
;;                :jeroen-cup)
;;               (:jeroen-cup
;;                :jeroen-cup)
;;               (:pringles
;;                :pringles))))
      





;;====================================================================================================
;;navigation

;;used in go-get-it
;;@author Torge Olliges    
(defun move-hsr (nav-goal-pose-stamped &optional (talk t))
  "Receives pose `nav-goal-pose-stamped'. Lets the robot move to the given pose with a motion designator"
  (talk-request "Take care, I will now move!" talk)
  (let* ((?successfull-pose (try-movement-stamped-list
                             (list nav-goal-pose-stamped))))
    (exe:perform 
     (desig:a motion
              (type going)
              (pose ?successfull-pose)))))

;;====================================================================================================
;;knowledge

;; (with-knowledge-result (a b) '(= (list 1 2) (list a b))
;;             (print a)
;;             (print b))
;;@author Tede von Knorre, Felix Krause
(defmacro with-knowledge-result (vars query &body body)
  `(let ((raw-response (simple-knowledge ,query (find-package :common-lisp-user))))
     (if (eq raw-response 1)
         (error "Knowledge query failed at the macro with-knowledge-result")
         (unwind-protect
              (let ,(loop for x in vars
                          collect `(,x (fix-prolog-string (cdr (assoc ',(match-prolog-symbol x (find-package :common-lisp-user)) (car raw-response))))))
                ,@body
                )
           (json-prolog:finish-query raw-response)))))



;;@author Tede von Knorre, Felix Krause
(defun simple-knowledge (query &optional (package *package*))
  (print package)
  (with-safe-prolog
    (json-prolog:prolog-1 query :package package)))

;;@author Felix Krause
(defun make-pose-stamped-from-knowledge-result (result)
  (cl-tf:make-pose-stamped
   (first result) 0.0
   (cl-tf:make-3d-vector
    (first (second result)) (second (second result)) (third (second result)))
   (cl-tf:make-quaternion (first (third result)) (second (third result)) (third (third result)) (fourth (third result))))
  )

;;@author Felix Krause
(defun make-pose-stamped-from-knowledge-result-for-smallies (result)
  (cl-tf:make-pose-stamped
   (first result) 0.0
   (cl-tf:make-3d-vector
    (first (second result)) (second (second result)) 0.7125000 )
   (cl-tf:make-quaternion (first (third result)) (second (third result)) (third (third result)) (fourth (third result))))
  )

;;@author Felix Krause
(defun make-pose-stamped-from-knowledge-result-for-smallies-breakfast (result)
  (let ((height (cond
                  ((> (third (second result)) 1.12000) 1.13500)
                  ((> (third (second result)) 0.82000) 0.83500)
                  ((> (third (second result)) 0.52000) 0.53500)
                  ((> (third (second result)) 0.06000) 0.07500))))
  (cl-tf:make-pose-stamped
   (first result) 0.0
   (cl-tf:make-3d-vector
    (first (second result)) (second (second result)) height)
   (cl-tf:make-quaternion (first (third result)) (second (third result)) (third (third result)) (fourth (third result))))))


;;@author Felix Krause
(defun make-pose-stamped-from-knowledge-result-for-bowl (result)
  (cl-tf:make-pose-stamped
   (first result) 0.0
   (cl-tf:make-3d-vector
    (+ (first (second result)) 0.00) (second (second result)) (- (third (second result)) 0.02))
   (cl-tf:make-identity-rotation))
  )

;;@author Felix Krause
(defun make-pose-stamped-from-knowledge-result-for-mug (result)
  (cl-tf:make-pose-stamped
   (first result) 0.0
   (cl-tf:make-3d-vector
    (- (first (second result)) 0.0) (+ (second (second result)) 0.02) 0.76000)
   (cl-tf:make-identity-rotation))
  )

;;@author Felix Krause
(defun make-pose-stamped-from-knowledge-result-for-bowl-breakfast (result)
  (cl-tf:make-pose-stamped
   (first result) 0.0
   (cl-tf:make-3d-vector
    (+ (first (second result)) 0.00) (+ (second (second result)) 0.04) (- (third (second result)) 0.01))
   (cl-tf:make-identity-rotation)))


;; TODO add list of possible querys
;; @author Luca Krohm
(defun call-knowledge (query &key param-list result)
  "Receives query `query', list of strings `param-list' and symbol `result' . Returns the query result for the chosen parameters. Used for querys that receive the 'result' as last parameter in prolog."
  (roslisp:ros-info (knowledge-object-client)
                    "Calling query ~a."
                    query)
  (let* ((raw-response (with-safe-prolog
                         (json-prolog:prolog
                          (alexandria:flatten (list query param-list result))))))
    ;; (roslisp:ros-info (knowledge-object-client)
    ;;                   "Goal pose for object ~a is ~a."
    ;;                   object-name
    ;;                   raw-response)
    (cond
      ((eq raw-response 1) (roslisp:ros-warn (knowledge-object-client)
                                             "Query ~a didn't reach any solution."
                                             query))
      (t (fix-prolog-string (cdr (assoc (match-prolog-symbol result) (cut:lazy-car raw-response))))))))

;; @author Luca Krohm
(defun call-knowledge2 (query &key param-list result)
  "Receives query `query', list of strings `param-list' and symbol `result' . Returns the query result for the chosen parameters. Used for querys that receive the 'result' as first parameter in prolog."
  (roslisp:ros-info (knowledge-object-client)
                    "Calling query ~a."
                    query)
  (let* ((raw-response (with-safe-prolog
                         (json-prolog:prolog
                          (alexandria:flatten (list query result param-list))))))
    (roslisp:ros-info (knowledge-object-client)
                      "Raw Response for object is ~a."
                      ;;object-name
                      raw-response)
    (cond
      ((eq raw-response 1) (roslisp:ros-warn (knowledge-object-client)
                                             "Query ~a didn't reach any solution."
                                             query))
      (t (fix-prolog-string (cdr (assoc (match-prolog-symbol result) (cut:lazy-car raw-response))))))))


(defun perceive-handle-closed ()
  "Test function that returns working pose for closed drawer handle for testing purposes. Pose was previously perceived by Perception."
   (cl-tf2::make-pose-stamped
    "map" 0
    (cl-tf2::make-3d-vector 0.18888034742219937d0 -0.30079838556398194d0 0.2854991327720375d0)
    (cl-tf2::make-quaternion 0.0 0.0 0.0 1.0)))

(defun perceive-handle-opened ()
   "Test function that returns working pose for opened drawer handle for testing purposes. Pose was calculated using a pose previously perceived by Perception."
   (cl-tf2::make-pose-stamped
    "map" 0
    (cl-tf2::make-3d-vector 0.18888034742219937d0 0.10079838556398194d0 0.2854991327720375d0)
    (cl-tf2::make-quaternion 0.0 0.0 0.0 1.0)))


;;;;;;;;;;;;;;;;;;
;; Fake-Designator

(defgeneric exe-perform-type (action-type &key &allow-other-keys))

(defmethod exe-perform-type ((action-type (eql :picking-up)) &key (arm :left) object)
  (let*((gripper-pose
            (cl-tf::lookup-transform cram-tf:*transformer* "map" "/hand_gripper_tool_frame"))
          (obj-name (desig:desig-prop-value object :name)))
      (roslisp:with-fields (translation rotation)
          gripper-pose
        (btr-utils:move-object
         obj-name
         (cl-tf:make-pose-stamped
          "map" 0.0
          translation
          (cl-tf:make-quaternion 0 0 0 1))))
      (cram-occasions-events:on-event
       (make-instance 'cpoe:object-attached-robot
                      :arm arm
                      :object-name obj-name
                      :object-designator object
                      :grasp :front))))

(defmethod exe-perform-type ((action-type (eql :placing)) &key (arm :left) pose)
  (let*((obj-name (check-arm-for-object)))
    (btr-utils:move-object
     obj-name
     pose)
    (cram-occasions-events:on-event
     (make-instance 'cpoe:object-detached-robot
                    :arm (list arm)
                    :object-name obj-name))))

(defmethod exe-perform-type ((action-type (eql :opening)) &key object (distance 1.7))
    (cond
      ((search "right" object)  (let ((?object-pose (knowledge-get-right-handle-pose)))
                                  (exe:perform
                                   (desig:an action
                                             (type looking)
                                             (target (desig:a location (pose ?object-pose)))))
                                  (sleep 1)
                                  (let ((dist (* distance 1)))
                                    (btr:set-robot-state-from-joints
                                     `((,object ,dist))
                                     (btr:get-environment-object)))))
       
      ((search "left" object)  (let ((?object-pose (knowledge-get-left-handle-pose)))
                                 (exe:perform
                                  (desig:an action
                                            (type looking)
                                            (target (desig:a location (pose ?object-pose)))))
                                 (sleep 1)
                                 (let ((dist (* distance -1)))
                                   (btr:set-robot-state-from-joints
                                    `((,object ,dist))
                                    (btr:get-environment-object)))))
      
      ((search "origin" object)  (let ((?object-pose (knowledge-get-entrance-handle-pose)))
                                  (exe:perform
                                   (desig:an action
                                             (type looking)
                                             (target (desig:a location (pose ?object-pose)))))
                                   (sleep 1)
                                   (let ((dist (if (> distance 0)
                                                    0
                                                    distance)))
                                     (btr:set-robot-state-from-joints
                                      `((,object ,dist))
                                      (btr:get-environment-object)))))
      (t (error "Please enter a valid openable object"))))

(defmethod exe-perform-type ((action-type (eql :closing)) &key object)
  (cond
      ((search "right" object)  (let ((?object-pose (knowledge-get-right-handle-pose)))
                                  (exe:perform
                                   (desig:an action
                                             (type looking)
                                             (target (desig:a location (pose ?object-pose)))))
                                  (sleep 1)
                                  (btr:set-robot-state-from-joints
                                   `((,object 0))
                                   (btr:get-environment-object))))
       
      ((search "left" object)  (let ((?object-pose (knowledge-get-left-handle-pose)))
                                 (exe:perform
                                  (desig:an action
                                            (type looking)
                                            (target (desig:a location (pose ?object-pose)))))
                                 (sleep 1)
                                 (btr:set-robot-state-from-joints
                                  `((,object 0))
                                  (btr:get-environment-object))))
      
      ((search "origin" object)  (let ((?object-pose (knowledge-get-entrance-handle-pose)))
                                   (exe:perform
                                    (desig:an action
                                              (type looking)
                                              (target (desig:a location (pose ?object-pose)))))
                                   (sleep 1)
                                     (btr:set-robot-state-from-joints
                                      `((,object -1.4))
                                      (btr:get-environment-object))))
      (t (error "Please enter a valid openable object"))))

(defmethod exe-perform-type ((action-type (eql :reaching)) &key (arm :left) ?object)
        (exe:perform
         (desig:an action
                   (type reaching)
                   (object ?object)
                   )))

;;;;;;;;;;;;;;
;; BTR-Utility

;; use to quickly test navigation poses 
(defun move-hsr-debug (?pose)
  (urdf-proj:with-simulated-robot
    (exe:perform
     (desig:an action
               (type going)
               (target (desig:a location (pose ?pose)))))))

;; example for how to spawn objects
(defun spawn-pringles()
  (btr:add-object btr:*current-bullet-world* :cylinder-item 'cylinder-1
                    '((1.5 -1.5 0.78) (0 0 0 1))
                    :mass 0.2 :size (cl-transforms:make-3d-vector 0.03 0.03 0.08)
                    :item-type :pringles))

(defun move-obj (obj-name pose)
  (btr-utils:move-object
   obj-name
   pose))

(defun check-arm-for-object ()
  (let ((link
        (cut:var-value
          '?link
          (car
           (prolog:prolog
            `(and (rob-int:robot ?rob)
                  (rob-int:end-effector-link ?rob :left ?link)))))))
    (car
     (btr:link-attached-object-names
     (btr:get-robot-object)
     link))))

;;;;;;;;;;;;
;; Knowledge

;; if you are using a different map, you might need to adjust the strings to reference the proper link

(defun knowledge-get-left-handle-pose()
  (cl-tf:pose->pose-stamped
   cram-tf:*fixed-frame*
   0 
   (btr:link-pose (btr:get-environment-object) "shelf:shelf:shelf_door_left:shelf_link_handle")))

(defun knowledge-get-right-handle-pose()
  (cl-tf:pose->pose-stamped
   cram-tf:*fixed-frame*
   0 
   (btr:link-pose (btr:get-environment-object) "shelf:shelf:shelf_door_right:shelf_link_handle")))

(defun knowledge-get-entrance-handle-pose()
  (cl-tf:pose->pose-stamped
   cram-tf:*fixed-frame*
   0 
   (btr:link-pose (btr:get-environment-object) "iai_kitchen:living_room:door_handle_inside")))


;; those are the joints that can be assigned different values to open/close objects, for example doors
(defun knowledge-get-shelf-left()
  "shelf:shelf:shelf_door_left:shelf_joint")

(defun knowledge-get-shelf-right()
  "shelf:shelf:shelf_door_right:shelf_joint")

(defun knowledge-get-entrance()
  "iai_kitchen:living_room:door_origin_revolute_joint")

(defun knowledge-get-looking-pose(obj-name)
  (cl-tf:pose->pose-stamped
                   cram-tf:*fixed-frame*
                   0 
                   (btr:object-pose obj-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun round-for-knowrob (v &optional (n 10))
  (declare (type float v)
           (type (integer 0) n))
  (setf v (coerce v 'single-float))
  (let ((10^-n (expt 10 (- n))))
     (* (fround v 10^-n)
               10^-n)))


;;;;;;;;;;;;
;; VANESSA ---------------------------------------------------------------------



(defun human-assist (talk)
  ;little bit different, talk, then move you arm, then open gripper otherwise she hits herself sometimes
  (talk-request "I will need some help from the human,i will now move my arm, please be care: " talk)
  ;;this can also be used for bowl
  (cpl:seq
    (wait-robot)
    (exe:perform (desig:a motion
                          (type gripper-motion)
                          (:open-close :open)
                          (effort 0.1))))
  ;;todo what if we dont find the plate?
  (talk-request "Please give me the Plate,
When you are ready poke the white part of my hand." talk)
  
  ;;waiting for human
    (exe:perform
     (desig:an action
               (type monitoring-joint-state)
               (joint-name "wrist_flex_joint")))
  
   (talk-request "Closing the Gripper, thank you." talk)
  ;;closing gripper
  (exe:perform (desig:a motion
                        (type gripper-motion)
                        (:open-close :close)
                        (effort 0.1))))



(defun talk-request (talk-string talk &key (current-knowledge-object nil))
  "Use this function as follow (talk-string 'I will now pick up ' t/n :current-knowledge-object 'http...#bowl_34123')"
  ;;just added a when around it so we can decide if toya should be silent
 (let* ((talkery talk-string))
  (when current-knowledge-object
    (setf talkery (concatenate 'string talk-string  (trim-knowledge-string current-knowledge-object))))
   (when talk (call-text-to-speech-action talkery)
         (vizbox-robot-says talkery))))

(defun trim-knowledge-string (current-knowledge-object)
  "trims the knowledge name, when it is with http"
  (let* ((?obj-start (or (search "#" current-knowledge-object)
                         0))
	(?obj-trim 
		      (string-trim "'"
				   (string-trim "|"
						(subseq current-knowledge-object ?obj-start)))))
    (let* ((?obj-end (search "_" ?obj-trim))
           (?obj (subseq (string-trim "#" ?obj-trim) 0 ?obj-end)))
      ?obj)))



(defun extract-percept-msg-obj-type-to-string (?list-of-objects)
  ;;to-do if u found 2 of the same maybe delee and say 2?
  ;;loop over the objects get the description out of it and make it as string
  (mapcar (lambda (percept-object)
            (roslisp:with-fields
                ((description
                  (cram-designators:description)))
                percept-object
              (cl::write-to-string
               (second
                (second description)))))
          ?list-of-objects))



;; (let *((?test-place-pose
;;            (cl-transforms-stamped:make-pose-stamped
;;             cram-tf:*fixed-frame*
;;             0.0
;;             (cl-transforms:make-3d-vector 1.0 0.0 0.5)
;;             (cl-transforms:make-quaternion 0 0 0.0 1))))
;;                   (exe:perform (desig:an action
;;                                  (type :placing-cart)
;;                                  (goal-pose ?cereal-target-pose)


;; (defun place-object (?arm ?target-pose-as-list
;;                      &key ?left-grasp ?right-grasp ?object-placed-on ?object-to-place ?attachment)
;;   (let ((?target-pose (ensure-pose-stamped ?target-pose-as-list)))

;;       (exe:perform 
;;        (desig:an action
;;                  (type placing)
;;                  (arm ?arm)
;;                  (desig:when ?object-to-place
;;                    (object ?object-to-place))
;;                  (desig:when ?right-grasp
;;                    (right-grasp ?right-grasp))
;;                  (desig:when ?left-grasp
;;                    (left-grasp ?left-grasp))
;;                  (target (desig:a location
;;                                   (desig:when ?object-placed-on
;;                                     (on ?object-placed-on))
;;                                   (desig:when ?object-to-place
;;                                     (for ?object-to-place))
;;                                   (desig:when ?attachment
;;                                     (attachment ?attachment))
;;                                   (pose ?target-pose)))))))

;;;;;;;;;;;;
;; VANESSA -----------------------------------------------------------------END


;;;;;;;;;;; VIZBOX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;@author Tim Rienits
;; Print on VizBox what the Robot is saying.
(defun vizbox-robot-says (message)

    (let ((pub (roslisp:advertise "robot_text" "std_msgs/String")))
        (sleep 1)
        (roslisp:publish-msg pub :data (format nil message)))
  )

;;@author Tim Rienits
;; Print on VizBox what the robot has heard the operator say.
(defun vizbox-robot-heard (message)
  
    (let ((pub (roslisp:advertise "operator_text" "std_msgs/String")))

        (sleep 1)
        (roslisp:publish-msg pub :data (format nil message)))
  )

;;@author Tim Rienits
;; Sets the current step of the plan to new_step (starts at 0).
(defun vizbox-set-step (new_step)

    (let ((pub (roslisp:advertise "challenge_step" "std_msgs/UInt32")))

        (sleep 1)
        (roslisp:publish-msg pub :data new_step))
  )

;; Luca



(defun park-robot ()
  "Default pose"
  (exe:perform (desig:an action
                        (type taking-pose)
                        (pose-keyword "park"))))

(defun pre-align-height-robot ()
  (exe:perform (desig:an action
                        (type taking-pose)
                        (pose-keyword "pre_align_height"))))

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

(defun carry-robot ()
  "Default pose"
  (exe:perform (desig:an action
                        (type taking-pose)
                        (pose-keyword "carry"))))

