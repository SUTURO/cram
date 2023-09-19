(in-package :su-real)

;; @author Luca Krohm
(def-fact-group suturo-action (desig:action-grounding)
  ;; @author Luca Krohm
  (<- (action-grounding ?designator (su-real:pick-up ?resolved-action-designator))
    (spec:property ?designator (:type :picking-up))

    ;; collisionmode stuff not properly utilized atm
    (once (or (desig:desig-prop ?designator (:collision-mode ?collision-mode))
              (equal ?collision-mode nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-b ?collision-object-b))
              (equal ?collision-object-b nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-b-link
                                             ?collision-object-b-link))
              (equal ?collision-object-b-link nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-a ?collision-object-a))
              (equal ?collision-object-a nil)))


    (or (desig:desig-prop ?designator (:object-name ?object-name))
        (and (format "WARNING: Please specify the objectname.~%")
             (fail)))

    (once (or (desig:desig-prop ?designator (:object-size ?object-size))
                  (lisp-fun su-real::get-object-size ?object-name
                            ?object-size)))
   
    (once (or (desig:desig-prop ?designator (:goal-pose ?goal-pose))
              (lisp-fun su-real::get-goal-pose ?object-name
                        ?goal-pose)))

    (and (once (or (desig:desig-prop ?designator (:from-above ?from-above))
                   (lisp-fun su-real::get-from-above ?object-name
                             ?from-above)))
         (once (or (desig:desig-prop ?designator (:object-type ?object-type))
                   (lisp-fun su-real::get-object-type ?object-name
                             ?object-type)))
         (once (or (desig:desig-prop ?designator (:object-shape ?object-shape))
                   (lisp-fun su-real::get-object-shape ?object-name
                             ?object-shape)))

         ;; ?action to give manipulation the appropriate context
         (equal ?action :grasping)

         ;; ?motion mostly fillervalue to make sure generate context works, so if you read
         ;; this I did not come up with a way to fix it
         ;; the problem: technically we want the "motions" parameter to be optional, but it is
         ;; not possible to have &optional and &key in the same defun. I dont want to put
         ;; "motions" to the other keys, because i iterate through "arguments", which would
         ;; then also include "motions" which would be even uglier
         (equal ?motions :reaching)
         
         (lisp-fun su-real::generate-context ?action ?motions :from-above ?from-above
                                                              :object-type ?object-type
                                                              :object-shape ?object-shape
                                                              ?context))

    (once (or (desig:desig-prop ?designator (:sequence-goal ?sequence-goal))
              (equal ?sequence-goal nil)))

    ;; object-type is technically redundand atm since it is in context and in here...this one
    ;; is just for easier access to the type for the purpose of reperceiving the object during
    ;; the failurehandling
    (desig:designator :action ((:type :picking-up)
                               (:collision-mode ?collision-mode)
                               (:collision-object-b ?collision-object-b)
                               (:collision-object-b-link ?collision-object-b-link)
                               (:collision-object-a ?collision-object-a)
                               (:object-type ?object-type)
                               (:goal-pose ?goal-pose)
                               (:object-size ?object-size)
                               (:object-name ?object-name)
                               (:context ?context)
                               (:sequence-goal ?sequence-goal))
                      ?resolved-action-designator))

  ;; @author Luca Krohm
  (<- (action-grounding ?designator (su-real:place ?resolved-action-designator))
    (spec:property ?designator (:type :placing))

    ;; collisionmode stuff not properly utilized atm
    (once (or (desig:desig-prop ?designator (:collision-mode ?collision-mode))
              (equal ?collision-mode nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-b ?collision-object-b))
              (equal ?collision-object-b nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-b-link
                                             ?collision-object-b-link))
              (equal ?collision-object-b-link nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-a ?collision-object-a))
              (equal ?collision-object-a nil)))

    (or (desig:desig-prop ?designator (:object-name ?object-name))
        (and (format "WARNING: Please specify the objectname.~%")
             (fail)))

    (or (desig:desig-prop ?designator (:object-size ?object-size))
        (lisp-fun su-real::get-object-size ?object-name
                  ?object-size))
   
    (once (or (desig:desig-prop ?designator (:goal-pose ?goal-pose))
              (lisp-fun su-real::get-goal-pose ?object-name
                        ?goal-pose)))
    
    (and (or (desig:desig-prop ?designator (:from-above ?from-above))
             (lisp-fun su-real::get-from-above ?object-name
                       ?from-above))
         (once (or (desig:desig-prop ?designator (:neatly ?neatly))
                   (lisp-fun su-real::get-neatly ?object-name
                             ?neatly)))

         ;; ?action to give manipulation the appropriate context
         (equal ?action :placing)

         ;; ?motion mostly fillervalue to make sure generate context works, so if you read
         ;; this I did not come up with a way to fix it
         ;; the problem: technically we want the "motions" parameter to be optional, but it is
         ;; not possible to have &optional and &key in the same defun. I dont want to put
         ;; "motions" to the other keys, because i iterate through "arguments", which would
         ;; then also include "motions" which would be even uglier
         (equal ?motions :reaching)
         
         (lisp-fun su-real::generate-context ?action ?motions :from-above ?from-above
                                                              :neatly ?neatly
                                                              ?context))
    
    (once (or (desig:desig-prop ?designator (:inside ?inside))
              (equal ?inside nil)))
    (once (or (desig:desig-prop ?designator (:sequence-goal ?sequence-goal))
              (equal ?sequence-goal nil)))

    (desig:designator :action ((:type :placing)
                               (:collision-mode ?collision-mode)
                               (:collision-object-b ?collision-object-b)
                               (:collision-object-b-link ?collision-object-b-link)
                               (:collision-object-a ?collision-object-a)
                               (:goal-pose ?goal-pose)
                               (:object-size ?object-size)
                               (:context ?context)
                               (:inside ?inside)
                               (:sequence-goal ?sequence-goal))
                      ?resolved-action-designator))

   ;; @author Luca Krohm
  (<- (action-grounding ?designator (su-real:open-door ?resolved-action-designator))
    (spec:property ?designator (:type :opening-door))
    (once (or (desig:desig-prop ?designator (:collision-mode ?collision-mode))
              (equal ?collision-mode nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-b ?collision-object-b))
              (equal ?collision-object-b nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-b-link
                                             ?collision-object-b-link))
              (equal ?collision-object-b-link nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-a ?collision-object-a))
              (equal ?collision-object-a nil)))

    (or (desig:desig-prop ?designator (:handle-link ?handle-link))
        (and (format "WARNING: Please specify the handle-link.~%")
             (fail)))

    ;; handle-pose is optional since we might not want to try to perceive
    ;; the handle before opening it 
    (once (or (desig:desig-prop ?designator (:handle-pose ?handle-pose))
              (equal ?handle-pose nil)))

    (or (desig:desig-prop ?designator (:joint-angle ?joint-angle))
        (and (format "WARNING: Please specify the joint-angle.~%")
             (fail)))

    (desig:designator :action ((:type :opening-door)
                               (:collision-mode ?collision-mode)
                               (:collision-object-b ?collision-object-b)
                               (:collision-object-b-link ?collision-object-b-link)
                               (:collision-object-a ?collision-object-a)
                               (:handle-link ?handle-link)
                               (:handle-pose ?handle-pose)
                               (:joint-angle ?joint-angle))
                      ?resolved-action-designator))

  ;; @author Luca Krohm
  (<- (action-grounding ?designator (su-real:su-pour ?resolved-action-designator))
    (spec:property ?designator (:type :su-pouring))
    (once (or (desig:desig-prop ?designator (:collision-mode ?collision-mode))
              (equal ?collision-mode nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-b ?collision-object-b))
              (equal ?collision-object-b nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-b-link
                                             ?collision-object-b-link))
              (equal ?collision-object-b-link nil)))
    (once (or (desig:desig-prop ?designator (:collision-object-a ?collision-object-a))
              (equal ?collision-object-a nil)))

    (or (desig:desig-prop ?designator (:target-name ?target-name))
        (and (format "WARNING: Please specify the targetname.~%")
             (fail)))

    (or (desig:desig-prop ?designator (:object-size ?object-size))
        (and (format "WARNING: Please specify the size of the to-be-poured object.~%")
             (fail)))

    (once (or (desig:desig-prop ?designator (:target-size ?target-size))
              (lisp-fun su-real::get-object-size ?target-name
                        ?target-size)))

    (once (or (desig:desig-prop ?designator (:target-object ?target-object))
                  (lisp-fun su-real::get-frame ?target-name
                            ?target-object)))

    (and ;; ?action to give manipulation the appropriate context
         (equal ?action :pouring)

         ;; ?motion mostly fillervalue to make sure generate context works, so if you read
         ;; this I did not come up with a way to fix it
         ;; the problem: technically we want the "motions" parameter to be optional, but it is
         ;; not possible to have &optional and &key in the same defun. I dont want to put
         ;; "motions" to the other keys, because i iterate through "arguments", which would
         ;; then also include "motions" which would be even uglier
         (equal ?motions :reaching)
         
         (lisp-fun su-real::generate-context ?action ?motions
                   ?context))
    
    (once (or (desig:desig-prop ?designator (:sequence-goal ?sequence-goal))
              (equal ?sequence-goal nil)))

    (desig:designator :action ((:type :su-pouring)
                               (:collision-mode ?collision-mode)
                               (:collision-object-b ?collision-object-b)
                               (:collision-object-b-link ?collision-object-b-link)
                               (:collision-object-a ?collision-object-a)
                               (:object-size ?object-size)
                               (:target-object ?target-object)
                               (:target-size ?target-size)
                               (:target-name ?target-name)
                               (:context ?context)
                               (:sequence-goal ?sequence-goal))
                      ?resolved-action-designator))

  ;; @author Luca Krohm
  (<- (desig:action-grounding ?designator (su-real:sequence-goal ?resolved-action-designator))
    (spec:property ?designator (:type :sequence-goal))
    (desig:desig-prop ?designator (:action ?action))

    (-> (desig:desig-prop ?designator (:motions ?motions))
        (lisp-fun su-real::get-all-attributes ?motions
                  ?attributes)
        (and (format "WARNING: Please specify a motion sequence.~%")
             (fail)))
    
    (once (or (desig:desig-prop ?designator (:collision-mode ?collision-mode))
              (equal ?collision-mode :allow-all)))
    
    (-> (member :object-name ?attributes)
        (or (desig:desig-prop ?designator (:object-name ?object-name))
            (and (format "WARNING: Please specify the objectname.~%")
                 (fail)))
        (equal ?object-name nil))

    (-> (member :object-size ?attributes)
        (once (or (desig:desig-prop ?designator (:object-size ?object-size))
                  (lisp-fun su-real::get-object-size ?object-name
                            ?object-size)))
        (equal ?object-size nil))

    (-> (member :goal-pose ?attributes)
        (once (or (desig:desig-prop ?designator (:goal-pose ?goal-pose))
                  (lisp-fun su-real::get-goal-pose ?object-name
                            ?goal-pose)))
        (equal ?goal-pose nil))

    (-> (member :tilt-angle ?attributes)
        (or (desig:desig-prop ?designator (:tilt-angle ?tilt-angle))
            (and (format "WARNING: Please specify the tilt-angle.~%")
                 (fail)))
        (equal ?tilt-angle nil))

    (-> (member :tilt-direction ?attributes)
        (or (desig:desig-prop ?designator (:tilt-direction ?tilt-direction))
            (and (format "WARNING: Please specify the tilt-direction.~%")
                 (fail)))
        (equal ?tilt-direction nil))

    (-> (member :reference-frame ?attributes)
        (or (desig:desig-prop ?designator (:reference-frame ?reference-frame))
            (and (format "WARNING: Please specify the reference-frame.~%")
                 (fail)))
        (equal ?reference-frame nil))

    (-> (member :gripper-state ?attributes)
        (or (desig:desig-prop ?designator (:gripper-state ?gripper-state))
            (and (format "WARNING: Please specify the gripper-state.~%")
                 (fail)))
        (equal ?gripper-state nil))

    (-> (member :pose-keyword ?attributes)
        (or (desig:desig-prop ?designator (:pose-keyword ?pose-keyword))
            (and (format "WARNING: Please specify the pose-keyword.~%")
                 (fail)))
        (equal ?pose-keyword nil))

    (-> (member :distance ?attributes)
        (or (desig:desig-prop ?designator (:distance ?distance))
            (and (format "WARNING: Please specify the distance.~%")
                 (fail)))
        (equal ?distance nil))
    
    (-> (member :context ?attributes)
        (and (once (or (desig:desig-prop ?designator (:from-above ?from-above))
                       (lisp-fun su-real::get-from-above ?object-name
                                 ?from-above)))
             (once (or (desig:desig-prop ?designator (:neatly ?neatly))
                       (lisp-fun su-real::get-neatly ?object-name
                                 ?neatly)))
             (once (or (desig:desig-prop ?designator (:object-type ?object-type))
                       (lisp-fun su-real::get-object-type ?object-name
                                 ?object-type)))
             (once (or (desig:desig-prop ?designator (:object-shape ?object-shape))
                       (lisp-fun su-real::get-object-shape ?object-name
                                 ?object-shape)))
             (once (or (desig:desig-prop ?designator (:vertical-align ?vertical-align))
                       (lisp-fun su-real::get-vertical-align ?object-name
                                 ?vertical-align)))
             
             (lisp-fun su-real::generate-context ?action ?motions :from-above ?from-above
                                                                  :neatly ?neatly
                                                                  :object-type ?object-type
                                                                  :object-shape ?object-shape
                                                                  :vertical-align ?vertical-align
                                                                  ?context))
        (equal ?context nil))

    (lisp-fun su-real::generate-motion-sequence ?motions ?context :goal-pose ?goal-pose
                                                                  :object-name ?object-name
                                                                  :object-size ?object-size
                                                                  :tilt-angle ?tilt-angle
                                                                  :tilt-direction ?tilt-direction
                                                                  :reference-frame ?reference-frame
                                                                  :gripper-state ?gripper-state
                                                                  :pose-keyword ?pose-keyword
                                                                  :distance ?distance
                                                                  ?motion-sequence)

    (desig:designator :action ((:type :sequence-goal)
                               (:motion-sequence ?motion-sequence)
                               (:collision-mode ?collision-mode))
                      ?resolved-action-designator))

  ;; @author Luca Krohm
  (<- (action-grounding ?designator (su-real:take-pose ?resolved-action-designator))
    (spec:property ?designator (:type :taking-pose))
    (once (or (desig:desig-prop ?designator (:pose-keyword ?pose-keyword))
              (equal ?pose-keyword nil)))
    (once (or (desig:desig-prop ?designator (:head-pan ?head-pan))
              (equal ?head-pan nil)))
    (once (or (desig:desig-prop ?designator (:head-tilt ?head-tilt))
              (equal ?head-tilt nil)))
    (once (or (desig:desig-prop ?designator (:arm-lift ?arm-lift))
              (equal ?arm-lift nil)))
    (once (or (desig:desig-prop ?designator (:arm-flex ?arm-flex))
              (equal ?arm-flex nil)))
    (once (or (desig:desig-prop ?designator (:arm-roll ?arm-roll))
              (equal ?arm-roll nil)))
    (once (or (desig:desig-prop ?designator (:wrist-flex ?wrist-flex))
              (equal ?wrist-flex nil)))
    (once (or (desig:desig-prop ?designator (:wrist-roll ?wrist-roll))
              (equal ?wrist-roll nil)))

    (desig:designator :action ((:type :taking-pose)
                               (:pose-keyword ?pose-keyword)
                               (:head-pan ?head-pan)
                               (:head-tilt ?head-tilt)
                               (:arm-lift ?arm-lift)
                               (:arm-flex ?arm-flex)
                               (:arm-roll ?arm-roll)
                               (:wrist-flex ?wrist-flex)
                               (:wrist-roll ?wrist-roll))
                      ?resolved-action-designator))
)
