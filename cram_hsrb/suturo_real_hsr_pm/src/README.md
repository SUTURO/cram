# SUTURO How to expand the current implementation

## Adding a new motion to sequence goals

### In suturo-plans.lisp
1. Add a new entry to the "get-attribute" function
   ```
    (defun get-attributes (motion)
      (case motion
            (:aligning-height (list :context :goal-pose :object-name))
            ...
            (:new-motion (list :of :new :attributes))
            (otherwise (error "~a is not a valid keyword" motion))))
   ```
2. Add a new entry to the case statement within the "generate-motion-sequence" function. Make sure that the String exactly matches the name of the giskard goal in manipulation.
   ```
    (defun generate-motion-sequence (motions context &rest args &key
                                                                  goal-pose
                                                                  ...
                                                                  new-attribute)
      ...
      (dolist (motion motions ?motion-sequence)
        (let* ((motion-name (case motion
                              (:aligning-height "AlignHeight")
                              ...
                              (:new-motion "NewMotionGoalName"))))
          ...)))
   ```
### In suturo-designators.lisp
1. If you added a new attribute, add one of the following two cases to the designator. If the user should have to specifiy the attribute choose the first version, otherwise choose the second and add the function "get-new-attribute"
   ```
    (-> (member :new-attribute ?attributes)
        (or (desig:desig-prop ?designator (:new-attribute ?new-attribute))
            (and (format "WARNING: Please specify the new-attribute.~%")
            (fail)))
        (equal ?new-attribute nil))

    (-> (member :new-attribute ?attributes)
        (once (or (desig:desig-prop ?designator (:new-attribute ?new-attribute))
                  (lisp-fun su-real::get-new-attribute ?parameter-that-helps-the-inference
                                                       ?new-attribute)))
       (equal ?new-attribute nil))
   ```

   As of late you may also use the following function to generate a string containing either of the two rules:
   ```
   Type mandatory
   (su-real::attrib->desig-rule :new-attribute :mandatory)

   Type inference
   (su-real::attrib->desig-rule :new-attribute :inference)
   ```
1.1 Add the function "get-new-attribute"
   ```
    (defun get-new-attribute (parameter-that-helps-the-inference)
      (or (su-demos::with-knowledge-result (result)
              ... knowledge query ...)
          (cond
            ... hardcode defaultvalues as a fallback if knowledge does not provide an answer ...)))
   ```
2. Add the new attribute to the "generate-motion-sequence" function
   ```
   (lisp-fun su-real::generate-motion-sequence ?motions ?context :goal-pose ?goal-pose
                                                                 ...
							                                     :new-attribute ?new-attribute
                                                                 ?motion-sequence)
   ```

## Adding a new attribute to context

### In suturo-plans.lisp
1. Add a new entry to the "rel-context" function
   ```
    (defun rel-context (motion)
      (case motion
            (:aligning-height (list :from-above))
            ...
            (:new-motion (list :of context :relevant :attributes ))
            (otherwise (error "~a is not a valid keyword" motion))))
   ```
2. Add the new attribute to the "generate-context" function
   ```
    (defun generate-context (action motions &rest arguments &key
                                                              from-above
                                                              ...
			    			                                  new-attribute)
      ...)
   ```
3. Add a new to the "to-giskard-msg-type" function
   ```
    (defun to-giskard-msg-type (param)
      (case param
            (:from-above "manipulation_msgs/ContextFromAbove")
            ...
            (:new-attribute "manipulation_msgs/ContextNewAttribute")
            (otherwise (error "Unknown parameter: ~a" param))))
   ```

### In suturo-designators.lisp
1. Add it to the designator and add the function "get-new-attribute". Afterwards add the new attribute to the "generate-context" function.
   ```
    (-> (member :context ?attributes)
        (and (once (or (desig:desig-prop ?designator (:from-above ?from-above))
                       (lisp-fun su-real::get-from-above ?object-name
                                                         ?from-above)))
             ...
             (once (or (desig:desig-prop ?designator (:new-attribute ?new-attribute))
                       (lisp-fun su-real::get-new-attribute ?parameter-that-helps-the-inference
                                                            ?new-attribute)))
             (lisp-fun su-real::generate-context ?action ?motions :from-above ?from-above
                                                                  ...
                                                                  :new-attribute ?new-attribute
                                                                  ?context))
        ...)
   ```
   As of late you may also use the following function to generate a string containing either of the two rules:
   ```
   Type context
   (su-real::attrib->desig-rule :new-attribute :mandatory)
   ```
1.1 Add the function "get-new-attribute"
   ```
    (defun get-new-attribute (parameter-that-helps-the-inference)
      (or (su-demos::with-knowledge-result (result)
              ... knowledge query ...)
          (cond
          .  .. hardcode defaultvalues as a fallback if knowledge does not provide an answer ...)))
   ```

### In dependencies_ws/src/suturo_resources/messages/manipulation_msgs/msg/
1. Add a new message.
   ```
   touch ContextNewAttribute.msg
   ```
2. Add a new field "content" and specify its type
   ```
   typeOfContent content
   ```
3. Rebuild the dependencies_ws and source it afterwards
   ```
   catkin clean -y && catkin build && . devel/setup.bash
   ```
4. Rebuild the planning_ws and source it afterwards
   ```
   catkin clean -y && catkin build && . devel/setup.bash
   ```