(in-package :gpsr-demo)

(defvar *robokudo-tf-buffer-client* nil)

(defun init-tf-buffer-client ()
  (setf *robokudo-tf-buffer-client* (make-instance 'cl-tf2:buffer-client)))

(roslisp-utilities:register-ros-init-function init-tf-buffer-client)

;;robocup-hack
(defun make-robokudo-human-query (key-value-pairs-list)
  (flet ((convert-entry (key &key (atom-or-list :atom))
           (let ((key-val-pair (find key key-value-pairs-list :key #'car)))
             (if key-val-pair
                 (let ((value (second key-val-pair)))
                   (if (eq atom-or-list :atom) 
                       (if (listp value)
                           (car value)
                           value)
                       (if (listp value)
                           (map 'vector #'identity value)
                           (vector value))))
                   (if (eq atom-or-list :atom)
                     ""
                     #(""))))))
    (format t "format uid: ~a" (convert-entry :uid)) 
    (roslisp:make-message
     'robokudo_msgs-msg:querygoal
     :type (convert-entry :type)
     :description (convert-entry :description)
     :obj (roslisp:make-message
           'robokudo_msgs-msg:objectdesignator
           :uid (convert-entry :uid :atom-or-list :atom)
           :type (convert-entry :type-obj)
           :shape (convert-entry :shape :atom-or-list :list)
           :shape_size (convert-entry :shape_size :atom-or-list :list)
           :color (convert-entry :color :atom-or-list :list)
           :location (convert-entry :location)
           :size (convert-entry :size)
           ;; :objectsize (convert-entry :objectsize)
           :pose (convert-entry :pose :atom-or-list :list)
           :pose_source (convert-entry :pose_source :atom-or-list :list)
           :attribute (convert-entry :attribute :atom-or-list :list)
           :description (convert-entry :description :atom-or-list :list)
           ))))


(defun ensure-robokudo-human-input-parameters (key-value-pairs-list quantifier)
  (let ((key-value-pairs-list
          (remove-if
           #'null
           (mapcar (lambda (key-value-pair)
                     (destructuring-bind (key &rest values)
                         key-value-pair
                       (let ((value (car values)))
                         ;; below are the only keys supported by RS atm
                         (if (or (eql key :uid)
                                 (eql key :type)
                                 (eql key :shape)
                                 (eql key :color)
                                 (eql key :location)
                                 (eql key :size)
                                 (eql key :shape_size)
                                 (eql key :pose)
                                 (eql key :pose_source)
                                 (eql key :attribute)
                                 (eql key :description))
                             (list key
                                   (etypecase value
                                     ;; RS is only case-sensitive on "TYPE"s
                                     (keyword
                                      (remove #\-
                                              (if (eql key :type)
                                                  (string-capitalize
                                                   (symbol-name
                                                    (case value
                                                      ;; (:bowl :ikea-red-bowl)
                                                      ;; (:cup :ikea-red-cup)
                                                      ;; (:spoon :soup-spoon)
                                                      (t value))))
                                                  (string-downcase
                                                   (symbol-name value)))))
                                     (string
                                      value)
                                     (list
                                      (mapcar (lambda (item)
                                                (etypecase item
                                                  (keyword
                                                   (string-downcase
                                                    (symbol-name item)))
                                                  (string
                                                   item)))
                                              value))
                                     (desig:location-designator
                                      (desig:desig-prop-value
                                       (or (desig:desig-prop-value value :on)
                                           (desig:desig-prop-value value :in))
                                       :owl-name))))))))
                   key-value-pairs-list)))
        (quantifier quantifier))
    (values key-value-pairs-list quantifier)))

;;;;;;;;;;;;;;;;;;;;;;;; OUTPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-human-result (message)
  ;;(declare (type robokudo_msgs-msg:objectdesignator message))
  "Returns a keyword key-value pairs list"
  (flet ((to-keyword (string &key (underscores-or-camelcase :underscores))
           (if (string-equal string "")
               nil
               (if (eq underscores-or-camelcase :underscores)
                   (roslisp-utilities:lispify-ros-underscore-name string :keyword)
                   (roslisp-utilities:lispify-ros-name string :keyword)))))
    `((:name ,(to-keyword
               ;; (roslisp:msg-slot-value message :uid)
               (format nil "~a-1" (roslisp:msg-slot-value message :type))
               :underscores-or-camelcase :camelcase))
      (:uid ,(to-keyword (roslisp:msg-slot-value message :uid)))
      (:type ,(to-keyword (roslisp:msg-slot-value message :type)))
      (:shape ,(map 'list #'to-keyword (roslisp:msg-slot-value message :shape)))
      (:color ,(map 'list #'to-keyword (roslisp:msg-slot-value message :color)))
      (:size ,(to-keyword (roslisp:msg-slot-value message :size)))
      ;;(:dimension ,(to-keyword (roslisp:msg-slot-value message :dimension)))
      (:shape_size ,(roslisp:msg-slot-value message :shape_size))
      (:location ,(to-keyword (roslisp:msg-slot-value message :location)))
      (:pose ,(map 'list #'cl-transforms-stamped:from-msg (roslisp:msg-slot-value message :pose)))
      (:pose_source ,(map 'list #'to-keyword (roslisp:msg-slot-value message :pose_source)))
      (:attribute ,(map 'list #'to-keyword (roslisp:msg-slot-value message :attribute)))
      (:description ,(map 'list #'to-keyword (roslisp:msg-slot-value message :description))))))


;; defun find-pose-in-object-designator ...
;;;;;;;;;;;;;;;;; ACTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *rs-result* nil)
(defun call-robokudo-human-action (keyword-key-value-pairs-list &key (quantifier :all))
  (declare (type (or keyword number) quantifier))
  (multiple-value-bind (key-value-pairs-list quantifier)
      (ensure-robokudo-human-input-parameters keyword-key-value-pairs-list quantifier)
    (multiple-value-bind (result status)
        (actionlib-client:call-simple-action-client
         'robokudo-action
         :action-goal (make-robokudo-human-query key-value-pairs-list))
      (setf *rs-result* result)
      (let* ((rs-parsed-result (ensure-robokudo-result result quantifier status))
             (rs-result (ecase quantifier
                          ((:a :an :the) (make-robokudo-designator
                                          rs-parsed-result
                                          keyword-key-value-pairs-list))
                          (:all (map 'list (alexandria:rcurry #'make-robokudo-designator
                                                              keyword-key-value-pairs-list)
                                     rs-parsed-result)))))
        rs-result))))
