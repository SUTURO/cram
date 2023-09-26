(in-package :gpsr-demo)


(defparameter *nlp-to-knowledge-rooms* '())

(defparameter *instruction-point* (cl-tf:make-pose-stamped
                                   "map" 0.0
                                   (cl-tf:make-3d-vector 1.227 0.365 0.0)
                                   (cl-tf:make-quaternion 0.0 0.0 0.7017 0.7017)))

(defun origin-point-of-frame (?frame)
  (cl-tf:make-pose-stamped
   ?frame 0.0
   (cl-tf:make-3d-vector 0.0 0.0 0.0)
   (cl-tf:make-quaternion 0.0 0.0 0.7017 0.7017)))
  

(defun get-living-room-string ()
  (cut:var-value '|?Room|
                 (cut:lazy-car (json-prolog:prolog-simple "is_living_room(Room)"))))

(defun check-if-room-exists (?keyword-room)
  "compare keyword-room with rooms knowledge knows and return iri"
  ;;get all rooms
  (let* ((?living-room-iri (cut:var-value '|?Room|
                                          (cut:lazy-car
                                           (json-prolog:prolog-simple "is_living_room(Room)."))))
         (?kitchen-iri (cut:var-value '|?Room|
                                          (cut:lazy-car
                                           (json-prolog:prolog-simple "is_kitchen(Room)."))))
         (?study-iri (cut:var-value '|?Room|
                                          (cut:lazy-car
                                           (json-prolog:prolog-simple "is_study_room(Room)."))))
         (?bedroom-iri (cut:var-value '|?Room|
                                          (cut:lazy-car
                                           (json-prolog:prolog-simple "is_bedroom(Room)."))))
         (?room-list (list ?living-room-iri ?kitchen-iri ?study-iri ?bedroom-iri))
         (?result))
    (if (search (remove #\- (string ?keyword-room)) (string-upcase (string ?kitchen-iri)))
        (setf ?result ?kitchen-iri))
    (if (search (remove #\- (string ?keyword-room)) (string-upcase (string ?living-room-iri)))
        (setf ?result ?living-room-iri))
    (if (search (remove #\- (string ?keyword-room)) (string-upcase (string ?study-iri)))
        (setf ?result ?study-iri))
    (if (search (remove #\- (string ?keyword-room)) (string-upcase (string ?bedroom-iri)))
        (setf ?result ?bedroom-iri))
    ?result))

(defun pose-of-knowrob-object (?object-iri)
  "gets an object iri, returns pose-stamped relative to self"
  (let* ((?pose-obj (cut:var-value '|?Pose|
                                   (cut:lazy-car (json-prolog:prolog-simple
                                                  (concatenate 'string "object_pose("
                                                               (string ?object-iri) ",Pose).")))))
         (?frame (remove #\' (string (first ?pose-obj))))
         (?vector (coerce (second ?pose-obj) 'list))
         (?quaternion (coerce (third ?pose-obj) 'list)))
    ;;make pose stamped
    (cl-tf:make-pose-stamped
     ?frame 0.0
     (cl-tf:make-3d-vector (first ?vector) (second ?vector) (third ?vector))
     (cl-tf:make-quaternion (first ?quaternion) (second ?quaternion) (third ?quaternion) (fourth ?quaternion)))))

(defun pose-for-perceiving-object (?object-iri)
  "gets an object iri, returns pose-stamped relative to self"
  (let* ((?pose-obj (cut:var-value '|?Pose|
                                   (cut:lazy-car (json-prolog:prolog-simple
                                                  (concatenate 'string "object_rel_pose("
                                                               (string ?object-iri)
                                                               ", perceive, [direction('-x')], Pose).")))))
         (?frame (remove #\' (string (first ?pose-obj))))
         (?vector (coerce (second ?pose-obj) 'list))
         (?quaternion (coerce (third ?pose-obj) 'list)))
    ;;make pose stamped
    (cl-tf:make-pose-stamped
     ?frame 0.0
     (cl-tf:make-3d-vector (first ?vector) (second ?vector) (third ?vector))
     (cl-tf:make-quaternion (first ?quaternion) (second ?quaternion) (third ?quaternion) (fourth ?quaternion)))))

(defun obj-of-type (?type)
  "returns an object-knowrob-iri which is based on the given type of soma obj"
  ;;TODO account for lists of same object as return
  (let* ((?object-soma-iri (cut:var-value '|?Obj|
                                     (cut:lazy-car (json-prolog:prolog-simple
                                                    (concatenate 'string "has_type(Obj, soma:'"
                                                                 (prologify ?type)"').")))))
         (?object-suturo-iri (cut:var-value '|?Obj|
                                     (cut:lazy-car (json-prolog:prolog-simple
                                                    (concatenate 'string "has_type(Obj, suturo:'"
                                                                 (prologify ?type)"').")))))
         (?result-iri nil))
    
    (if (eq ?object-suturo-iri '|?Obj|) 
        (setf ?result-iri ?object-soma-iri)
        (setf ?result-iri ?object-suturo-iri))
    ?result-iri))



(defun get-obj-in-room (?object-type-key ?room-key)
  "Returns the object-iri of an object of the given type within a room"
  (let* ((?room-iri (string (check-if-room-exists ?room-key)))
         (?object-iri (string (cut:var-value '|?Obj|
                                     (cut:lazy-car (json-prolog:prolog-simple
                                                    (concatenate 'string "has_type(Obj, soma:'"
                                                                 (prologify ?object-type-key)"')."))))))
         (?obj-in-room-p (json-prolog:prolog-simple
                        (concatenate 'string "is_inside_of(" ?object-iri ", " ?room-iri ").")))
         (?result nil))
    
    (if ?obj-in-room-p
        (setf ?result (pose-for-perceiving-object ?object-iri)))
    ?result))
         
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;; Dependencies Functions
(defun make-pose (reference-frame pose)
  (cl-transforms-stamped:make-pose-stamped
   reference-frame 0.0
   (apply #'cl-transforms:make-3d-vector (first pose))
   (apply #'cl-transforms:make-quaternion (second pose))))


(defun find-keyword(?word-to-find ?list-of-list)
  (mapcar (lambda (element)
            (position ?word-to-find element))
          ?list-of-list)) 

(defun find-non-nil-element (?list)
  (find :non-nil-element ?list :test (lambda (non-nil-element element-in-list) (not (null element-in-list)))))

(defun all-positions-list (?word ?lookup-list)
  (let ((?outcome nil))
    (dotimes (i (length ?lookup-list))
      (if (eq (nth i ?lookup-list) ?word)
          (push i ?outcome)))
    (nreverse ?outcome)))


(defun get-nth-element(?number-list ?lookup-list)
  (let ((?outcome nil))
    (dolist (n ?number-list)
      (push (nth n ?lookup-list) ?outcome))
    (nreverse ?outcome))
  )

 ;;;;;; Head movement psoitions
(defparameter *left-downward*
  (cl-transforms-stamped:make-pose-stamped
   "base_footprint" 0.0
   (cl-transforms:make-3d-vector 0.65335d0 0.76d0 0.758d0)
   (cl-transforms:make-identity-rotation)))

(defparameter *left-upward*
  (cl-transforms-stamped:make-pose-stamped
   "base_footprint" 0.0
   (cl-transforms:make-3d-vector 0.65335d0 0.76d0 0.958d0)
   (cl-transforms:make-identity-rotation)))

(defparameter *right-downward*
  (cl-transforms-stamped:make-pose-stamped
   "base_footprint" 0.0
   (cl-transforms:make-3d-vector 0.65335d0 -0.76d0 0.758d0)
   (cl-transforms:make-identity-rotation)))

(defparameter *right-upward*
  (cl-transforms-stamped:make-pose-stamped
   "base_footprint" 0.0
   (cl-transforms:make-3d-vector 0.65335d0 -0.76d0 0.958d0)
   (cl-transforms:make-identity-rotation)))

(defparameter *forward-downward*
  (cl-transforms-stamped:make-pose-stamped
   "base_footprint" 0.0
   (cl-transforms:make-3d-vector 0.65335d0 0.076d0 0.758d0)
   (cl-transforms:make-identity-rotation)))

(defparameter *forward-upward*
  (cl-transforms-stamped:make-pose-stamped
   "base_footprint" 0.0
   (cl-transforms:make-3d-vector 0.65335d0 0.076d0 0.958d0)
   (cl-transforms:make-identity-rotation)))


;;;;;;;;;;;;;;;;;;;;;;;;;;Mapping Between KnowRob and NLP;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; object knowledge

;; nlp-name cram-name category df-room df-loc-in-room
(defparameter *gpsr-objects* '((:apple :apple :fruits :living-room :bookcase)      
                               (:bag :bag :container :living-room :end-table)
                               (:basket :basket :container :living-room :end-table)
                               (:bottle :bottle :container :kitchen :end-table)
                               (:bowl :bowl :tableware :kitchen :storage-table)
                               (:cloth :cloth :cleaningstuff :bedroom :side-table)
                               (:cup :cup :tableware :kitchen :storage-table)
                               (:cascade :cascade :container :bedroom :side-table)
                               (:chocolate :chocolate :snacks :living-room :bookcase)
                               (:crackers :crackers :snacks :living-room :bookcase)
                               (:coke :coke :drinks :kitchen :counter)
                               (:cereal :cerealbox :food :kitchen :cupboard)
                               (:dish :dish :tableware :kitchen :storage-table)
                               (:fork :fork :cutlery :kitchen :storage-table)
                               (:glass :glass :tableware :kitchen :storage-table)
                               (:grape-juice :grapejuice :drinks :kitchen :counter)
                               (:juice :juice :drinks :kitchen :counter)
                               (:knife :knife :cutlery :kitchen :storage-table)
                               (:milk :milkpack :drinks :kitchen :counter)
                               (:noodles :noodles :food :kitchen :cupboard)
                               (:orange :orange :fruits :living-room :bookcase)
                               (:orange-juice :orangejuice :drink :kitchen :counter)
                               (:paprika :paprika :fruits :living-room :bookcase)
                               (:pringles :pringles :snacks :living-room :bookcase)
                               (:plate :plate :tableware :kitchen :end-table)
                               (:potatochips :potatochips :snacks :living-room :bookcase)
                               (:spoon :spoon :cutlery :kitchen :storage-table)
                               (:sprite :sprite :drinks :kitchen :counter)
                               (:sausages :sausages :food :kitchen :cupboard)
                               (:scrubby :scrubby :cleaningstuff :bedroom :side-table)
                               (:sponge :sponge :cleaningstuff :bedroom :side-table)
                               (:trash :trash :cleaningstuff :bedroom :side-table)
                               (:tray :tray :containers :living-room :end-table)))


;;;; location knowledge
;;; :room :location1-in-room :location2-in-room ...
(defparameter *gpsr-rooms-locations* '((:bedroom :bed :desk :side-table)  
                                       (:living-room :exit :couch :end-table :bookcase)
                                       (:kitchen :cupboard :storage-table :sink :counter :dishwasher)
                                       (:dinning-room :dinning-table)
                                       (:corridor :entrance)))
 

;;; person 
(defvar *gpsr-persons-name* '((:alex :female :male)
                              (:charlie :female :male)
                              (:elizabeth :female)
                              (:francis :female :male)
                              (:jennifer :female)
                              (:mehreen :female)
                              (:mohammad :male)
                              (:malta :male)
                              (:linda :female)
                              (:mary :female)
                              (:patricia :female)
                              (:robin :female :male)
                              (:james :male)
                              (:john :male)
                              (:michael :male)
                              (:robert :male)
                              (:skyler :female :male)
                              (:william :male)))

;;; person actions
(defvar *gpsr-persons-action* '((:sitting :sitting) ;;;; :nlp-name cram-name
                                (:standing :standing)
                                (:talking :talking)
                                (:lying-down :lying)
                                (:pointing-to-the-left :pointLeft)
                                (:pointing-to-the-right :pointRight)
                                (:raising-right-arm :raiseRight)
                                (:raising-left-arm :raiseLeft)
                                (:waving :waving)))


;;;; nlp personal pronouns mapping with cram
;; :title :per-pronoun1 :per-pronoun2 ...
(defvar *gpsr-pronouns* '((:object :objects :it)  
                          (:location :there :here)
                          (:person :me :him :her :them :they)))

(defvar *gpsr-attributes* '((:heavy)  
			    (:heaviest)
			    (:small)
			    (:smallest)
			    (:large)
			    (:largest)
			    (:big)
			    (:biggest)
			    (:tall)
			    (:tallest)
			    (:younger)
			    (:youngest)
			    (:older)
			    (:oldest)
			    (:fattest)
			    (:fat)
			    (:yourself)
			    (:left)
			    (:right)
			    (:top)
			    (:bottom)
			    (:gender)
			    (:pose)
			    ))

;;;; numbers
;;TODO make this into a map?
(defvar *nlp-numbers* '(:one :two :three :four :five :six :seven :eight :nine :ten))
(defvar *cram-numbers* '(1 2 3 4 5 6 7 8 9 10))

 
;;;;navigating locations for Robot
;;TODO this should come from knowledge
(defvar *gpsr-navigation-locations* '((:start-point (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                      (:bedroom (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0)) 
                                      (:living-room (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                      (:kitchen (-0.1d0 0.1d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                      (:dinning-room (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                      (:corridor (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                      (:entrance (-0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))				
                                      ))

(defvar *gpsr-navigation-locations-near-furniture* '((:counter (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:side-table(0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:end-table (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:storage-table (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:cupboard (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:bookcase (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:entrance (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:dinning-table (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:bed (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:desk (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:exit (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:couch (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:dishwasher (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:sink (0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     (:floor (-0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0 1.0d0))
                                                     ))

				
;;;; locations on the object / searching locations / pickup and place locations

(defvar *gpsr-locations-on-furniture* '((:counter (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:side-table (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:end-table (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:storage-table (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:cupboard (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:bookcase (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:entrance (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:dinning-table (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:bed (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:desk (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:exit (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:couch (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					(:dishwasher (0.65335d0 0.076d0 0.758d0) (0.0d0 0.0d0 0.0d0 1.0d0))
					
					))
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-info-word (?searching-word ?list)  ;;; (get-info-word :spoon *gpsr-objects*)
  
  (let ((?result (find-keyword ?searching-word ?list))
        (?element nil)
        (?positions-in-list))
    (setf ?element (find-non-nil-element ?result))
    (if (eq ?element nil)
        (return-from get-info-word nil))
    
    (if (not (eq ?element nil))
        (setf ?positions-in-list (all-positions-list ?element ?result))) 

    (get-nth-element ?positions-in-list ?list)))





(defun get-object-position (?searching-word ?array) ;;; e.g  (get-word-position :kitchen (first (get-info-word :spoon *objects*)))
  (let* ((?position-outcome (all-positions-list ?searching-word ?array))
         (?output nil))
    (dolist (n ?position-outcome)
      (if (eq n 0)
          (push :nlp-name ?output))
      (if (eq n 1)
          (push :cram-name ?output))
      (if (eq n 2)
          (push :object-category ?output))
      (if (eq n 3)
          (push :default-room-location ?output))
      (if (eq n 4)
          (push :default-location-in-room ?output)))
    (nreverse ?output)))

(defun get-specific-info-word (?word ?specification ?list)
  (let ((?word-info (first (get-info-word ?word ?list)))
        (?result nil)) 
    (if (eq ?specification :nlp-name)
        (setf ?result (nth 0 ?word-info)))
    (if (eq ?specification :cram-name)
        (setf ?result (nth 1 ?word-info)))
    (if (eq ?specification :object-category)
        (setf ?result (nth 2 ?word-info)))
    (if (eq ?specification :default-room-location)
        (setf ?result (nth 3 ?word-info)))
    (if (eq ?specification :default-location-in-room)
        (setf ?result (nth 4 ?word-info)))
    (return-from get-specific-info-word ?result)))
	
	
(defun get-object-cram-name (?nlp-object-name)
  (get-specific-info-word ?nlp-object-name :cram-name *gpsr-objects*))

(defun identify-object-keyword (?object-keyword)
  (nth 0 (first (get-info-word ?object-keyword *gpsr-pronouns*))))


(defun object-to-be (?object-nlp)
  (let* ((?object-in-list (get-specific-info-word ?object-nlp :cram-name *gpsr-objects*)))
    (if  (eq (get-specific-info-word ?object-nlp :cram-name *gpsr-objects*) nil)
         (setf ?object-in-list (identify-object-keyword ?object-nlp)))
    (return-from object-to-be ?object-in-list)))

(defun get-navigation-pose (?keyword) ;; give room name  
  (make-pose "base_footprint" (list (nth 1 (first (get-info-word ?keyword  *gpsr-navigation-locations*)))
                                    (nth 2 (first (get-info-word ?keyword  *gpsr-navigation-locations*))))))

;;; give object keyword
(defun get-searching-look-direction (?keyword) ;;;  (get-searching-look-direction :counter) or  (get-searching-look-direction :juice)
  (let ((?get-location (get-specific-info-word ?keyword :default-location-in-room *gpsr-objects*)))
    (make-pose "base_footprint" (list (nth 1 (first (get-info-word ?get-location *gpsr-locations-on-furniture*)))
                                      (nth 2 (first (get-info-word ?get-location *gpsr-locations-on-furniture*)))))))
	

(defun get-navigation-location-near-furniture (?location-keyword) ;;;  (get-navigation-location-near-furniture :counter) 
  ;;(let ((?get-location (get-specific-info-word ?keyword :default-location-in-room *gpsr-objects*)))
  (nth 1 (first (get-info-word ?location-keyword *gpsr-navigation-locations-near-furniture*)))
  (make-pose "base_footprint" (list (nth 1 (first (get-info-word ?location-keyword *gpsr-navigation-locations-near-furniture*)))
                                    (nth 2 (first (get-info-word ?location-keyword *gpsr-navigation-locations-near-furniture*))))))

(defun get-any-person-feature (?pr-name ?pr-type ?pr-action)
  (if (not(eq ?pr-name :nil))
      (return-from get-any-person-feature ?pr-name))
  (if (not(eq ?pr-type :nil))
      (return-from get-any-person-feature ?pr-type))
  (if (not(eq ?pr-action :nil))
      (return-from get-any-person-feature ?pr-action))
  (return-from get-any-person-feature :nil))

;;; check for person pronoun

(defun get-pronoun-title(?pronoun)
  (nth 0 (first (get-info-word ?pronoun *gpsr-pronouns*))))

(defun check-person-pronoun(?pronoun)
  (let ((?searching-list (cdr (first (get-info-word ?pronoun *gpsr-pronouns*)))))
    (if (member ?pronoun ?searching-list)
        (return-from  check-person-pronoun T)
        (return-from  check-person-pronoun nil))))

(defun get-person-action-name(?person-action-nlp-name)
  (nth 1 (first (get-info-word ?person-action-nlp-name *gpsr-persons-action*))))

(defun get-objects-list-by-number (?number ?object-category) ;;;;  (get-objects-list-by-number :seven :fruits) or  (get-objects-list-by-number :seven :nil)
  (let* ((?some-list nil))
    (when (member ?number *nlp-numbers*)
      (let ((?position-in-list (first (all-positions-list ?number *nlp-numbers*))))
        (nth ?position-in-list *cram-numbers*)
        (when (not(eq ?object-category :nil))
          (dotimes (n (nth ?position-in-list *cram-numbers*)) 
            (push (nth n (get-info-word ?object-category *gpsr-objects*)) ?some-list))
          (setf ?some-list (remove nil ?some-list))
          )
        (when (eq ?object-category :nil)
          (dotimes (n (nth ?position-in-list *cram-numbers*))
            (push (nth n *gpsr-objects*) ?some-list))
          )))
    (return-from get-objects-list-by-number ?some-list)))

