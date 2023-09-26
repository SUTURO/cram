(in-package :gpsr-demo)

;; connect to nlp
;; send it sentance
;; collect reply
;; write to file

(defparameter *sentance-list-stage-1*
  (list "Please give me the left most object from the cupboard"
        "Place 3 objects randomly, at least one on the floor"
        "Tell me how many people in the dining room are boys"
        "Robot please meet Charlie at the desk and follow her to the corridor"
        "Give the spoon to me"
        "Please escort Patricia from the bed to the bookcase"
        "Follow Robin"
        "Robot please meet Alex at the bed, follow him, and go to the living room"
        "Give me the right most object from the dining table"
        "Please Tell me the gender of the person at the exit"
        "Please Tell me the pose of the person at the dishwasher"
        "Guide James to the bed, you can find him at the end table"
        "Navigate to the sink, meet Alex, and guide her"
        "Could you give me the cloth"
        "Bring me the largest object from the dining table"
        "Robot please guide Robert from the sink to the couch"
        "Tell me the pose of the person in the corridor"
        "Could you please meet Linda at the sink and follow her"
        "Could you find the fruits in the dining room"
        "Tell something about yourself to the person pointing to the right in the bedroom"
        "Robot please get the tray from the desk and put it on the dining table"
        "Please take the chocolate drink to the cupboard"

        "Could you please meet Charlie at the end table and guide him to the sink"
        "Navigate to the desk, meet Skyler, and accompany him to the bookcase"
        "Take out the trash"
        "Could you greet Jennifer at the end table and ask her to leave"
        "Meet Charlie at the entrance, follow her, and go to the living room"
        "Go to the living room, locate a person pointing to the left, and tell something about yourself"
        "Tell me which are the three smallest objects on the storage table"
        "Dump the junk"
        "Robot please serve drinks to all the people in the bedroom"
        "Face Robin at the end table and accompany her to her uber"
        "Deliver fruits to me"
        "Could you please greet Elizabeth at the bookcase and lead her to her cab"))
  


(defun write-result (?sentance)
  (nlp-feedback ?sentance)
  (with-open-file (str "nlp_testing_output.txt"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format str ?sentance)
    (cpl:wait-for *plan-details-fluent*)
    (sleep 2)
    (nlp-feedback "DONE")
    ;;(mapcar (lambda (?item) (format str ?item)) (plan-details-string-list (cpl:value *plan-details-fluent*)))
    ;;parse que into strings
    (let* ((?plan-string-list (mapcar #'plan-details-string-list *plan-queue*)))
      ;;write to file
      (mapcar (lambda (?item) (progn (format str ?item) (format str "~% - ~%"))) ?plan-string-list))
    ;;reset queue
    (setf *plan-queue* nil)
    (format str "~% --- ~%"))
  ;(setf (cpl:value *plan-details-fluent*) nil)
  (sleep 3)) ;;give nlp time to process

;; use this by starting rasa, then do (start-stage-1 3 "33")
;,then run (run-test *sentance-list-stage-1*)
(defun run-test (?sentance-list)
  (mapcar 'write-result ?sentance-list))


;;navigation from room to room
(defun nlp-navi()
  (nlp-feedback "please go from the living room to the kitchen"))
