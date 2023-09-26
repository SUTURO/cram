(in-package :gpsr-demo)

;;(suturo-real-hsr-pm:with-real-hsr-pm ....)
;;(roslisp:start-ros-node "alina-cram")

;;call (init-stage-1 AMOUNT-OF-TASKS) before this#
;;when launching, please do:

;;(suturo-real-hsr-pm::wait-for-startsignal
;;(suturo-real-hsr-pm:with-real-hsr-pm
(defun gpsr-demo(&optional ?test-task)
  "This function is the main control function for gpsr.
It controls the flow of the program and the mapping of NLP to plans.
?test-task is a string."
  ;;??? It might be an idea to move this into a different function also. dunno

  ;;step1 - init challenge
  ;;wait for door to be opened (see start-signal.lisp)
  ;;go to pre-defined location
  ;;look at / locate operator
  ;;TODO "Hello, I'm Toya. Please give me a task" < this should be in NLP?
  ;;(init-stage-1)


  ;; step 1: wait for door to open -> macro around this plan
  ;;go to middle of room
  (say "Going to the instruction point in the living room")
  (let* ((?nav-pose (origin-point-of-frame "iai_kitchen/living_room:living_room:room_center_link")))
    (publish-marker-pose ?nav-pose)
    (su-demos::move-hsr ?nav-pose)
    (say "please take care, still moving")

    ;; go to instruction pose
    (publish-marker-pose *instruction-point*)
    (su-demos::move-hsr *instruction-point*))
    
  ;;step 2: wait for nlp input
  ;;loop for multiple tasks
  (dotimes  (n 3)
    (nlp-feedback "Listen")
    ;; ~~~ NLP Processing ~~~
    (cpl:wait-for *plan-fluent*)

    ;;match plan-fluent, to plan
    (let* ((?plan (cpl:value *plan-fluent*))
           (?plan-details (cpl:value *plan-details-fluent*))) ;;struct
      (case ?plan
        (:fetch (fetch-plan ?plan-details))
        (:deliver (deliver-plan ?plan-details))
        (:search (search-plan ?plan-details))
        (:navigate (navigate-plan ?plan-details))
        (:transport (transport-plan ?plan-details))
        (:guide (guide-plan ?plan-details))
        (:count (count-plan ?plan-details))
        (:follow (follow-plan ?plan-details))
        (:describe (describe-plan ?plan-details))
        (:greet (greet-plan ?plan-details))
        (:nlu_fallback (nlu-fallback-plan ?plan-details)))
      (su-demos::move-hsr *instruction-point*)))
  (say "I tried to do three tasks. I am done now. Good bye and have a nice day.")
  )

