(ql:quickload "cl-postgres")
(ql:quickload "cl-json")
(ql:quickload "cl-ppcre")
(ql:system-apropos "postgre")

(use-package :cl-postgres)
(use-package :cl-json)
(use-package :cl-ppcre)

(defparameter *conn* (open-database "protoboard" "django" "masterkey" "localhost" 9090))

(defparameter *row-reader* (row-reader (fields)
			     (loop :while (next-row)
				:collect (loop :for field :across fields
					    :collect (next-field field)))))

;;
;; Some utility functions
;;

(defun query (query)
  "Performs an SQL query"
  (exec-query *conn* query *row-reader*))

(defun get-field-names (table-name)
  "Extracts the field names of a column from the database"
  (exec-query *conn* (format nil "select column_name from information_schema.columns where table_name='~a';" table-name) *row-reader*))

(defun get-contexts (id-lst)
  "Extracts contexts, orders them, it gets 1. Self explanatory by looking at the query in the source code"
  (mapcar (lambda (id)
	    (query (format nil "select context from activitytree_ula_event where \"ULA_id\"= ~a ORDER BY time_stamp DESC limit 1" (first id))))
	  id-lst))

(defun interleave (&rest lists)
  (apply #'mapcar (lambda (&rest elts)
                    elts) lists))

(defun id-list+1 (lst)
  "Just adds 1 to an id list. This is used to get the corresponding EESM for an LA (la_id + 1 = eesm_id)"
  (mapcar (lambda (elt)
	    (list (1+ (first elt)))) lst))

(defun mean (lst)
  "Average of the numbers of a list"
  (when (> (length lst) 0)
    (float (/ (apply '+ lst) (length lst)))))

(defun sd (lst)
  "Standard deviation of a list of numbers"
  (when lst
    (let ((mean (mean lst)))
      (sqrt (/ (apply '+ (mapcar (lambda (elt)
                                   (expt (- elt mean) 2)) lst)) (length lst))))))

;;
;; Some extraction, cleaning, preprocessing functions
;;

(defun protoboard-contexts (exercise-uri)
  "Gets all the contexts from an excercise. i.e., all the attempts at solving an excersie by
all the users. Result: list of learning activity - eesm"
  (let* ((la-id (caar (query (format nil "select id from activitytree_learningactivity where uri = '~a';" exercise-uri))))
	 (ula-ids (query (format nil "select id from activitytree_userlearningactivity where learning_activity_id = ~a and user_id > 290" la-id)))
	 (eesm-ids (id-list+1 ula-ids))
	 (ula-contexts (get-contexts ula-ids))
	 (eesm-contexts (get-contexts eesm-ids))
         (attempts (get-attempts ula-ids)))
    (interleave ula-contexts eesm-contexts attempts)))

;;(protoboard-contexts "/program/PPP/1")
;;*E2*

;;*ctxts

(defun get-attempts (ula-ids)
  "How many attempts a user needed to solve a problem"
  (mapcar (lambda (ula-id)
            (length (query (format nil "select context from activitytree_ula_event where \"ULA_id\"= ~a" (first ula-id))))) ula-ids))

(defun clean-pythons-json (json-string)
  "Python's idea of JSON's format is not the
same as Common Lisp's idea. Some cleaning is necessary."
  (if json-string (progn
  (regex-replace-all "False"
  (regex-replace-all "True"
  (regex-replace-all "'(.+?)'"
  (regex-replace-all "u'(.+?)'"
  (regex-replace-all "u\".+?\""
  (regex-replace-all "u'params':.+?, u'"
  (regex-replace-all "(\\d+?):"
                     json-string
  "\"\\1\":")
  " u'")
  "\"\"")
  "\"\\1\"")
  "\"\\1\"")
  "\"True\"")
  "\"False\"")
  )
      "{}"))

;;
;; Extracting excercises' contexts from Protoboard
;;

(defparameter *exercises (mapcan (lambda (uri)
          (protoboard-contexts uri)) '("/program/PPP/1" "/program/PPP/2" "/program/PPP/3" "/program/PPP/4" "/program/PPP/5"
                                       "/program/PPP/6" "/program/PPP/7" "/program/PPP/8" "/program/PPP/9" "/program/PPP/10")))

(defun all-ks ()
  "Get all the keystrokes from all the database. This is used for statistics purposes (get the most
popular keystroke combinations)"
  (let ((all-e (concatenate 'list *E1* *E2* *E3* *E4* *E5* *E6* *E7* *E8* *E9* *E10*))
        (result nil))
    (dolist (r all-e)
      (setf result (append result (preproc-keystrokes (prune-la (decode-json-from-string (clean-pythons-json (caaar r)))))
            )))
    (remove nil result)))

(defun prune-la (la)
  "We extract only the mouse and keyboard keypresses and the positions of the mouse from the JSON"
  (when la
    (list
     (assoc :mousepresses la)
     (assoc :mousemovement la)
     (assoc :keypresses la)
     )))

(defun prune-eesm (eesm)
  "We clean the eesm context. The result are tuples in the format of '(1 2 3 4 5), where 1-> Strongly disagree, 2-> Disagree, 3-> Neutral, 4-> Agree, 5-> Strongly agree"
  (mapcar (lambda (e)
            (when e
              (let ((answer (cdr (assoc :user--answer (cdr e)))))
                (cond
                  ((eq (first answer) 1) 1)
                  ((eq (second answer) 1) 2)
                  ((eq (third answer) 1) 3)
                  ((eq (fourth answer) 1) 4)
                  ((eq (fifth answer) 1) 5)))))
          (remove :total--correct eesm :key (lambda (elt) (car elt)))))

;;
;; Preprocessing of the Keystrokes in a Learning Activity
;;

(defun preproc-keystrokes (la)
  "Some formatting. Result: '(1231313231 \"keydown\" 41), where first one is time, second type and third keycode."
  (mapcar (lambda (r)
            (list (cdr (first r))
                  (cdr (second r))
                  (cdr (third r)))) (cdr (assoc :keypresses la))))

;;(preproc-keystrokes (prune-la *la1))

(defun preproc-mousepresses (la)
  (remove nil (mapcar (lambda (r)
            (if (= (cdr (third r)) 1)
            (list (cdr (first r))
                  (cdr (second r))
                  (cdr (third r))))) (cdr (assoc :mousepresses la)))))

;;(preproc-mousepresses (prune-la *la1))

(defun preproc-mousemovement (la)
  (remove nil
          (mapcar (lambda (r)
                    (when (and (cdr (second (cdr (second r))))
                               (cdr (first (cdr (second r)))))
                      (list (cdr (first r))
                            (cdr (second (cdr (second r))))
                            (cdr (first (cdr (second r))))))) (cdr (assoc :mousemovement la)))))

;;(preproc-mousemovement (prune-la *la1))

;;
;; Digraphs and Trigraphs
;;
;;==\ Only the 100 most common are used
;;

(defun find-digraphs (ks)
  "Get every digraph from all the keystrokes in the database, along with their count. Result: '(3 (5 6)), 3 times, key 5 followed by key 6."
  (let* ((kd (remove nil (mapcar (lambda (r) (when (string= (second r) "keydown") r)) ks)))
         (digraphs (remove nil (maplist (lambda (rs)
                              (unless (null (third (second rs)))
                              (list (third (first rs))
                                    (third (second rs))))) kd))))
    (sort (remove-duplicates (mapcar (lambda (dg)
              (list (count dg digraphs :test 'equalp) dg)) digraphs) :test 'equalp) #'>
              :key (lambda (elt) (car elt)))
    ))

(defun find-trigraphs (ks)
  "Similar to find-digraph. Result: '(3 (5 6 7))"
  (let* ((kd (remove nil (mapcar (lambda (r) (when (string= (second r) "keydown") r)) ks)))
         (trigraphs (remove nil (maplist (lambda (rs)
                              (unless (null (third (third rs)))
                              (list (third (first rs))
                                    (third (second rs))
                                    (third (third rs))))) kd))))
    (sort (remove-duplicates (mapcar (lambda (tg)
              (list (count tg trigraphs :test 'equalp) tg)) trigraphs) :test 'equalp) #'>
              :key (lambda (elt) (car elt)))
    ))

(defparameter *dgs (subseq (find-digraphs (all-ks)) 0 100)
  "Top 100 digraphs")
(defparameter *tgs (subseq (find-trigraphs (all-ks)) 0 100)
  "Top 100 trigraphs")

;;
;; Digraph Preprocessing
;;

(defun extract-digraphs (ks dgs)
  "Gets a list of all the 'popular' digraphs in a list of keypresses."
  (let ((result))
    (mapcar (lambda (dg)
              (maplist (lambda (k)
                         (push (digraph-subseq dg k) result)) ks)) dgs)
    (remove nil (nreverse result))
    ))

(defun digraph-subseq (dg seq)
  "Used by extract-digraph"
  (when (and
         (string= (second (first seq)) "keydown")
         (= (caadr dg) (third (first seq)))
         (eql (cadadr dg)
            (third (find "keydown" (cdr seq) :test 'string= :key (lambda (elt)
                                                                   (second elt))))))
    (let ((sub nil))
      (block ntz
        (map nil (lambda (elt)
                   (push elt sub)
                     (when (has-digraph? (reverse sub) dg)
                       (return-from ntz (nreverse sub)))) seq)))))

(defun has-digraph? (sub dg)
  "Used by (digraph-subseq)"
  (flet ((cdr-ks (elt) (cdr elt)))
    (let* ((kc1 (caadr dg))
           (kc2 (cadadr dg))
           (fst-dwn (remove `("keydown" ,kc1) sub :key #'cdr-ks :test 'equalp :count 1))
           (snd-dwn (remove `("keydown" ,kc2) fst-dwn :key #'cdr-ks :test 'equalp :count 1))
           (fst-up (remove `("keyup" ,kc1) snd-dwn :key #'cdr-ks :test 'equalp :count 1))
           (snd-up (remove `("keyup" ,kc2) fst-up :key #'cdr-ks :test 'equalp :count 1))
           (fst-kc? (and (= (third (first sub)) kc1) (string= "keydown" (second (first sub)))))
           (pos-kc1d (position `("keydown" ,kc1) sub :key #'cdr-ks :test 'equalp))
           (pos-kc1u (position `("keyup" ,kc1) sub :key #'cdr-ks :test 'equalp))
           (pos-kc2d (position `("keydown" ,kc2) sub :key #'cdr-ks :test 'equalp))
           (pos-kc2u (position `("keyup" ,kc2) sub :key #'cdr-ks :test 'equalp))
           (num-kd (count "keydown" sub :test 'string= :key (lambda (elt) (second elt))))
           )
      (if (and fst-kc?
               (= num-kd 2)
               (when (and (numberp pos-kc1d)
                          (numberp pos-kc1u)
                          (numberp pos-kc2d)
                          (numberp pos-kc2u))
                 (and (< pos-kc1d pos-kc2d)
                      (< pos-kc1d pos-kc1u)
                      (< pos-kc2d pos-kc2u)))
               (/= (length fst-dwn)
                   (length snd-dwn)
                   (length fst-up)
                   (length snd-up)))
          T))))

;;(time (print (extract-digraphs *ks *dgs)))

;;
;; Trigraph Preprocessing
;;

(defun extract-trigraphs (ks tgs)
  "Gets a list of all the 'popular' trigraphs in a list of keypresses."
  (let ((result))
    (mapcar (lambda (tg)
              (maplist (lambda (k)
                         (push (trigraph-subseq tg k) result)) ks)) tgs)
    (remove nil (nreverse result))
    ))

(defun trigraph-subseq (tg seq)
  "Used by (extract-trigraphs)"
  (let* ((kc1 (first (cadr tg)))
         (kc2 (second (cadr tg)))
         (kc3 (third (cadr tg)))
         (snd-kd (let ((c 0))
                   (find '("keydown" 2) seq :test 'equalp :key 
                         (lambda (elt)
                           (when (string= "keydown" (second elt))
                             (incf c)
                             (list (second elt) c))
                           ))
                   ))
         (trd-kd (let ((c 0))
                   (find '("keydown" 3) seq :test 'equalp :key 
                         (lambda (elt)
                           (when (string= "keydown" (second elt))
                             (incf c)
                             (list (second elt) c))
                           ))
                   ))
         (fst-kc? (and (numberp kc1) (= (third (first seq)) kc1) (string= (second (first seq)) "keydown")))
         (snd-kc? (and snd-kd (numberp kc2) (= (third snd-kd) kc2)))
         (trd-kc? (and trd-kd (numberp kc3) (= (third trd-kd) kc3))))
    (when (and fst-kc? snd-kc? trd-kc?)
      (let ((sub nil))
        (block ntz
          (map nil (lambda (elt)
                     (push elt sub)
                     (when (has-trigraph? (reverse sub) tg)
                       (return-from ntz (nreverse sub)))) seq))))))

;;(time (print (extract-trigraphs *ks *tgs)))

(defun has-trigraph? (sub tg)
  "Used by (trigraph-subseq)"
  (flet ((cdr-ks (elt) (cdr elt)))
    (let* ((kc1 (first (cadr tg)))
           (kc2 (second (cadr tg)))
           (kc3 (third (cadr tg)))
           (fst-dwn (remove `("keydown" ,kc1) sub :key #'cdr-ks :test 'equalp :count 1))
           (snd-dwn (remove `("keydown" ,kc2) fst-dwn :key #'cdr-ks :test 'equalp :count 1))
           (trd-dwn (remove `("keydown" ,kc3) snd-dwn :key #'cdr-ks :test 'equalp :count 1))
           (fst-up (remove `("keyup" ,kc1) trd-dwn :key #'cdr-ks :test 'equalp :count 1))
           (snd-up (remove `("keyup" ,kc2) fst-up :key #'cdr-ks :test 'equalp :count 1))
           (trd-up (remove `("keyup" ,kc3) snd-up :key #'cdr-ks :test 'equalp :count 1))
           (fst-kc? (and (= (third (first sub)) kc1) (string= "keydown" (second (first sub)))))
           (pos-kc1d (position `("keydown" ,kc1) sub :key #'cdr-ks :test 'equalp))
           (pos-kc1u (position `("keyup" ,kc1) sub :key #'cdr-ks :test 'equalp))
           (pos-kc2d (position `("keydown" ,kc2) sub :key #'cdr-ks :test 'equalp))
           (pos-kc2u (position `("keyup" ,kc2) sub :key #'cdr-ks :test 'equalp))
           (pos-kc3d (position `("keydown" ,kc3) sub :key #'cdr-ks :test 'equalp))
           (pos-kc3u (position `("keyup" ,kc3) sub :key #'cdr-ks :test 'equalp))
           (num-kd (count "keydown" sub :test 'string= :key (lambda (elt) (second elt))))
           )
      (if (and fst-kc?
               (= num-kd 3)
               (when (and (numberp pos-kc1d)
                          (numberp pos-kc1u)
                          (numberp pos-kc2d)
                          (numberp pos-kc2u)
                          (numberp pos-kc3d)
                          (numberp pos-kc3u))
                 (and (< pos-kc1d pos-kc2d)
                      (< pos-kc2d pos-kc3d)
                      (< pos-kc1d pos-kc1u)
                      (< pos-kc2d pos-kc2u)
                      (< pos-kc3d pos-kc3u)))
               (/= (length fst-dwn)
                   (length snd-dwn)
                   (length fst-up)
                   (length snd-up)
                   (length trd-dwn)
                   (length trd-up)))
          T))))

;;(time (print (extract-digraphs *ks *dgs)))
;;(time (print (extract-trigraphs *ks *tgs)))


;;
;; Feature extracting functions
;;

(defun 2G-1D2D (dgs)
  (let ((times-lst (mapcar (lambda (dg)
            (let ((fst-kd (first dg))
                  (snd-kd (find "keydown" dg :test 'string= :key (lambda (elt) (second elt)) :from-end t)))
              (- (first snd-kd) (first fst-kd)))) dgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (2G-1D2D (extract-digraphs *la *dgs)))

(defun 2G-1Dur (dgs)
  (let ((times-lst (mapcar (lambda (dg)
            (let* ((fst-kd (first dg))
                   (fst-ku (find `("keyup" ,(third fst-kd)) dg :test 'equalp :key (lambda (elt) (cdr elt)))))
              (- (first fst-ku) (first fst-kd)))) dgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (2G-1Dur (extract-digraphs *la *dgs)))

(defun 2G-1KeyLat (dgs)
  (let ((times-lst (mapcar (lambda (dg)
                             (let* ((fst-kd (first dg))
                                    (fst-ku (find `("keyup" ,(third fst-kd)) dg :test 'equalp :key (lambda (elt) (cdr elt))))
                                    (pos-fst-ku (position `("keyup" ,(third fst-kd)) dg :test 'equalp :key (lambda (elt) (cdr elt))))
                                   (nxt-kd (find "keydown" (subseq dg (1+ pos-fst-ku)) :test 'string= :key (lambda (elt) (second elt)))))
                               (if (null (first nxt-kd))
                                   0
                                   (- (first nxt-kd) (first fst-ku)))
                               )) dgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (2G-1KeyLat (extract-digraphs *la *dgs)))

(defun 2G-2Dur (dgs)
       (let ((times-lst (mapcar (lambda (dg)
                                  (let* (
                                        (snd-kd (find "keydown" dg :test 'string= :key (lambda (elt) (second elt)) :from-end t))
                                        (snd-ku (find `("keyup" ,(third snd-kd)) dg
                                                      :test 'equalp :key (lambda (elt) (cdr elt)) :from-end t)))
                                    (- (first snd-ku) (first snd-kd)))) dgs)))
         `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (2G-2Dur (extract-digraphs *la *dgs)))

(defun 2G-Dur (dgs)
  (let ((times-lst (mapcar (lambda (dg)
            (let ((fst-kd (first dg))
                  (snd-ku (find "keyup" dg :test 'string= :key (lambda (elt) (second elt)) :from-end t)))
              (- (first snd-ku) (first fst-kd)))) dgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (2G-Dur (extract-digraphs *la *dgs)))

(defun 2G-NumEvents (dgs)
  (let ((numevents-lst (mapcar (lambda (dg)
                             (length dg)) dgs)))
    `((:mean ,(mean numevents-lst)) (:sd ,(sd numevents-lst)))))

;;(time (2G-NumEvents (extract-digraphs *la *dgs)))

(defun 3G-1D2D (tgs)
  (let ((times-lst (mapcar (lambda (tg)
            (let ((fst-kd (first tg))
                  (snd-kd (find "keydown" (cdr tg) :test 'string= :key (lambda (elt) (second elt)))))
              (- (first snd-kd) (first fst-kd)))) tgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (3G-1D2D (extract-trigraphs *la *tgs)))

(defun 3G-1Dur (tgs)
  (let ((times-lst (mapcar (lambda (tg)
            (let* ((fst-kd (first tg))
                   (fst-ku (find `("keyup" ,(third fst-kd)) tg :test 'equalp :key (lambda (elt) (cdr elt)))))
              (- (first fst-ku) (first fst-kd)))) tgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (3G-1Dur (extract-trigraphs *la *tgs)))

(defun 3G-1KeyLat (tgs)
  (let ((times-lst (mapcar (lambda (tg)
                             (let* (
                                    (fst-kd (first tg))
                                    (fst-ku (find `("keyup" ,(third fst-kd)) tg :test 'equalp :key (lambda (elt) (cdr elt))))
                                    (pos-fst-ku (position `("keyup" ,(third fst-kd)) tg :test 'equalp :key (lambda (elt) (cdr elt))))
                                    (nxt-kd (find "keydown" (subseq tg (1+ pos-fst-ku)) :test 'string= :key (lambda (elt) (second elt)))))
                               (if (null (first nxt-kd))
                                   0
                                   (- (first nxt-kd) (first fst-ku)))
                               )) tgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (3G-1KeyLat (extract-trigraphs *la *tgs)))

(defun 3G-2D2D (tgs)
  (let ((times-lst (mapcar (lambda (tg)
            (let ((snd-kd (find "keydown" (cdr tg) :test 'string= :key (lambda (elt) (second elt))))
                  (trd-kd (find "keydown" tg :test 'string= :key (lambda (elt) (second elt)) :from-end t)))
              (- (first trd-kd) (first snd-kd)))) tgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (3G-2D2D (extract-trigraphs *la *tgs)))

(defun 3G-2Dur (tgs)
  (let ((times-lst (mapcar (lambda (tg)
            (let* ((snd-kd (find "keydown" (cdr tg) :test 'string= :key (lambda (elt) (second elt))))
                   (snd-ku (find `("keyup" ,(third snd-kd)) tg :test 'equalp :key (lambda (elt) (cdr elt)))))
              (- (first snd-ku) (first snd-kd)))) tgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (3G-2Dur (extract-trigraphs *la *tgs)))

(defun 3G-2KeyLat (tgs)
  (let ((times-lst (mapcar (lambda (tg)
                             (let* ((snd-ku (let ((c 0))
                                              (find '("keyup" 2) tg :test 'equalp :key 
                                                    (lambda (elt)
                                                      (when (string= "keyup" (second elt))
                                                        (incf c)
                                                        (list (second elt) c))))))
                                    (pos-snd-ku (let ((c 0))
                                              (position '("keyup" 2) tg :test 'equalp :key 
                                                    (lambda (elt)
                                                      (when (string= "keyup" (second elt))
                                                        (incf c)
                                                        (list (second elt) c))))))

                                    (nxt-kd (let ((c 0))
                                                  (find '("keydown" 3) tg :test 'equalp :key 
                                                            (lambda (elt)
                                                              (when (string= "keydown" (second elt))
                                                                (incf c)
                                                                (list (second elt) c))))))
                                    (pos-nxt-kd (let ((c 0))
                                                  (position '("keydown" 3) tg :test 'equalp :key 
                                                            (lambda (elt)
                                                              (when (string= "keydown" (second elt))
                                                                (incf c)
                                                                (list (second elt) c)))))))
                               (if (or (null (first nxt-kd)) (< pos-nxt-kd pos-snd-ku))
                                   0
                                   (- (first nxt-kd) (first snd-ku))))) tgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (3G-2KeyLat (extract-trigraphs *la *tgs)))

(defun 3G-3Dur (tgs)
  (let ((times-lst (mapcar (lambda (tg)
                             (let* ((trd-kd (let ((c 0))
                                              (find '("keydown" 3) tg :test 'equalp :key 
                                                    (lambda (elt)
                                                      (when (string= "keydown" (second elt))
                                                        (incf c)
                                                        (list (second elt) c))))))
                                    (pos-trd-kd (let ((c 0))
                                              (position '("keydown" 3) tg :test 'equalp :key 
                                                    (lambda (elt)
                                                      (when (string= "keydown" (second elt))
                                                        (incf c)
                                                        (list (second elt) c))))))
                                    (trd-ku (find `("keyup" ,(third trd-kd)) (subseq tg (1+ pos-trd-kd))
                                                  :test 'equalp :key (lambda (elt) (cdr elt)))))
                                   (- (first trd-ku) (first trd-kd)))) tgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (3G-3Dur (extract-trigraphs *la *tgs)))

(defun 3G-Dur (tgs)
  (let ((times-lst (mapcar (lambda (tg)
            (let* ((fst-kd (first tg))
                   (trd-ku (car (last tg))))
              (- (first trd-ku) (first fst-kd)))) tgs)))
    `((:mean ,(mean times-lst)) (:sd ,(sd times-lst)))))

;;(time (3G-Dur (extract-trigraphs *la *tgs)))

(defun 3G-NumEvents (tgs)
  (let ((numevents-lst (mapcar (lambda (tg)
                             (length tg)) tgs)))
    `((:mean ,(mean numevents-lst)) (:sd ,(sd numevents-lst)))))

;;(time (3G-NumEvents (extract-trigraphs *la *tgs)))

;;
;; Non-Graph Keystroke Features
;;

(defun NumBackspaces (ks)
  (let ((c 0))
    (map nil (lambda (k)
               (when (= (third k) 8)
                 (incf c))) ks)
    c))

;;(NumBackspaces *ks)

(defun NumKeystrokes (ks)
  (count "keydown" ks :test 'string= :key (lambda (elt) (second elt))))

;;(NumKeystrokes *ks)

(defun ActivityTime (ks)
  (- (caar (last ks))
     (caar ks)))

;;(ActivityTime *ks)

;;
;; Mouse Dynamics Features
;;

(defun MousePressesDur (mps)
  "Deltas between each mousepress. Mean and Standard Deviation as result."
  (let ((deltas (remove nil (maplist (lambda (mp)
                                       (when (and (second mp)
                                                  (string= "mousedown" (second (first mp)))
                                                  (string= "mouseup" (second (second mp))))
                                         (- (car (second mp))
                                            (car (first mp))))) mps))))
    `((:mean ,(mean deltas)) (:sd ,(sd deltas)))))

;;(MousePressesDur *mps)

(defun MouseMovementDur (mms)
  (let ((deltas (remove nil (maplist (lambda (mm)
                                       (when (second mm)
                                         (let ((delta (- (car (second mm))
                                                         (car (first mm)))))
                                           (when (> delta 150)
                                             delta)))) mms))))
    `((:mean ,(mean deltas)) (:sd ,(sd deltas)))))

;;(MouseMovementDur *mms)

(defun MouseMovementX (mms)
  (let ((deltas (remove nil (maplist (lambda (mm)
                                       (when (second mm)
                                         (abs (- (cadr (second mm))
                                                 (cadr (first mm)))))) mms))))
    `((:mean ,(mean deltas)) (:sd ,(sd deltas)))))

;;(MouseMovementX *mms)

(defun MouseMovementY (mms)
  (let ((deltas (remove nil (maplist (lambda (mm)
                                       (when (second mm)
                                         (abs (- (caddr (second mm))
                                                 (caddr (first mm)))))) mms))))
    `((:mean ,(mean deltas)) (:sd ,(sd deltas)))))

;;(MouseMovementY *mms)


;;
;; Generating Results
;;

(defun csv (final)
  (with-open-file (str "~/thesis.csv" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format str (apply #'concatenate 'string "2G_1D2D_MEAN,2G_1D2D_SD,2G_1Dur_MEAN,2G_1Dur_SD,2G_1KeyLat_MEAN,2G_1KeyLat_SD,2G_2Dur_MEAN,2G_2Dur_SD,2G_Dur_MEAN,2G_Dur_SD,2G_NumEvents_MEAN,2G_NumEvents_SD,3G_1D2D_MEAN,3G_1D2D_SD,3G_1Dur_MEAN,3G_1Dur_SD,3G_1KeyLat_MEAN,3G_1KeyLat_SD,3G_2D2D_MEAN,3G_2D2D_SD,3G_2Dur_MEAN,3G_2Dur_SD,3G_2KeyLat_MEAN,3G_2KeyLat_SD,3G_3Dur_MEAN,3G_3Dur_SD,3G_Dur_MEAN,3G_Dur_SD,3G_NumEvents_MEAN,3G_NumEvents_SD,Mouse_Presses_Dur_MEAN,Mouse_Presses_Dur_SD,Mouse_Movement_Dur_MEAN,Mouse_Movement_Dur_SD,Mouse_Movement_X_MEAN,Mouse_Movement_X_SD,Mouse_Movement_Y_MEAN,Mouse_Movement_Y_SD,Attempts,Frustration,Boredom,Distraction,Relaxation,Focus,Excitement
"
         (remove nil (mapcar (lambda (f)
                               (if (and (= (length f) 40) (not (some #'null f)) (not (null (car (last f)))))
                                   (format nil "~{~a,~}~{~a~^,~}~%" (subseq f 0 39) (car (last f))))) final))))))

(defun final (exercises)
  (mapcar (lambda (e)
            (let* ((ula (prune-la (decode-json-from-string (clean-pythons-json (caar (first e))))))
                   (eesm (prune-eesm (decode-json-from-string (clean-pythons-json (caaadr e)))))
                   (ks (preproc-keystrokes ula))
                   (mms (preproc-mousemovement ula))
                   (mps (preproc-mousepresses ula))
                   (attempts (third e)))
              ;;(error ula)
              ;;(error ks)
              (append
               (when ks
                 (let ((dgs (extract-digraphs ks *dgs))
                       (tgs (extract-trigraphs ks *tgs)))
                   (if (and dgs tgs)
                     (list
                      ;;(error ks)
                  (cadr (assoc :mean (2G-1D2D dgs)))
                  (cadr (assoc :sd (2G-1D2D dgs)))
                  (cadr (assoc :mean (2G-1Dur dgs)))
                  (cadr (assoc :sd (2G-1Dur dgs)))
                  (cadr (assoc :mean (2G-1KeyLat dgs)))
                  (cadr (assoc :sd (2G-1KeyLat dgs)))
                  (cadr (assoc :mean (2G-2Dur dgs)))
                  (cadr (assoc :sd (2G-2Dur dgs)))
                  (cadr (assoc :mean (2G-Dur dgs)))
                  (cadr (assoc :sd (2G-Dur dgs)))
                  (cadr (assoc :mean (2G-NumEvents dgs)))
                  (cadr (assoc :sd (2G-NumEvents dgs)))

                  (cadr (assoc :mean (3G-1D2D tgs)))
                  (cadr (assoc :sd (3G-1D2D tgs)))
                  (cadr (assoc :mean (3G-1Dur tgs)))
                  (cadr (assoc :sd (3G-1Dur tgs)))
                  (cadr (assoc :mean (3G-1KeyLat tgs)))
                  (cadr (assoc :sd (3G-1KeyLat tgs)))
                  (cadr (assoc :mean (3G-2D2D tgs)))
                  (cadr (assoc :sd (3G-2D2D tgs)))
                  (cadr (assoc :mean (3G-2Dur tgs)))
                  (cadr (assoc :sd (3G-2Dur tgs)))
                  (cadr (assoc :mean (3G-2KeyLat tgs)))
                  (cadr (assoc :sd (3G-2KeyLat tgs)))
                  (cadr (assoc :mean (3G-3Dur tgs)))
                  (cadr (assoc :sd (3G-3Dur tgs)))
                  (cadr (assoc :mean (3G-Dur tgs)))
                  (cadr (assoc :sd (3G-Dur tgs)))
                  (cadr (assoc :mean (3G-NumEvents tgs)))
                  (cadr (assoc :sd (3G-NumEvents tgs)))
                  )
                     '(NIL))))
               (when mps
                 (list
                  (cadr (assoc :mean (MousePressesDur mps)))
                  (cadr (assoc :sd (MousePressesDur mps)))
                  ))
               (when mms
                 ;;(error mms)
                (list
                 (cadr (assoc :mean (MouseMovementDur mms)))
                 (cadr (assoc :sd (MouseMovementDur mms)))
                 (cadr (assoc :mean (MouseMovementX mms)))
                 (cadr (assoc :sd (MouseMovementX mms)))
                 (cadr (assoc :mean (MouseMovementY mms)))
                 (cadr (assoc :sd (MouseMovementY mms)))
              ))
               (list attempts)
               (list eesm)
              )
              )) exercises))

(defparameter *final (final *exercises))

;;We print the result to a csv file
;;(csv *final)
