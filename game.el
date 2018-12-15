(require 'cl)

(setq jb-game-properties 
      (list :bot-color "red")
      current-player-id 1)

(defvar jb-game-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'jb-game-read-print)
    map))

(define-derived-mode jb-game-mode text-mode "jb-game"
  "Major mode for running the jb-game program.  Like Text mode with Auto Fill mode except that RET when point is after a newline, or LFD at any time, reads the sentence before point, and prints jb-game's answer."
  (game-structure)
  (turn-on-auto-fill)
  (insert (jb-game-process "jb-game-start"))
  (insert "\n>"))

(defun jb-game ()
  "Switch to jb-game buffer and start interacting with user."
  (interactive)
  (switch-to-buffer "*jb-game*")
  (jb-game-mode))

(defun jb-game-ret-or-read (arg)
  (interactive "*p")
  (insert "\n>")
  (jb-game-read-print))

(defun jb-game-read-print ()
  "Top level loop."
  (interactive)
  (insert "\n")
  (insert (jb-game-process (jb-game-read-sentence)))
  (insert "\n>"))

(defun string-trim(s)
  (replace-regexp-in-string "^\\( \\|\n\\)+\\|\\( \\|\n\\)+$" "" s))

(defun delete-trailing-blank-lines ()
          "Deletes all blank lines at the end of the file, even the last one"
          (interactive)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-max))
              (delete-blank-lines)
              (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
                (if (> trailnewlines 0)
                    (progn
                      (delete-char trailnewlines)))))))

(defun jb-game-read-sentence ()
  (goto-char (point-max))
  (search-backward ">" nil t)
  (forward-char 1)
  (let* ((input (string-trim 
                 (if (> (point-max) (point))
                     (buffer-substring (point) (point-max))
                   ""))))
    (logit (format "input: %s" input))
    (goto-char (point-max))
    (delete-trailing-blank-lines)
    (goto-char (point-max))
    input))

(defun jb-game-input-words (input)
  (mapcar 'downcase (split-string input "[^a-z0-9-_']+" t)))

(defun jb-game-process (input)
  (let* ((words (jb-game-input-words input))
         (response (jb-game-process-words words))
         (bot-color (getf jb-game-properties :bot-color "red")))
    (add-text-properties 0 (length response)
                         `(face (:foreground ,bot-color))
                         response)
    response))

(defmacro dispatch (words &rest body)
  `(cond
    ,@(loop with words = (car words)
            for element in body
            for match = (car element)
            for target = (second element)
            collect `((match-words ',match ,words)
                      (,target remaining-words)))
    (t (jg-unknown-command ,(car words)))))

(defun jb-game-process-words (words)
  (dispatch (words)
    (("jb-game-start") jg-start-command)
    (("ping") jg-ping-command)
    (("go" "move" "climb" "walk" "run") jg-move-command)
    ((("pick" "up") "pick" "grab" "take" "get") jg-pickup-command)
    ((("let" "go" "of") "drop" "discard" "dump") jg-drop-command)
    ((("show" "pockets") "list" "enumerate") jg-list-command)
    ((("where" "am" "i") ("examine" "room") ("look") ("what's" "up") "describe" "hello" "hi") jg-describe-command)
    (("reset" "restart" "reboot") jg-reset-command)
    (("please" "kindly") jg-please-command)
    (("use") jg-use-command)
    (("unlock") jg-lock-command)
    (("bounce" "throw") jg-bounce-command)
    ((("use" "clock") ("change" "time") ("move" "hand") ("move" "clockhand")) jg-time-change-command)
    ((("good" "bye") "bye" "good-bye") jg-goodbye-command)))

(defun match-words (match-list words)
  (logit (format "matching '%s' to '%s'" match-list words))
  (loop for match-item in match-list
        for match = (if (listp match-item) match-item (list match-item))
        for words-sublist = (subseq words 0
                                    (if (<= (length match) (length words))
                                        (length match)
                                      1))
        do (logit "match=" match "; words=" words-sublist)
        when (equal match words-sublist)
        do (progn
             (logit "found match: " match)
             (setq remaining-words (subseq words (length match)))
             (logit "remaining words: " remaining-words)
             (return t))
        finally (progn
                  (logit "no match")
                  (setq remaining-words words)
                  (return nil))))

(defun remaining-words () remaining-words)

(defun jg-unknown-command (words)
  (jb-game-output "Unknown command."))

(defun jg-ping-command (words)
  (jb-game-output "pong."))

(defun jg-please-command (words)
  (jb-game-process-words words))

(defun jg-goodbye-command (words)
  (jb-game-output "Good bye."))

(defun jg-start-command (words)
  (jb-game-output "Welcome to jb-game."))

(defun jb-game-output (&rest messages)
  (setq words nil)
  (concat "\n" (mapconcat 'identity messages "")))

(defun logit (&rest messages)
  (with-current-buffer (get-buffer-create "log")
    (goto-char (point-max))
    (insert
     (mapconcat
      (lambda (s) (if (listp s) (mapconcat 'identity s " ") s))
      messages ""))
    (insert "\n")))

(defun jg-reset-command (words)
  (game-structure)
  (erase-buffer)
  (jb-game-output "Welcome to jb-game."))

(defun jg-list-command (words)
  (let* ((player-id current-player-id)
         (pocket (get-attribute :players player-id :pocket))
         (thing-names (mapcar (lambda (x) (get-attribute :things x :name))
                             pocket)))
    (if (null pocket)
        (jb-game-output "You are destitue.")
      (jb-game-output
       (format "You have possession of the following items:\n%s."
               (mapconcat 'identity thing-names ", "))))))

(defun jg-describe-command (words)
  (let* ((player-id current-player-id)
         (room-id (get-attribute :players player-id :room)))
    (jg-describe-room room-id)))

(defun jg-describe-room (room-id)
  (let* ((player-id current-player-id)
         (player (select-by-id :players player-id))
         (pocket (getf player :pocket))
         (directions (list-keys (get-attribute :rooms room-id :directions)))
         (things-in-room (remove-if-not
                          (lambda (x)
                            (and (null (getf x :owner))
                                 (not (member x pocket))
                                 (= (getf x :room) room-id)))
                          (getf game-structure :things)))
         (description (format "You are now in room %s.\n%s"
                              (get-attribute :rooms room-id :name)
                              (get-attribute :rooms room-id :description))))
    (when things-in-room
      (setf description
            (concat 
             description 
             (format "\nThe following things are in the room: %s."
                     (mapconcat (lambda (x) (getf x :name))
                                things-in-room ", ")))))
    (loop for direction in directions
          collect (format "There's a room %s %s."
                          (if (member direction '(:up :down))
                              "if you go"
                            "to the")
                          (substring (symbol-name direction) 1))
          into directions
          finally
          (return
           (jb-game-output description "\n"
(mapconcat 'identity directions "\n"))))))

                                        ;attempt to make portal locked condition for moving

;(defun jg-move-command (words)
;  (let* ((valid-directions (getf game-structure :valid-directions))
;        (direction (intern-soft (format ":%s" (car words))))
;        (player-id current-player-id))
;    (if (member direction valid-directions)
;        (let ((new-room-id (move player-id direction)))
;          (cond ((equal result :portal-locked)
;                 (jb-game-output
;                  (format "This portal is locked")))
;                ((equal new-room-id :invalid-direction)
;                 (jb-game-output
;                  (format "There's nothing in that direction.")))
;                (jg-describe-room new-room-id)))
;      (jb-game-output (format "Unkown direction '%s'!" (car words))))))

(defun jg-move-command (words)
  (let ((valid-directions (getf game-structure :valid-directions))
        (direction (intern-soft (format ":%s" (car words))))
        (player-id current-player-id))
    (if (member direction valid-directions)
        (let ((new-room-id (move player-id direction)))
          (if (equal new-room-id :invalid-direction)
              (jb-game-output "There's nothing in that direction.")
            (jg-describe-room new-room-id)))
      (jb-game-output (format "Unkown direction '%s'!" (car words))))))


(defun jg-pickup-command (words)
  (logit "jg-pickup-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that"))
        do (setq words (cdr words)))
  (logit "words after cleanup " words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (pickup player-id thing-id)))
    (cond ((equal result :thing-already-in-pocket)
           (jb-game-output (format "You already have the %s." thing-name)))
          ((equal result :personal-space)
           (jb-game-output (format "I don't think the %s would appreciate that." thing-name)))
          ((equal result :too-heavy)
           (jb-game-output (format "The %s is too heavy." thing-name)))
          ((equal result :thing-owned-by-other-player)
           (jb-game-output
            (format "The %s is already in someone else's possession.")))
          ((equal result :thing-not-in-room)
           (jb-game-output (format "There's no %s in this room.")))
          ((equal result :unknown-thing)
           (jb-game-output
            (format "The thing you want, %s, doesn't exist in this world."
                    thing-name)))
          (t (jb-game-output
              (format "You now have possession of the %s." thing-name))))))

(defun jg-lock-command (words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (lock player-id thing-id)))
    (cond ((equal result :no-key)
           (jb-game-output (format "You need a key.")))
          ((equal result :thing-not-lockable)
           (jb-game-output
            (format "Thing %s is not lockable." thing-name)))
          ((equal result :no-key)
           (jb-game-output
            (format "You must have a key to unlock the %s."
                    thing-name)))
          ((equal result :already-locked)
           (jb-game-output
            (format "The %s is already locked." thing-name)))
          (t (jb-game-output
              (format ("You have locked the %s." thing-name)))))))

(defun lock (player-id thing-id)
  (let* ((pocket (get-attribute :players player-id :pocket))
         (locked (get-attribute :things thing-id :locked))
         (key-id 20))
    (cond ((null locked) :thing-not-lockable)
          ((not (member key-id pocket))
           :no-key)
          ((equal locked 1) :already-locked)
          (t (set-attribute :things thing-id :locked 2)))))

(defun jg-drop-command (words)
  (when (equal (car words) "the") (setq words (cdr words)))
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (drop player-id thing-id)))
    (cond ((equal result :thing-not-in-pocket)
           (jb-game-output 
            (format "The %s is not yours to drop." thing-name)))
          ((equal result :unknown-thing)
           (jb-game-output 
            (format "The %s doesn't exist." thing-name)))
          (t (jb-game-output
              (format "You no longer have possession of the %s." thing-name))))))

(cl-defun list-keys (l &key sorted (sort-predicate #'string<))
  (loop for a from 0 below (length l) by 2
        collect (elt l a) into keys
        finally (return (if sorted (sort keys sort-predicate) keys))))

(defun select-by-id (list-key id)
  (first (remove-if-not
          (lambda (x) (= (getf x :id) id))
          (getf game-structure list-key))))

(defun name-to-id (list-key name)
  (loop with name-list = (if (listp name) name (list name))
        for element in (getf game-structure list-key)
        for element-name-list = (split-string 
                                 (getf element :name)
                                 " +")
        when (match-words element-name-list name-list)
        do (progn
             (setf words remaining-words)
             (return (getf element :id)))))
  
(defun id-to-index (list-key id)
  (loop with list = (getf game-structure list-key)
        for index from 0 below (length list)
        for element in list
        for element-id = (getf element :id)
        when (= id element-id)
        do (return index)))

(defun set-attribute (list-key id attribute value)
  (setf (getf (elt (getf game-structure list-key) 
                   (id-to-index list-key id))
              attribute)
        value))

(defun set-attribute (list-key id attribute value)
  (setf (getf (elt (getf game-structure list-key) 
                   (id-to-index list-key id))
              attribute) value))

(defun get-attribute (list-key id attribute)
  (getf (elt (getf game-structure list-key) 
             (id-to-index list-key id))
        attribute))
  
(defun pickup (player-id thing-id)
  (cond ((null thing-id) :unknown-thing)
        ((equal (get-attribute :things thing-id :owner)
                player-id)
         :thing-already-in-pocket)
        ((member thing-id (get-attribute :players player-id :pocket))
         :thing-already-in-pocket)
        ((get-attribute :things thing-id :owner)
         :thing-owned-by-other-player)
        ((get-attribute :things thing-id :creature)
         :personal-space)
        ((get-attribute :things thing-id :heavy)
         :too-heavy)
        ((not (equal (get-attribute :players player-id :room)
                     (get-attribute :things thing-id :room)))
         :thing-not-in-room)
        (t (let ((pocket (get-attribute :players player-id :pocket)))
             (push thing-id pocket)
             (set-attribute :players player-id :pocket pocket)
             (set-attribute :things thing-id :owner player-id)
             thing-id))))

(defun drop (player-id thing-id)
  (if (null thing-id)
      :unknown-thing
    (let ((pocket (get-attribute :players player-id :pocket)))
      (if (or (not (member thing-id pocket))
              (not (equal player-id (get-attribute :things thing-id :owner))))
          :thing-not-in-pocket
        (let ((pocket (remove thing-id pocket)))
          (set-attribute :players player-id :pocket pocket)
          (set-attribute :things thing-id :owner nil))))))

                                        ;attempt to make room portal locked condition
;(defun move (player-id direction)
;  (let* ((current-room-id (get-attribute :players player-id :room))
;         (locked (get-attribute :room room-id :portal-locked))
;         (pocket (get-attribute :players player-id :pocket))
;         (valid-directions (get-attribute :rooms current-room-id
;                                           :directions)))
;    (if (member direction (list-keys valid-directions))
;        (let ((new-room (getf valid-directions direction)))
;          (set-attribute :players player-id :room new-room)
;          (loop for thing-id in pocket
;                do (set-attribute :things thing-id :room new-room))
;          new-room)
;      :invalid-direction)))



(defun move (player-id direction)
  (let* ((current-room-id (get-attribute :players player-id :room))
         (pocket (get-attribute :players player-id :pocket))
         (valid-directions (get-attribute :rooms current-room-id
                                           :directions)))
    (if (member direction (list-keys valid-directions))
        (let ((new-room (getf valid-directions direction)))
          (set-attribute :players player-id :room new-room)
          (loop for thing-id in pocket
                do (set-attribute :things thing-id :room new-room))
          new-room)
      :invalid-direction)))



; make a thing-in-others-pocket
;(defun use (thing-id player-id)
;  (if ((null thing-id)
;      :unknown-thing (jb-game-output (format "The %s isn't in your pocket and you can't see it in this room." thing-name))
;    (let (get-attribute :things thing-id :room room-id))
;      (if (or (not (member))
;              (not (equal thing-id (get-attribute :things thing-id :owner))))
;          :thing-in-others-pocket (jb-game-output (format "The %s isn't in your pocket and you can't see it in this room" thing-name))
;          (let
;              (if (get-attribute :things thing-id :interactible equals t list-keys)
;                  (jb-game-output (format "Nothing interesting happened"))))))))

(defun jg-use-command (words)
  (logit "jg-use-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that"))
        do (setq words (cdr words)))
  (let* ((player current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (player-location (getf player :room))
         (thing-location (getf thing :room))
         (usable (getf thing :usable)))
    (cond ((equal result :usable)
             (format "Using %s." (get-attribute :things thing-id :name)))
          ((jb-game-output "Thing %s is not usable." thing-name))
          (not ((equal result :player-location))
               (jb-game-output "The %s is not in this room." thing-name))
          (t (get-attribute :things thing-id :usable list-keys)))))

    
(defun dc-use (player-id thing-id)
  (let* ((player (select-by-id player-id))
         (thing (select-by-id thing-id))
         (thing-name (getf thing :name))
         (pocket (getf player :pocket))
         (usable (getf thing :usable))
         (interact (getf thing :use-type "interact"))
         (bounce (getf thing :use-type "bounce"))
         (cut (getf thing :use-type "cut"))
         (swing (getf thing :use-type "swing"))
         (jab (getf thing :use-type "jab"))
         (move (getf thing :use-type "move"))
         (turn (getf thing :use-type "turn"))
         (rotate (getf thing :use-type "rotate"))
         (open (getf thing :use-type "open"))
         (unlock (getf thing :use-type "unlock"))
         (search (getf thing :use-type "search"))
         (unlock (getf thing :use-type "unlock"))
         (turn (getf thing :use-type "turn"))
         (fix (getf thing :use-type "fix"))
         (turn-on (getf thing :use-type "turn on"))
         (turn-off (getf thing :use-type "turn off"))
         (shine (getf thing :use-type "shine"))
         (spray (getf thing :use-type "spray"))
         (squirt (getf thing :use-type "squirt"))
         (operate (getf thing :use-type "operate"))
         (recharge (getf thing :use-type "recharge"))
         (open-a-portal (getf thing :use-type "open a portal"))
         (open-a-portal-with (getf thing :use-type "open a portal with"))
         (pour (getf thing :use-type "pour"))
         (dump (getf thing :use-type "dump"))
         (fill (getf thing :use-type "fill"))
         (refill (getf thing :use-type "refill"))
         (insert (getf thing :use-type "insert"))
         (replace (getf thing :use-type "replace"))
         (put (getf thing :use-type "put")))
    (if (member thing-id room)
        (if usable
            (jb-game-output
             (format "Using %s." (get-attribute :things thing-id :name)))
          (jb-game-output "Thing %s is not usable." thing-name))
      (jb-game-output "The %s is not in this room to use." thing-name))))

(defun jg-lock-command (words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (lock player-id thing-id)))
    (cond  ((equal result :thing-not-lockable)
           (jb-game-output
            (format "Thing %s is not lockable." thing-name)))
          ((equal result :no-key)
           (jb-game-output
            (format "You must have a key to unlock the %s."
                    thing-name)))
          ((equal result :already-locked)
           (jb-game-output
            (format "The %s is already locked." thing-name)))
          (t (jb-game-output
              (format ("You have locked the %s." thing-name)))))))

(defun jg-bounce-command (words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (player-location (getf player :room))
         (result (bounce player-id thing-id)))
    (cond ((equal result :no-ball)
           (jb-game-output (format "You can't bounce what you don't have")))
          (t (jb-game-output
              (format ("You bounce the %s off the floor.  That was fun." thing-name)))))))


(defun bounce (player-id thing-id)
  (let* ((player-id current-player-id)
         (pocket (get-attribute :players player-id :pocket))
         (bounce (get-attribute :things thing-id :bounce-able)))
    (if (member thing-id pocket)
        (if usable
            (jb-game-output
             (format "Using %s." (get-attribute :things thing-id :name)))
(jb-game-output "Thing %s is not usable." thing-name))
(jb-game-output "The %s is not in this room to use." thing-name))))

(defun lock (player-id thing-id)
  (let* ((pocket (get-attribute :players player-id :pocket))
         (locked (get-attribute :things thing-id :lockable)))
    (cond ((null locked) :thing-not-lockable)
          ((not (member key-id pocket))
           :no-key)
          ((equal locked 2) :already-locked)
          (t (set-attribute :things thing-id :locked 2)))))

(defun jab (player-id thing-id)
  (let* ((pocket (get-attribute :players player-id :pocket))
         (jab (get-attribute :things thing-id :jab-able)))
    (cond ((not (member key-id pocket))
           :no-sword)
    (t (jb-game-output
        (format ("I know this thing isn't sharp, but you shouldn't swing it around willy nilly." thing-name)))))))

(defun jg-bounce-command (words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (jab player-id thing-id)))
    (cond ((equal result :no-sword)
           (jb-game-output (format "You have most impressive skills with imaginary cutlery.  All of the imaginary pirates are terrified of you.  I wonder how you would fair if you had a not-pretend %s in your possesion?" thing-name)))
          (t (jb-game-output
              (format ("I know this %s isn't sharp, but you shouldn't swing it around willy nilly." thing-name)))))))

(defun time-change (player-id thing-id)
  (let* ((pocket (get-attribute :players player-id :pocket))
         (clock (get-attribute :things thing-id :time-change)))
    (cond ((null time-change) :not-a-clock)
          ((equal clock 1) :on-break)
          ((equal clock 2) :break-over)
          (t (set-attribute :things thing-id :time-change 2)))))

(defun jg-time-change-command (words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (clock player-id thing-id)))
    (cond ((equal result :not-a-clock)
           (jb-game-output (format "You can't change the time on that.")))
          ((equal result :not-a-clock)
           (jb-game-output
            (format "Thing %s is not lockable." thing-name)))
          ((equal result :thing-not-in-room)
           (jb-game-output
            (format "There is no %s in this room" thing-name)))
          ((equal result :break-over)
           (jb-game-output
            (format "You already fiddled with it.  You should leave the %s alone before you break it." thing-name)))
          (t (jb-game-output
              (format ("You move the hour hand forward.")))))))

(defun replace-batteries (player-id thing-id)
  (let* ((pocket (get-attribute :players player-id :pocket))
         (batteries-replaceable (get-attribute :things thing-id :batteries-replaceable))
         (batteries-id 31))
    (cond ((null batteries-replaceable) :batteries-not-replaceable)
          ((not (member batteries-id pocket))
           :no-batteries)
          ((equal batteries-replaceable 2) :already-replaced)
          (t (set-attribute :things thing-id :batteries-replaceable 2)))))

(defun refill (plater-id thing-id)
  (let* ((pocket (get-attribute :platers plater-id :pocket))
         (refill (get-attribute :things thing-id :refill-able))
         (water-id 30))
    (cond ((null refill) :refill-able)
          ((not (member water-id pocket))
           :no-water)
          ((equal refill 2) :already-filled)
          (t (set-attribute :things thing-id :refill-able 2)))))

(defun jg-refill-command (words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (lock player-id thing-id)))
    (cond  ((equal result :thing-not-refillable)
           (jb-game-output
            (format "Thing %s is not lockable." thing-name)))
          ((equal result :no-key)
           (jb-game-output
            (format "You must have a key to unlock the %s."
                    thing-name)))
          ((equal result :already-locked)
           (jb-game-output
            (format "The %s is already locked." thing-name)))
          (t (jb-game-output
              (format ("You have locked the %s." thing-name)))))))


(defun game-structure ()
  (setq
   game-structure
   (list :rooms (list
                 (list :name "Lobby"
                       :id 0
                       :directions
                       (list
                        :north 1
                        :east 2
                        :south 3
                        :west 4
                        :down 999)
                       :description
                       "You stand in a room that aches with boredom.  You would rather gnaw off your leg than stand another moment in this place.  It must be a government building."
                       :room-puzzle nil)
                 
                 (list :name "Home"
                       :id 999
                       :portal-locked t
                       :directions
                       ()
                       :description
                       "You made it home!  You win the game"
                       :room-puzzle nil)
                 
                 (list :name "Service"
                       :id 1
                       :directions
                       (list :south 0)
                       :description
                       (list "Grouchy zombies shuffle around behind service desks.  Presumably, their job is to help people get paper work squared away as their number is called over the loud speaker, but no numbers are being called."
                       "Grouchy zombies shuffle around behind a service desk.  As peoples numbers are called, they walk up to the zombies for assistance with their paper work.")
                       :room-puzzle nil)
                 
                 (list :name "File Room"
                       :id 2
                       :directions
                       (list :west 0)
                       :description "This room has a large, important looking filing cabinet.  The walls are a blinding yellow color that hurt your eyes if you look too long."
                       :room-puzzle nil)
                 
                 (list :name "Storage Room"
                       :id 3
                       :directions
                       (list :north 0)
                       :description
                       "A janitor is sleeping on a cot in the corner.  Various cleaning items are scattered around the room."
                       :room-puzzle nil)
                 
                 (list :name "Waiting Room"
                       :id 4
                       :directions
                       (list :east 0)
                       :description
                       "Creatures are sitting on chairs as they wait for their number to be called so that they can get different sorts of paper work squared away.  The gray carpet beneath your feet is sticky.  Why is it sticky?"
                       :room-puzzle nil))
                 

         :players (list
                   (list :id 1 :room 0 :pocket nil))

         :things (list
                  (list :name "mirror"
                        :id 1
                        :owner nil
                        :room 0
                        :description "There is a myserious figure inside."
                        :retrievable t
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :contains-item nil
                        :conversable t)
                  (list :name "ball"
                        :id 2
                        :owner nil
                        :room 0
                        :description "This ball is round and sqeaky."
                        :retrievable t
                        :usable t
                        :bounce-able t
                        :use-type
                        (list "interact" t
                              "bounce" t)
                        :interact-return "You bounce the ball off the floor.  That was fun"
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :contains-item nil
                        :conversable nil)
                  (list :name "sword"
                        :id 3
                        :owner nil
                        :room 0
                        :description "This sword is rusty and dull."
                        :retrievable t
                        :jab-able t
                        :usable t
                        :use-type
                        (list "interact" t
                              "cut" t
                              "swing" t
                              "jab" t)
                        :interact-return "I know this thing isn't sharp, but you shouldn't swing it around willy nilly."
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :contains-item nil
                        :converable nil)
                  (list :name "lazy zombie"
                        :id 4
                        :owner nil
                        :room 1
                        :description
                        (list "The lazy zombie is standing behind the service desk eating a sandwhich." "The lazy zombie is standing behind the service desk.  He doesn't appear to be busy.  Maybe he can help you?")
                        :retrievable nil
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :contains-item (list :id 5 :name "portal traveling permit")
                        :conversable t
                        :creature t)
                  (list :name "portal traveling permit"
                        :id 5
                        :room 1
                        :description "With these, you can travel through portals.  Maybe you can make your way home."
                        :retrievable t
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item (list :id 4 :name "lazy zombie")
                        :contains-item nil
                        :conversable nil)
                  (list :name "busy zombie"
                        :id 6
                        :room 1
                        :description "This zombie is typing something into a computer.  I guess in every office, there is one diligent worker, even if that worker is a zombie."
                        :retrievable nil
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :contains-item nil
                        :conversable t
                        :creature t)
                  (list :name "sleepy zombie"
                        :id 7
                        :room 1
                        :description "This zombie seems to be struggling to keep his eyes open as he reads a report.  Apparently, even zombies get sleepy"
                        :retrievable nil
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :contains-item nil
                        :conversable t
                        :creature t)
                  (list :name "clock"
                        :id 8
                        :room 1
                        :description "The time reads 12:00.  It doesn't seem to be working." 
                        :retrievable nil
                        :interactible
                        (list "You moved the hour hand forward" "You already fiddled with it.  Now that we know you don't have a future in the clock fixing business, you should leave it alone.")
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :time-change 1
                        :usable t
                        :use-type
                        (list "move" t
                              "turn" t
                              "rotate" t)
                        :time :1
                        :contains-item nil
                        :conversable nil
                        :stuck t)
                  (list :name "filing cabinet"
                        :id 9
                        :room 2
                        :description
                        (list "There is a large, important looking filing cabinet.  Its seems to be locked." "There is a large, important looking filing cabinet.  They are no longer locked")
                        :retrievable nil
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :heavy t
                        :usable
                        :use-type
                        (list "open" t :active :20
                              "unlock" t :active :20
                              "search" t)
                        :locked 1
                        :contains-item (list :id 10 :name "portal traveling permit papers")
                        :conversable nil)
                  (list :name "small grey key"
                        :id 20
                        :room 3
                        :description "This looks like it might unlock a filing cabinet"
                        :retrievable t
                        :usable t
                        :use-type
                        (list "rotate" t :passive :20
                              "turn" t :passive :20
                              "unlock" t :passive :20)
                        :interactible "You turn the key in the air.  Nothing interesting seems to happen, but I'll keep you posted.  You just may have unlocked the secrets of the universe.  Who knew all it took was turning a random key in the air?  All those scientists are wasting their time, they should have called you!"
                        :combinable nil
                        :combined nil
                        :inside-item (list :id 11 :name "zombie janitor")
                        :contains-item nil
                        :conversable nil)
                  (list :name "mop"
                        :id 10
                        :room 3
                        :description "This mop is broken"
                        :retrievable t
                        :interactible nil
                        :combinable (list 30)
                        :combined nil
                        :inside-item nil
                        :usable t
                        :use-type
                        (list :fix t)
                        :contains-item nil
                        :conversable nil)
                  (list :name "zombie janitor"
                        :id 11
                        :room 3
                        :description "The zombie janitor is sprawled across an old cot snoring loudly.  This guy is dead asleep"
                        :retrievable nil
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :contains-item
                        (list :id 9 :name "small grey key")
                        :conversable nil
                        :creature)
                  (list :name "flashlight"
                        :id 12
                        :room 3
                        :description "This could be handy if we had some batteries"
                        :retrievable t
                        :interactible "You switch on the flashlight, nothing happens."
                        :combinable (list 31)
                        :combined nil
                        :inside-item nil
                        :batteries-replaceable 1
                        :usable t
                        :use-type
                        (list "turn on" t
                              "turn off" t
                              "shine" t)
                        :charged 1
                        :on 1
                        :contains-item nil
                        :conversable nil)
                  (list :name "spray bottle"
                        :id 13
                        :room 3
                        :description "It is an empty spray bottle"
                        :retrievable t
                        :interactible "You use the empty spray bottle.  Nothing interesting happens.  You probably should put something inside it first"
                        :combinable (list 30)
                        :combined nil
                        :inside-item nil
                        :refill-able 1
                        :usable t
                        :use-type
                        (list "spray" t
                              "squirt" t)
                        :contains-item nil
                        :conversable nil)
                  (list :name "gnome"
                        :id 15
                        :room 4
                        :description "A restless knome with a long, white beard waits in a chair.  He is clutching a ticket."
                        :retrievable nil
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :contains-item
                        (list :id 16 :name "ticket 1-A")
                        :conversable t
                        :creature t)
                  (list :name "ticket 7A"
                        :id 16
                        :room 4
                        :description "The gnome threw this on the floor before stomping out"
                        :retrievable t
                        :interactible nil
                        :combinable nil
                        :inside-item (list :id 15 :name "gnome")
                        :contains-item nil
                        :conversable nil)
                  (list :name "lizard man"
                        :id 17
                        :room 4
                        :description "The lizard man is bored.  He is picking at his scales.  I wonder how sanitary that is?"
                        :retrievable nil
                        :interactible nil
                        :combinable nil
                        :inside-item nil
                        :contains-item nil
                        :conversable t
                        :creature t)
                  (list :name "fairy"
                        :id 18
                        :room 4
                        :description "The fairy is playing a hand held video game"
                        :retrievable nil
                        :interactible nil
                        :combinable nil
                        :inside-item nil
                        :contains-item nil
                        :converable t
                        :creature t)
                  (list :name "ticket machine"
                        :id 19
                        :room 4
                        :description "The ticket machine prints tickets with service numbers.  When your number is called, you may go to the service room and be helped"
                        :retrievable nil
                        :interactible
                        (list "You press a button and a ticket pops out.  It reads 9-Z.  The tickets everyone else has don't go past D.  Discouraged, you throw the ticket away.  Maybe you can trick someone else into giving you their ticket." "Last time you used this machine, it made you sad.  Maybe you can trick someone else into giving you their ticket.")
                        :combinable nil
                        :inside-item nil
                        :contains-item nil
                        :usable t
                        :use-type
                        (list "operate" t)
                        :conversable nil
                        :heavy t)
                  (list :name "green portal jar"
                        :id 21
                        :room 3
                        :description "If you use this in the lobby, you will make a portal to a new area.  You will be one step closer to getting home."
                        :retrievable t
                        :interactible nil
                        :combinable nil
                        :inside-item nil
                        :use-type
                        (list "recharge" t
                              "open a portal" t
                              "open a portal with" t)
                        :contains-item nil
                        :conversable nil)
                  (list :name "water"
                        :id 30
                        :room 3
                        :description "A bucket of water."
                        :retrievable t
                        :interactible nil 
                        :combinable (list 13 10)
                        :inside-item nil
                        :usable t
                        :use-type
                        (list "pour" t :passive :13
                              "dump" t :passive :13
                              "fill" t :passive :13
                              "refill" t :passive :13)
                        :contains-item nil
                        :conversable nil)
                  (list :name "batteries"
                        :id 31
                        :room 3
                        :description "Batteries."
                        :retrievable t
                        :interactible nil
                        :combinable (list 12)
                        :usable
                        :use-type
                        (list "insert" t :active :12
                              "replace" t :active :12
                              "put" t :active :12) 
                        :inside-item nil
                        :contains-item nil
                        :conversable nil))
                        
         
         :valid-directions
         (list :north :east :south :west :up :down))))

;; (defmacro game-action (command subj obj place &body body)
;;   `(progn (defun ,command (subject object)
;;             (if (and (eq *rooms* ',place)
;;                      (eq subject ',subj)
;;                      (eq object ',obj)
;;                      (have ',subj))
;;                 ,@body
;;               '(i cannot ,command like that.)))))

(defconst *filing-cabinet-unlocked* nil)

;; (game-action (unlock filing-cabinet)
;;              (if (and (have "small grey key") (not *filing-cabinet-unlocked*))
;;                  (progn (setf *filing-cabinet-unlocked* 't)
;;                         '("The filing cabinet is now unlocked'"))
;;                '("You do not have they key that fits this filing cabinet")))

(setf *filing-cabinet-unlocked* nil)



;debug notes
;pick up zombie equals lazy zombie.  Fix so that you need all words in multiple word objects
;fix use function
;lazy zombie not being recognized as a creature
;problems with zombie janitor, being confused with lazy zombie
;problems with ticket machine/ticket 7a, related to same word in name problem that is an issue with the zombies
