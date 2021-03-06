;; current issues

;; there are a few things that started having problems when
;; we made it possible to have changing descriptions.  This includes
;;-moving in a direction that is not possible but is a valid direction
;;-calling attributes for items not being in the same room as the player
;;the other bug is dropping items.  It works, but you have to do it twice. The first time does wrong type arguement char-or-string-p, nil.  However, because dropping an item isn't important to the game as it stands, and it isn't mentioned anywhere in the game that you should drop an item, I don't think it is important to fix right now.



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
    (("reply" "talk" "respond") jg-conversation-command)
    ((("pick" "up") "pick" "grab" "take" "get") jg-pickup-command)
    ((("let" "go" "of") "drop" "discard" "dump") jg-drop-command)
    ((("show" "pockets") ("what" "do" "I" "have") ("what" "is" "in" "my" "pockets") ("what" "is" "in" "my" "pocket") ("check" "pockets") ("check" "my" "pockets") ("check" "pocket") ("show" "my" "items") ("check" "my" "pocket") "list" "check" "enumerate" "inventory") jg-list-command)
    ((("where" "am" "i") ("examine" "room") ("where" "am" "I") ("look") ("what's" "up") "describe" "hello" "hi") jg-describe-command)
    (("reset" "restart" "reboot") jg-reset-command)
    (("please" "kindly") jg-please-command)
  ;;  (("use") jg-use-command)
    (("unlock" "open") jg-unlock-command)
    (("lock" "close") jg-lock-command)
  ;;  (("bounce" "throw") jg-bounce-command)
    (("give" "hand over" "show") jg-give-command)
    (("help" "please help" "query" "instructions") jg-help-command)
    (("ask" "query" "question") jg-ask-command)
    ((("time-change") ("change" "the" "time") ("adjust" "the" "hand") ("adjust") ("move" "clockhand") "rip" "set" "wind" "tap" "punch" "kick" ("fix") ("change" "time") ("change" "the" "time") ("change" "time" "on") ("change" "the" "time" "on" "the") ("watch")) jg-time-change-command)
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

(defun jg-help-command (words)
  (jb-game-output
   "To win the game, you must get a portal traveling permit and go down the portal in the lobby.  The following are a few of the available commands you may use

move
pick up
look
show my items

Talking to people will not work aside from asking about in world items.
You cannot die in the game and going through the portal will never have lasting consequences.

You may not look at items individually.  Look will always return the room description.

Please only interact with people or objects when you are in the same room as them apart from asking questions about items.

Enjoy the game and good luck getting home!"))


(defun jg-unknown-command (words)
  (jb-game-output "Unknown command."))

(defun jg-ping-command (words)
  (jb-game-output "pong."))

(defun jg-please-command (words)
  (jb-game-process-words words))

(defun jg-goodbye-command (words)
  (jb-game-output "Good bye."))


(defun jg-start-command (words)
  (jb-game-output "Welcome to The First Game.

You have angered a wizard.  In his rage, he tossed you through a portal.  Because you do not have a portal traveling permit, you were sent by default to the the Department of Magical Vehicles and Transportation, or DMVT.  

To get home, you must procure a portal traveling permit, or you will be trapped in the realm of bureaucracy for all of time.  

Please type the word help for a list of a few of the commands you may need to get started in the game."))

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
  (jb-game-output "Welcome to The First Game.
You have angered a wizard.  In his rage, he tossed you through a portal.  Because you do not have a portal traveling permit, you were sent by default to the the Department of Magical Vehicles and Transportation, or DMVT.  To get home, you must procure a portal traveling permit, or you will be trapped in the realm of bureaucracy for all of time.  You may type the word help at any time for a list of a few of the commands you may need to get started in the game.  You may start by looking at the room."))

(defun jg-list-command (words)
  (logit "jg-move-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that" "through" "to" "my" "pocket" "pockets"))
        do (setq words (cdr words)))
  (let* ((player-id current-player-id)
         (pocket (get-attribute :players player-id :pocket))
         (thing-names (mapcar (lambda (x) (get-attribute :things x :name))
                             pocket)))
    (if (null pocket)
        (jb-game-output "You are destitue.")
      (jb-game-output
       (format "You have possession of the following items:
\n%s.
"
               (mapconcat 'identity thing-names ", "))))))

(defun jg-describe-command (words)
  (let* ((player-id current-player-id)
         (room-id (get-attribute :players player-id :room)))
    (jg-describe-room room-id)))

(defun jg-describe-conversation (words)
  (let* ((player-id current-player-id)
         (conversation-id (get-attribute :players player-id :conversation)))
    (jg-describe-conversation conversation-id)))

(defun things-in-room (pocket room-id)
  (remove-if-not
   (lambda (x)
     (and (null (getf x :owner))
          (null (getf x :invisible nil))
          (not (member x pocket))
          (= (getf x :room) room-id)))
   (getf game-structure :things)))

(defun add-things-in-room (description things-in-room)
  (concat 
   description 
   (format "\nThe following things are in the room: %s."
           (mapconcat (lambda (x) (getf x :name))
                      things-in-room ", "))))

;;
(defun jg-describe-room (room-id)
  (logit "jg-move-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that" "through" "to"))
        do (setq words (cdr words)))
  (let* ((player-id current-player-id)
         (player (select-by-id :players player-id))
         (pocket (getf player :pocket))
         (directions (list-keys (get-attribute :rooms room-id :directions)))
         (things-in-room (things-in-room pocket room-id))
         (description-index (get-attribute :rooms room-id :description-index))
         (descriptions (get-attribute :rooms room-id :descriptions))
         (room-name (get-attribute :rooms room-id :name))
         (description-text (elt descriptions description-index))
         (description (format "You are now in the %s.\n%s"
                              room-name description-text)))
    (when things-in-room
      (setf description (add-things-in-room description things-in-room)))
    (loop for direction in directions
          collect (format "There is a %s %s."
                          (if (member direction '(:up :down))
                              "portal in the ground if you go"
                            "door if you go")
                          (substring (symbol-name direction) 1))
          into directions
          finally
          (return
           (jb-game-output description "\n"
(mapconcat 'identity directions "\n"))))))

(defun jg-describe-conversation (conversation-id)
  (let* ((player-id current-player-id)
         (player (select-by-id :players player-id))
         (pocket (getf player :pocket))
         (directions (list-keys (get-attribute :conversation conversation-id :directions)))
         (description (format "\n%s"
                              (get-attribute :conversation conversation-id :name)
                              (get-attribute :conversation conversation-id :question))))
    (loop for direction in directions
          collect (format "Respond %s %s."
                          (substring (symbol-name direction) 1))
          into directions
          finally
          (return
           (jb-game-output description "\n"
(mapconcat 'identity directions "\n"))))))

(defun relevant (words)
  (loop with irrelevant = '("the" "a" "that" "to" "on" "about")
        for word in words
        when (not (member word irrelevant))
        collect word))



(defun jg-move-command (words)
  (logit "jg-move-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that"))
        do (setq words (cdr words)))
  (logit "words after cleanup " words)
  (let ((valid-directions (getf game-structure :valid-directions))
        (direction (intern-soft (format ":%s" (car words))))
        (player-id current-player-id))
    (if (member direction valid-directions)
        (let ((new-room-id (move player-id direction)))
          (cond ((equal new-room-id :invalid-direction)
                 (jb-game-output "There's nothing in that direction."))
               ((equal new-room-id :room-is-locked)
                 (jb-game-output "You go through the portal.  Because you do not have a portal traveling permit, you are sent back to the Department of Magical Vehicles and Transportation.  The busy zombie angrily bursts into the room and smacks you over the head.  He explains that you have to have a permit to use a portal.  He then instructs you to go go the service room and ask the lazy-zombie about the paperwork.  The busy zombie runs back out of the room."))
                (t (jg-describe-room new-room-id))))
      (jb-game-output (format "Unkown direction '%s'!"
                              (car words))))))

;;attempt to make portal locked condition for moving

;;(defun jg-move-command (words)
;;  (let ((valid-directions (getf game-structure :valid-directions))
;;        (direction (intern-soft (format ":%s" (car words))))
;;        (player-id current-player-id)
;;        (current-room-id (get-attribute :players player-id :room))
;;        (room-locked (get-attribute :rooms current-room-id :portal-locked))
;;)
;;    (if (member direction valid-directions)
;;        (let ((new-room-id (move player-id direction)))
;;          (cond ((equal new-room-id room-locked)
;;                 (jb-game-output
;;                  (format "The portal leading that direction is locked")))
;;                ((equal new-room-id :invalid-direction)
;;                 (jb-game-output
;;                  (format "There's nothing in that direction.")))
;;                (t (jg-describe-room new-room-id)))
;;          (jb-game-output (format "Unkown direction '%s'!" (car words)))))))


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
          ((equal result :object-stuck)
           (jb-game-output
            (format "The %s is firmly secured on the wall.  You can't pick it up." thing-name)))
          ((equal result :too-heavy)
           (jb-game-output (format "The %s is too heavy." thing-name)))
          ((equal result :thing-owned-by-other-player)
           (jb-game-output
            (format "The %s is already in someone else's possession.")))
          ((equal result :thing-not-in-room)
           (jb-game-output (format "There's no %s in this room.")))
          ((equal result :invisible)
           (jb-game-output (format "There's no %s in this room.")))
          ((equal result :unknown-thing)
           (jb-game-output
            (format "The thing you want, %s, doesn't exist in this world."
                    thing-name)))
          (t (jb-game-output
              (format "You now have possession of the %s." thing-name))))))

(defun jg-lock-command (words)
  (logit "jg-lock-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that"))
        do (setq words (cdr words)))
  (logit "words after cleanup " words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (lock player-id thing-id)))
    (cond ((equal result :thing-not-in-room)
           (jb-game-output (format "There is no %s in this room" thing-name)))
          ((equal result :no-key)
           (jb-game-output (format "You need a key.")))
          ((equal result :thing-not-lockable)
           (jb-game-output
            (format "Thing %s is not lockable." thing-name)))
          ((equal result :no-key)
           (jb-game-output
            (format "You must have a key to lock the %s."
                    thing-name)))
          ((equal result :already-locked)
           (jb-game-output
            (format "The %s is already locked." thing-name)))
          (t
           (set-attribute :things 32 :invisible t)
           (jb-game-output
              (format "You have locked the %s." thing-name))))))

(defun jg-unlock-command (words)
  (logit "jg-unlock-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that"))
        do (setq words (cdr words)))
  (logit "words after cleanup " words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (lock player-id thing-id t)))
    (cond ((equal result :thing-not-in-room)
           (jb-game-output (format "There is no %s in this room" thing-name)))
          ((equal result :no-key)
           (jb-game-output (format "You need a key.")))
          ((equal result :thing-not-lockable)
           (jb-game-output
            (format "The %s is not lockable." thing-name)))
          ((equal result :already-locked)
           (jb-game-output
            (format "The %s is already locked." thing-name)))
          ((equal result :thing-already-unlocked)
           (jb-game-output
            (format "The %s is already unlocked." thing-name)))
          (t
           (set-attribute :things 32 :invisible nil)
           (jb-game-output
            (format "You have unlocked the %s.  Inside is paperwork.  You should pick this up.  If you give the paperwork to the lazy-zombie, he will give you a portal traveling permit." thing-name))))))

(defun lock (player-id thing-id &optional unlock)
    (let* ((pocket (get-attribute :players player-id :pocket))
         (locked (get-attribute :things thing-id :locked))
         (invisable (get-attribute :things thing-id :invisible))
         (key-id 20))
    (if unlock
        (cond ((null locked) :thing-not-lockable)
              ((not (member key-id pocket))
             :no-key)
              ((not (equal (get-attribute :players player-id :room)
                           (get-attribute :things thing-id :room)))
               :thing-not-in-room)
              ((equal locked 1) :thing-already-unlocked)
              (t (set-attribute :things thing-id :locked 1)))
      (cond ((null locked) :thing-not-lockable)
            ((not (member key-id pocket))
             :no-key)
            ((not (equal (get-attribute :players player-id :room)
                         (get-attribute :things thing-id :room)))
             :thing-not-in-room)
            ((equal locked 2) :already-locked)
            (t (set-attribute :things thing-id :locked 2))))))

(defun jg-drop-command (words)
  (logit "jg-drop-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that"))
        do (setq words (cdr words)))
  (logit "words after cleanup " words)
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
            ((set-attribute :things format "The %s doesn't exist." thing-name)))
          (t (jb-game-output
              (format "You no longer have possession of the %s." thing-name)))))))

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
        ((get-attribute :things thing-id :stuck)
         :object-stuck)
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

;;attempt to make room portal locked condition


(defun move (player-id direction)
  "Move a player and his or her belongings from one room to another."
  (let* ((room-id (get-attribute :players player-id :room))
         (pocket (get-attribute :players player-id :pocket))
         (valid-directions (get-attribute :rooms room-id
                                          :directions))
         (new-room-id (getf valid-directions direction))
         (new-room-locked (get-attribute :rooms new-room-id :locked)))
    (cond (new-room-locked :room-is-locked)
          ((not (member direction (list-keys valid-directions)))
           :invalid-direction)
          (t (progn
               (set-attribute :players player-id :room new-room-id)
               (loop for thing-id in pocket
                     do (set-attribute :things thing-id :room new-room-id))
               new-room-id)))))

(defun jg-ask-command (words) 
  (let* ((player-id current-player-id)
         (person-id (name-to-id :things (relevant words)))
         (person-name (get-attribute :things person-id :name))
         (subject-id (name-to-id :things (relevant words)))
         (subject-name (get-attribute :things subject-id :name))
         (result (ask player-id person-id subject-id)))
    (cond ((equal result :not-a-person)
           (jb-game-output
            (format "Maybe you have been watching Brave Little Toaster too much.  You can't talk to inanimate objects.")))
          ((equal result :not-askable-person)
           (jb-game-output
            (format "The %s doesn't want to talk to you" person-name)))
          ((equal result :break-not-over)
           (jb-game-output
            (format "The lazy-zombie is munching down on a brain sandwich.  With a mouth full of mush, he bitterly grunts that he is on his break.  He snarkily remarks that you may watch the clock in the waiting room like everyone else.")))
          ((equal result :subject-not-in-room)
           (jb-game-output
            (format "The %s is not in this room.")))
           ((equal result :not-valid-subject)
            (jb-game-output
             (format "He doesn't want to talk about that.")))
          (t
           (jb-game-output
            (format "The lazy-zombie explains that paperwork is required to recieve a portal traveling permit.  Unfortunately, the only paperwork left is in the file room.  The lazy-zombie goes on to say that he is too busy right now to retrieve it for you.  He then goes on to say if you retrieve the paperwork you should bring it to the service room and give it to him.  Then he will give you a portal traveling permit."))))))
    
(defun ask (player-id person-id subject-id)
  (let* ((player (select-by-id :players player-id))
         (person-to-ask (select-by-id :things person-id))
         (subject-to-ask (select-by-id :things subject-id))
         (person (get-attribute :things person-id :creature))
         (askable-person (get-attribute :things person-id :conversable))
         (valid-subject (get-attribute :things subject-id :valid-subject))
         (can-ask-subject (member person-id valid-subject))
         (time-change (get-attribute :things 8 :time-change)))
    (cond
     ((not person) :not-a-person)
     ((not askable-person) :not-askable-person)
     ((not valid-subject) :not-valid-subject)
     ((equal time-change 1) :break-not-over)
     ((not (equal (get-attribute :players player-id :room)
                  (get-attribute :things person-id :room)))
      :subject-not-in-room)
     (t t))))

(defun jg-conversation-command (words)
  (logit "jg-conversation-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that"))
        do (setq words (cdr words)))
  (logit "words after cleanup " words)
  (when words
    (let* ((player-id current-player-id)
           (conversation-id
            (get-attribute :players player-id :conversation-id))
           (responses
            (get-attribute :conversations conversation-id :result))
           (selected-response
            (car (remove-if-not (lambda (x)
                                  (equal (format "%s" (getf x :answer))
                                         (car words)))
                                responses)))
           (destination
            (select-by-id :conversations (getf selected-response
                                               :destination-id))))
      (when destination
        (set-attribute :players player-id :conversation-id
                       (getf destination :id)))))
  (let* ((valid-conversation-responses (getf game-structure :valid-conversation-responses))
         (player-id current-player-id)
         (conversation-id (get-attribute :players player-id :conversation-id))
         (response (when words (intern-soft (format ":%s" (car words))))))
    (conversation player-id conversation-id)))
  
(defun conversation (player-id direction)
  (let* ((current-conversation-id
          (get-attribute :players player-id :conversation-id))
         (conversation-id
          (get-attribute :players player-id :conversation-id))
         (question (get-attribute :conversations conversation-id :question))
         (possible-answers
          (get-attribute :conversations conversation-id :result))
         (answers (loop for possible-answer in possible-answers
                        collect (format "%s. %s\n" 
                                        (getf possible-answer :answer)
                                        (getf possible-answer :text))))
         (output (format "%s\n%s" question (string-join "" answers))))
    (jb-game-output output)))

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

(defun jab (player-id thing-id)
  (let* ((pocket (get-attribute :players player-id :pocket))
         (jab (get-attribute :things thing-id :jab-able)))
    (cond ((not (member key-id pocket))
           :no-sword)
    (t (jb-game-output
        (format ("I know this thing isn't sharp, but you shouldn't swing it around willy nilly." thing-name)))))))

(defun jg-bounce-command (words)
  (logit "jg-bounce-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that"))
        do (setq words (cdr words)))
  (logit "words after cleanup " words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things words))
         (thing-name (get-attribute :things thing-id :name))
         (result (jab player-id thing-id)))
    (cond ((equal result :no-sword)
           (jb-game-output (format "You have most impressive skills with imaginary cutlery.  All of the imaginary pirates are terrified of you.  I wonder how you would fair if you had a not-pretend %s in your possesion?" thing-name)))
          (t (jb-game-output
              (format ("I know this %s isn't sharp, but you shouldn't swing it around willy nilly." thing-name)))))))

(defun jg-time-change-command (words)
  (logit "jg-time-change-command remaining words " words)
  (loop while (member (car words) '("the" "a" "that" "off" "of" "the" "wall" "and" "jump" "up" "and" "down" "on" "it" "until" "it" "starts"))
        do (setq words (cdr words)))
  (logit "words after cleanup " words)
  (let* ((player-id current-player-id)
         (thing-id (name-to-id :things (relevant words)))
         (thing-name (get-attribute :things thing-id :name))
         (result (time-change player-id thing-id)))
    (cond ((equal result :not-a-clock)
           (jb-game-output
            (format "You can't change the time on a %s!" thing-name)))
          ((equal result :break-over)
           (jb-game-output
            (format "You already fiddled with it.  Now that we know you don't have a future in the clock fixing business, you should leave it alone.")))
          ((equal result :not-in-room)
           (jb-game-output
            (format "There is no %s in this room." thing-name)))
          (t
         (set-attribute :rooms 4 :description-index 1)
           (jb-game-output
              (format "You move the hour hand forward on the otherwise unchanging clock.  A bell goes off and you can hear the busy zombie yell at the lazy-zombie that break time is over and that he needs to get back to work, followed by grumbling and shuffling." 
                      thing-name))))))

(defun time-change (player-id thing-id)
  (let* ((pocket (get-attribute :players player-id :location))
         (time-change (get-attribute :things thing-id :time-change)))
    (cond ((null time-change) :not-a-clock)
          ((not (equal (get-attribute :players player-id :room)
                       (get-attribute :things thing-id :room)))
           :thing-not-in-room)
          ((equal time-change 2) :break-over)
          (t (set-attribute :things thing-id :time-change 2)))))


(defun jg-give-command (words)
  (let* ((player-id current-player-id)
         (give-id (name-to-id :things (relevant words)))
         (give-name (get-attribute :things give-id :name))
         (receive-id (name-to-id :things (relevant words)))
         (receive-name (get-attribute :things receive-id :name))
         (result (give player-id give-id receive-id)))
    (cond ((equal result :thing-not-in-room)
           (jb-game-output (format "There is no %s in this room" receive-name)))
          ((equal result :no-papers)
           (jb-game-output
            (format "The lazy-zombie sticks his nose up at you, for you do not have paperwork.  Without the proper paperwork, the lazy-zombie will not give you a portal traveling permit.  When you inquire about where to receive the proper paperwork, the lazy-zombie says that there are some in the storage room, but he is far to busy to retrieve them right now.")))
          ((equal result :cannot-receive)
           (jb-game-output
            (format "The %s doesn't want that." receive-name)))
          ((equal result :break-not-over)
           (jb-game-output
            (format "The lazy-zombie is munching down on a brain sandwich.  With a mouth full of mush, he bitterly grunts that he is on his break.  He snarkily remarks that you may watch the clock in the waiting room like everyone else.")))
          (t
           (set-attribute :things give-id :invisible t)
           (set-attribute :things 5 :invisible nil)
           (set-attribute :rooms 1 :description-index 1)
           (set-attribute :rooms 999 :locked nil)
           (jb-game-output
            (format "You give the paperwork to the lazy-zombie.  He mumbles to himself as as he scribbles down some information and types into his computer.  Then the lazy-zombie sets a traveling permit down on the counter before you and wishes you a mediocre day."))))))

(defun give (player-id give-id receive-id)
  (let* ((pocket (get-attribute :players player-id :pocket))
         (thing-to-receive (select-by-id :things receive-id))
         (thing-to-give (select-by-id :things give-id))
         (can-receive (get-attribute :things receive-id :can-receive))
         (time-change (get-attribute :things 8 :time-change))
         (receivable (member give-id can-receive)))
    (cond
     ((not receivable) :cannot-receive)
     ((not (member give-id pocket)) :no-papers)
     ((equal time-change 1) :break-not-over)
     ((not (equal (get-attribute :players player-id :room)
                  (get-attribute :things receive-id :room)))
      :thing-not-in-room)
     (t t))))

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
  (let* ((pocket (get-attribute :players player-id :pocket))
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
                       :descriptions
                       (list
                        "You stand in a room that aches with boredom.  You would rather gnaw off your leg than stand another moment in this place.  It must be a government building.  There is a sign on the wall that instructs you to ask the lazy-zombie about paperwork in the service room, which is to your north.  It then goes on to explain the wonders of portal traveling when you have your very own portal traveling permit."
                        "No description")
                       :description-index 0
                       :locked nil
                       :room-puzzle nil)
                 
                 (list :name "Home"
                       :id 999
                       :directions
                       ()
                       :descriptions
                       (list
                        "You made it home!  You win the game

"
                        "No description.")
                       :description-index 0
                       :locked t
                       :room-puzzle nil)
                 
                 (list :name "Service Room"
                       :id 1
                       :directions
                       (list :south 0)
                       :descriptions
                       (list
                        "Grouchy zombies shuffle around behind service desks.  Presumably, their job is to help people get paper work squared away as their number is called over the loud speaker, but no numbers are being called."
                        "Grouchy zombies shuffle around behind a service desk.  As peoples numbers are called, they walk up to the zombies for assistance with their paper work.")
                       :description-index 0
                       :locked nil
                       :room-puzzle nil)
                 
                 (list :name "File Room"
                       :id 2
                       :directions
                       (list :west 0)
                       :descriptions
                       (list
                        "This room has a large, important looking filing cabinet.  The walls are a blinding yellow color that hurt your eyes if you look too long."
                        "No description.")
                       :description-index 0
                       :locked nil
                       (list
                        :room-puzzle nil))
                 
                 (list :name "Storage Room"
                       :id 3
                       :directions
                       (list :north 0)
                       :descriptions
                       (list 
                        "A janitor is sleeping on a cot in the corner.  Various cleaning items are scattered around the room."
                        "No description.")
                       :description-index 0
                       :locked nil
                       :room-puzzle nil)
                 
                 (list :name "Waiting Room"
                       :id 4
                              :directions
                       (list :east 0)
                       :descriptions
                       (list 
                        "Creatures are sitting on chairs.  There is a broken clock on the wall.  There are signs that someone tampered with it.  The time is unchanging.  The fairy complains to the gnome that someone should fix the clock."
                        "Creatures are sitting in chairs.  It would seem that when you moved the hands on the clock, you ended the lazy-zombie's break.")
                       :description-index 0
                       :locked nil
                       :room-puzzle nil))
                 
         :conversations
         (list
          (list :name :monster-get-name
                :id 1
                :subject-id 1
                :context "The monster wants to know your name.  If you provide the name, the monster will haunt your dreams forever.  If you don't provide your name, the monster will forget about you."
                :question "What is your name?"
                :result (list (list :answer :a :text "My name is Hero"
                                    :destination :gave-name-to-monster
                                    :destination-id 2)
                              (list :answer :b
                                    :text "I don't want to give you my name"
                                    :destination :dont-give-name-to-monster
                                    :destination-id 3)))
          
          (list :name :give-name-to-monster
                :id 2
                :subject-id 1
                :context "You've given your name to the monster and the monster will haunt your dreams forever.  The monster wants to know if that's your real name. If it's not, the monster will forget all about you."
                :question "Because you gave me your name, I will haunt your dreams forever"
                :result (list (list :answer nil :text nil
                                    :destination :end-conversation)))
          
          (list :name :dont-give-name-to-monster
                :id 3
                :subject-id 1
                :context "You've withheld your name from the monster and the monster wants to insist on you providing that name."
                :question "Because you have withheld your name, I will slime you."
                :result (list (list :answer nil :text nil
                                    :destination :end-conversation))))

         :players (list
                   (list :id 1 :room 0 :pocket nil :conversation-id 1))
         
         :things (list
                 ;; (list :name "mirror"
                 ;;       :id 1
                 ;;       :owner nil
                 ;;       :invisible t
                 ;;       :room 0
                 ;;       :description "There is a myserious figure inside."
                 ;;       :retrievable t
                 ;;       :interactible nil
                 ;;       :combinable nil
                 ;;       :combined nil
                 ;;       :inside-item nil
                 ;;       :contains-item nil
                 ;;       :conversable t)
                 ;; (list :name "ball"
                 ;;       :id 2
                 ;;       :owner nil
                 ;;       :room 0
                 ;;       :description "This ball is round and sqeaky."
                 ;;       :retrievable t
                 ;;       :usable t
                 ;;       :bounceable t
                 ;;       :use-type
                 ;;       (list "interact" t
                 ;;             "bounce" t)
                 ;;       :interact-return "You bounce the ball off the floor.  That was fun"
                 ;;       :combinable nil
                 ;;       :combined nil
                 ;;       :inside-item nil
                 ;;       :contains-item nil
                 ;;       :conversable nil)
                 ;; (list :name "sword"
                 ;;       :id 3
                 ;;       :owner nil
                 ;;       :room 0
                 ;;       :description "This sword is rusty and dull."
                 ;;       :retrievable t
                 ;;       :jab-able t
                 ;;       :usable t
                 ;;       :use-type
                 ;;       (list "interact" t
                 ;;             "cut" t
                 ;;             "swing" t
                 ;;             "jab" t)
                 ;;       :interact-return "I know this thing isn't sharp, but you shouldn't swing it around willy nilly."
                 ;;       :combinable nil
                 ;;       :combined nil
                 ;;       :inside-item nil
                 ;;       :contains-item nil
                 ;;       :converable nil)
                  (list :name "lazy-zombie"
                        :id 4
                        :owner nil
                        :room 1
                        :description
                        (list "The lazy-zombie is standing behind the service desk eating a sandwhich." "The lazy-zombie is standing behind the service desk.  He doesn't appear to be busy.  Maybe he can help you?")
                        :retrievable nil
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item nil
                        :contains-item (list :id 5 :name "portal traveling permit")
                        :conversable t
                        :creature t
                        :askable t
                        :can-receive (list 32))
                  (list :name "portal traveling permit"
                        :id 5
                        :room 1
                        :description "With these, you can travel through portals.  Maybe you can make your way home."
                        :retrievable t
                        :interactible nil
                        :combinable nil
                        :combined nil
                        :inside-item (list :id 4 :name "lazy-zombie")
                        :contains-item nil
                        :invisible t
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
                        :room 4
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
                        :usable nil
                        :use-type
                        (list "open" t :active :20
                              "unlock" t :active :20
                              "search" t)
                        :locked 2
                        :contains-item (list :id  :name "portal traveling permit papers")
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
         ;;         (list :name "mop"
         ;;               :id 10
         ;;               :room 3
         ;;               :description "This mop is broken"
         ;;               :retrievable t
         ;;               :interactible nil
         ;;               :combinable (list 30)
         ;;               :combined nil
         ;;               :inside-item nil
         ;;               :usable t
         ;;               :use-type
         ;;              (list :fix t)
         ;;               :contains-item nil
         ;;               :conversable nil)
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
         ;;         (list :name "flashlight"
         ;;               :id 12
         ;;               :room 3
         ;;               :description "This could be handy if we had some batteries"
         ;;               :retrievable t
         ;;               :interactible "You switch on the flashlight, nothing happens."
         ;;               :combinable (list 31)
         ;;               :combined nil
         ;;               :inside-item nil
         ;;               :batteries-replaceable 1
         ;;               :usable t
         ;;               :use-type
         ;;               (list "turn on" t
         ;;                     "turn off" t
         ;;                     "shine" t)
         ;;               :charged 1
         ;;               :on 1
         ;;               :contains-item nil
         ;;               :conversable nil)
         ;;         (list :name "spray bottle"
         ;;               :id 13
         ;;               :room 3
         ;;               :description "It is an empty spray bottle"
         ;;               :retrievable t
         ;;               :interactible "You use the empty spray bottle.  Nothing interesting happens.  You probably should put something inside it first"
         ;;               :combinable (list 30)
         ;;               :combined nil
         ;;               :inside-item nil
         ;;               :refill-able 1
         ;;               :usable t
         ;;               :use-type
         ;;               (list "spray" t
         ;;                     "squirt" t)
         ;;               :contains-item nil
         ;;               :conversable nil)
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
                        :invisible
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
         ;;         (list :name "ticket machine"
         ;;               :id 19
         ;;               :room 4
         ;;               :description "The ticket machine prints tickets with service numbers.  When your number is called, you may go to the service room and be helped"
         ;;               :retrievable nil
         ;;               :interactible
         ;;               (list "You press a button and a ticket pops out.  It reads 9-Z.  The tickets everyone else has don't go past D.  Discouraged, you throw the ticket away.  Maybe you can trick someone else into giving you their ticket." "Last time you used this machine, it made you sad.  Maybe you can trick someone else into giving you their ticket.")
         ;;               :combinable nil
         ;;               :inside-item nil
         ;;               :contains-item nil
         ;;               :usable t
         ;;               :use-type
         ;;               (list "operate" t)
         ;;               :conversable nil
         ;;               :heavy t)
         ;;         (list :name "green portal jar"
         ;;               :id 21
         ;;               :room 3
         ;;               :description "If you use this in the lobby, you will make a portal to a new area.  You will be one step closer to getting home."
         ;;               :retrievable t
         ;;               :interactible nil
         ;;               :combinable nil
         ;;               :inside-item nil
         ;;               :use-type
         ;;               (list "recharge" t
         ;;                     "open a portal" t
         ;;                     "open a portal with" t)
         ;;               :contains-item nil
         ;;               :conversable nil)
         ;;         (list :name "water"
         ;;               :id 30
         ;;               :room 3
         ;;               :description "A bucket of water."
         ;;               :retrievable t
         ;;               :interactible nil 
         ;;               :combinable (list 13 10)
         ;;               :inside-item nil
         ;;               :usable t
         ;;               :use-type
         ;;               (list "pour" t :passive :13
         ;;                     "dump" t :passive :13
         ;;                     "fill" t :passive :13
         ;;                     "refill" t :passive :13)
         ;;               :contains-item nil
         ;;               :conversable nil)
         ;;         (list :name "batteries"
         ;;               :id 31
         ;;               :room 3
         ;;               :description "Batteries."
         ;;               :retrievable t
         ;;               :interactible nil
         ;;               :combinable (list 12)
         ;;               :usable
         ;;               :use-type
         ;;               (list "insert" t :active :12
         ;;                     "replace" t :active :12
         ;;                     "put" t :active :12) 
         ;;               :inside-item nil
         ;;               :contains-item nil
         ;;               :conversable nil)
                  (list :name "paperwork"
                        :id 32
                        :room 2
                        :dead t
                        :description "With these, you can apply for a portal traveling permit"
                        :retrievable t
                        :invisible t
                        :interactible nil
                       ; :combinable (list 12)
                       ; :usable t
                       ; :use-type t
                       ; (list "insert" t :active :12
                       ;       "replace" t :active :12
                       ;       "put" t :active :12) 
                       ; :inside-item t
                      
                        :contains-item t
                        :conversable nil
                        :valid-subject t
                        :give 1))

         
         
         :valid-conversation-responses
         (list :a :b :c :d)
         
         :valid-directions
         (list :north :east :south :west :up :down))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
