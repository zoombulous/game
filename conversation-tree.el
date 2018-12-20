(list (list :name :monster-get-name
            :subject :monster
            :context "The monster wants to know your name.  If you provide the name, the monster will haunt your dreams forever.  If you don't provide your name, the monster will forget about you."
            :question "What is your name?"
            :result (list :give-name-to-monster
                          :dont-give-name-to-monster))
      (list :name :give-name-to-monster
            :subject :monster
            :context "You've given your name to the monster and the monster will haunt your dreams forever.  The monster wants to know if that's your real name. If it's not, the monster will forget all about you."
            :question "Is that your real name?"
            :result (list :real-name-yes-monster
                          :real-name-no-monster))
      (list :name :dont-give-name-to-monster
            :subject :monster
            :context "You've withheld your name from the monster and the monster wants to insist on you providing that name."
            :question "Tell me your name or I will slime you."
            :result (list :give-name-to-monster
                          :get-slimed-by-monster))
      (list :name :real-name-yes-monster
            :subject :monster
            :context "You gave your name to the monster and then confirmed that it was your real name.  The monster will haunt your dreams forever."
            :question "I will haunt your dreams forever.")
      (list :name :real-name-no-monster
            :subject :monster
            :context "You gave your name to the monster and then indicated to the monster that the name you gave was not your real name.  The monster will forget about you."
            :question "I'm starting to forget about you.")
      (list :name :get-slimed-by-monster
            :subject :monster
            :context "You gave your name to the monster and then indicated to the monster that the name you gave was not your real name.  When the monster insisted you provide your real name, on pain of getting slimed, you stil refused to provide your real name.  Therefore, the monster will slime you."
            :question "I slime you."))
