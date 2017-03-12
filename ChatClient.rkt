;; Created using DrRacket

;; LIBRARIES --------------------------------------------------------
(require racket/string)
(require 2htdp/image)
(require 2htdp/universe)

;; DATA DEFINITIONS -------------------------------------------------
;; A History is one of
;;  -- '()
;;  -- (cons Chat History)

;; A Chat is one of
;;  -- (list "MSG" String [list-of String] String)
;;  -- (list "JOIN" String)
;;  -- (list "EXIT" String)
;;  -- (list "ERROR" String)
;;  -- (list "EMOTICON" String String String)
#; (define (chat-temp c)
     (cond [(string=? (first c) "MSG") ...]
           [(string=? (first c) "JOIN") ...]
           [(string=? (first c) "EXIT") ...]
           [(string=? (first c) "ERROR") ...]
           [(string=? (first c) "EMOTICON") ...]))

;; A MSG is a (list "MSG" String [list-of String] String)

;; A Command is one of
;;  -- String
;;  -- (list "COLOR" String)
;;  -- (list "BLACKLIST" String)
;;  -- (list "WHITELIST" String)
;;  -- (list "WHITELIST" String)
;;  -- (list "URL" String String)
;; Interpretation:
;;    Plain String is a normal message
;;    "!color cs" changes user's color to cs with the "COLOR" command
;;    "!block n" blocks user n with the "BLACKLIST" command
;;    "!friend n" allows n to send emoticons
;;    "!emote s u" sends 


(define-struct emote [word image])
;; An Emote is a (make-emote String Image)
;; word is the plaintext word to be replaced
;; image is the image with which to replace word

(define-struct chatclient [history current emotes])
;; A Chatclient is a (make-chatclient History String [list-of Emote])


(define CHAT-HEIGHT 400)
(define WIDTH 800)
(define INPUT-HEIGHT 50)
(define CHAT-HISTORY (empty-scene WIDTH CHAT-HEIGHT))
(define INPUT-BOX (empty-scene WIDTH INPUT-HEIGHT))
(define CLIENT-WINDOW (empty-scene WIDTH (+ CHAT-HEIGHT INPUT-HEIGHT)))
(define SERVER "dictionary.ccs.neu.edu")
(define PORT 10008)
(define NAMES "goldsmith.a:5037+valluru.m:1411")

(define em (make-emote "testword" (text "TESTIMG" 20 "orange")))
(define ch1 (list "MSG" "Bob" (list "Hi" "how" "are" "you" "ajdsad") "Purple"))
(define ch2 (list "JOIN" "Jim has entered"))
(define ch3 (list "EXIT" "Tim has left"))
(define ch4 (list "ERROR" "ERROR: Bad message"))
(define ch5 (list "MSG" "David" (list "test" "message" "hi") "green"))
(define ch6 (list "EMOTICON" "Larry" "Larry"
                  "http://simpleicon.com/wp-content/uploads/smile.png"))
(define ch7 (list "MSG" "Larry" (list "testword" "hi") "black"))
(define cmd1 "This is a normal message")
(define cmd2 (list "COLOR" "blue"))
(define cmd3 (list "BLACKLIST" "Bob"))
(define cmd4 (list "URL" "word" "www.example.com"))
(define cmd5 (list "WHITELIST" "Larry"))
(define testcc (make-chatclient (list ch4 ch5) "asdf" (list em)))

(define BLANK (make-chatclient '() "" '()))



;; CODE ---------------------------------------------------------------------------
;; Chatclient Chat -> Chatclient
;; Receive a message and add new message to chat history
(define (msg-receive cc msg)
  (cond [(string=? (first msg) "EMOTICON")
         (make-chatclient
          (cons msg (chatclient-history cc))
          (chatclient-current cc)
          (cons (make-emote (third msg) (bitmap/url (fourth msg)))
                (chatclient-emotes cc)))]
        [else (make-chatclient
               (cons msg (chatclient-history cc)
               (chatclient-current cc)
               (chatclient-emotes cc))]))

(check-expect (msg-receive testcc ch1)
             (make-chatclient (cons ch1 (chatclient-history testcc))
                              (chatclient-current testcc)
                              (chatclient-emotes testcc)))
(check-expect (msg-receive testcc ch6)
              (make-chatclient (cons ch6 (chatclient-history testcc))
                               (chatclient-current testcc)
                               (cons (make-emote "Larry"
                                                 (bitmap/url
                      "http://simpleicon.com/wp-content/uploads/smile.png"))
                                     (list em))))


;; String -> Command
;; Reads a String c and makes an appropriate Command
(define (send-command c)
  (cond [(not (string=? "!" (substring c 0 1)))
         c]
        [(string=? "!color" (substring c 0 6))
         (list "COLOR" (substring c 7))]
        [(string=? "!block" (substring c 0 6))
         (list "BLACKLIST" (substring c 7))]
        [(string=? "!friend" (substring c 0 7))
         (list "WHITELIST" (substring c 8))]
        [(string=? "!emote" (substring c 0 6))
         (url-command (string-split c))]))
(check-expect (send-command "!color blue") cmd2)
(check-expect (send-command "This is a normal message") cmd1)
(check-expect (send-command "!block Bob") cmd3)
(check-expect (send-command "!emote word www.example.com") cmd4)
(check-expect (send-command "!friend Larry") cmd5)

;; [list-of String] -> Command
;; Turns a split up URL command string into a Command
;; Expects String split by str-split as input
(define (url-command los)
  (list "URL" (second los) (third los)))
(check-expect (url-command (str-split
                            (explode "!emote testword www.example.com")
                            0
                            (list "" "" "")))
              (list "URL" "testword" "www.example.com"))

;; [list-of String] Number [list-of String] -> [list-of String]
;; Splits an exploded string up by spaces
;; (Attempt at implementing string-split from racket/string)
(define (str-split los i res)
  (cond [(empty? los) res]
        [(string=? " " (first los)) (str-split (rest los) (add1 i) res)]
        [else (str-split (rest los) i
              (cond [(= i 0)
                     (list (string-append (first res) (first los))
                           (second res) (third res))]
                    [(= i 1)
                     (list (first res)
                           (string-append (second res) (first los))
                           (third res))]
                    [(= i 2)
                     (list (first res) (second res)
                           (string-append (third res) (first los)))]
                    [else (list (first res) (second res) (third res))]))]))
(check-expect (str-split (explode "!emote testword www.example.com")
                            0 (list "" "" ""))
              (list "!emote" "testword" "www.example.com"))

;; Chatclient KeyEvent -> Chatclient
;; On KeyEvent, adds all 1Strings to the current message being typed
;;      If key is "\b", backspaces on current message being typed
;;      If key is "\r", sends current message to server
(define (key-press cc key)
  (cond
    [(string=? "\b" key)
     (if (not (= (string-length (chatclient-current cc)) 0))
            (make-chatclient
             (chatclient-history cc)
             (substring (chatclient-current cc)
                        0 (- (string-length (chatclient-current cc)) 1))
             (chatclient-emotes cc))
            cc)]
    [(and (string=? "\r" key) (> (string-length (chatclient-current cc)) 0))
     (make-package (make-chatclient (chatclient-history cc)
                                    ""
                                    (chatclient-emotes cc))
                   (send-command (chatclient-current cc)))]
    [(and (= (string-length key) 1) (not (string=? key "\r")))
     (make-chatclient (chatclient-history cc)
                      (string-append (chatclient-current cc) key)
                      (chatclient-emotes cc))]
    [else cc]))

(check-expect (key-press testcc "\b")
              (make-chatclient (chatclient-history testcc)
                               "asd"
                               (chatclient-emotes testcc)))
(check-expect (key-press BLANK "\b") BLANK)


(check-expect (key-press testcc "\r")
              (make-package (make-chatclient (chatclient-history testcc)
                                             ""
                                             (chatclient-emotes testcc))
                            "asdf"))
(check-expect (key-press testcc "g")
              (make-chatclient (chatclient-history testcc)
                               (string-append (chatclient-current testcc) "g")
                               (chatclient-emotes testcc)))
(check-expect (key-press testcc "shift") testcc)
(check-expect (key-press (make-chatclient (list ch4 ch5)
                                          "!color blue"
                                          (chatclient-emotes testcc)) "\r")
                         (make-package (make-chatclient (list ch4 ch5)
                                                        ""
                                                     (chatclient-emotes testcc))
                                       (list "COLOR" "blue")))
(check-expect (key-press (make-chatclient (list ch4 ch5)
                                          "!block bob"
                                          (chatclient-emotes testcc)) "\r")
                         (make-package (make-chatclient (list ch4 ch5) ""
                                                     (chatclient-emotes testcc))
                                       (list "BLACKLIST" "bob")))

;; History [list-of Emote] -> Image
;; Builds the chat history window
(define (draw-history h emotes)
    (cond
        [(empty? h) empty-image]
        [else (above/align "left"
                          (draw-history (rest h) emotes)
                          (draw-chat (first h) emotes))]))
(check-expect (draw-history (list ch5) (list em))
              (above/align "left" empty-image
                           (text "David: test message hi" 20 "green")))

;; Chat [list-of Emote]-> Image
;; Draws a Chat
(define (draw-chat c emotes
     (cond [(string=? (first c) "MSG")
            (beside (text (string-append (second c) ":") 20 (fourth c))
                    (draw-msg c emotes))]
           [(string=? (first c) "JOIN")
            (text (string-append (second c) " joined.") 20 "chartreuse")]
           [(string=? (first c) "EXIT")
            (text (string-append (second c) " left.") 20 "chartreuse")]
           [(string=? (first c) "ERROR")
            (text (second c) 20 "red")]
           [(string=? (first c) "EMOTICON")
            (text (string-append (second c) " has replaced " (third c))
                  20 "black")]))
(check-expect (draw-chat ch5 (list em))
              (text "David: test message hi" 20 "green"))
(check-expect (draw-chat ch4 (list em))
              (text "ERROR: Bad message" 20 "red"))
(check-expect (draw-chat ch6 (list em))
              (text "Larry has replaced Larry" 20 "black"))
(check-expect (draw-chat ch2 (list em))
              (text "Jim has entered joined." 20 "chartreuse"))
(check-expect (draw-chat ch3 (list em))
              (text "Tim has left left." 20 "chartreuse"))


;; MSG [list-of Emote]-> Image
;; Draws a MSG, accounting for Emote replacements
(define (draw-msg m emotes)
  (cond [(empty? (third m)) empty-image]
        [else (beside (text " " 20 "black")
                      (emote-in (first (third m))
                                emotes
                                (fourth m))
                      (draw-msg (list (first m)
                                      (second m)
                                      (rest (third m))
                                      (fourth m))
                                      emotes))]))

(check-expect (draw-msg ch5 (list em)) (text " test message hi" 20 "green"))
(check-expect (draw-msg ch7 (list em))
              (beside (text " " 20 "black")
                      (scale (/ 20 23) (text "TESTIMG" 20 "orange"))
                      (text " hi" 20 "black")))

;; String [list-of Emote] -> Image
;; Gets the Emote associated with word
;; Otherwise, returns word as an image
(define (emote-in word emotes color)
  (cond [(empty? emotes) (text word 20 color)]
        [else (if (string=? word (emote-word (first emotes)))
                  (scale (/ 20 (image-height (emote-image (first emotes))))
                         (emote-image (first emotes)))
                  (emote-in word (rest emotes) color))]))

(check-expect (emote-in "testword" (list em) "blue")
              (scale (/ 20 23) (text "TESTIMG" 20 "orange")))
(check-expect (emote-in "asdf" (list em) "blue")
              (text "asdf" 20 "blue"))


;; Chatclient -> image
;; Creates a window to display chat history and input box
(define (draw-window cc)
  (place-image/align (draw-history (chatclient-history cc)
                                   (chatclient-emotes cc))
                     0 CHAT-HEIGHT "left" "bottom"
   (place-image/align
    (place-image/align
     (text (chatclient-current cc) 20 "black")
     0 (/ INPUT-HEIGHT 2) "left" "center" INPUT-BOX)
                      0 CHAT-HEIGHT "left" "top" CLIENT-WINDOW)))

(check-expect (draw-window BLANK)
              (place-image/align
               (above/align "left" empty-image (text "" 20 "black"))
               0 CHAT-HEIGHT "left" "bottom"
               (place-image/align
                (place-image/align (text "" 20 "black")
                                0 (/ INPUT-HEIGHT 2) "left" "center" INPUT-BOX)
               0 CHAT-HEIGHT "left" "top" CLIENT-WINDOW)))
(check-expect (draw-window testcc)
              (place-image/align
               (above/align "left" empty-image
                            (text "David: test message hi " 20 "green")
                            (text "ERROR: Bad message" 20 "red"))
               0 CHAT-HEIGHT "left" "bottom"
               (place-image/align
                (place-image/align (text "asdf" 20 "black")
                                0 (/ INPUT-HEIGHT 2) "left" "center" INPUT-BOX)
                0 CHAT-HEIGHT "left" "top" CLIENT-WINDOW)))



;; Chatclient -> Chatclient
;; Chat client that communicates with SERVER
(define (client cc)
  (big-bang cc
            [register SERVER]
            [port PORT]
            [name NAMES]
            [on-receive msg-receive]
            [on-key key-press]
            [to-draw draw-window]))