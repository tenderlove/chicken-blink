(module blink
  (blink-devices
   blink-open
   blink-set!
   blink-fade!
   blink-off!
   blink-on!
   blink-write-pattern-line!
   blink-play!
   blink-version
   blink-read-pattern-line
   blink-read-pattern-lines
   blink-serverdown!)
  (import scheme chicken ports usb srfi-1 srfi-69 srfi-18 posix)
  (use usb srfi-1 srfi-69 srfi-18)

  (define << arithmetic-shift)
  (define (>> n count) (arithmetic-shift n (- count)))

  (define (pack buf)
    (with-output-to-string (lambda ()
        (for-each (lambda (byte) (write-char (integer->char byte))) buf))))

  (define (drop-null buf)
    (with-output-to-string (lambda ()
      (for-each write-char
                (take-while (lambda (chr) (not (= 0 (char->integer chr))))
                            (string->list buf))))))

  (define write-request (bitwise-ior usb::request-type-class
                                     usb::recipient-interface
                                     usb::endpoint-out))

  (define read-request (bitwise-ior usb::request-type-class
                                    usb::recipient-interface
                                    usb::endpoint-in))
  (define request #x09)
  (define get-report #x01)
  (define timeout 1000)
  (define interface-number 0)

  (define (blink-write handle buf)
    (let ((value (bitwise-ior (<< usb::request-set-feature 8)
                              (bitwise-and (car buf) #xFF)))
          (bytes (pack buf)))
      (usb-control-transfer handle
                            write-request
                            request
                            value
                            interface-number
                            bytes
                            timeout)))

  (define (blink-read handle buf)
    (blink-write handle buf)
    (let ((value (bitwise-ior (<< usb::request-set-feature 8)
                              (bitwise-and (car buf) #xFF)))
          (bytes (pack buf)))
      (usb-control-transfer handle
                            read-request
                            get-report
                            value
                            interface-number
                            bytes
                            timeout)
      bytes))

  ; set the color to r g b on dev.  r g b should be values from 0 to 255
  (define (blink-set! dev r g b)
    (blink-write dev (list 1 #x6e r g b 0 0 0 0)))

  ; Turn the device off
  (define (blink-off! dev) (blink-serverdown! dev #f 0))

  ; Turn the device on
  (define (blink-on! dev #!optional (ms 10))
    (blink-serverdown! dev #t ms))

  ; Turn the server on or off.  
  (define (blink-serverdown! dev on ms)
    (let* ((dms (/ ms 10))
           (th (>> dms 8))
           (tl (bitwise-and dms #xFF))
           (onv (if on 1 0)))
      (blink-write dev (list 1 #x44 onv th tl 0 0 0 0))))

  ; Fade to r g b in ms milliseconds
  (define (blink-fade! dev ms r g b)
    (let* ((dms (/ ms 10))
           (th (>> dms 8))
           (tl (bitwise-and dms #xFF)))
      (blink-write dev (list 1 #x63 r g b th tl 0 0))))

  ; Write a pattern line to the blink(1).  There are only 12 slots, and they
  ; are 0 indexed.  Specifying a position over 11 will always set the value
  ; on position 0.
  (define (blink-write-pattern-line! dev ms r g b pos)
    (let* ((dms (/ ms 10))
           (th (>> dms 8))
           (tl (bitwise-and dms #xFF)))
      (blink-write dev (list 1 #x50 r g b th tl pos 0))))

  ; Read a pattern line
  (define (blink-read-pattern-line dev pos)
    (let ((packet (list 1 #x52 0 0 0 0 0 pos 0)))
      (blink-write dev packet)
      (let* ((buf (blink-read dev packet))
             (chars (map char->integer (string->list buf)))
             (r (list-ref chars 2))
             (g (list-ref chars 3))
             (b (list-ref chars 4))
             (ms (* 10 (+ (<< (list-ref chars 5) 8)
                       (bitwise-and #xFF (list-ref chars 6))))))
        (list ms r g b))))

  ; Read all the pattern lines
  (define (blink-read-pattern-lines dev)
    (let loop ((i 11) (acc '()))
      (if (< i 0)
          acc
          (loop (- i 1) (cons (blink-read-pattern-line dev i) acc)))))

  ; Play the pattern
  (define (blink-play! dev #!optional (pos 0) (play 1))
    (blink-write dev (list 1 #x70 play pos 0 0 0 0)))

  ; Get the version
  (define (blink-version dev)
    (let ((packet (list 1 #x76 0 0 0 0 0 0)))
      (blink-write dev packet)
      (let* ((str (blink-read dev packet))
             (version (map char->integer (string->list str))))
        (+ (* 10 (- (list-ref version 3) (char->integer #\0)))
           (- (list-ref version 4) (char->integer #\0))))))
        

  (define (blink-dev? dev)
    (let ((desc (usb-device-descriptor dev)))
      (and (= #x27B8 (usb-device.idVendor desc))
           (= #x01ED (usb-device.idProduct desc)))))

  (define usb-ctx (usb-make-context))

  ; Return a list of all the blink(1) devices on this machine
  (define (blink-devices) (filter blink-dev? (usb-devices usb-ctx)))

  ; Open the device for writing.  Returns a handle for the blink(1)
  ; that you can manipulate.
  (define (blink-open dev)
    (let ((dev (usb-open dev)))
      (usb-claim-interface! dev) dev))
)

(import blink)
(use test posix)

(test-begin "blink")

(define dev (car (blink-devices)))
(test-assert dev)

(define handle (blink-open dev))
(test-assert handle)

; (blink-off! handle)
; (sleep 1)

; (test 10 (blink-version handle))
; (blink-write-pattern-line! handle 100 255 0 0 0)
; (test (list 100 255 0 0) (blink-read-pattern-line handle 0))
; (blink-write-pattern-line! handle 100 0 255 0 0)
; (test (list 100 0 255 0) (blink-read-pattern-line handle 0))

; (map (lambda (color)
;        (let ((r (car color)) (g (cadr color)) (b (caddr color)))
;          (blink-set! handle r g b)
;          (sleep 1)))
;      '((255 0 0) (0 255 0) (0 0 255)))
; 
; (blink-off! handle)
; (sleep 1)

; (map (lambda (color)
;        (let ((r (car color)) (g (cadr color)) (b (caddr color)))
;          (blink-fade! handle 500 r g b)
;          (sleep 1)))
;      '((255 0 0) (0 255 0) (0 0 255)))
; 
; (blink-off! handle)
; (sleep 1)
; 
; (define (playit handle index)
;   (if (= 100 index)
;     (blink-play! handle)
;     (begin
;       (blink-write-pattern-line! handle 100 0 255 0 index)
;       (playit handle (+ index 1)))))
; 
; (playit handle 0)

; (blink-write-pattern-line! handle 100 255 0 0 0)
; (blink-write-pattern-line! handle 100 0 255 0 1)
; (blink-write-pattern-line! handle 100 0 0 255 2)
; (blink-play! handle)

(test-end)

(define (times proc t)
  (let loop ((i 0))
    (if (not (= t i))
      (begin
        (proc i)
        (loop (+ i 1))))))

(define (doit t)
  (times (lambda (i) (blink-write-pattern-line! handle 0 0 0 0 i)) t))

; (doit 12)

; (blink-write-pattern-line! handle 1000 255 0 0 0)
; (blink-write-pattern-line! handle 1000 0 255 0 11)
; (print (blink-read-pattern-line handle 0))
; (blink-play! handle)

; (blink-write-pattern-line! handle 100 0 0 0 0)
; (blink-write-pattern-line! handle 100 0 0 0 1)
; (blink-write-pattern-line! handle 100 0 255 0 11)
; (print (blink-read-pattern-lines handle))

; (blink-on! handle)
; (blink-set! handle 255 0 0)

(test-exit)
