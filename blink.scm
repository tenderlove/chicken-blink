(module blink
  (blink-devices
   blink-open
   blink-set!
   blink-fade!
   blink-off!
   blink-write-pattern-line!
   blink-play!)
  (import scheme chicken ports usb srfi-1 srfi-69)
  (use usb srfi-1 srfi-69)

  (define << arithmetic-shift)
  (define (>> n count) (arithmetic-shift n (- count)))

  (define (pack buf)
    (with-output-to-string (lambda ()
        (for-each (lambda (byte) (write-char (integer->char byte))) buf))))

  (define request-type (bitwise-ior usb::request-type-class
                                    usb::recipient-interface
                                    usb::endpoint-out))
  (define request #x09)
  (define timeout 1000)
  (define interface-number 0)

  (define (blink-write handle buf)
    (let ((value (bitwise-ior (<< usb::request-set-feature 8)
                              (bitwise-and (car buf) #xFF)))
          (bytes (pack buf)))
      (usb-control-transfer handle
                            request-type
                            request
                            value
                            interface-number
                            bytes
                            timeout)))

  ; set the color to r g b on dev.  r g b should be values from 0 to 255
  (define (blink-set! dev r g b)
    (blink-write dev (list 1 #x6e r g b 0 0 0 0)))

  ; Turn the light off
  (define (blink-off! dev) (blink-set! dev 0 0 0))

  ; Fade to r g b in ms milliseconds
  (define (blink-fade! dev ms r g b)
    (let* ((dms (/ ms 10))
           (th (>> dms 8))
           (tl (bitwise-and dms #xFF)))
      (blink-write dev (list 1 #x63 r g b th tl 0 0))))

  ; Write a pattern line to the blink(1)
  (define (blink-write-pattern-line! dev ms r g b pos)
    (let* ((dms (/ ms 10))
           (th (>> dms 8))
           (tl (bitwise-and dms #xFF)))
      (blink-write dev (list 1 #x50 r g b th tl pos 0))))

  ; Play the pattern
  (define (blink-play! dev #!optional (pos 0) (play 1))
    (blink-write dev (list 1 #x70 play pos 0 0 0 0)))

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

(blink-off! handle)
(sleep 1)

; (map (lambda (color)
;        (let ((r (car color)) (g (cadr color)) (b (caddr color)))
;          (blink-set! handle r g b)
;          (sleep 1)))
;      '((255 0 0) (0 255 0) (0 0 255)))
; 
; (blink-off! handle)
; (sleep 1)

(map (lambda (color)
       (let ((r (car color)) (g (cadr color)) (b (caddr color)))
         (blink-fade! handle 500 r g b)
         (sleep 1)))
     '((255 0 0) (0 255 0) (0 0 255)))

(blink-off! handle)
(sleep 1)

(define (playit handle index)
  (if (= 100 index)
    (blink-play! handle)
    (begin
      (blink-write-pattern-line! handle 100 0 255 0 index)
      (playit handle (+ index 1)))))

(playit handle 0)

; (blink-write-pattern-line! handle 100 255 0 0 0)
; (blink-write-pattern-line! handle 100 0 255 0 1)
; (blink-write-pattern-line! handle 100 0 0 255 2)
; (blink-play! handle)

(test-end)
(test-exit)
