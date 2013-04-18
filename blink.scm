(use usb srfi-1)

(define (pack buf)
  (with-output-to-string (lambda ()
      (for-each (lambda (byte) (write-char (integer->char byte))) buf))))

(define (blink-write handle buf)
  (let ((request-type (bitwise-ior usb::request-type-class 
                                   usb::recipient-device
                                   usb::endpoint-out))
        (request #x09)
        (value (bitwise-ior (arithmetic-shift 3 8)
                            (bitwise-and (car buf) #xFF)))
        (index 0)
        (bytes (pack buf))
        (m 5000))
    (usb-control-transfer handle
                     request-type
                     request
                     value
                     index
                     bytes
                     m)))

(define (set dev r g b)
  (blink-write dev (list 1 #x6e r g b 0 0 0 0)))

(define (blink-dev? dev)
  (let ((desc (usb-device-descriptor dev)))
    (and (= #x27B8 (usb-device.idVendor desc))
         (= #x01ED (usb-device.idProduct desc)))))

(define blink-dev (find blink-dev?
                        (usb-devices (usb-make-context))))

(define blink-handle (usb-open blink-dev))
(usb-claim-interface! blink-handle)

(display blink-dev)
(print)

(set blink-handle 0 0 0)
