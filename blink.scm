(module blink
  (blink-devices
   blink-open
   blink-color!)
  (import scheme chicken ports usb srfi-1 srfi-69)
  (use usb srfi-1 srfi-69)

  (define (pack buf)
    (with-output-to-string (lambda ()
        (for-each (lambda (byte) (write-char (integer->char byte))) buf))))

  (define request-type (bitwise-ior usb::request-type-class
                                    usb::recipient-device
                                    usb::endpoint-out))
  (define request #x09)
  (define timeout 5000)
  (define index 0)

  (define (blink-write handle buf)
    (let ((value (bitwise-ior (arithmetic-shift 3 8)
                              (bitwise-and (car buf) #xFF)))
          (bytes (pack buf)))
      (usb-control-transfer handle
                            request-type
                            request
                            value
                            index
                            bytes
                            timeout)))

  (define (blink-color! dev r g b)
    (blink-write dev (list 1 #x6e r g b 0 0 0 0)))

  (define (blink-dev? dev)
    (let ((desc (usb-device-descriptor dev)))
      (and (= #x27B8 (usb-device.idVendor desc))
           (= #x01ED (usb-device.idProduct desc)))))

  (define usb-ctx (usb-make-context))

  (define (blink-devices)
    (filter blink-dev? (usb-devices usb-ctx)))

  (define (blink-open dev)
    (let ((dev (usb-open dev)))
      (usb-claim-interface! dev) dev))
)

(import blink)

(map (lambda (dev)
        (blink-color! (blink-open dev)
                      (* 8 (random 32))
                      (* 8 (random 32))
                      (* 8 (random 32))))
     (blink-devices))
