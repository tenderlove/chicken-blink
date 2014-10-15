# blink(1) API for Chicken scheme

This egg provides an API for interacting with [blink(1)](http://blink1.thingm.com/) hardware.

## Synopsis

Here is a program that fades from red to green to blue.  It takes 1000ms to
switch colors, then waits 2 seconds before going to the next color.

```scheme
(use blink)
(define colors (circular-list '(255 0 0)   ; fade to red
                              '(0 255 0)   ; fade to green
                              '(0 0 255))) ; fade to blue

(define t 1000) ; take 1000ms to change

(with-first-blink (lambda (blink)
                    (for-each (lambda (color)
                                (let ((cmd (cons blink (cons t color))))
                                      (print (list "fading to" color))
                                      (apply blink-fade! cmd)
                                      (sleep 2))) ; wait 2 sec before looping
                              colors)))
```

