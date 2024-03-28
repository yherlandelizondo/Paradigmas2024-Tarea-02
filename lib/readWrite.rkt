#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Read and write section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function to write data to a .txt file
(define (writeFile graph edges aux path)

  ;creates the .txt file (if exist, replace it.)

  (define output-port (open-output-file path #:exists 'replace))

  ;adding the file content
  (write (list graph edges aux) output-port)

  ;close the file
  (close-output-port output-port))

;Function for reading data from a file
(define (readFile path)

  ;(open-input-file "./tmp/temp.txt") -> store the file descriptor (number to refer to an open file in the OS)
  ;(read (open-input-file "./tmp/temp.txt")) -> read the file, using the file descriptor
  ;(close-input-port (open-input-file "./tmp/temp.txt")) -> close the open file

  (define input-port (open-input-file path))
  (define file-content (read input-port))
  (close-input-port input-port)
  file-content)


(provide writeFile readFile)