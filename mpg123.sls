;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2020 Julian Herrera
;; SPDX-License-Identifier: MIT
#!r6rs

(library (mpg123)
  (export
   ;; Library and Handle Setup
   initialize
   new-handle
   delete-handle

   ;; Decoder Selection
   get-decoders
   get-supported-decoders
   get-current-decoder
   set-decoder

   ;; Output Audio Format
   get-rates
   get-encodings
   get-encoding-size
   set-handle-format-none
   set-handle-format-all
   set-format
   get-format-support
   get-format

   ;; File Input and Decoding
   ;;open-fixed-stream
   open-stream
   open-fd-stream
   open-handle
   open-feed
   close-stream
   read-stream
   get-stream-feed-data
   decode-stream
   decode-frame
   decode-frame-by-frame
   get-frame-data
   get-frame-position

   ;; Low Level I/O
   outblock
   )
  (import (chezscheme))

  (define lib-mpg123 (load-shared-object "libmpg123.so"))

  ;; Helpers
  
  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name fpname args)
       (define name
         (foreign-procedure (symbol->string 'fpname) args ret)))))

  (define (c-string-array->list-of-strings ptr)
    (let ([mem-block-ptr ptr])
      (define (inner index list-of-strings string-value prev-value)
        (let ([value (foreign-ref 'char mem-block-ptr index)])
          (if (eq? value #\nul)
              (if (eq? prev-value #\nul)
                  (reverse list-of-strings)
                  (begin
                    (inner (+ 1 index)
                           (cons string-value list-of-strings) "" value)))
              (inner (+ 1 index) list-of-strings (string-append string-value (string value)) value))))
      (inner 0 '() "" #\nul)))

  ;; Library and Handle Setup
  
  (define-function int %initialize mpg123_init ())
  (define-function void* %new-handle mpg123_new (ptr))
  (define-function void delete-handle mpg123_delete (void*))
  
  (define (initialize)
    (handle-mpg123-error (%initialize)))

  (define new-handle
    (case-lambda
      [() (%new-handle (eval 0))]
      [(decoder) (%new-handle decoder)]))

  ;; Error Handling

  (define-function string get-error-text mpg123_plain_strerror (int))

  (define (handle-mpg123-error err-code)
    (if (not (zero? err-code))
        (raise (condition
                (make-error)
                (make-message-condition (get-error-text err-code))))))

  ;; Decoder Selection
  (define-function void* %decoders mpg123_decoders ())
  (define-function string get-current-decoder mpg123_current_decoder (void*))
  (define-function void* %supported-decoders mpg123_supported_decoders ())

  (define (get-decoders)
    (c-string-array->list-of-strings (foreign-ref 'void* (%decoders) 0)))

  (define (get-supported-decoders)
    (c-string-array->list-of-strings (foreign-ref 'void* (%supported-decoders) 0)))

  (define-function int %set-decoder mpg123_decoder (void* string))

  (define (set-decoder handle decoder-name)
    (display (handle-mpg123-error (%set-decoder handle decoder-name))))

  ;; Output Audio Format

  (define-function void %get-rates mpg123_rates (void* void*))

  (define-ftype Rates (* long))

  (define-ftype PRates (* Rates))

  (define (get-rates)
    (let ([l-ptr (foreign-alloc (ftype-sizeof PRates))]
          [n-ptr (foreign-alloc (ftype-sizeof size_t))])
      (%get-rates l-ptr n-ptr)
      (let build-rates-list ([n (foreign-ref 'size_t n-ptr 0)]
                             [l '()])
        (if (zero? n)
            (begin
              (foreign-free l-ptr)
              (foreign-free n-ptr)
              l)
            (let
                ([nl
                  (cons
                   (foreign-ref 'long (foreign-ref 'void* l-ptr 0) (* (ftype-sizeof long) (- n 1)))
                   l)])
              (build-rates-list (- n 1) nl))))))

  (define-function void %get-encodings mpg123_encodings (void* void*))
  
  (define-ftype Encodings (* int))
  (define-ftype PEncodings (* Encodings))

  (define (get-encodings)
    (let ([l-ptr (foreign-alloc (ftype-sizeof PEncodings))]
          [n-ptr (foreign-alloc (ftype-sizeof size_t))])
      (%get-encodings l-ptr n-ptr)
      (let build-encodings-list ([n (foreign-ref 'size_t n-ptr 0)]
                                 [l '()])
        (if (zero? n)
            (begin
              (foreign-free l-ptr)
              (foreign-free n-ptr)
              l)
            (let
                ([nl
                  (cons
                   (foreign-ref 'int (foreign-ref 'void* l-ptr 0) (* (ftype-sizeof int) (- n 1)))
                   l)])
              (build-encodings-list (- n 1) nl)))))  )

  (define-function int get-encoding-size mpg123_encsize (int))

  (define-function int %set-handle-format-none mpg123_format_none (void*))

  (define (set-handle-format-none handle)
    (handle-mpg123-error (%set-handle-format-none handle)))

  (define-function int %set-handle-format-all mpg123_format_all (void*))

  (define (set-handle-format-all handle)
    (handle-mpg123-error (%set-handle-format-all handle)))

  (define-function int %set-format mpg123_format (void* long int int))

  (define (set-format handle rate channels encodings)
    (handle-mpg123-error (%set-format handle rate channels encodings)))

  (define-function int get-format-support mpg123_format_support (void* long int))

  (define-function int %get-format mpg123_getformat2 (void* void* void* void* int))
  
  (define (get-format handle k . rest)
    (let ([clear (if (null? rest) 1 (car rest))])
      (let ([rate-ptr (foreign-alloc (ftype-sizeof long))]
            [channels-ptr (foreign-alloc (ftype-sizeof int))]
            [encoding-ptr (foreign-alloc (ftype-sizeof int))])
        (%get-format handle rate-ptr channels-ptr encoding-ptr clear)
        (let ([rate (foreign-ref 'long rate-ptr 0)]
              [channels (foreign-ref 'int channels-ptr 0)]
              [encoding (foreign-ref 'int encoding-ptr 0)])
          (foreign-free rate-ptr)
          (foreign-free channels-ptr)
          (foreign-free encoding-ptr)
          (k rate channels encoding)))))

  ;; File Input And Decoding

  ;; (define-function int %open-fixed-stream mpg123_open_fixed (void* string int int))

  ;; (define (open-fixed-stream handle path channels encoding)
  ;;   (handle-mpg123-error (%open-fixed-stream handle path channels encoding)))  

  (define-function int %open-stream mpg123_open (void* string))

  (define (open-stream handle path)
    (handle-mpg123-error (%open-stream handle path)))

  (define-function int %open-fd-stream mpg123_open_fd (void* int))

  (define (open-fd-stream handle fd)
    (handle-mpg123-error (%open-fd-stream handle fd)))

  (define-function int %open-handle mpg123_open_handle (void* void*))

  (define (open-handle handle io-handle)
    (handle-mpg123-error (%open-handle handle io-handle)))

  (define-function int %open-feed mpg123_open_feed (void*))

  (define (open-feed handle)
    (handle-mpg123-error (%open-feed handle)))

  (define-function int %close-stream mpg123_close (void*))

  (define (close-stream handle)
    (handle-mpg123-error (%close-stream handle)))

  (define-function int %read-stream mpg123_read (void* void* size_t (* size_t)))

  (define (read-stream handle output-memory output-memory-size done)
    (handle-mpg123-error (%read-stream handle output-memory output-memory-size done)))

  (define-function int %get-stream-feed-data mpg123_feed (void* void* size_t))

  (define (get-stream-feed-data handle in size)
    (handle-mpg123-error (%get-stream-feed-data handle in size)))

  (define-function int %decode-stream mpg123_decode
    (void* void* size_t void* size_t void*))

  (define (decode-stream handle input-memory input-memory-size output-memory output-memory-size done)
    (handle-mpg123-error (%decode-stream handle input-memory input-memory-size output-memory output-memory-size done)))

  (define-function int %decode-frame mpg123_decode_frame(void* void* void* void*))

  (define (decode-frame handle num audio bytes)
    (handle-mpg123-error (%decode-frame handle num audio bytes)))

  (define-function int %decode-frame-by-frame mpg123_framebyframe_decode (void* void* void* void*))

  (define (decode-frame-by-frame handle num audio bytes)
    (handle-mpg123-error (%decode-frame-by-frame handle num audio bytes)))

  (define-function int %frame-by-frame-next mpg123_framebyframe_next (void*))

  (define (frame-by-frame-next handle)
    (handle-mpg123-error (%frame-by-frame-next handle)))

  (define-function int %get-frame-data mpg123_open_fd (void* void* void* void*))
  
  (define (get-frame-data handle header body-data body-bytes)
    (handle-mpg123-error (%get-frame-data handle header body-data body-bytes)))

  (define-function int %get-frame-position mpg123_framepos (void*))
  
  (define (get-frame-position handle)
    (handle-mpg123-error (%get-frame-position handle)))
  
  ;; Low Level I/O
  (define-function int %replace-buffer (void* void* size_t))

  (define (replace-buffer handle data size)
    (handle-mpg123-error (%replace-buffer handle data size)))
  
  (define-function size_t outblock mpg123_outblock	(void*))

  ;; TODO
  ;; mpg123_replace_reader()
  ;; mpg123_replace_reader_handle()
  )

