;; TS全体に占める各データの割合をカウント／表示するサンプル

(defconstant +TS_PACKET_HEADER_SIZE+ 4) ;; byte
(defconstant +TS_PACKET_SIZE+ 188) 

(defun adaptation-field-size (field)
  (+ 1 (ts::adaptation-field-length field)))

(defun pes-header-size (pes)
  (+ 8 (ts::payload-pes-pes-header-length pes)))

(defstruct size-stat
  pid 
  (total 0)
  (packet-header-size 0)
  (adpt-field-size 0)
  (pes-header-size 0))
  

(defun count-size (ts-file-path)
  (with-open-file (in ts-file-path :element-type '(unsigned-byte 8))
    (let ((stat-map (make-hash-table)))
      (ts:do-each-packet (pkt in)
        (let* ((adpt-field (ts::packet-adaptation-field pkt))
               (payload (ts::packet-payload pkt))
               (pid (ts::ts-header-pid (ts::packet-ts-header pkt)))
               (stat (gethash pid stat-map)))
          (unless stat
            (setf (gethash pid stat-map) (setf stat (make-size-stat :pid pid))))
          
          (with-slots (total packet-header-size adpt-field-size pes-header-size) stat
            (incf total +TS_PACKET_SIZE+)
            (incf packet-header-size +TS_PACKET_HEADER_SIZE+)
            (when adpt-field
              (incf adpt-field-size (adaptation-field-size adpt-field)))
            (when (typep payload 'ts:payload-pes)
              (incf pes-header-size (pes-header-size payload)))
            )))
      stat-map)))

(defun show-stat (stat-map)
  (flet ((show (name pid stat)
           (with-slots (total packet-header-size adpt-field-size pes-header-size) stat
             (let ((data-size (- total packet-header-size adpt-field-size pes-header-size)))
               (format t "~&; [~a(~a)]~%" name pid)
               (format t ";   total:         ~a KB~%" (round (/ total 1024)))
               (format t ";   packet-header: ~a KB (~a %)~%" (round (/ packet-header-size 1024)) (round (* packet-header-size 100) total))
               (format t ";   adpt-field:    ~a KB (~a %)~%" (round (/ adpt-field-size 1024)) (round (* adpt-field-size 100) total))
               (format t ";   pes-header:    ~a KB (~a %)~%" (round (/ pes-header-size 1024)) (round (* pes-header-size 100) total))
               (format t ";   data:          ~a KB (~a %)~%" (round (/ data-size 1024)) (round (* data-size 100) total))
               (format t "; ~%")))))
               
    (let ((whole (make-size-stat :pid :total)))
      (maphash
       (lambda (pid stat)
         (let ((name (case pid
                       (0    :pat)
                       (4096 :pmt)       ; XXX: 不正確 (pidとpmtのマッピングはpatの中で指定される)
                       (17   :sdt)
                       (256  :pes-video) ; XXX: 不正確 (pidとpesのマッピングはpmtの中で指定される)
                       (257  :pes-audio)
                       (otherwise :unknown))))
           (show name pid stat)
           (with-slots (total packet-header-size adpt-field-size pes-header-size) stat
             (incf (size-stat-total whole) total)
             (incf (size-stat-packet-header-size whole) packet-header-size)
             (incf (size-stat-adpt-field-size whole) adpt-field-size)
             (incf (size-stat-pes-header-size whole) pes-header-size))
           ))
       stat-map)
      (show "TOTAL" "none" whole)
      t)))
