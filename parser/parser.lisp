(in-package :ts.parser)

(defstruct context
  pmt-pids
  pes-pids)

(defun init-context () (make-context))

(defun check-sync-byte (stream)
  (let ((sync-byte (read-byte stream)))
    (assert (= sync-byte ts:+SYNC_BYTE+) () "not a ts packet: sync-byte=0x~2,'0x" sync-byte)
    t))

(defun parse-ts-header (stream)
  (check-sync-byte stream)
  (let ((byte1 (read-byte stream))
        (byte2 (read-byte stream))
        (byte3 (read-byte stream)))
    (ts:make-ts-header 
     :sync-byte                    ts:+SYNC_BYTE+
     :transport-error-indicator    (ldb (byte 1 7) byte1)
     :payload-unit-start-indicator (ldb (byte 1 6) byte1)
     :transport-priority           (ldb (byte 1 5) byte1)
     :pid                          (+ (ash (ldb (byte 5 0) byte1) 8) 
                                      byte2)
     :scrambling-control           (ldb (byte 2 6) byte3)
     :adaptation-field-exist       (ldb (byte 2 4) byte3)
     :continuity-counter           (ldb (byte 4 0) byte3))))

(defun parse-psi-pat (header stream rest-count &aux (count 0) (byte 0) (section-length 0))
  (flet ((next-byte () (incf count) (setf byte (read-byte stream))))
    (prog1
    (ts:make-payload-pat
     :pointer-field (and (= 1 (ts:ts-header-payload-unit-start-indicator header))
                         (next-byte))
     :table-id      (next-byte)
     :section-syntax-indicator (ldb (byte 1 7) (next-byte))
     :zero                     (ldb (byte 1 6) byte)
     :reserved1                (ldb (byte 2 4) byte)
     :section-length           (setf section-length
                                     (+ (ash (ldb (byte 4 0) byte) 8) 
                                        (next-byte)))
     :transport-stream-id      (+ (ash (next-byte) 8) (next-byte))
     :reserved2                (ldb (byte 2 6) (next-byte))
     :version-number           (ldb (byte 5 1) byte)
     :current/next-indicator   (ldb (byte 1 0) byte)
     :section-number           (next-byte)
     :last-section-number      (next-byte)

     :pmt-map (loop REPEAT (/ (- section-length 9)
                              (/ (+ 16 3 13) 8))
                    COLLECT 
                    (list (+ (ash (next-byte) 8) (next-byte))             ; program-num
                          (ldb (byte 3 5) (next-byte))                    ; reserved
                          (+ (ash (ldb (byte 5 0) byte) 8) (next-byte)))) ; program-PID
     
     :crc32   (+ (ash (next-byte) 24)
                 (ash (next-byte) 16)
                 (ash (next-byte)  8)
                 (next-byte)))

    (loop FOR i FROM count BELOW rest-count DO (read-byte stream))))) ; XXX:

(defun parse-psi-pmt (header stream rest-count)
  (let ((count 0)
        (byte 0)
        (section-length 0)
        (program-info-length 0)
        (es-info-length 0))
    (flet ((next-byte () (incf count) (setf byte (read-byte stream))))
      (prog1
          (ts:make-payload-pmt ; TODO: patと共通化できる
          :pointer-field (and (= 1 (ts:ts-header-payload-unit-start-indicator header))
                         (next-byte))
          :table-id      (next-byte)
          :section-syntax-indicator (ldb (byte 1 7) (next-byte))
          :zero                     (ldb (byte 1 6) byte)
          :reserved1                (ldb (byte 2 4) byte)
          :section-length           (setf section-length
                                          (+ (ash (ldb (byte 4 0) byte) 8) 
                                             (next-byte)))
          :program-num            (+ (ash (next-byte) 8) (next-byte))
          :reserved2              (ldb (byte 2 6) (next-byte))
          :version-number         (ldb (byte 5 1) byte)
          :current/next-indicator (ldb (byte 1 0) byte)
          :section-number         (next-byte)
          :last-section-number    (next-byte)
          :reserved3              (ldb (byte 3 5) (next-byte))
          :pcr-pid                (+ (ash (ldb (byte 5 0) byte) 8)
                                     (next-byte))
          :reserved4              (ldb (byte 4 4) (next-byte))
          :program-info-length    (setf program-info-length
                                        (+ (ash (ldb (byte 4 0) byte) 8)
                                           (next-byte)))
          :program-descriptors (loop REPEAT program-info-length
                                     COLLECT (next-byte))
          :stream-infos (loop WHILE (< count (- section-length 4))
                          COLLECT
                          (list (next-byte) ; stream-type
                                (ldb (byte 3 5) (next-byte)) ; reserved
                                (+ (ash (ldb (byte 5 0) byte) 8) (next-byte)) ; elementary-PID
                                (ldb (byte 4 4) (next-byte)) ; reserved
                                (setf es-info-length
                                      (+ (ash (ldb (byte 4 0) byte) 8) (next-byte)))  ; ES info length
                                (loop REPEAT es-info-length
                                      COLLECT (next-byte))   ; ES Descriptor
                                ))
          
          :crc32   (+ (ash (next-byte) 24)
                      (ash (next-byte) 16)
                      (ash (next-byte)  8)
                      (next-byte))
          
          )
        (loop FOR i FROM count BELOW rest-count DO (read-byte stream))))))

(defun parse-payload (header stream context rest-count)
  (ecase (ts:get-packet-type header (context-pmt-pids context))
    (:psi-pat (let ((pat (parse-psi-pat header stream rest-count)))
                (loop FOR (_ __ program-pid) IN (ts::payload-pat-pmt-map pat)
                      DO
                      (pushnew program-pid (context-pmt-pids context)))
                pat))
    (:psi-pmt (let ((pmt (parse-psi-pmt header stream rest-count)))
                (loop FOR (_1 _2 pes-pid _3 _4 _5 _6) IN (ts::payload-pmt-stream-infos pmt)
                      DO
                      (pushnew pes-pid (context-pes-pids context)))
                pmt))
    (:null 
     (dotimes (i rest-count) (read-byte stream))
     (ts:make-payload-null))
    (:unknown 
     (dotimes (i rest-count)
       (read-byte stream))
     (ts:make-payload-unknown :data (sb-ext:string-to-octets "")))))

(defun parse-adaptation-field (stream)
  (let ((length 0)
        (count 0)
        (byte 0)
        (f1 0)
        (f2 0)
        (f3 0)
        (f4 0)
        (f5 0))
    (flet ((next-byte () (incf count) (setf byte (read-byte stream))))
      (values
        (ts:make-adaptation-field 
         :length                  (setf length (next-byte))
         :discontinuity-indicator (ldb (byte 1 7) (next-byte))
         :random-access-indicator (ldb (byte 1 6) byte)
         :es-priority-indicator   (ldb (byte 1 5) byte)
         :pcr-flag                (setf f1 (ldb (byte 1 4) byte))
         :opcr-flag               (setf f2 (ldb (byte 1 3) byte))
         :splicing-point-flag     (setf f3 (ldb (byte 1 2) byte))
         :transport-private-data-flag     (setf f4 (ldb (byte 1 1) byte))
         :adaptation-field-extension-flag (setf f5 (ldb (byte 1 0) byte))
         :pcr              (when (plusp f1) (loop FOR i FROM 5 DOWNTO 0 SUM (ash (next-byte) (* i 8))))
         :opcr             (when (plusp f2) (loop FOR i FROM 5 DOWNTO 0 SUM (ash (next-byte) (* i 8))))
         :splice-countdown (when (plusp f3) (next-byte))
         :stuffing-bytes (list (when (plusp f4)
                                 (loop REPEAT (next-byte) COLLECT (next-byte)))
                               (when (plusp f5)
                                 (loop REPEAT (next-byte) COLLECT (next-byte)))
                               (loop FOR i FROM count TO length COLLECT (next-byte))))
        count))))

(defun parse-one (stream context)
  (let ((header (parse-ts-header stream)))
    (multiple-value-bind (adaptation-field read-count)
                         (if (ldb-test (byte 1 1) (ts:ts-header-adaptation-field-exist header))
                             (parse-adaptation-field stream)
                           (values nil 0))
      (let ((payload (when (ldb-test (byte 1 0) (ts:ts-header-adaptation-field-exist header))
                       (parse-payload header stream context (- 188 4 read-count)))))
        (ts:make-packet :ts-header header
                        :adaptation-field adaptation-field
                        :payload payload)))))

(defun parse (stream &key (context (init-context)))
  (values
   (loop WHILE (listen stream)
         COLLECT
         (parse-one stream context))
   context))
