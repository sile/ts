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

(defun parse-pes (stream rest-count)
  (let ((header-start 0)
        (header-length 0)
        (count 0)
        (byte 0)
        (f1 0)
        (f2 0)
        (f3 0)
        (f4 0)
        (f5 0)
        (f6 0)
        (f7 0))
    (labels ((next-byte () (incf count) (setf byte (read-byte stream)))
             (read-dts/pts (dts?)
               (declare (ignore dts?))
               (let ((check-bits (ldb (byte 4 4) (next-byte)))
                     (data32-30  (ldb (byte 3 1) byte))
                     (marker1    (ldb (byte 1 0) byte))
                     (data29-15  (+ (ash (next-byte) 7)
                                    (ldb (byte 7 1) (next-byte))))
                     (marker2    (ldb (byte 1 0) byte))
                     (data14-0   (+ (ash (next-byte) 7)
                                    (ldb (byte 7 1) (next-byte))))
                     (marker3    (ldb (byte 1 0) byte)))
                 (declare (ignore check-bits marker1 marker2 marker3))
                 (+ (ash data32-30 30)
                    (ash data29-15 15)
                    data14-0))))

      (prog1
          (ts:make-payload-pes
           :packet-start-prefix-code (+ (ash (next-byte) 16)
                                        (ash (next-byte) 8)
                                        (next-byte))
           :stream-id (next-byte)
           :pes-packet-length (+ (ash (next-byte) 8)
                                 (next-byte))

           :marker-bits        (ldb (byte 2 6) (next-byte))
           :scrambling-control (ldb (byte 2 4) byte)
           :priority           (ldb (byte 1 3) byte)
           :data-alignment-indicator (ldb (byte 1 2) byte)
           :copyright          (ldb (byte 1 1) byte)
           :original-or-copy   (ldb (byte 1 0) byte)
           :pts-dts-indicator  (setf f1 (ldb (byte 2 6) (next-byte)))
           :escr-flag          (setf f2 (ldb (byte 1 5) byte))
           :es-rate-flag       (setf f3 (ldb (byte 1 4) byte))
           :dsm-trick-mode-flag(setf f4 (ldb (byte 1 3) byte))
           :additional-copy-into-flag (setf f5 (ldb (byte 1 2) byte))
           :crc-flag           (setf f6 (ldb (byte 1 1) byte))
           :extension-flag     (setf f7 (ldb (byte 1 0) byte))
           :pes-header-length  (prog1 (setf header-length (next-byte))
                                 (setf header-start count))

           :pts  (when (ldb-test (byte 1 1) f1) (read-dts/pts nil))
           :dts  (when (ldb-test (byte 1 0) f1) (read-dts/pts t))
           :escr (when (= f2 1) (loop FOR i FROM 5 DOWNTO 0 SUM (ash (next-byte) (* i 8))))
           :es   (when (= f3 1) (loop FOR i FROM 3 DOWNTO 0 SUM (ash (next-byte) (* i 8))))
           :dsm-trick-mode (when (= f4 1) (next-byte))
           :additional-copy-info (when (= f5 1) (next-byte))
           :pes-crc (when (= f6 1) (loop FOR i FROM 2 DOWNTO 0 SUM (ash (next-byte) (* i 8))))
           
           :stuffing-bytes     (loop FOR i FROM (- count header-start) BELOW header-length
                                     COLLECT (next-byte))
           )
        (loop FOR i FROM count BELOW rest-count DO (read-byte stream))))))

(defun parse-payload (header stream context rest-count)
  (ecase (ts:get-packet-type header (context-pmt-pids context)
                                    (context-pes-pids context))
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
    (:pes (let ((pes (parse-pes stream rest-count)))
            pes))
    (:null 
     (dotimes (i rest-count) (read-byte stream))
     (ts:make-payload-null))
    (:data
     (ts:make-payload-data :data (coerce (loop REPEAT rest-count COLLECT (read-byte stream))
                                         '(simple-array (unsigned-byte 8) (*)))))
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
    (labels ((next-byte () (incf count) (setf byte (read-byte stream))))
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
         :pcr              (when (plusp f1) (ash (loop FOR i FROM 5 DOWNTO 0 SUM (ash (next-byte) (* i 8))) -15))
         :opcr             (when (plusp f2) (ash (loop FOR i FROM 5 DOWNTO 0 SUM (ash (next-byte) (* i 8))) -15))
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
