(in-package :ts.parser)

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
    
(defun parse (stream)
  (let ((header (parse-ts-header stream)))
    
    header))
