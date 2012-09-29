(in-package :ts)

(defconstant +SYNC_BYTE+ #x47)

(defconstant +PID_PAT+ 0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ts-header-document* (format nil "~
[Transport Stream Header]
----------------------------------------------------------------------------------------------------
sync byte                    | 0x47
----------------------------------------------------------------------------------------------------
Transport Error Indicator    | Set by demodulator if can't correct errors in the stream, 
                             | to tell the demultiplexer that the packet has an uncorrectable error.
----------------------------------------------------------------------------------------------------
Payload Unit Start Indicator | 1 means start of PES data or PSI otherwise zero only.
----------------------------------------------------------------------------------------------------
Transport Priority           | 1 means higher priority than other packets with the same PID.
----------------------------------------------------------------------------------------------------
PID                          | Packet ID
----------------------------------------------------------------------------------------------------
Scrambling control           | '00' = Not scrambled   
                             | '01' = Reserved for future use
                             | '10' = Scrambled with even key
                             | '11' = Scrambled with odd key
----------------------------------------------------------------------------------------------------
Adaptation field exist       | '01' = no adaptation fields, payload only
                             | '10' = adaptation field only
                             | '11' = adaptation field and payload
----------------------------------------------------------------------------------------------------
Continuity counter           | Incremented only when a payload is present
                             | (i.e., adaptation field exist is 01 or 11)
----------------------------------------------------------------------------------------------------")))

(defstruct ts-header #.*ts-header-document*
  (sync-byte                    0 :type (unsigned-byte  8))
  (transport-error-indicator    0 :type (unsigned-byte  1))
  (payload-unit-start-indicator 0 :type (unsigned-byte  1))
  (transport-priority           0 :type (unsigned-byte  1))
  (pid                          0 :type (unsigned-byte 13))
  (scrambling-control           0 :type (unsigned-byte  2))
  (adaptation-field-exist       0 :type (unsigned-byte  2))
  (continuity-counter           0 :type (unsigned-byte  4)))

;; TODO: スペシャル変数でいくつかの表示モードを切り替えられるようにしておく
(defmethod print-object ((o ts-header) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "\"~a:~a:~a:~4,'0x:~2,'0b:~2,'0b:~x\""
            (if (= 1 (ts-header-transport-error-indicator o))    "T" "F")
            (if (= 1 (ts-header-payload-unit-start-indicator o)) "T" "F")
            (if (= 1 (ts-header-transport-priority o))           "T" "F")
            (ts-header-pid o)
            (ts-header-scrambling-control o)
            (ts-header-adaptation-field-exist o)
            (ts-header-continuity-counter o))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *pat-document* (format nil "~
[PAT: Program Association Table]
----------------------------------------------------------------------------------------------------
Pointer field            | Present if payload_unit_start_indicator bit is set in the TS header bytes. 
                         | Gives the number of bytes from the end of this field to the start of payload data.
----------------------------------------------------------------------------------------------------
Table ID                 | 0x00
----------------------------------------------------------------------------------------------------
Section syntax Indicator | Always 1 for PAT
----------------------------------------------------------------------------------------------------
0                        | Always 0 for PAT
----------------------------------------------------------------------------------------------------
Reserved                 | Always set to binary '11'
----------------------------------------------------------------------------------------------------
Section Length           | Informs how many programs are listed below by specifying the number of bytes of
                         | this section, starting immediately following this field and including the CRC. 
                         | First two bits must be zero.
----------------------------------------------------------------------------------------------------
Transport stream ID      | User defined data. Value not important to demuxers or players.
----------------------------------------------------------------------------------------------------
Reserved                 | Always set to binary '11'
----------------------------------------------------------------------------------------------------
Version number           | Table version number. Incremented by 1 when data in table changes. 
                         | Wraps around from 31 to 0.
----------------------------------------------------------------------------------------------------
Current/next indicator   | If 0, table data isn't applicable yet (becomes applicable when set to 1)
----------------------------------------------------------------------------------------------------
Section number           | Index of this section in the sequence of all PAT table sections. 
                         | First section is numbered 0
----------------------------------------------------------------------------------------------------
Last section number      | Index of last section of PAT table
----------------------------------------------------------------------------------------------------
===
# Repeated N times depending on section length
----------------------------------------------------------------------------------------------------
Program Num              |
----------------------------------------------------------------------------------------------------
Reserved                 | Always set to binary '111'
----------------------------------------------------------------------------------------------------
Program PID              | packets with this PID are assumed to be PMT tables (see below)
----------------------------------------------------------------------------------------------------
===
----------------------------------------------------------------------------------------------------
CRC32                    |
----------------------------------------------------------------------------------------------------
")))

(defstruct payload)

;; XXX: 名前は不適切: payloadではない
(defstruct (payload-pat (:include payload)) #.*pat-document*
  (pointer-field            0 :type (or null (unsigned-byte 8)))
  (table-id                 0 :type (unsigned-byte  8)) ; always set to 0
  (section-syntax-indicator 1 :type (unsigned-byte  1)) ; always set to 1
  (zero                     0 :type (unsigned-byte  1)) ; always set to 0
  (reserved1                3 :type (unsigned-byte  2)) ; always set to binary '11'
  (section-length           0 :type (unsigned-byte 12))
  (transport-stream-id      0 :type (unsigned-byte 16))
  (reserved2                3 :type (unsigned-byte  2)) ; always set to binary '11'
  (version-number           0 :type (unsigned-byte  5))
  (current/next-indicator   0 :type (unsigned-byte  1))
  (section-number           0 :type (unsigned-byte  8))
  (last-section-number      0 :type (unsigned-byte  8))
  (pmt-map                  t :type list)  ; (list (list program-number reserved program-PID))
  (crc32                    0 :type (unsigned-byte 32)))

(defstruct (payload-unknown (:include payload))
  (data t :type (array (unsigned-byte 8))))

(defun get-packet-type (header)
  (case (ts-header-pid header)
    (#x00 :psi-pat)  ; Program-specific information: program association table
    (otherwise :unknown))) ; TODO:
    

(defstruct packet 
  (ts-header        t :type ts-header)
  adaptation-field
  (payload          t :type payload))

  ;;(adaptation-field t :type (or null))
  ;;(payload          t :type (or null))
;;)
