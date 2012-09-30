(in-package :ts)

(defconstant +SYNC_BYTE+ #x47)

(defconstant +PID_PAT+ 0)
(defconstant +PID_CAT+ 1) ; TODO: Conditional Access Table
(defconstant +PID_NULL+ #x1FFF)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *pmt-document* (format nil "~
[PMT: Program Map Table]
----------------------------------------------------------------------------------------------------
Pointer field            | Present if payload_unit_start_indicator bit is set in the TS header bytes. 
                         | Generally 0x00 for PMT.
----------------------------------------------------------------------------------------------------
Table ID                 | Always 0x02 for PMT
----------------------------------------------------------------------------------------------------
Section syntax Indicator | Always 1
----------------------------------------------------------------------------------------------------
0                        | Always 0
----------------------------------------------------------------------------------------------------
Reserved                 | Always set to binary '11'
----------------------------------------------------------------------------------------------------
Section Length           | Number of programs listed below. First two bits always zero.
----------------------------------------------------------------------------------------------------
Program num              |
----------------------------------------------------------------------------------------------------
Reserved                 |
----------------------------------------------------------------------------------------------------
Version number           | Incremented by 1 mod 32 each time the table data changes
----------------------------------------------------------------------------------------------------
Current/Next Indicator   | If 1, this table is currently valid. If 0, this table will become valid next.
----------------------------------------------------------------------------------------------------
Section number           | Always 0x00
----------------------------------------------------------------------------------------------------
Last section number      | Always 0x00
----------------------------------------------------------------------------------------------------
Reserved                 |
----------------------------------------------------------------------------------------------------
PCR PID                  | PID of general timecode stream, or 0x1FFF
----------------------------------------------------------------------------------------------------
Reserved                 |
----------------------------------------------------------------------------------------------------
Program info length      | Sum size of following program descriptors. First two bits must be zero.
----------------------------------------------------------------------------------------------------
Program descriptor       |
----------------------------------------------------------------------------------------------------
===
# Repeated N times depending on section length
----------------------------------------------------------------------------------------------------
stream type              |
----------------------------------------------------------------------------------------------------
Reserved                 | Always set to binary '111'
----------------------------------------------------------------------------------------------------
Elementary PID           |
----------------------------------------------------------------------------------------------------
Reserved                 |
----------------------------------------------------------------------------------------------------
ES Info length           | First two bits must be zero. Entire value may be zero
----------------------------------------------------------------------------------------------------
ES Descriptor            | If ES Info length is zero, this is omitted.
----------------------------------------------------------------------------------------------------
===
----------------------------------------------------------------------------------------------------
CRC32                    |
----------------------------------------------------------------------------------------------------
")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *pes-document* (format nil "~
[PES: Packetized Elementary Stream]
----------------------------------------------------------------------------------------------------
Packet start code prefix | 0x000001
----------------------------------------------------------------------------------------------------
Stream id                | Examples: Audio streams (0xC0-0xDF), Video streams (0xE0-0xEF)
----------------------------------------------------------------------------------------------------
PES Packet length        | Can be zero. If the PES packet length is set to zero, 
                         | the PES packet can be of any length.
                         | A value of zero for the PES packet length can be used only when 
                         | the PES packet payload is a video elementary stream.
----------------------------------------------------------------------------------------------------
Optional PES header      | not present in case of Padding stream & Private stream 2 (navigation data)
----------------------------------------------------------------------------------------------------
Stuffing bytes           |
----------------------------------------------------------------------------------------------------
Data                     | See elementary stream. In the case of private streams the first byte of 
                         | the payload is the sub-stream number.
----------------------------------------------------------------------------------------------------

==============
[Optional PES header]
----------------------------------------------------------------------------------------------------
Marker bits              | 10 binary or 0x2 hex
----------------------------------------------------------------------------------------------------
Scrambling control       | 00 implies not scrambled
----------------------------------------------------------------------------------------------------
priority                 | 
----------------------------------------------------------------------------------------------------
Data alignment indicator | 1 indicates that the PES packet header is immediately followed by 
                         | the video start code or audio syncword
----------------------------------------------------------------------------------------------------
Copyright                | 1 implies copyrighted
----------------------------------------------------------------------------------------------------
Original or Copy         | 1 implies original
----------------------------------------------------------------------------------------------------
PTS DTS indicator        | 11 = both present, 10 = only PTS
----------------------------------------------------------------------------------------------------
ESCR flag                |
----------------------------------------------------------------------------------------------------
ES rate flag             |
----------------------------------------------------------------------------------------------------
DSM trick mode flag      |
----------------------------------------------------------------------------------------------------
Additional copy info flag|
----------------------------------------------------------------------------------------------------
CRC flag                 |
----------------------------------------------------------------------------------------------------
extension flag           |
----------------------------------------------------------------------------------------------------
PES header length        | gives the length of the remainder of the PES header
----------------------------------------------------------------------------------------------------
Optional fields          | presence is determined by flag bits above
----------------------------------------------------------------------------------------------------
Stuffing Bytes           | 0xFF
----------------------------------------------------------------------------------------------------
")))

(defstruct payload)

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

(defstruct (payload-pmt (:include payload)) #.*pmt-document*
  (pointer-field            0 :type (or null (unsigned-byte 8)))
  (table-id                 0 :type (unsigned-byte 8))
  (section-syntax-indicator 0 :type (unsigned-byte 1))
  (zero                     0 :type (unsigned-byte 1))
  (reserved1                0 :type (unsigned-byte 2)) ; always set to binary '11'
  (section-length           0 :type (unsigned-byte 12)) 
  (program-num              0 :type (unsigned-byte 16))
  (reserved2                0 :type (unsigned-byte 2))
  (version-number           0 :type (unsigned-byte 5))
  (current/next-indicator   0 :type (unsigned-byte 1))
  (section-number           0 :type (unsigned-byte 8)) ; always set to 0x00
  (last-section-number      0 :type (unsigned-byte 8)) ; always set to 0x00
  (reserved3                0 :type (unsigned-byte 3))
  (pcr-pid                  0 :type (unsigned-byte 13))
  (reserved4                0 :type (unsigned-byte 4))
  (program-info-length      0 :type (unsigned-byte 12))
  (program-descriptors      t :type list) ; (list (unsigned-byte 8)). length is (/ program-info-length 8)
  (stream-infos             t :type list) ; (list (list stream-type reserved elementary-pid reserved 
                                          ;             es-info-length es-descriptors))
  (crc32                    0 :type (unsigned-byte 32)))

(defstruct (payload-pes (:include payload)) #.*pes-document*
  (packet-start-prefix-code 0 :type (unsigned-byte 24))
  (stream-id                0 :type (unsigned-byte 8))
  (pes-packet-length        0 :type (unsigned-byte 16))
  
  (marker-bits              0 :type (unsigned-byte 2))
  (scrambling-control       0 :type (unsigned-byte 2))
  (priority                 0 :type (unsigned-byte 1))
  (data-alignment-indicator 0 :type (unsigned-byte 1))
  (copyright                0 :type (unsigned-byte 1))
  (original-or-copy         0 :type (unsigned-byte 1))
  (pts-dts-indicator        0 :type (unsigned-byte 2))
  (escr-flag                0 :type (unsigned-byte 1))
  (es-rate-flag             0 :type (unsigned-byte 1))
  (dsm-trick-mode-flag      0 :type (unsigned-byte 1))
  (additional-copy-into-flag 0 :type (unsigned-byte 1))
  (crc-flag                 0 :type (unsigned-byte 1))
  (extension-flag           0 :type (unsigned-byte 1))
  (pes-header-length        0 :type (unsigned-byte 8))
  
  (pts                      0 :type (or null (unsigned-byte 34)))
  (dts                      0 :type (or null (unsigned-byte 34)))
  (escr                     0 :type (or null (unsigned-byte 48)))
  (es                       0 :type (or null (unsigned-byte 24)))
  (dsm-trick-mode           0 :type (or null (unsigned-byte 8)))
  (additional-copy-info     0 :type (or null (unsigned-byte 8)))
  (pes-crc                  0 :type (or null (unsigned-byte 16)))
  
  ;; pes-extension

  (stuffing-bytes           t :type list)
  )

(defstruct (payload-unknown (:include payload))
  (data t :type (array (unsigned-byte 8))))

(defstruct (payload-null (:include payload)))

(defstruct (payload-data (:include payload))
  (data t :type (array (unsigned-byte 8))))

(defmethod print-object ((o payload-data) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~s ~a" :length (length (payload-data-data o)))))
  
(defun get-packet-type (header &optional pmt-pids pes-pids)
  (if (= 0 (ts-header-payload-unit-start-indicator header))
      :data
  (case (ts-header-pid header)
    (#x0000 :psi-pat)  ; Program-specific information: program association table
    (#x1FFF :null)     ; Null packets
    (otherwise 
     (cond ((member (ts-header-pid header) pmt-pids) :psi-pmt)
           ((member (ts-header-pid header) pes-pids) :pes)
           (t :unknown))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *adaptation-field-document* (format nil "~
[Adaptation Field]
----------------------------------------------------------------------------------------------------
Adaptation Field Length              | Number of bytes in the adaptation field immediately following this byte.
----------------------------------------------------------------------------------------------------
Discontinuity indicator              | Set to 1 if current TS packet is in a discontinuity state with respect 
                                     | to either the continuity counter or the program clock reference.
----------------------------------------------------------------------------------------------------
Random Access indicator              | Set to 1 if the PES packet in this TS packet starts a video/audio sequence
----------------------------------------------------------------------------------------------------
Elementary stream priority indicator | 1 = higher priority
----------------------------------------------------------------------------------------------------
PCR flag                             | 1 means adaptation field does contain a PCR field
----------------------------------------------------------------------------------------------------
OPCR flag                            | 1 means adaptation field does contain an OPCR field
----------------------------------------------------------------------------------------------------
Splicing point flag                  | 1 means presence of splice countdown field in adaptation field
----------------------------------------------------------------------------------------------------
Transport private data flag          | 1 means presence of private data bytes in adaptation field
----------------------------------------------------------------------------------------------------
Adaptation field extension flag      | 1 means presence of adaptation field extension
----------------------------------------------------------------------------------------------------
# Below fields are optional          | Depends on flags
----------------------------------------------------------------------------------------------------
PCR                                  | Program clock reference, stored in 6 octets in big-endian as 33 bits base, 
                                     | 6 bits padding, 9 bits extension.
----------------------------------------------------------------------------------------------------
OPCR                                 | Original Program clock reference. Helps when one TS is copied into another
----------------------------------------------------------------------------------------------------
Splice countdown                     | Indicates how many TS packets from this one a splicing point occurs 
                                     | (may be negative)
----------------------------------------------------------------------------------------------------
stuffing bytes                       |
----------------------------------------------------------------------------------------------------
")))

(defstruct adaptation-field 
  (length                          0 :type (unsigned-byte 8))
  (discontinuity-indicator         0 :type (unsigned-byte 1))
  (random-access-indicator         0 :type (unsigned-byte 1))
  (es-priority-indicator           0 :type (unsigned-byte 1))
  (pcr-flag                        0 :type (unsigned-byte 1))
  (opcr-flag                       0 :type (unsigned-byte 1))
  (splicing-point-flag             0 :type (unsigned-byte 1))
  (transport-private-data-flag     0 :type (unsigned-byte 1))
  (adaptation-field-extension-flag 0 :type (unsigned-byte 1))
  (pcr                             0 :type (or null (unsigned-byte 48)))
  (opcr                            0 :type (or null (unsigned-byte 48)))
  (splice-countdown                0 :type (or null (unsigned-byte 8)))
  (stuffing-bytes                  0 :type (or null list)))

(defstruct packet 
  (ts-header        t :type ts-header)
  (adaptation-field t :type (or null adaptation-field))
  (payload          t :type (or null payload)))
