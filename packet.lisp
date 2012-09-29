(in-package :ts)

(defconstant +SYNC_BYTE+ #x47)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ts-header-document* (format nil "~
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


#+C
(defstruct packet 
  (ts-header        t :type ts-header)
  (adaptation-field t :type (or null))
  (payload          t :type (or null)))

  