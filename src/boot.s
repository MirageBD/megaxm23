.feature pc_assignment
.feature labels_without_colons
.feature c_comments
; .feature org_per_seg

.include "macros.s"

filebuffer = $0200

; -----------------------------------------------------------------------------------------------

.include "main.s"
.include "irqload.s"
.include "decruncher.s"

; ----------------------------------------------------------------------------------------------------
