.feature pc_assignment
.feature labels_without_colons
.feature c_comments
; .feature org_per_seg

.include "macros.s"
.include "mathmacros.s"

filebuffer = $0200

; -----------------------------------------------------------------------------------------------

.include "main.s"
.include "modplay.s"
.include "aaline.s"
.include "irqload.s"
.include "decruncher.s"

; ----------------------------------------------------------------------------------------------------
