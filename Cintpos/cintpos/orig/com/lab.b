GET "libhdr"

LET start() BE
$(  LET ch=rdch()
    WHILE ch~=endstreamch & ch~='*E' & ch~='*N' & ch~=';' DO ch:=rdch()
$)

