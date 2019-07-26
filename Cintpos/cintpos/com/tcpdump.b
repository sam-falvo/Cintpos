/*
This invokes the TCPHAND debugging facility to dump the
list of TCP connection blocks

*/

GET "libhdr"
GET "manhdr"

LET start() BE
{ sendpkt(-1, Task_tcphandler, Action_debug, ?, ?)
}
