// This file is currently being convertes from BCPL to VSPL

// header file for the bench mark test

 
static  // Comment out one of the following lines
  Count,
  Qpktcountval,
  Holdcountval,
 
  i_idle,
  i_work,
  i_handlera,
  i_handlerb,
  i_deva,
  i_devb,
  tasktab_upb,
 
  p_link,
  p_id,
  p_kind,
  p_a1,
  p_a2,
  pkt_upb,
 
  t_link,
  t_id,
  t_pri,
  t_wkq,
  t_state,
  t_fn,
  t_v1,
  t_v2,
  tcb_upb,
 
  bufsize,
 
  pktbit,
  waitbit,
  holdbit,
 
  notpktbit,
  notwaitbit,
  notholdbit,
 
  s_run,
  s_runpkt,
  s_wait,
  s_waitpkt,
  s_hold,
  s_holdpkt,
  s_holdwait,
  s_holdwaitpkt,
 
  k_dev,
  k_work,

  tasktab ,
  tasklist,
  tcb,
  taskid,
  v1,
  v2,
 
  qpktcount,
  holdcount,
  tracing,
  layout,
  TRUE,
  FALSE,
  spacev,
  spacep,
  spacet

let getvec(upb) = valof
{ let p = spacep-upb-1;
  if p<spacev do resultis 0;
  spacep := p;
  resultis p
}

let wrch(ch) be printf("%c", ch)
let newline() be wrch('\n')
let putbyte(v, i, byte) be
{ let p = i/4;
  let sh = (i mod 4) * 8;
  let mask = ~(#FF<<sh);
  let bytesh = byte<<sh;
  v[p] := (v[p] & mask) + bytesh
}

let getbyte(v, i) = valof
{ let p = i/4;
  let sh = (i mod 4) * 8;
  resultis v[p] >> sh
}

let start() = valof
{ let quiet = 0;
  let wkq = 0;
  vec v[1000];
  spacev := v;
  spacep := v+1000;
  spacet := spacep;
  
  // Initialise the static variables

//Count:=10000;            // For bench
//Qpktcountval:=23246;
//Holdcountval:=9297;
  Count:=10000*100;        //For bench100
  Qpktcountval:=2326410;
  Holdcountval:=930563;

  i_idle       := 1;
  i_work       := 2;
  i_handlera   := 3;
  i_handlerb   := 4;
  i_deva       := 5;
  i_devb       := 6;
  tasktab_upb  := 10;
 
  p_link := 0;
  p_id   := 1;
  p_kind := 2;
  p_a1   := 3;
  p_a2   := 4;
  pkt_upb:= 8;
 
  t_link := 0;
  t_id   := 1;
  t_pri  := 2;
  t_wkq  := 3;
  t_state:= 4;
  t_fn   := 5;
  t_v1   := 6;
  t_v2   := 7;
  tcb_upb:= 7;
 
  bufsize := 3;
 
  pktbit         := 1;
  waitbit        := 2;
  holdbit        := 4;
 
  notpktbit      := ~pktbit;
  notwaitbit     := ~waitbit;
  notholdbit     := ~holdbit;
 
  s_run          := 0;
  s_runpkt       := pktbit;
  s_wait         := waitbit;
  s_waitpkt      := waitbit + pktbit;
  s_hold         := holdbit;
  s_holdpkt      := holdbit + pktbit;
  s_holdwait     := holdbit + waitbit;
  s_holdwaitpkt  := holdbit + waitbit + pktbit;
 
  k_dev   := 1000;
  k_work  := 1001;

  FALSE := 0;
  TRUE  := -1;

  quiet := 0;
   
  unless quiet do printf("\nbench mark starting, Count=%d\n", Count);
 
  tasktab := getvec(tasktab_upb);
  tasktab[0] := tasktab_upb;
  for taskno = 1 to tasktab_upb do tasktab[taskno] := 0;
 
  tasklist := 0;
 
  createtask(i_idle, 0, wkq, s_run, idlefn, 1, Count);
 
  wkq := pkt(0,   0, k_work);
  wkq := pkt(wkq, 0, k_work);
  createtask(i_work, 1000, wkq, s_waitpkt, workfn, i_handlera, 0);
 
  wkq := pkt(0,   i_deva, k_dev);
  wkq := pkt(wkq, i_deva, k_dev);
  wkq := pkt(wkq, i_deva, k_dev);
  createtask(i_handlera, 2000, wkq, s_waitpkt, handlerfn, 0, 0);
 
  wkq := pkt(0,   i_devb, k_dev);
  wkq := pkt(wkq, i_devb, k_dev);
  wkq := pkt(wkq, i_devb, k_dev);
  createtask(i_handlerb, 3000, wkq, s_waitpkt, handlerfn, 0, 0);
 
  wkq := 0;
  createtask(i_deva, 4000, wkq, s_wait, devfn, 0, 0);
  createtask(i_devb, 5000, wkq, s_wait, devfn, 0, 0);
 
  tcb := tasklist;
 
  qpktcount := 0;
  holdcount := 0;

  unless quiet do
  { printf("\nstarting\n")
    // printf("starting time = %d\n", time())
  };
  tracing := FALSE;
  //tracing := TRUE;
  layout  := FALSE;
  schedule();
  unless quiet do
  { printf("\nfinished\n");
    //printf("\nfinishing time = %d\n", time());
 
    printf("qpkt count = %d  holdcount = %d\n",
            qpktcount,       holdcount);
 
    printf("these results are ");
    test qpktcount=Qpktcountval & holdcount=Holdcountval
    then printf("correct")
    else printf("incorrect");
 
    printf("\nend of run\n")
  };
  resultis 0
}
 
let createtask(id, pri, wkq, state, fn, v1, v2) be
{ let t = getvec(tcb_upb);
  tasktab[id] := t;  // insert in the task table
  t_link[t] := tasklist;
  t_id[t]   := id;
  t_pri[t]  := pri;
  t_wkq[t]  := wkq;
  t_state[t]:= state;
  t_fn[t]   := fn;
  t_v1[t]   := v1;
  t_v2[t]   := v2;
  tasklist := t
  //printf("crestetask: tcb=%d ", t);
  //printf("id=%d ", id);
  //printf("%c", '\n');
  //for i = 1 to 1000000 do i := i
}
 
let pkt(link, id, kind) = valof
{ let p = getvec(pkt_upb);
  for i = 0 to pkt_upb do p[i] := 0;
  p_link[p]    := link;
  p_id[p]      := id;
  p_kind[p]    := kind;
  resultis p
}
 
 
let trace(ch) be
{ layout := layout - 1;
  if layout<=0 do
  { newline();
    layout := 50
  };
  wrch(ch);
  ch := 7
}
 
let schedule() be until tcb=0 do
{ let pkt = 0;
  let newtcb = 0;
  let state = t_state[tcb];
//printf("tcb=%d state=%d task=%d\n", tcb, state, t_id[tcb]);
//for i = 1 to 100000 do i:=i+1;

  test state=s_wait |
       state=s_hold |
       state=s_holdpkt |
       state=s_holdwait |
       state=s_holdwaitpkt
  then { tcb := t_link[tcb]
       }
  else { // tcb is the highest priority task that is ready to run.
         if state=s_waitpkt do
         { pkt := t_wkq[tcb];         // Dequeue a pkt from wkq
           t_wkq[tcb] := p_link[pkt];
           test t_wkq[tcb]=0          // Change the task state
           then state := s_run
	   else state := s_runpkt;
           t_state[tcb] := state
         };

         if state=s_run | state=s_runpkt do
         { taskid := t_id[tcb];
           v1     := t_v1[tcb];
           v2     := t_v2[tcb];
	   // The variables v1 and v2 belong to the current task
	   // and their use depends on the type of the task.
           if tracing do trace(taskid+'0');
           //for i = 1 to 10000 do i:=i;
           newtcb := (t_fn[tcb])(pkt); // pkt is only non zero
	                               // if state was waitpkt
           t_v1[tcb] := v1;
           t_v2[tcb] := v2;
           tcb := newtcb
         }
       }
}
 
let qpkt(pkt) = valof
{ let t = findtcb(p_id[pkt]);

  qpktcount := qpktcount + 1;
 
  p_link[pkt] := 0;
  p_id[pkt]   := taskid;
  test t_wkq[t]=0
  then { t_wkq[t] := pkt;
         t_state[t] := t_state[t] | pktbit;
         if t_pri[t] > t_pri[tcb] do resultis t
       }
  else append(pkt, @ t_wkq[t]);
  resultis tcb
}
 
let wait() = valof
{ t_state[tcb] := t_state[tcb] | waitbit;
  resultis tcb
}
 
let holdself() = valof
{ holdcount := holdcount + 1;
  t_state[tcb] := t_state[tcb] | holdbit;
  resultis t_link[tcb]
}
 
let release(id) = valof
{ let t = findtcb(id);
  if t=0 do resultis 0;
 
  t_state[t] := t_state[t] & notholdbit;
  if t_pri[t] > t_pri[tcb] do resultis t;
  resultis tcb

}
 
let findtcb(id) = valof
{ let t = 0;
  if 1 <= id & id <= tasktab[0] do t := tasktab[id];
  if t=0 do printf("\nbad task id\n");
  resultis t
}
 
let idlefn(pkt) = valof
{ v2 := v2 - 1;
  if v2=0 do resultis holdself();
  test (v1&1)=0
  then { v1 := v1>>1;
         resultis release(i_deva)
       }
  else { v1 := v1>>1 ^ #D008;
         resultis release(i_devb)
       }
}
 
let workfn(pkt) = valof test pkt=0
  then resultis wait()
  else { let buf = @ p_a2[pkt];
         v1 := i_handlera + i_handlerb - v1;
         // v1 is alternately i>handlera and i_handlerb
 
         p_id[pkt] := v1;   // set the destination task id
         p_a1[pkt] := 0;    // set the buffer subscript
         for i = 0 to bufsize do
         { v2 := v2 + 1;
           if v2>26 do v2 := 1;
           putbyte(buf, i, getbyte("ABCDEFGHIJKLMNOPQRSTUVWXYZ", v2-1))
         };
         resultis qpkt(pkt)
       }
 
let handlerfn(pkt) = valof
{ unless pkt=0 do append(pkt, valof test p_kind[pkt]=k_work
                                    then resultis @v1
                                    else resultis @v2);
  unless v1=0 do
  { let workpkt = v1;
    let count = p_a1[workpkt];
    let buf   = @ p_a2[workpkt];
    if count>bufsize do
    { v1 := p_link[v1];
      resultis qpkt(workpkt)  // send back the exhausted a work packet
    };
    unless v2=0 do
    { let devpkt = v2;
      v2 := p_link[v2];
      p_a1[devpkt] := getbyte(buf,count);  // copy character into it
      p_a1[workpkt] := count+1;   // incrementing the character count
      resultis qpkt(devpkt)       // send the packet to the device task
    }
  };
 
  // cannot proceed for lack of a packet so wait for one
  resultis wait()
}
 
let devfn(pkt) = valof test pkt=0
  then { if v1=0 do resultis wait();
         pkt := v1;
         v1 := 0;
         resultis qpkt(pkt)
       }
  else { v1 := pkt;
         if tracing do trace(p_a1[pkt]);
         resultis holdself()
       }
 
let append(pkt, ptr) be
{ p_link[pkt] := 0;
  until !ptr=0 do ptr := !ptr;
  !ptr := pkt
}
