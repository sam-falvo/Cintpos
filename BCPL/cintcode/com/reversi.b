/* REVERSI PART1              RELEASE 26/7/1976  */
 /* MODIFICATIONS FOR CDC 6000 VERSION --
  1. BUG IN NEWLINE() IS ALLOWED FOR BY OMITTING
            wrch(' ') IN ROUTINE DISPLAY.
  2. MESSAGES TO A TERMINAL ARE SENT ONLY AFTER A NEWLINE.
       THEREFORE EXTRA NEWLINES ARE GIVEN IN
       START.

The author of this program is unknown.

Somewhat modified by MR 15 Oct 2024
It needs more work
*/

GET "libhdr"

MANIFEST {
  BLACK=-1;WHITE=1;
  FORCED=1; OK=0; DROP=-1;
}

GLOBAL {
  COPYE:202; RNDM:203; WSUM:204; MAKEMOVE:205; ROWSTEP:206; COLSTEP:207;
  UPDATE:208; MOVESPLAYED:209; WEIGHTS:210; UPLIM:211;MAXCANDS:212;
  MAXDEPTH:213;
  MVALUE:214; PRUNEDEPTH:215; DEPTH:216; ROWSEQ:217; COLSEQ:218;
  UNFINISHED:219; 
  MINMAXSOFAR:220; ZUGZWANG:221; ZWEIGHT:222; TRACE:223; SETTRACE:224;
  DISPW:225;
  CLEARSCREEN:226; DISPLAY:227; TERM:228; LP:229; QUICK:230; SETUP:231;
  CENTRESQUARES:232; T4010:233; TOPSEP:234; BTMSEP:235; LINE:236
}

STATIC {
  VALUE=0
}

LET DISPLAY(POS) BE
{ LET CH='A'; LET SEP='!'
    CLEARSCREEN()
    writes("*N   ")
    FOR M=1 TO 8 DO { wrch(TOPSEP); writes(LINE) }; wrch(TOPSEP)
    FOR N = 8 TO 1 BY -1 DO
     { IF T4010
        THEN { writes("*N   ")
                FOR M=1 TO 8 DO writes("!   "); wrch('!')
             }
        newline(); wrch(' '); writed(N,1); wrch(' ')
        FOR M = 1 TO 8 DO
         { writes("! ")
            SWITCHON POS!(10*N+M) INTO
             { CASE WHITE : wrch('O'); ENDCASE
                CASE BLACK : wrch('**'); ENDCASE
                DEFAULT    : wrch(' ')
             }
            wrch(' ')
         }
        wrch('!')
        IF N=1 THEN SEP:=BTMSEP
        writes("*N   "); wrch(SEP)
        FOR M = 1 TO 8 DO { writes(LINE); wrch(SEP) }
     }
    writes ("*N  ")
    FOR M = 1 TO 8 DO
     { writes("   "); wrch(CH); CH:=CH+1
     }
 } 

LET LISTCANDIDATES(PLAYER,POS,MOVELIST,NCANDS) BE
 { LET MOVEP,OPPONENT=1,-PLAYER
    LET NMOVES = 0
    LET ROW,COL,ROW1,COL1=0,0,0,0
    LET MAXN = MAXCANDS!DEPTH
    FOR SQUARE=0 TO UPLIM DO
     { ROW:=ROWSEQ!SQUARE; COL:=COLSEQ!SQUARE
        IF POS!(ROW+COL)=0 THEN
          { FOR DIRECTION=1 TO 8 DO
              { ROW1:=ROW1+ROWSTEP!DIRECTION
                 COL1:=COL1+COLSTEP!DIRECTION
                 IF POS!(ROW1+COL1) NE OPPONENT THEN LOOP
                  { ROW1:=ROW1+ROWSTEP!DIRECTION
                     COL1:=COL1+COLSTEP!DIRECTION
                  } REPEATWHILE POS!(ROW1+COL1)=OPPONENT
                 IF POS!(ROW1+COL1)=PLAYER THEN
                  { // ENTER MOVE IN LIST & QUIT FOR LOOP
                    MOVELIST!MOVEP := ROW/10
                    MOVELIST!(MOVEP+1):=COL
                    IF DEPTH < PRUNEDEPTH THEN   // KEEP ALL CANDIDATES
                      { MOVEP:=MOVEP+2
                         NMOVES :=NMOVES+1
                         BREAK
                      }
                    /* OTHERWISE, CHECK WHETHER THE MOVE IS SENSIBLE */
                    SWITCHON MVALUE(MOVELIST+MOVEP,POS,PLAYER) INTO
                     { CASE FORCED: IF DEPTH > PRUNEDEPTH THEN
                                      { !NCANDS:=NMOVES+1; RETURN }
                        CASE OK    : MOVEP:=MOVEP+2
                                     NMOVES:=NMOVES+1
                        CASE DROP  : BREAK
                     }
                  }
               }  // END FOR
             IF NMOVES=MAXN THEN BREAK
         }  // END IF
     } //  END FOR                      
   IF NMOVES=0 THEN { MOVELIST!1:=0; !NCANDS:=1; RETURN }
   !NCANDS :=NMOVES
   IF DEPTH<PRUNEDEPTH & (MVALUE(MOVELIST+1,POS,PLAYER)=DROP)
   THEN  // RE-ORDER THE CANDIDATES TO REINFORCE PRUNING
    { FOR MOVEP=3 TO 2*NMOVES-1 BY 2 DO
        { IF MVALUE(MOVELIST+MOVEP,POS,PLAYER) NE DROP
           THEN   // SWAP WITH 1ST MOVE OF LIST
            { LET SAVE1,SAVE2 = MOVELIST!1,MOVELIST!2
               MOVELIST!1:=MOVELIST!MOVEP; MOVELIST!2:=MOVELIST!(MOVEP+1)
               MOVELIST!MOVEP:=SAVE1; MOVELIST!(MOVEP+1):=SAVE2
               BREAK
            }
        }
     }
  } // END OF LISTCANDIDATES
    /*              ---                */  

LET SELECTMOVE (PLAYER,POS,MOVE,RESULT) BE 
  { LET MOVELIST=VEC 40    // 2* MAXCANDS
     LET NEWPOS=VEC 100
       LET CANDRESULT,CURRCAND,NCANDS,BESTCAND,BESTSOFAR=0,1,0,1,3
       LET OPPONENT,LISTEND,SUM=-PLAYER,0,0
       LET NEXTMOVE = VEC 2
       DEPTH:=DEPTH+1
       LISTCANDIDATES(PLAYER,POS,MOVELIST,@NCANDS)
       IF (DEPTH+MOVELIST!1)=1 THEN { MOVE!0:=0;
                                       DEPTH:=DEPTH-1
                                       RETURN
                                    }
        LISTEND:=NCANDS*2
        TEST PLAYER=WHITE // SET UP ALPHA-BETA PRUNING
        THEN { BESTSOFAR:= -4000
                MINMAXSOFAR!(DEPTH+1):=BESTSOFAR
             }
        ELSE { BESTSOFAR:= 4000
                MINMAXSOFAR!(DEPTH+1):=BESTSOFAR
             }
        WHILE CURRCAND<=LISTEND DO
          { MAKEMOVE(PLAYER,POS,MOVELIST+CURRCAND,NEWPOS)
             /* ATTEMPT TO EVALUATE RESULTING POSN */
             TEST DEPTH < MAXDEPTH
             THEN SELECTMOVE(OPPONENT,NEWPOS,NEXTMOVE,@CANDRESULT)
             ELSE  // TRY TO EVALUATE DIRECTLY
              { CANDRESULT := 0
                 TEST MOVESPLAYED > 52
                 THEN FOR I=11 TO 88 DO CANDRESULT:=CANDRESULT+NEWPOS!I
                 ELSE 
                  { CANDRESULT := WSUM(NEWPOS,WEIGHTS)
                     IF ZUGZWANG(POS,PLAYER,MOVELIST+CURRCAND)
                       THEN CANDRESULT:=CANDRESULT - PLAYER*ZWEIGHT
                  }
              }
            IF (DEPTH=1) THEN
             { IF CURRCAND=13 THEN newline(); wrch(' ');
                wrch('A'-1+MOVELIST!(CURRCAND+1))
                wrch('0'+MOVELIST!CURRCAND)
                writed(CANDRESULT,3)
             }
            /* UPDATE BESTSOFAR */
            TEST PLAYER=WHITE
            THEN {
                    IF CANDRESULT>BESTSOFAR THEN
                      { BESTCAND:=CURRCAND
                         BESTSOFAR:=CANDRESULT
                         IF CANDRESULT>=MINMAXSOFAR!DEPTH THEN BREAK
                      }
                 }
            ELSE {
                    IF CANDRESULT<BESTSOFAR THEN
                      { BESTCAND:=CURRCAND
                         BESTSOFAR:=CANDRESULT
                         IF CANDRESULT<=MINMAXSOFAR!DEPTH THEN BREAK
                      }
                 }
            CURRCAND:=CURRCAND+2
           } // END OF WHILE
        !RESULT:=BESTSOFAR
        MOVE!0:=MOVELIST!BESTCAND
        MOVE!1:=MOVELIST!(BESTCAND+1)
        IF DEPTH>1 THEN //UPDATE MINMAXSOFAR
          TEST PLAYER=WHITE
          THEN {
                 IF BESTSOFAR<MINMAXSOFAR!DEPTH THEN
                 MINMAXSOFAR!DEPTH:=BESTSOFAR
               }
          ELSE {
                 IF BESTSOFAR>MINMAXSOFAR!DEPTH THEN
                 MINMAXSOFAR!DEPTH:=BESTSOFAR
               }
      DEPTH:=DEPTH-1
    } // END OF SELECTMOVE
    /*  -----  MAIN PROGRAM FOLLOWS  -----  */

LET start(PARM) BE
   { LET READMOVE(POS,READSTATUS) BE
       { LET CH=' '
          LET ROW,COL=0,0
          LET LEGALMOVES=VEC 50  // 2*MAXCANDS
          LET NLMOVES=0
          LET MOVE=VEC 2
          LET VALID=FALSE
          !READSTATUS:=FALSE // SET TRUE IF PASS
          WHILE NOT VALID DO
    // *N AT END OF NEXT LINE IS FOR 6000
           { writes("*N ENTER YOUR MOVE - *N")
              CH:=rdch() REPEATUNTIL CH NE ' '
              IF CH='W' THEN { DISPW(POS); LOOP }
              COL:=CH-'A'+1
              IF CH='P' THEN COL:=0 // PASS
              CH:=rdch() REPEATUNTIL CH NE ' ' 
              ROW:=CH-'0'
              CH :=rdch() REPEATUNTIL CH='*N'
              IF COL=0 THEN ROW:=0 // PASS 
              IF 0 <= ROW <= 8 &
                 0 <= COL <= 8 &
                 POS!(10*ROW+COL)=0 
    /* --CHECK FOR VALIDITY AND IF VALID, MAKE MOVE -- */
                 THEN
          { LET MOVEP=1
             LISTCANDIDATES(WHITE,POS,LEGALMOVES,@NLMOVES)
              /* CHECK WHETHER MOVE IS IN THE LIST */
                FOR I=1 TO NLMOVES DO
                 { IF ROW=LEGALMOVES!MOVEP & COL=LEGALMOVES!(MOVEP+1)
                    THEN { VALID:=TRUE; BREAK }
                    MOVEP:=MOVEP+2
                 }
                IF LEGALMOVES!1=0 & COL=0 THEN  // VALID PASS
                 { VALID:=TRUE; !READSTATUS:=TRUE
                 }
         TEST VALID
         THEN { MOVE!0:=ROW; MOVE!1:=COL
                 MAKEMOVE(WHITE,POS,MOVE,POS)
              }
         ELSE { writes(" INVALID MOVE, TRY AGAIN - ")
                 writes(" LEGAL MOVE ARE :-*N")
                 TEST LEGALMOVES!1 = 0
                 THEN writes(" PASS*N")
                 ELSE FOR I=1 TO NLMOVES DO 
                       { wrch(' ')
                          wrch('A'-1+LEGALMOVES!(2*I))
                          wrch('0'+LEGALMOVES!(2*I-1))
                       }
              }
            }  // END OF THEN
          }  // END OF 'WHILE NOT VALID'
          }
         LET VECALPHA=VEC 20 // USED IN ALPHA-BETA PRUNING
         LET NCANDS = 0
         LET SUM,VALID=0,FALSE
         LET READSTATUS=0; LET POS=VEC 100
         LET RMOVE,ROW,COL,CH,DUMMY=0,0,0,0,0
         LET FIRSTPLAYER=WHITE
         LET WHITEPASS,BLACKPASS=FALSE,FALSE
         LET MOVE=VEC 2
         TERM := output()  // ON ENTRY, HP BCPL SETS TO TERM. NO.
         MINMAXSOFAR:=VECALPHA // STATIC VECTOR FOR ALPHA-BETA PRUNING
         DEPTH := 0            // CURRENT DEPTH OF LOOKAHEAD
         TRACE := 0            // LOOKAHEAD TRACE OPTION
         MINMAXSOFAR!1:=-1000     // SO THAT DEPTH 1 NEED NOT BE SPECIAL
         PRUNEDEPTH := 3
         MAXCANDS:=TABLE 20,5,5,4,4,3,3,3,3,3,3,3,3,3,3
         MAXDEPTH:=5
         MOVESPLAYED:=0
         UPLIM:=63
         ZWEIGHT := 2
         CENTRESQUARES := TABLE 44,45,54,55
         CLEARSCREEN();
         ROWSTEP:=TABLE -10,-10,-10,0,10,10,10,0,-10
         COLSTEP:=TABLE -1,0,1,1,1,0,-1,-1,-1
         WEIGHTS:=TABLE 0,0,0,0,0,0,0,0,0,0,
                   0,140,16,40,30,30,40,16,140,0,
                   0,16,-30,-4,-6,-6,-4,-30,16,0,
                   0,40,-4,3,1,1,3,-4,40,0,
                   0,30,-6,1,1,1,1,-6,30,0,         
                   0,30,-6,1,1,1,1,-6,30,0,         
                   0,40,-4,3,1,1,3,-4,40,0,
                   0,16,-30,-4,-6,-6,-4,-30,16,0,
                   0,140,16,40,30,30,40,16,140,0
          ROWSEQ:=TABLE 1,1,8,8,1,1,1,1,8,8,8,8,3,3,4,4,5,5,6,6,
                        1,1,8,8,2,2,7,7, // EGDE SQS. ADJACENT TO CORNERS
                        3,3,6,6,      // CORNERS OF 4 X 4 CENTRE SQUARE
                        3,3,6,6,4,4,5,5, // OUTER RING OF CENTRE SQUARE
                        4,4,5,5,  // CENTRE
                        2,2,2,2,7,7,7,7,3,3,4,4,5,5,6,6,2,2,7,7
          COLSEQ:=TABLE 1,8,1,8,3,4,5,6,3,4,5,6,1,8,1,8,1,8,1,8,
                        2,7,2,7,1,8,1,8, // EDGE SQS ADJACENT TO CORNERS
                        3,6,3,6,  // CORNERS CENTRE SQ
                        4,5,4,5,3,6,3,6,  // OUTER RING OF CEN.SQ.
                        4,5,4,5,  // CENTRE
                        3,4,5,6,3,4,5,6,2,7,2,7,2,7,2,7,2,7,2,7
          FOR I=0 TO 100 DO POS!I := 0
          FOR I=0 TO 63 DO ROWSEQ!I:=10*ROWSEQ!I  // FOR USE AS INDEX
          SETUP(POS,@FIRSTPLAYER) 
          FOR I=1 TO 2 DO // COMPLETE 4 CENTRE SQUARES
           { DISPLAY(POS); VALID:=FALSE
              WHILE NOT VALID DO
               { writes("*N ENTER YOUR MOVE - ") 
                  newline() //NECESSARY ON 6000
                  CH:=rdch() REPEATUNTIL CH NE ' '
                  COL:=CH-'A'+1
                  CH:=rdch() REPEATUNTIL CH NE ' '
                  ROW:=CH-'0'
                  CH:=rdch() REPEATUNTIL CH='*N'
                  TEST 4<=ROW<=5 & 4<COL<5 &
                       POS!(10*ROW+COL)=0
                  THEN
                   { VALID:=TRUE
                      POS!(10*ROW+COL):=1
                   }
                  ELSE writes("*N INVALID MOVE,TRY AGAIN - ")
               } // END WHILE
              MOVESPLAYED:=MOVESPLAYED+1
              IF MOVESPLAYED=4 THEN BREAK
             /* PICK ANOTHER RANDOM MOVE */
            DISPLAY(POS)
            RMOVE:=RNDM(0)!CENTRESQUARES REPEATUNTIL POS!RMOVE = 0
            POS!RMOVE := BLACK
            MOVESPLAYED:=MOVESPLAYED+1
        }  // END FOR
       /* SEE WHO STARTED */
         IF FIRSTPLAYER=BLACK
          THEN
           { DISPLAY(POS)
              SELECTMOVE(BLACK,POS,MOVE,@VALUE)
              wrch(' '); wrch('A'-1+MOVE!1)
              // NEWLINE IN NEXT LINE SPECIAL FOR 6000
              wrch('0'+MOVE!0); newline(); // CH:=rdch()
              MAKEMOVE(BLACK,POS,MOVE,POS)
              MOVESPLAYED:=MOVESPLAYED+1
           }
          UNFINISHED:=TRUE
          WHILE UNFINISHED DO
          { DISPLAY(POS)
             IF MOVESPLAYED=64 THEN BREAK
             READMOVE(POS,@WHITEPASS)
             IF BLACKPASS & WHITEPASS THEN BREAK
             IF NOT WHITEPASS THEN MOVESPLAYED:=MOVESPLAYED+1
             DISPLAY(POS)
             IF MOVESPLAYED=64 THEN BREAK
             SELECTMOVE(BLACK,POS,MOVE,@VALUE)
             TEST MOVE!0=0
             THEN writes(" PASS*N") // *N FOR 6000
             ELSE   { wrch(' '); wrch('A'-1+MOVE!1)
                     wrch('0'+MOVE!0); newline() // *N FOR 6000 
                  }
             // NEXT LINE INAPPROPRIATE ON 6000
             CH:=rdch() REPEATUNTIL CH='*N' // ALLOWPLAYER TO READ MOVE
             MAKEMOVE(BLACK,POS,MOVE,POS)
             IF MOVE!0 NE 0 THEN { MOVESPLAYED:=MOVESPLAYED+1 }
             BLACKPASS := ( MOVE!0 = 0 )
             UNFINISHED:= NOT (WHITEPASS & BLACKPASS)
             UPDATE(ROWSEQ,COLSEQ,POS)
          }  
         newline();
         SUM:=0
         FOR I=0 TO 99 DO SUM:=SUM+POS!I
         TEST SUM = 0
         THEN writes(" DRAW !")
         ELSE
           { TEST SUM > 0
              THEN writes(" YOU WIN BY ")
              ELSE   { SUM:=-SUM; writes(" I WIN BY ") }
              writed(SUM,3)
           }
         writes("*N THANK YOU.")
         FINISH
      }
      /* REVERSIE PART 2             RELEASE 9/12/1976  */
     LET SETUP(POS,ADDR.FIRSTPLAYER) BE
      { LET CH,DUMMY=0,0; LET WOW,COL,RMOVE=0,0,0
       /*  THE *N AT THE END OF EACH PROMPT LINE ARE FOR INTERCOM */
          { writes("*N WHAT SORT OF TERMINAL ARE YOU USING?")
             writes("*N (REPLY Y FOR T4010 TYPE OR")
             writes("*N B FOR BEEHIVE/T4023 TYPE -*N")
             CH:=rdch(); DUMMY:=rdch() REPEATUNTIL DUMMY='*N'
          } REPEATUNTIL CH='T' LOGOR CH='B'
         TEST CH='T'
         THEN { T4010:=TRUE; TOPSEP:='.'; BTMSEP:='!'
                 LINE:="___"  // THIS SHOULD BE 3 UNDERLINES 
              }
         ELSE { T4010:=FALSE; TOPSEP:=':'; BTMSEP:='*''
                 LINE:="---"
              }
         writes("*N DO YOU PREFER THE PROGRAM TO PLAY QUICKLY OR WELL?")
         writes("*N (REPLY Q OR W) -*N")
         CH:=rdch() REPEATUNTIL CH='Q' LOGOR CH='W'
         DUMMY:=rdch() REPEATUNTIL DUMMY='*N' // FINISH READING THE LINE
         QUICK := CH='Q'
          { writes("*N DO YOU WISH TO MOVE FIRST?")
             writes("*N (REPLAY Y OR N ) -*N")
             CH:=rdch(); DUMMY:=rdch() REPEATUNTIL DUMMY='*N'
          } REPEATUNTIL CH='Y' LOGOR CH='N'
         IF CH='N'
         THEN // MAKE A MOVE
          { !ADDR.FIRSTPLAYER:=BLACK
             RMOVE:= RNDM(0)!CENTRESQUARES
             POS!RMOVE := BLACK
             MOVESPLAYED:=1
          }
      }
     LET MAKEMOVE(PLAYER,POS,MOVE,NEWPOS) BE
      { // THE MOVE IS IN MOVE!0 AND MOVE!1
         LET ROW,COL,OPPONENT = MOVE!0,MOVE!1, -PLAYER
         LET ROW1,COL1=0,0
         COPYE(POS,NEWPOS,100)  // COPY POS TO NEWPOS
         IF ROW = 0 THEN RETURN // HANDLE PASS CORRECTLY
         ROW:=10*ROW   // FOR USE AS INDEX
         NEWPOS!(ROW+COL):=PLAYER
         FOR DIRECTION=0 TO 7 DO
          { ROW1:=ROW+ROWSTEP!DIRECTION
             COL1:=COL+COLSTEP!DIRECTION
             IF NEWPOS!(ROW!+COL1) NE OPPONENT THEN LOOP
             WHILE NEWPOS!(ROW1+COL1) = OPPONENT DO
              { ROW1:=ROW1+ROWSTEP!DIRECTION
                 COL1:=COL1+COLSTEP!DIRECTION
              }
             IF NEWPOS!(ROW1+COL1)=PLAYER THEN
              { ROW1:=ROW1-ROWSTEP!DIRECTION
                 COL1:=COL1-COLSTEP!DIRECTION
                 WHILE ROW1 NE ROW LOGOR COL1 NE COL DO
                  { ROW1:=ROW1-ROWSTEP!DIRECTION
                     COL1:=COL1-COLSTEP!DIRECTION
                  }
              } // END IF
           } // END FOR 
          IF DEPTH > 0 THEN RETURN
          /*     UPDATE WEIGHTS     */
          TEST ROW=10 LOGOR ROW=80
          THEN
           { ROW:= ROW=10 -> 20,70
              TEST COL=1 LOGOR COL=8
              THEN { COL:= COL=1 ->2,7
                      WEIGHTS!(ROW+COL):=1
                   }
              ELSE IF 3<=COL<=6
                  THEN WEIGHTS!(ROW+COL):=1
           }
          ELSE IF (COL=1 LOGOR COL=8) & 30<= ROW <= 60
             THEN { COL:= COL=1 -> 2,7
                     WEIGHTS!(ROW+COL):=1
                  }
      }  // END MAKEMOVE
     LET UPDATE(ROWSEQ,COLSEQ,POS) BE
      { LET COPY=0
         LET LIMIT = (QUICK -> 6,8)
         FOR LOOK=0 TO UPLIM DO
          { IF POS!(ROWSEQ!LOOK+COLSEQ!LOOK)=0
            THEN  // RETAIN THESE ENTRIES
             { ROWSEQ!COPY:=ROWSEQ!LOOK
                COLSEQ!COPY:=COLSEQ!LOOK
                COPY:=COPY+1
             }
          } // END FOR
         UPLIM:=COPY-1  // NO. OF REMAINING ENTRIES
         IF 52<UPLIM<56 THEN { MAXCANDS!1:=10
                                MAXCANDS!2:=10
                                MAXCANDS!3:=5
                                MAXCANDS!4:=5
                                MAXCANDS!5:=3; MAXCANDS!6:=3
                                TEST QUICK
                                THEN MAXDEPTH:=3 ELSE MAXDEPTH:=4
                             }
         IF 40<UPLIM<44 THEN { MAXCANDS!1:=12; MAXCANDS!2 := 12 
                                MAXCANDS!3:=6 ; MAXCANDS!4:= 6
                                ZWEIGHT := 6
                             }
         IF UPLIM<32 & WEIGHTS!11 = 60
         THEN MAXCANDS!5 :=4
         IF 11 < UPLIM < 32 THEN
          FOR I=11 TO 88 DO
           { LET DIFF = UPLIM - 11 AND FINALW = 1
              IF (I/10 = 1) LOGOR (I/10 = 8) THEN FINALW := FINALW+FINALW
              IF (I REM 10 = 1) LOGOR (I REM 10 = 8)
                                           THEN FINALW := FINALW+FINALW
              TEST DIFF <= 2
              THEN WEIGHTS!I := FINALW
              ELSE   WEIGHTS!I := WEIGHTS!I - 2*(WEIGHTS!I-FINALW) / DIFF
           }
         IF UPLIM < 16 THEN { MAXCANDS!6:=2; 
                               ZWEIGHT := 1
                               TEST QUICK
                               THEN MAXDEPTH:=4 ELSE MAXDEPTH:=5
                               FOR I = 11 TO 88 DO
                               { IF WEIGHTS!I<1 THEN WEIGHTS!I:=WEIGHTS!I+1   
                               }
                            }
         IF UPLIM < 14 THEN { FOR I=PRUNEDEPTH TO 4 DO
                                    MAXCANDS!I:=MAXCANDS!I + 2
                               PRUNEDEPTH := 3
                            }
         IF UPLIM < 13 THEN { MAXCANDS!1:=UPLIM+1
                               MAXCANDS!2:=UPLIM
                               MAXCANDS!6 :=4
                               PRUNEDEPTH:=5
                            }
         IF UPLIM < LIMIT THEN { MAXDEPTH:= UPLIM+3 // ALLOWS FOR ONLY 3 PASSES
                                  PRUNEDEPTH:=15     // DON'T PRUNE
                                  FOR I=1 TO MAXDEPTH DO
                                   MAXCANDS!I:=UPLIM+1
                               }
         MAXCANDS!MAXDEPTH := 1
      } // END UPDATE
     LET MVALUE(MOVE,POS,PLAYER) = VALOF
      { LET ROW,COL = MOVE!0, MOVE!1; LET OPPONENT=-PLAYER
         LET PLAY,START,STEP,NXT,LIMIT = 0,0,0,0,0
         LET SQUARE=0
         LET PATTERN = #140000
         LET NPATTERNS = 24
         LET KEY  = TABLE #34000, #35400, #130000, #34600,
                #26000,  #7000, #7300,  #7140,  #7154,  #5400,
                #5440,  #1600,  #1660, #115400, #17000, #17300,
                #132000, #26200, #26260, #1633, #1660, #21600,
                #26400,  #3600
         LET MASK = TABLE #177400, #177400, #176000, #177700,
                #177700, #177700, #177700, #177774, #177774, #37760,
                #37760,  #37760,  #37760, #177700, #177700, #177700,
                #177400, #177760, #177760, #7777, #7760, #177760,
                #177774, #177760
         TEST ROW=1 LOGOR ROW=8
         THEN
          { TEST COL=1 LOGOR COL=8
             THEN RESULTIS FORCED
             ELSE
              { TEST COL>4
                 THEN { START:=10*ROW + 8
                         STEP:=-1
                      }
                 ELSE   { START:=10*ROW + 1; STEP:=1
                      }
              }  
          } 
         ELSE // NOT AN EDGE ROW
          { TEST COL=1 LOGOR COL=8
             THEN { TEST ROW>4
                     THEN { START:=80+COL; STEP:=-10
                          }
                     ELSE { START:=10+COL; STEP:=10
                          }
                  }
             ELSE RESULTIS OK // NOT AN EDGE PLAY. 
           }
             /* ---------------------------------------- */
             /* SET UP THE PATTERN.                      */
             /* ---------------------------------------- */
             PLAY:= 10*ROW + COL                    
             NXT:= START+7*STEP; LIMIT:=START-STEP
             WHILE NXT NE LIMIT DO
              { PATTERN:=PATTERN >> 2
                 IF NXT=PLAY THEN { PATTERN:=PATTERN + #140000
                                        NXT:=NXT-STEP; LOOP
                                     }
                 SQUARE:=POS!NXT
                 IF SQUARE=0
                 THEN { NXT:=NXT-STEP; LOOP
                      }
                 TEST SQUARE=PLAYER
                 THEN PATTERN:=PATTERN + #40000
                 ELSE  PATTERN:=PATTERN + #100000  // OPPONENT
                 WHILE POS!NXT=SQUARE DO NXT:=NXT- STEP
              }
      // PATTERN NOW SET UP.. EVALUATE IT. 
            FOR I=0 TO NPATTERNS-1 DO
             IF (PATTERN & MASK!I) = KEY!I RESULTIS DROP
            RESULTIS OK
      }
     LET ZUGZWANG(POS,PLAYER,MOVE) = VALOF
      { LET EDGE=VEC 2
         EDGE!1 := MOVE!1
         SWITCHON MOVE!0 INTO
          { CASE 0: RESULTIS TRUE
             CASE 1: RESULTIS FALSE 
             CASE 8: RESULTIS FALSE
             CASE 2: EDGE!0:=1; ENDCASE
             CASE 7: EDGE!0:=8; ENDCASE
            DEFAULT: EDGE!0:= MOVE!0
                     SWITCHON MOVE!1 INTO
                      { CASE 2: EDGE!1:=1; ENDCASE
                         CASE 7: EDGE!1:=8; ENDCASE
                         DEFAULT: RESULTIS FALSE
                      }
          }
         // CHECK WHETHER THE NEIGHBOURING EDGE SQ CONTAINS PLAYER
         TEST POS!(10*EDGE!0 + EDGE!1) = PLAYER
         THEN RESULTIS FALSE
         ELSE RESULTIS TRUE
      }
     LET DISPW(POS) BE
      { LET CH=0
         IF rdch() NE '-' THEN RETURN   // PROTECT AGAINST UNAUTH. USE
         CH:=rdch() REPEATUNTIL CH='*N'
         CLEARSCREEN();
         FOR ROW=80 TO 10 BY -10 DO
          { newline()
             FOR COL=1 TO 8 DO writed(WEIGHTS!(ROW+COL),4)
          }
         CH:=rdch() REPEATUNTIL CH='*N'; DISPLAY(POS)
      }
      /* REVERSI PART 3        RELEASE 76-9-4
       THIS MODULE CONSIST OF ROUTINES WHICH
         ARE NORMALLY MACHNE-DEPENDENT  
         BCPL VERSIONS ARE PROVIDED ONLY FOR
         TESTING.
      */ 
     //-----------
     //- COPYE(ARRAY1,ARRAY2,NWORDS) COPIES FROM ARRAY1 INTO ARRAY2
     //  MACHINE-CODED VERSIONS MAY ASSURE THAT NWORDS
     //  ALWAYS EVEN.
     LET COPYE(ARRAY1,ARRAY2,NWORDS) BE
      { FOR I=0 TO NWORDS-1 DO ARRAY2!I := ARRAY1!I }
     //-----------------------------
     // WSUM (POS,WEIGHTS) = SUM OF POS!I*WEIGHTS!I
     //  FOR I 10 TO 89
     // EXCEPT WHEN NO WHITE PIECES
     // ARE LEFT,WHEN WSUM= -2000*BLACK.
     // POS CONTAINS ONLY THE VALUES -1,0 AND +1.
     LET WSUM(POS,WEIGHTS) = VALOF
      { LET SUM=0; LET WHITEFOUND=FALSE
         FOR I=10 TO 89 DO
          { SWITCHON POS!I INTO
              { CASE WHITE: WHITEFOUND:=TRUE
                             SUM:=SUM+WEIGHTS!I; ENDCASE
                 CASE BLACK: SUM:=SUM-WEIGHTS!I
              }
           }
          TEST WHITEFOUND
          THEN RESULTIS SUM
          ELSE RESULTIS 2000*BLACK
      }
     //------------------------------
     //  RANDOM NUMER GENERATOR -- SHOULD USE THE SYSTEM CLOCK
     //    TO RETURN TRULY RANDOM INTEGER IN (0,3).
     LET RNDM(A) = VALOF
      { STATIC { N=1 }
         N := (N=3 -> 0, N+1); RESULTIS N
      }
     //------------------------------
     LET CLEARSCREEN() BE
     // { TEST T4010
     //    THEN { LET N= (#32<<6) + output()
     //            EXEC(2,(TABLE 3),@N)
     //         }
     //    ELSE { wrch(#33); wrch('E')
     //         }
     // }
      { writes("*e[H*e[2J")
      }














