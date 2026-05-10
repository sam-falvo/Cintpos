/* REVERSI PART1              RELEASE 26/7/1976  */
 /* MODIFICATIONS FOR CDC 6000 VERSION --
  1. BUG IN NEWLINE() IS ALLOWED FOR BY OMITTING
            wrch(' ') IN ROUTINE DISPLAY.
  2. MESSAGES TO A TERMINAL ARE SENT ONLY AFTER A NEWLINE.
       THEREFORE EXTRA NEWLINES ARE GIVEN IN
       START.
 */
GET "libhdr"
MANIFEST {
          black=-1;white=1;
          forced=1; ok=0; drop=-1;
         }
GLOBAL { copye:202; rndm:203; wsum:204; makemove:205; rowstep:206; colstep:207;
	 update:208; movesplayed:209; weights:210; uplim:211;maxcands:212; maxdepth:213;
	 mvalue:214; prunedepth:215; depth:216; rowseq:217; colseq:218; unfinished:219; 
	 minmaxsofar:220; zugzwang:221; zweight:222; trace:223; settrace:224; dispw:225;
	 clearscreen:226; display:227; term:228; lp:229; quick:230; setup:231;
	 centresquares:232; t4010:233; topsep:234; btmsep:235; line:236 }
STATIC { value=0  }
LET display(pos) BE
 { LET ch='A'; LET sep='!'
    clearscreen()
    writes("*N   ")
    FOR m=1 TO 8 DO { wrch(topsep); writes(line) }; wrch(topsep)
    FOR n = 8 TO 1 BY -1 DO
     { IF t4010
        THEN { writes("*N   ")
                FOR m=1 TO 8 DO writes("!   "); wrch('!')
             }
        newline(); wrch(' '); writed(n,1); wrch(' ')
        FOR m = 1 TO 8 DO
         { writes("! ")
            SWITCHON pos!(10*n+m) INTO
             { CASE white : wrch('O'); ENDCASE
                CASE black : wrch('**'); ENDCASE
                DEFAULT    : wrch(' ')
             }
            wrch(' ')
         }
        wrch('!')
        IF n=1 THEN sep:=btmsep
        writes("*N   "); wrch(sep)
        FOR m = 1 TO 8 DO { writes(line); wrch(sep) }
     }
    writes ("*N  ")
    FOR m = 1 TO 8 DO
     { writes("   "); wrch(ch); ch:=ch+1
     }
 } 
LET listcandidates(player,pos,movelist,ncands) BE
 { LET movep,opponent=1,-player
    LET nmoves = 0
    LET row,col,row1,col1=0,0,0,0
    LET maxn = maxcands!depth
    FOR square=0 TO uplim DO
     { row:=rowseq!square; col:=colseq!square
        IF pos!(row+col)=0 THEN
          { FOR direction=1 TO 8 DO
              { row1:=row1+rowstep!direction
                 col1:=col1+colstep!direction
                 IF pos!(row1+col1) NE opponent THEN LOOP
                  { row1:=row1+rowstep!direction
                     col1:=col1+colstep!direction
                  } REPEATWHILE pos!(row1+col1)=opponent
                 IF pos!(row1+col1)=player THEN
                  { // ENTER MOVE IN LIST & QUIT FOR LOOP
                    movelist!movep := row/10
                    movelist!(movep+1):=col
                    IF depth < prunedepth THEN   // KEEP ALL CANDIDATES
                      { movep:=movep+2
                         nmoves :=nmoves+1
                         BREAK
                      }
                    /* OTHERWISE, CHECK WHETHER THE MOVE IS SENSIBLE */
                    SWITCHON mvalue(movelist+movep,pos,player) INTO
                     { CASE forced: IF depth > prunedepth THEN
                                      { !ncands:=nmoves+1; RETURN }
                        CASE ok    : movep:=movep+2
                                     nmoves:=nmoves+1
                        CASE drop  : BREAK
                     }
                  }
               }  // END FOR
             IF nmoves=maxn THEN BREAK
         }  // END IF
     } //  END FOR                      
   IF nmoves=0 THEN { movelist!1:=0; !ncands:=1; RETURN }
   !ncands :=nmoves
   IF depth<prunedepth & (mvalue(movelist+1,pos,player)=drop)
   THEN  // RE-ORDER THE CANDIDATES TO REINFORCE PRUNING
    { FOR movep=3 TO 2*nmoves-1 BY 2 DO
        { IF mvalue(movelist+movep,pos,player) NE drop
           THEN   // SWAP WITH 1ST MOVE OF LIST
            { LET save1,save2 = movelist!1,movelist!2
               movelist!1:=movelist!movep; movelist!2:=movelist!(movep+1)
               movelist!movep:=save1; movelist!(movep+1):=save2
               BREAK
            }
        }
     }
  } // END OF LISTCANDIDATES
    /*              ---                */  
 LET selectmove (player,pos,move,result) BE 
  { LET movelist=VEC 40    // 2* MAXCANDS
     LET newpos=VEC 100
       LET candresult,currcand,ncands,bestcand,bestsofar=0,1,0,1,3
       LET opponent,listend,sum=-player,0,0
       LET nextmove = VEC 2
       depth:=depth+1
       listcandidates(player,pos,movelist,@ncands)
       IF (depth+movelist!1)=1 THEN { move!0:=0;
                                       depth:=depth-1
                                       RETURN
                                    }
        listend:=ncands*2
        TEST player=white // SET UP ALPHA-BETA PRUNING
        THEN { bestsofar:= -4000
                minmaxsofar!(depth+1):=bestsofar
             }
        ELSE { bestsofar:= 4000
                minmaxsofar!(depth+1):=bestsofar
             }
        WHILE currcand<=listend DO
          { makemove(player,pos,movelist+currcand,newpos)
             /* ATTEMPT TO EVALUATE RESULTING POSN */
             TEST depth < maxdepth
             THEN selectmove(opponent,newpos,nextmove,@candresult)
             ELSE  // TRY TO EVALUATE DIRECTLY
              { candresult := 0
                 TEST movesplayed > 52
                 THEN FOR i=11 TO 88 DO candresult:=candresult+newpos!i
                 ELSE 
                  { candresult := wsum(newpos,weights)
                     IF zugzwang(pos,player,movelist+currcand)
                       THEN candresult:=candresult - player*zweight
                  }
              }
            IF (depth=1) THEN
             { IF currcand=13 THEN newline(); wrch(' ');
                wrch('A'-1+movelist!(currcand+1))
                wrch('0'+movelist!currcand)
                writed(candresult,3)
             }
            /* UPDATE BESTSOFAR */
            TEST player=white
            THEN {
                    IF candresult>bestsofar THEN
                      { bestcand:=currcand
                         bestsofar:=candresult
                         IF candresult>=minmaxsofar!depth THEN BREAK
                      }
                 }
            ELSE {
                    IF candresult<bestsofar THEN
                      { bestcand:=currcand
                         bestsofar:=candresult
                         IF candresult<=minmaxsofar!depth THEN BREAK
                      }
                 }
            currcand:=currcand+2
           } // END OF WHILE
        !result:=bestsofar
        move!0:=movelist!bestcand
        move!1:=movelist!(bestcand+1)
        IF depth>1 THEN //UPDATE MINMAXSOFAR
          TEST player=white
          THEN {
                 IF bestsofar<minmaxsofar!depth THEN
                 minmaxsofar!depth:=bestsofar
               }
          ELSE {
                 IF bestsofar>minmaxsofar!depth THEN
                 minmaxsofar!depth:=bestsofar
               }
      depth:=depth-1
    } // END OF SELECTMOVE
    /*  -----  MAIN PROGRAM FOLLOWS  -----  */
  LET start(parm) BE
   { LET readmove(pos,readstatus) BE
       { LET ch=' '
          LET row,col=0,0
          LET legalmoves=VEC 50  // 2*MAXCANDS
          LET nlmoves=0
          LET move=VEC 2
          LET valid=FALSE
          !readstatus:=FALSE // SET TRUE IF PASS
          WHILE NOT valid DO
    // *N AT END OF NEXT LINE IS FOR 6000
           { writes("*N ENTER YOUR MOVE - *N")
              ch:=rdch() REPEATUNTIL ch NE ' '
              IF ch='W' THEN { dispw(pos); LOOP }
              col:=ch-'A'+1
              IF ch='P' THEN col:=0 // PASS
              ch:=rdch() REPEATUNTIL ch NE ' ' 
              row:=ch-'0'
              ch :=rdch() REPEATUNTIL ch='*N'
              IF col=0 THEN row:=0 // PASS 
              IF 0 <= row <= 8 &
                 0 <= col <= 8 &
                 pos!(10*row+col)=0 
    /* --CHECK FOR VALIDITY AND IF VALID, MAKE MOVE -- */
                 THEN
          { LET movep=1
             listcandidates(white,pos,legalmoves,@nlmoves)
              /* CHECK WHETHER MOVE IS IN THE LIST */
                FOR i=1 TO nlmoves DO
                 { IF row=legalmoves!movep & col=legalmoves!(movep+1)
                    THEN { valid:=TRUE; BREAK }
                    movep:=movep+2
                 }
                IF legalmoves!1=0 & col=0 THEN  // VALID PASS
                 { valid:=TRUE; !readstatus:=TRUE
                 }
         TEST valid
         THEN { move!0:=row; move!1:=col
                 makemove(white,pos,move,pos)
              }
         ELSE { writes(" INVALID MOVE, TRY AGAIN - ")
                 writes(" LEGAL MOVE ARE :-*N")
                 TEST legalmoves!1 = 0
                 THEN writes(" PASS*N")
                 ELSE FOR i=1 TO nlmoves DO 
                       { wrch(' ')
                          wrch('A'-1+legalmoves!(2*i))
                          wrch('0'+legalmoves!(2*i-1))
                       }
              }
            }  // END OF THEN
          }  // END OF 'WHILE NOT VALID'
          }
         LET vecalpha=VEC 20 // USED IN ALPHA-BETA PRUNING
         LET ncands = 0
         LET sum,valid=0,FALSE
         LET readstatus=0; LET pos=VEC 100
         LET rmove,row,col,ch,dummy=0,0,0,0,0
         LET firstplayer=white
         LET whitepass,blackpass=FALSE,FALSE
         LET move=VEC 2
         term := output()  // ON ENTRY, HP BCPL SETS TO TERM. NO.
         minmaxsofar:=vecalpha // STATIC VECTOR FOR ALPHA-BETA PRUNING
         depth := 0            // CURRENT DEPTH OF LOOKAHEAD
         trace := 0            // LOOKAHEAD TRACE OPTION
         minmaxsofar!1:=-1000     // SO THAT DEPTH 1 NEED NOT BE SPECIAL
         prunedepth := 3
         maxcands:=TABLE 20,5,5,4,4,3,3,3,3,3,3,3,3,3,3
         maxdepth:=5
         movesplayed:=0
         uplim:=63
         zweight := 2
         centresquares := TABLE 44,45,54,55
         clearscreen();
         rowstep:=TABLE -10,-10,-10,0,10,10,10,0,-10
         colstep:=TABLE -1,0,1,1,1,0,-1,-1,-1
         weights:=TABLE 0,0,0,0,0,0,0,0,0,0,
                   0,140,16,40,30,30,40,16,140,0,
                   0,16,-30,-4,-6,-6,-4,-30,16,0,
                   0,40,-4,3,1,1,3,-4,40,0,
                   0,30,-6,1,1,1,1,-6,30,0,         
                   0,30,-6,1,1,1,1,-6,30,0,         
                   0,40,-4,3,1,1,3,-4,40,0,
                   0,16,-30,-4,-6,-6,-4,-30,16,0,
                   0,140,16,40,30,30,40,16,140,0
          rowseq:=TABLE 1,1,8,8,1,1,1,1,8,8,8,8,3,3,4,4,5,5,6,6,
                        1,1,8,8,2,2,7,7, // EGDE SQS. ADJACENT TO CORNERS
                        3,3,6,6,      // CORNERS OF 4 X 4 CENTRE SQUARE
                        3,3,6,6,4,4,5,5, // OUTER RING OF CENTRE SQUARE
                        4,4,5,5,  // CENTRE
                        2,2,2,2,7,7,7,7,3,3,4,4,5,5,6,6,2,2,7,7
          colseq:=TABLE 1,8,1,8,3,4,5,6,3,4,5,6,1,8,1,8,1,8,1,8,
                        2,7,2,7,1,8,1,8, // EDGE SQS ADJACENT TO CORNERS
                        3,6,3,6,  // CORNERS CENTRE SQ
                        4,5,4,5,3,6,3,6,  // OUTER RING OF CEN.SQ.
                        4,5,4,5,  // CENTRE
                        3,4,5,6,3,4,5,6,2,7,2,7,2,7,2,7,2,7,2,7
          FOR i=0 TO 100 DO pos!i := 0
          FOR i=0 TO 63 DO rowseq!i:=10*rowseq!i  // FOR USE AS INDEX
          setup(pos,@firstplayer) 
          FOR i=1 TO 2 DO // COMPLETE 4 CENTRE SQUARES
           { display(pos); valid:=FALSE
              WHILE NOT valid DO
               { writes("*N ENTER YOUR MOVE - ") 
                  newline() //NECESSARY ON 6000
                  ch:=rdch() REPEATUNTIL ch NE ' '
                  col:=ch-'A'+1
                  ch:=rdch() REPEATUNTIL ch NE ' '
                  row:=ch-'0'
                  ch:=rdch() REPEATUNTIL ch='*N'
                  TEST 4<=row<=5 & 4<col<5 &
                       pos!(10*row+col)=0
                  THEN
                   { valid:=TRUE
                      pos!(10*row+col):=1
                   }
                  ELSE writes("*N INVALID MOVE,TRY AGAIN - ")
               } // END WHILE
              movesplayed:=movesplayed+1
              IF movesplayed=4 THEN BREAK
             /* PICK ANOTHER RANDOM MOVE */
            display(pos)
            rmove:=rndm(0)!centresquares REPEATUNTIL pos!rmove = 0
            pos!rmove := black
            movesplayed:=movesplayed+1
        }  // END FOR
       /* SEE WHO STARTED */
         IF firstplayer=black
          THEN
           { display(pos)
              selectmove(black,pos,move,@value)
              wrch(' '); wrch('A'-1+move!1)
              // NEWLINE IN NEXT LINE SPECIAL FOR 6000
              wrch('0'+move!0); newline(); // CH:=rdch()
              makemove(black,pos,move,pos)
              movesplayed:=movesplayed+1
           }
          unfinished:=TRUE
          WHILE unfinished DO
          { display(pos)
             IF movesplayed=64 THEN BREAK
             readmove(pos,@whitepass)
             IF blackpass & whitepass THEN BREAK
             IF NOT whitepass THEN movesplayed:=movesplayed+1
             display(pos)
             IF movesplayed=64 THEN BREAK
             selectmove(black,pos,move,@value)
             TEST move!0=0
             THEN writes(" PASS*N") // *N FOR 6000
             ELSE   { wrch(' '); wrch('A'-1+move!1)
                     wrch('0'+move!0); newline() // *N FOR 6000 
                  }
             // NEXT LINE INAPPROPRIATE ON 6000
             ch:=rdch() REPEATUNTIL ch='*N' // ALLOWPLAYER TO READ MOVE
             makemove(black,pos,move,pos)
             IF move!0 NE 0 THEN { movesplayed:=movesplayed+1 }
             blackpass := ( move!0 = 0 )
             unfinished:= NOT (whitepass & blackpass)
             update(rowseq,colseq,pos)
          }  
         newline();
         sum:=0
         FOR i=0 TO 99 DO sum:=sum+pos!i
         TEST sum = 0
         THEN writes(" DRAW !")
         ELSE
           { TEST sum > 0
              THEN writes(" YOU WIN BY ")
              ELSE   { sum:=-sum; writes(" I WIN BY ") }
              writed(sum,3)
           }
         writes("*N THANK YOU.")
         FINISH
      }
      /* REVERSIE PART 2             RELEASE 9/12/1976  */
     LET setup(pos,addr_firstplayer) BE
      { LET ch,dummy=0,0; LET wow,col,rmove=0,0,0
       /*  THE *N AT THE END OF EACH PROMPT LINE ARE FOR INTERCOM */
          { writes("*N WHAT SORT OF TERMINAL ARE YOU USING?")
             writes("*N (REPLY Y FOR T4010 TYPE OR")
             writes("*N B FOR BEEHIVE/T4023 TYPE -*N")
             ch:=rdch(); dummy:=rdch() REPEATUNTIL dummy='*N'
          } REPEATUNTIL ch='T' LOGOR ch='B'
         TEST ch='T'
         THEN { t4010:=TRUE; topsep:='.'; btmsep:='!'
                 line:="___"  // THIS SHOULD BE 3 UNDERLINES 
              }
         ELSE { t4010:=FALSE; topsep:=':'; btmsep:='*''
                 line:="---"
              }
         writes("*N DO YOU PREFER THE PROGRAM TO PLAY QUICKLY OR WELL?")
         writes("*N (REPLY Q OR W) -*N")
         ch:=rdch() REPEATUNTIL ch='Q' LOGOR ch='W'
         dummy:=rdch() REPEATUNTIL dummy='*N' // FINISH READING THE LINE
         quick := ch='Q'
          { writes("*N DO YOU WISH TO MOVE FIRST?")
             writes("*N (REPLAY Y OR N ) -*N")
             ch:=rdch(); dummy:=rdch() REPEATUNTIL dummy='*N'
          } REPEATUNTIL ch='Y' LOGOR ch='N'
         IF ch='N'
         THEN // MAKE A MOVE
          { !addr_firstplayer:=black
             rmove:= rndm(0)!centresquares
             pos!rmove := black
             movesplayed:=1
          }
      }
     LET makemove(player,pos,move,newpos) BE
      { // THE MOVE IS IN MOVE!0 AND MOVE!1
         LET row,col,opponent = move!0,move!1, -player
         LET row1,col1=0,0
         copye(pos,newpos,100)  // COPY POS TO NEWPOS
         IF row = 0 THEN RETURN // HANDLE PASS CORRECTLY
         row:=10*row   // FOR USE AS INDEX
         newpos!(row+col):=player
         FOR direction=0 TO 7 DO
          { row1:=row+rowstep!direction
             col1:=col+colstep!direction
             IF newpos!(row!+col1) NE opponent THEN LOOP
             WHILE newpos!(row1+col1) = opponent DO
              { row1:=row1+rowstep!direction
                 col1:=col1+colstep!direction
              }
             IF newpos!(row1+col1)=player THEN
              { row1:=row1-rowstep!direction
                 col1:=col1-colstep!direction
                 WHILE row1 NE row LOGOR col1 NE col DO
                  { row1:=row1-rowstep!direction
                     col1:=col1-colstep!direction
                  }
              } // END IF
           } // END FOR 
          IF depth > 0 THEN RETURN
          /*     UPDATE WEIGHTS     */
          TEST row=10 LOGOR row=80
          THEN
           { row:= row=10 -> 20,70
              TEST col=1 LOGOR col=8
              THEN { col:= col=1 ->2,7
                      weights!(row+col):=1
                   }
              ELSE IF 3<=col<=6
                  THEN weights!(row+col):=1
           }
          ELSE IF (col=1 LOGOR col=8) & 30<= row <= 60
             THEN { col:= col=1 -> 2,7
                     weights!(row+col):=1
                  }
      }  // END MAKEMOVE
     LET update(rowseq,colseq,pos) BE
      { LET copy=0
         LET limit = (quick -> 6,8)
         FOR look=0 TO uplim DO
          { IF pos!(rowseq!look+colseq!look)=0
            THEN  // RETAIN THESE ENTRIES
             { rowseq!copy:=rowseq!look
                colseq!copy:=colseq!look
                copy:=copy+1
             }
          } // END FOR
         uplim:=copy-1  // NO. OF REMAINING ENTRIES
         IF 52<uplim<56 THEN { maxcands!1:=10
                                maxcands!2:=10
                                maxcands!3:=5
                                maxcands!4:=5
                                maxcands!5:=3; maxcands!6:=3
                                TEST quick
                                THEN maxdepth:=3 ELSE maxdepth:=4
                             }
         IF 40<uplim<44 THEN { maxcands!1:=12; maxcands!2 := 12 
                                maxcands!3:=6 ; maxcands!4:= 6
                                zweight := 6
                             }
         IF uplim<32 & weights!11 = 60
         THEN maxcands!5 :=4
         IF 11 < uplim < 32 THEN
          FOR i=11 TO 88 DO
           { LET diff = uplim - 11 AND finalw = 1
              IF (i/10 = 1) LOGOR (i/10 = 8) THEN finalw := finalw+finalw
              IF (i REM 10 = 1) LOGOR (i REM 10 = 8)
                                           THEN finalw := finalw+finalw
              TEST diff <= 2
              THEN weights!i := finalw
              ELSE   weights!i := weights!i - 2*(weights!i-finalw) / diff
           }
         IF uplim < 16 THEN { maxcands!6:=2; 
                               zweight := 1
                               TEST quick
                               THEN maxdepth:=4 ELSE maxdepth:=5
                               FOR i = 11 TO 88 DO
                               { IF weights!i<1 THEN weights!i:=weights!i+1   
                               }
                            }
         IF uplim < 14 THEN { FOR i=prunedepth TO 4 DO
                                    maxcands!i:=maxcands!i + 2
                               prunedepth := 3
                            }
         IF uplim < 13 THEN { maxcands!1:=uplim+1
                               maxcands!2:=uplim
                               maxcands!6 :=4
                               prunedepth:=5
                            }
         IF uplim < limit THEN { maxdepth:= uplim+3 // ALLOWS FOR ONLY 3 PASSES
                                  prunedepth:=15     // DON'T PRUNE
                                  FOR i=1 TO maxdepth DO
                                   maxcands!i:=uplim+1
                               }
         maxcands!maxdepth := 1
      } // END UPDATE
     LET mvalue(move,pos,player) = VALOF
      { LET row,col = move!0, move!1; LET opponent=-player
         LET play,start,step,nxt,limit = 0,0,0,0,0
         LET square=0
         LET pattern = #140000
         LET npatterns = 24
         LET key  = TABLE #34000, #35400, #130000, #34600,
                #26000,  #7000, #7300,  #7140,  #7154,  #5400,
                #5440,  #1600,  #1660, #115400, #17000, #17300,
                #132000, #26200, #26260, #1633, #1660, #21600,
                #26400,  #3600
         LET mask = TABLE #177400, #177400, #176000, #177700,
                #177700, #177700, #177700, #177774, #177774, #37760,
                #37760,  #37760,  #37760, #177700, #177700, #177700,
                #177400, #177760, #177760, #7777, #7760, #177760,
                #177774, #177760
         TEST row=1 LOGOR row=8
         THEN
          { TEST col=1 LOGOR col=8
             THEN RESULTIS forced
             ELSE
              { TEST col>4
                 THEN { start:=10*row + 8
                         step:=-1
                      }
                 ELSE   { start:=10*row + 1; step:=1
                      }
              }  
          } 
         ELSE // NOT AN EDGE ROW
          { TEST col=1 LOGOR col=8
             THEN { TEST row>4
                     THEN { start:=80+col; step:=-10
                          }
                     ELSE { start:=10+col; step:=10
                          }
                  }
             ELSE RESULTIS ok // NOT AN EDGE PLAY. 
           }
             /* ---------------------------------------- */
             /* SET UP THE PATTERN.                      */
             /* ---------------------------------------- */
             play:= 10*row + col                    
             nxt:= start+7*step; limit:=start-step
             WHILE nxt NE limit DO
              { pattern:=pattern >> 2
                 IF nxt=play THEN { pattern:=pattern + #140000
                                        nxt:=nxt-step; LOOP
                                     }
                 square:=pos!nxt
                 IF square=0
                 THEN { nxt:=nxt-step; LOOP
                      }
                 TEST square=player
                 THEN pattern:=pattern + #40000
                 ELSE  pattern:=pattern + #100000  // OPPONENT
                 WHILE pos!nxt=square DO nxt:=nxt- step
              }
      // PATTERN NOW SET UP.. EVALUATE IT. 
            FOR i=0 TO npatterns-1 DO
             IF (pattern & mask!i) = key!i RESULTIS drop
            RESULTIS ok
      }
     LET zugzwang(pos,player,move) = VALOF
      { LET edge=VEC 2
         edge!1 := move!1
         SWITCHON move!0 INTO
          { CASE 0: RESULTIS TRUE
             CASE 1: RESULTIS FALSE 
             CASE 8: RESULTIS FALSE
             CASE 2: edge!0:=1; ENDCASE
             CASE 7: edge!0:=8; ENDCASE
            DEFAULT: edge!0:= move!0
                     SWITCHON move!1 INTO
                      { CASE 2: edge!1:=1; ENDCASE
                         CASE 7: edge!1:=8; ENDCASE
                         DEFAULT: RESULTIS FALSE
                      }
          }
         // CHECK WHETHER THE NEIGHBOURING EDGE SQ CONTAINS PLAYER
         TEST pos!(10*edge!0 + edge!1) = player
         THEN RESULTIS FALSE
         ELSE RESULTIS TRUE
      }
     LET dispw(pos) BE
      { LET ch=0
         IF rdch() NE '-' THEN RETURN   // PROTECT AGAINST UNAUTH. USE
         ch:=rdch() REPEATUNTIL ch='*N'
         clearscreen();
         FOR row=80 TO 10 BY -10 DO
          { newline()
             FOR col=1 TO 8 DO writed(weights!(row+col),4)
          }
         ch:=rdch() REPEATUNTIL ch='*N'; display(pos)
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
     LET copye(array1,array2,nwords) BE
      { FOR i=0 TO nwords-1 DO array2!i := array1!i }
     //-----------------------------
     // WSUM (POS,WEIGHTS) = SUM OF POS!I*WEIGHTS!I
     //  FOR I 10 TO 89
     // EXCEPT WHEN NO WHITE PIECES
     // ARE LEFT,WHEN WSUM= -2000*BLACK.
     // POS CONTAINS ONLY THE VALUES -1,0 AND +1.
     LET wsum(pos,weights) = VALOF
      { LET sum=0; LET whitefound=FALSE
         FOR i=10 TO 89 DO
          { SWITCHON pos!i INTO
              { CASE white: whitefound:=TRUE
                             sum:=sum+weights!i; ENDCASE
                 CASE black: sum:=sum-weights!i
              }
           }
          TEST whitefound
          THEN RESULTIS sum
          ELSE RESULTIS 2000*black
      }
     //------------------------------
     //  RANDOM NUMER GENERATOR -- SHOULD USE THE SYSTEM CLOCK
     //    TO RETURN TRULY RANDOM INTEGER IN (0,3).
     LET rndm(a) = VALOF
      { STATIC { n=1 }
         n := (n=3 -> 0, n+1); RESULTIS n
      }
     //------------------------------
     LET clearscreen() BE
     // { TEST T4010
     //    THEN { LET N= (#32<<6) + output()
     //            EXEC(2,(TABLE 3),@N)
     //         }
     //    ELSE { wrch(#33); wrch('E')
     //         }
     // }
      { writes("*e[H*e[2J")
      }














