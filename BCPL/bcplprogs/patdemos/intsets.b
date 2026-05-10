/*

At this time this BCPL program is incomplete and untested.



This will be a free translation of an ML of a program written
by Jon Freeman which was one of the winning entries in the ML
programming competition devised by Andrew W. Appel.  The problem
is to implement invent a structure to represent integer sets and
implement ten specified functions using this representation.  The 
first few lines of Jon Freeman program are as follows:

(*
   Name:  Jon Freeman
   Department, Institution:  Department of Computer and Information
     Science, School of Engineering and Applied Science, University
     of Pennsylvania
   Address:  200 South 33rd Street, Philadelphia, PA  19104
   E-mail:  freeman@gradient.cis.upenn.edu
   Comments:  I'm taking advantage of the bounds on integers in SML.
     My data structure is a binary tree with sorted lists as leaves.
     It can easily become unbalanced, but its depth is bounded by the
     number of bits in an integer.
*)

(*signature INTSET = sig type intset
                       val empty : intset
                       val singleton : int -> intset
                       val union : intset * intset -> intset
                       val add : intset * int -> intset
                       val intersection : intset * intset -> intset
                       val member : int * intset -> bool
                       val members : intset -> int list
                       val cardinality : intset -> int
                       val difference : intset * intset -> intset
                       val delete : intset * int -> intset
                   end;
*)
*/
/* An intset is either a sorted list or a tree thereof. In the 
   following datatype, the zeroth component is IntList or IntTree
   indicating the kind of node, the first component is the smallest
   possible value, the second is the largest possible value, and
   the third is the middle value.  The fourth and fifth components
   are the sizes of the left and right substructures; the sixth
   and seventh components are the substructures themselves.

Ilist -> 0
      |  [val, Ilist]

Iset  -> [IntList, low, high, mid, lcount, rcount, Ilist, Ilist]
      |  [IntTree, low, high, mid, lcount, rcount, Iset,  Iset]
*/

GET "libhdr"

GLOBAL {
split_list:ug
lists_to_tree
list_member
list_add
list_delete
list_union
list_intersection
list_diference
empty
singleton
cardinality
members
member
add
delete
simple_union
union
simple_intersect
intersection
diff_list_tree
diff_tree_list
differene
mk1
mk2
mk4
mk8
concat
}

MANIFEST {
  IntList=1
  IntTree
}

/* The largest possible size of an IntList.  It should be a power
   of 2.  The actual value is determined empirically, and can be
   optimized for a particular suite of test cases.  */

MANIFEST { List_max = 32 }

LET split_list 
:      0,      ?,     ? => mk4(0,0,0,0)
: x[h,t], mid <h, count => mk4(0,0,count,x)
:  [h,t],    mid, count =>
     MATCH (split_list (t,mid,count-1))
     : a, lis1, b, lis2 => mk4(a+1,mk2(h,lis1),b,lis2)

/* Precondition:  lcount + rcount > List_max.  Note that computing
   (low + mid) / 2 or (mid + 1 + high) / 2 could raise Overflow. */

LET lists_to_tree
: ?,low,high,mid,lcount,rcount,lis1,lis2 => VALOF
{ LET mid1 = (low / 2) + (mid / 2)
  LET mid2 = ((mid + 1) / 2) + (high / 2)
  MATCH ( split_list (lis1,mid1,lcount),
          split_list (lis2,mid2,rcount)
        )
  : [lcount1,lis11,rcount1,lis12], [lcount2,lis21,rcount2,lis22] BE
    { LET newleft = lcount > List_max ->
                 lists_to_tree(low,mid,mid1,lcount1,rcount1,lis11,lis12),
                 mk8(IntList,low,mid,mid1,lcount1,rcount1,lis11,lis12)
      LET newright = rcount > List_max ->
                 lists_to_tree(mid+1,high,mid2,lcount2,rcount2,lis21,lis22),
                 mk8(IntList,mid+1,high,mid2,lcount2,rcount2,lis21,lis22)
      RESULTIS IntTree(low,high,mid,lcount,rcount,newleft,newright)
    }
}
/* Straightforward functions for sets as sorted lists.  All these
   functions return the cardinality of the resulting list. */

LET list_member
: ?,      0 => FALSE
: n, [=n,t] => TRUE
: n, [h,t]  => list_member (n,t)

LET list_add
: 0,     n   , count => mk2(count+1, mk2(n,0))
: [h,t], n <h, count => mk2(count+1, mk2(n, mk2(h,t)))
: [h,t], n =h, count => mk2(count, mk2(h,t))
: [h,t], n   , count => MATCH (list_add (t,n,count))
                        : count2, t2 => mk2(count2, mk2(h,t2))
LET list_delete
:     0,    ?, count => mk2(count,0)
:x[h,t], n <h, count => mk2(count,x)
: [h,t], n =h, count => mk2(count-1,t)
: [h,t], n   , count => MATCH (list_delete (t,n,count))
                        : count2, t2 => mk2(count2,mk2(h,t2))

LET list_union
: 0, 0, ?, ? => mk2(0, 0)
: lis1, 0, count1, ? => mk2(count1, lis1)
: 0, lis2, ?, count2 => mk2(count2, lis2)
: [h1 <h2,t1],x[h2,t2], count1, count2 =>
                           MATCH (list_union(t1,x,count1-1,count2))
                           : c, t => mk2(c+1, mk2(h1,t))
                           .
: [h1 =h2, t1], [h2,t2], count1, count2 =>
                           MATCH (list_union(t1,t2,count1-1,count2-1))
                           : c, t => mk2(c+1, mk2(h1,t))
                           .
:x[h1 , t1], [h2,t2], count1, count2 =>
                           MATCH (list_union (x,t2,count1,count2-1))
                           : c, t => mk2(c+1, mk2(h2,t))
                           .

LET list_intersection
: 0, 0 => mk2(0, 0)
: 0, ? => mk2(0,0)
: ?, 0 => mk2(0,0)
: [h1 <h2,t1],x[h2,t2] => list_intersection (t1, x)
: [h1=h2,t1], [h2,t2] => MATCH (list_intersection (t1,t2))
                         : c, t => mk2(c+1, mk2(h1,t) )
                         .
:x[h1 ,t1], [h2,t2] => list_intersection(x, t2)

/* count1 only matters when lis2 is not null. */

LET list_difference
: 0, ?, ?          => mk2(0, 0)
: lis1, 0, count1  => mk2(count1, lis1)
: [h1,t1], lis2, ? => list_member(h1,lis2) ->
                      list_difference (t1,lis2,0),
                      MATCH (list_difference (t1,lis2,0))
                      : c, t => mk2(c+1, mk2(h1, t))
                      .

/* The important stuff. */

LET empty : => mk8(IntList,-1073741824,1073741823,-1,0,0,0,0)

LET singleton
: n>=0 => mk8(IntList,-1073741824,1073741823,-1,0,1,0,mk1(n))
: n    => mk8(IntList,-1073741824,1073741823,-1,1,0,mk1(n), 0)

LET cardinality
: [IntList,?,?,?,n1,n2,?,?] => n1 + n2
: [IntTree,?,?,?,n1,n2,?,?] => n1 + n2

LET members
: [IntList,?,?,?,?,?,lis1, lis2] => concat(lis1, lis2)
: [IntTree,?,?,?,?,?,left,right] => concat(members(left), members(right))

LET member
: n >mid, [IntList,?,?,mid,?,?, ?, lis2] => list_member(n,lis2)
: n ,     [IntList,?,?,mid,?,?,lis1,  ?] => list_member(n,lis1) 
: n >mid, [IntTree,?,?,mid,?,?, ?,right] => member(n,right)
: n ,     [IntTree,?,?,mid,?,?,left,  ?] => member(n,left)
.
LET add
: [IntList,low,high,mid,lcount,rcount,lis1,lis2], n >mid =>
          MATCH (list_add (lis2,n,rcount))
          : newcount, newlis => lcount+newcount > List_max ->
                lists_to_tree(low,high,mid,lcount,newcount,lis1,newlis),
                mk8(IntList,low,high,mid,lcount,newcount,lis1,newlis)
          .

: [IntList,low,high,mid,lcount,rcount,lis1,lis2], n =>
          MATCH (list_add (lis1,n,lcount))
          : newcount, newlis => newcount+rcount > List_max ->
                lists_to_tree(low,high,mid,newcount,rcount,newlis,lis2),
                mk8(IntList,low,high,mid,newcount,rcount,newlis,lis2)
          .
: [IntTree,low,high,mid,lcount,rcount,left,right], n >mid => VALOF
{ LET newright = add (right,n)
  RESULTIS mk8(IntTree,low,high,mid,lcount,cardinality,newright,left,newright)
}
: [IntTree,low,high,mid,lcount,rcount,left,right], n => VALOF
{ LET newleft = add (left,n)
  RESULTIS mk8(IntTree,low,high,mid,cardinality,newleft,rcount,newleft,right)
}

LET delete
: [IntList,low,high,mid,lcount,rcount,lis1,lis2],n >mid =>
          MATCH (list_delete (lis2,n,rcount))
          : newcount,newlis =>
               mk8(IntList,low,high,mid,lcount,newcount,lis1,newlis)
          .

: [IntList,low,high,mid,lcount,rcount,lis1,lis2],n =>
          MATCH (list_delete (lis1,n,lcount))
          : newcount,newlis =>
               mk8(IntList,low,high,mid,newcount,rcount,newlis,lis2)
          .

: [IntTree,low,high,mid,lcount,rcount,left,right],n >mid => VALOF
{ LET newright = delete (right,n) /* Could shrink to a list */
  LET newcount = cardinality(newright)
  RESULTIS lcount + newcount > List_max ->
             mk8(IntTree,low,high,mid,lcount,newcount,left,newright),
             mk8(IntList,low,high,mid,lcount,newcount,
                                      members(left),members(newright))
}

: [IntTree,low,high,mid,lcount,rcount,left,right],n => VALOF
{ LET newleft = delete (left,n) /* Could shrink to a list */
  LET newcount = cardinality(newleft)
  RESULTIS newcount + rcount > List_max ->
             mk8(IntTree,low,high,mid,newcount,rcount,newleft,right),
             mk8(IntList,low,high,mid,newcount,rcount,
                                      members(newleft),members(right))
}

/* Input: (listct,list,treect,tree).  Output: (count,tree). */

LET simple_union
: ?, 0,treect,tree => mk2(treect,tree)
: ?,[h,t],treect,tree [IntTree] =>
       member(h,tree) -> simple_union(0,t,treect,tree),
                         simple_union(0,t,treect+1,add(tree,h))
: listct,lis,treect,[IntList,low,high,mid,lcount,rcount,lis1,lis2] =>
      MATCH (split_list (lis,mid,listct))
      : [cta,lisa,ctb,lisb] =>
        MATCH ( list_union (lisa,lis1,cta,lcount),
                list_union (lisb,lis2,ctb,rcount)
              )
        : [cta,lisa,ctb,lisb], [count1,newlis1], [count2,newlis2] =>
            count1 + count2 > List_max ->
                 mk2(count1 + count2,
                   lists_to_tree(low,high,mid,count1,count2,newlis1,newlis2)),
                 mk2(count1 + count2,
                   mk8(IntList,low,high,mid,count1,count2,newlis1,newlis2))
        .
      .
.
LET union
: [IntList,low,high,mid,lcount1,rcount1,lis11,lis21],
  [IntList,  ?,   ?,  ?,lcount2,rcount2,lis12,lis22] =>
      MATCH ( list_union (lis11,lis12,lcount1,lcount2),
              list_union (lis21,lis22,rcount1,rcount2)
            )
      : [lcount,lis1], [rcount,lis2] =>
          lcount + rcount > List_max ->
              lists_to_tree (low,high,mid,lcount,rcount,lis1,lis2),
              mk8(IntList,low,high,mid,lcount,rcount,lis1,lis2)
      .

: [IntList,low,high,mid,lcount1,rcount1,lis1, lis2],
  [IntTree,  ?,   ?,  ?,lcount2,rcount2,left,right] =>
      MATCH ( simple_union (lcount1,lis1,lcount2,left),
              simple_union (rcount1,lis2,rcount2,right)
            )
      : [lcount,newleft], [rcount,newright] =>
           mk8(IntTree,low,high,mid,lcount,rcount,newleft,newright)
      .

: [IntTree,  ?,   ?,  ?,lcount2,rcount2,left,right],
  [IntList,low,high,mid,lcount1,rcount1,lis1, lis2] =>
      MATCH ( simple_union (lcount1,lis1,lcount2,left),
              simple_union (rcount1,lis2,rcount2,right)
            )
      : [lcount,newleft], [rcount,newright] =>
          mk8(IntTree,low,high,mid,lcount,rcount,newleft,newright)
      .

: [IntTree,low,high,mid,?,?,left1,right1],
  [IntTree,  ?,   ?  ,?,?,?,left2,right2] => VALOF
  {   LET newleft = union (left1,left2)
      LET newright = union (right1,right2)
      RESULTIS mk8(IntTree,low,high,mid,
          cardinality(newleft),cardinality(newright),
          newleft,newright)
  }
  
/* Input: (list,tree).  Output:  (count,newlist). */

LET simple_intersect
: 0, ? => mk2(0,0)
: [h,t],tree [IntTree] =>
      member(h,tree) ->
          MATCH (simple_intersect (t,tree))
          : [count,t2] => mk2(count+1, mk2(h,t2))
          .,
      simple_intersect (t,tree)

: lis, [IntList,?,?,?,?,?,lis1,lis2] =>
      MATCH ( list_intersection (lis,lis1),
              list_intersection (lis,lis2)
            )
      : [count1,newlis1], [count2,newlis2] =>
           mk2(count1+count2, concat(newlis1, newlis2))
      .
.
LET intersection
: [IntList,low,high,mid,?,?,lis11,lis21],
  [IntList,  ?,   ?,  ?,?,?,lis12,lis22] =>
      MATCH ( list_intersection (lis11,lis12),
              list_intersection (lis21,lis22)
            )
      : [lcount,lis1], [rcount,lis2] =>
           mk8(IntList,low,high,mid,lcount,rcount,lis1,lis2)
      .

: [IntList,low,high,mid,?,?,lis1, lis2],
  [IntTree,  ?,   ?,  ?,?,?,left,right] =>
      MATCH ( simple_intersect (lis1,left),
              simple_intersect (lis2,right)
            )
      : [lcount,leftlist], [rcount,rightlist] =>
           mk8(IntList,low,high,mid,lcount,rcount,leftlist,rightlist)
      .

: [IntTree,  ?,   ?,  ?,?,?,left,right],
  [IntList,low,high,mid,?,?,lis1, lis2] =>
      MATCH ( simple_intersect (lis1,left),
              simple_intersect (lis2,right)
            )
      : [lcount,leftlist], [rcount,rightlist] =>
         mk8(IntList,low,high,mid,lcount,rcount,leftlist,rightlist)
      .

: [IntTree,low,high,mid,?,?,left1,right1],
  [IntTree,  ?,   ?,  ?,?,?,left2,right2] => VALOF
  {   LET newleft = intersection (left1,left2)
      LET newright = intersection (right1,right2)
      LET lcount = cardinality(newleft)
      LET rcount = cardinality(newright)
      RESULTIS lcount + rcount > List_max ->
        mk8(IntTree,low,high,mid,lcount,rcount,newleft,newright),
         mk8(IntList,low,high,mid,lcount,rcount,members(newleft),
                      members(newright))
  }

/* Input: (listcount,lis,tree).  Output: (count,newlist).  listct
   only matters when lis is not null and tree is an IntList. */

LET diff_list_tree
: ?, 0, ? => mk2(0,0)
: ?,[h,t], tree [IntTree] =>
      member(h,tree) -> diff_list_tree (0,t,tree),
        MATCH (diff_list_tree (0,t,tree))
        : [c,t2] => mk2(c+1, mk2(h,t2))
        .

: listct,lis,[IntList,low,high,mid,lcount,rcount,lis1,lis2] =>
      MATCH (split_list (lis,mid,listct))
      : [cta,lisa,ctb,lisb] =>
           MATCH ( list_difference (lisa,lis1,cta),
                   list_difference (lisb,lis2,ctb)
                 )
           : [count1,newlis1], [count2,newlis2] =>
               mk2(count1+count2, concat(newlis1, newlis2))
           .
      .
.
/* Input: (treecount,tree,listct,lis).  Output: (count,newtree).
   listct only matters when lis is not null and tree is an IntList. */

LET diff_tree_list
: treect,tree,?,0 => mk2(treect,tree)
: treect,tree[IntTree],?,[h,t] => member(h,tree) ->
                                  diff_tree_list(treect-1,delete(tree,h),0,t),
                                  diff_tree_list(treect,tree,0,t)
: treect,[IntList,low,high,mid,lcount,rcount,lis1,lis2],listct,lis =>
      MATCH (split_list (lis,mid,listct))
      : [?,lisa,?,lisb] =>
          MATCH ( list_difference(lis1,lisa,lcount),
                  list_difference(lis2,lisb,rcount)
                )
          : [count1,newlis1], [count2,newlis2] =>
               mk2(count1+count2,
                   mk8(IntList,low,high,mid,count1,count2,newlis1,newlis2))
          .
      .
.
LET difference
: [IntList,low,high,mid,lcount1,rcount1,lis11,lis21],
  [IntList,  ?,   ?,  ?,      ?,      ?,lis12,lis22] =>
      MATCH ( list_difference (lis11,lis12,lcount1),
              list_difference (lis21,lis22,rcount1)
            )
      : [lcount,lis1], [rcount,lis2] =>
         mk8(IntList,low,high,mid,lcount,rcount,lis1,lis2)
      .

: [IntList,low,high,mid,lcount1,rcount1,lis1, lis2],
  [IntTree,  ?,   ?,  ?,      ?,      ?,left,right] =>
      MATCH ( diff_list_tree (lcount1,lis1,left),
              diff_list_tree (rcount1,lis2,right)
            )
      : [lcount,newlis1], [rcount,newlis2] =>
         mk8(IntList,low,high,mid,lcount,rcount,newlis1,newlis2)
      .

: [IntTree,low,high,mid,treect1,treect2,left,right],
  [IntList,  ?,   ?,  ?,listct1,listct2,lis1, lis2] =>
      MATCH ( diff_tree_list (treect1,left,listct1,lis1),
              diff_tree_list (treect2,right,listct2,lis2)
            )
      : [lcount,newleft], [rcount,newright] =>
           lcount + rcount > List_max ->
              mk8(IntTree,low,high,mid,lcount,rcount,newleft,newright),
              mk8(IntList,low,high,mid,lcount,rcount,members(newleft),
                      members(newright))
      .
: [IntTree,low,high,mid,?,?,left1,right1],
  [IntTree,  ?,   ?,  ?,?,?,left2,right2] => VALOF
  {   LET newleft = difference (left1,left2)
      LET newright = difference (right1,right2)
      LET lcount = cardinality(newleft)
      LET rcount = cardinality(newright)
      RESULTIS lcount + rcount > List_max ->
         mk8(IntTree,low,high,mid,lcount,rcount,newleft,newright),
         mk8(IntList,low,high,mid,lcount,rcount,members(newleft),
                      members(newright))
  }
  
LET mk1 : => 0

LET mk2 : => 0

LET mk4 : => 0

LET mk8 : => 0

LET concat : => 0

// End of program

