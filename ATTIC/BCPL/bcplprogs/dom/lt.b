/* 
This is a simple implementation of the Lengauer-Tarjan algorithm
for computing the dominator tree of a flowgraph.

This implementation compares the efficiency of three different
implementations of eval and link.

Implemented in BCPL by Martin Richards (c) April 2001

Revised: 18 July 2017

References:

Lengauer,T. and Tarjan R.E.
A Fast Algorithm for Finding Dominators in a Flowgraph.
ACM Trans on Programming Languages and Systems, Vol 1, No. 1, July 1979.
From now on called LT79.

Tarjan R.E.
Applications of path Compression on Balanced Trees
Journal of the ACM, Vol 26, No 4 October 1979, pp 690-715
From now on called T79
This paper helps to explain how the sophicated version of 
LINK in LT79 works.

Also influenced by
Muchnick, S.S.
Advanced Compiler Design Implementation
Morgan Kaufmann Publishers, 1997

The sophisticated algorithm is better than the O(n log n) one,
provided the graph is large enough and has at least about 3 times as
many edges as vertices. If lt is called with no first arguments it
generates the following output showing the performance in terms of
Cintcode instructions executions of the three versions of the
algorithm when applied to various test graphs.


                                  Instruction Counts
 Nodes    Edges  Seed     v.simple    simple  sophisticated

  1000     1500    1        284778    272290         321548
  1000     2000    1        455077    317213         358242
  1000     2500    1       1180615    376591         388715
  1000     3000    1       2440693    444899         416557
  1000     5000    1       5680715    630141         542019
  1000    10000    1      12848321   1048970         850534
 10000    50000    1     334314578   6613341        5431447
 10000   100000    1     949582133  10926989        8540733
100000   400000    1             -  56924002       48580972

100000   123289    1 f    24591918  25380331       32042009

For the last few I entered the interpreter by:

       cintsys -m 40000000

and executed after obeying the command:

       stack 500000

In LT79, vertices of the given flow graph are represented by vertex
numbers which are subscripts of various arrays that hold values
associated with each vertex.  The arrays are called semi, ancestor,
label, child, size, parent, pred, succ, bucket and vertex. The number
of vertices is given by n and all arrays have subscripts ranging from
1 to n except label, semi and size that range from zero to n. Most of
these fields hold vertex numbers but the semi field holds the discovery
time of the semi dominator. vertex(semi(v)) will yield the vertex
number of the semi dominator when needed. The elements of vertex are
set by the depth first search function dfs. If i is the DFS discovery
time of vertex number v, then vertex(i)=v. The elements label(0),
semi(0) and size(0) are each set to zero. This simplifies the
treatment of special cases in the program.

In this BCPL implementation, vertices are represented by pointers to
vertex nodes that have fields holding information about each vertex.
If v points to a vertex node then Id!v, Parent!v, Dom!v, Semi!v,
Ancestor!v, Best!v, Succ!v, Pred!v, Bucket!v and Size!v hold the
values associated with the vertex. In LT79 succ, pred and bucket are
possibly empty sets of vertex numbers. In the BCPL version, these are
implemented as lists. If p points to an element in such a list, p!0
points to the next element of the list and p!1 is the pointer to a
vertex node. A null pointer is represented by zero. In the BCPL
version, the label values use the selector Best. The three fields
size, label and semi require special treatment.

size(v)  inLT79 is implemented in BCPL by v -> Size!v, 0
label(v) inLT79 is implemented in BCPL by v -> Best!v, 0
semi(v)  inLT79 is implemented in BCPL by v -> Semi!v, 0

Just as in LT79 care is needed to distinquish between the vertex
number as supplied in the given flow graph and its discovery time,
both are in the range 1 to n. Note that semi(v) yields a discovery
time not a vertex number.

*/

GET "libhdr"

MANIFEST {

// Structure of a node

// node -> [id,parent,dom,semi,ancestor,best,succ,pred,bucket,size,child]

Id=0     // DFS discovery time of this vertex.
Parent   // DFS tree parent vertex. For the root node this field is zero.
Dom      // Immediate dominator -- the closest node to this node
         // that is on every path from the root to this node.
         // For the root node this field is zero.
Semi     // The discovery time of the semi dominator vertex.
         // The semi dominator vertex is the vertex with the earliest
         // discovery time that has a path to this node only passing
         // through vertices with later discovery times than this node.
Ancestor // Set by link and used in eval.
Best     // Pointer to the node in the ancestor path having a
         // semidominator with earliest discovery time. 
Succ     // List of immediate successors of this node in the
         // given flow graph.
Pred     // List of immediate predecessors of this node in the
         // given flow graph. This list is created by dfs.
Bucket   // List of vertices whose semi dominators are this vertex.

// The following two fields are only used in the sophisticated version
// of LINK and EVAL. They are only valid in root vertices of trees in
// the forest.
Size     // The total number of nodes in the tree rooted at this node and
         // all those in the child chain.
Child    // Next node in the child chain.

NodeSize // The number of fields in a vertex node.
NodeUpb=NodeSize-1
}

GLOBAL {
vertex:ug       // Vector of vertex nodes in dfs order

root            // Root of the flow graph
nodes           // The number of nodes in the graph
edges           // The number of edges in the graph

newvec; initspace; freespace; mkNode; mk2; freelist
dfs
eval; link
dominators      // Set the dom field of each node to
                // its immediate dominator.
edge            // Add an edge when building a graph
mkdefaultgraph1 // make the default graph as in the paper
mkdefaultgraph2 // make the another simple graph.
mkgraph         // make a random flow graph
prnodes
prstruct
hashgraph
hashtree
prevhash

spacev; spacep; spacet  // Pointers in the free space.
mk2list                 // The free list of mk2 nodes.
debug                   // debug flag
flow                    // flow flag
loadp
clab; blab; glab
stdout
tostream
}

LET newvec(upb) = VALOF
{ LET p = spacep - upb - 1
  IF p<spacev DO
  { writef("Out of space*n")
    abort(999)
    RESULTIS 0
  }
  spacep := p
  RESULTIS p
}

LET initspace(nodes, edges) BE
{ LET upb = nodes*(NodeSize + 2) + edges*5 + 2
  // Allocate a generous amount of space for the given problem.
  spacev := getvec(upb)
  spacet := spacev+upb
  spacep := spacet
  mk2list := 0
  vertex := getvec(nodes)
  UNLESS spacev & vertex DO
  { writef("Not enough space*n")
    abort(999)
    RETURN
  }
  FOR i = 0 TO upb   DO spacev!i := 0
  FOR i = 0 TO nodes DO vertex!i := 0
}

LET freespace() BE
{ IF debug DO
    writef("*nSpace used %n out of %n*n", spacet-spacep, spacet-spacev)
  freevec(spacev)
  freevec(vertex)
}

LET mkNode() = VALOF
{ // Only used when creating test graphs for the dominator function.
  LET p = newvec(NodeUpb)
  UNLESS p DO { writef("Out of space*n"); abort(999) }
  FOR i = 0 TO NodeUpb DO p!i := 0
  nodes := nodes+1
  // These values in vertex will all be be changed when dfs runs. 
  vertex!nodes := p
}

AND mk2(x, y) = VALOF
{ // Create a list element [x,y] using the free list if possible.
  LET p = mk2list
  TEST p THEN mk2list := !p
         ELSE p := newvec(1)
  !p, p!1 := x, y
  RESULTIS p
}

AND freelist(p) BE
{ // Put the elements of list p into the free list.
  LET rest = mk2list
  mk2list := p
  WHILE !p DO p := !p
  !p := rest
}

// dfs is a translation of DFS defined on page 139 of LT79,
// slightly modified to count the number of edges in the
// flow graph.

LET dfs(v) BE  // DFS on previously unseen vertex v
{ // When first entered v is the root and
  // nodes=0 and edges=0.
  // When dfs completes the Semi field of every vertex will hold
  // its discoery time.
  LET p = Succ!v // List of successor vertices
  nodes := nodes+1
  vertex!nodes := v      // vertex holds pointers to vertex nodes
                         // in discovery time order.
  Id!v         := nodes  // This holds the discovery time of this node.
  Semi!v       := nodes  // Initialise semi to the discovery time.
  Best!v       := v      //            best
  Ancestor!v   := 0      // and        ancestor
  Child!v      := 0
  Size!v       := 1

  WHILE p DO         // Apply DFS to all unseen successors.
  { LET w = p!1      // w ia a successor of v
    p := !p
    UNLESS Semi!w DO // Semi!w=0 if w has not yet been visited, so
    { Parent!w := v  // set its Parent field
      dfs(w)         // and apply dfs to it.
    }
    Pred!w := mk2(Pred!w, v) // Add v to list of predecessors of w.
    edges := edges+1         // Count the number of edges so far.
  }
}

AND compress(v) BE
{ // Make all the Ancestor fields in the Ancestor path point to
  // its root, and update every Best field appropriately, ie make
  // each point to the vertex in the Ancestor chain with a
  // semi dominator having the earliest discovery time.
  LET a = Ancestor!v
  UNLESS Ancestor!a RETURN // We can only compress paths of length 2 or more.
  compress(a)
  IF Semi!(Best!a) < Semi!(Best!v) DO Best!v := Best!a
  Ancestor!v := Ancestor!a
}

// The very simple version of eval and link not using
// compression or tree balancing.

AND eval1(v) = VALOF
{ // Return the pointer to the node in the Ancestor chain having
  // semi dominator with the earliest discovery time. The Ancestor
  // chain will eventually be a path from v to the semidominator
  // of v. In the unoptimised version, eval follows the Ancestor
  // chain (which are all DFS parent links) from v to the
  // semidominator. In optimised versions of eval, the length of
  // the Ancestor chain is reduced by bypassing vertices whose
  // semidominators have discovery times that are too large.

  LET a = Ancestor!v
  UNLESS a DO
  { RESULTIS v
  }
  WHILE Ancestor!a DO
  { IF Semi!v > Semi!a DO v := a
    a := Ancestor!a
  }
  RESULTIS v
}
  
AND link1(v, w) BE
{ // v will be the DFS parent of w.
  // This function sets the ancestor link from w to v.

  Ancestor!w := v
}

// The Simple version of eval and link
// using compression but no tree balancing.

AND eval2(v) = VALOF
{ // Only call compress if v has an ancestor.
  UNLESS Ancestor!v RESULTIS v
  compress(v)
  RESULTIS Best!v
}

AND link2(v, w) BE
{ // Same as link1
  Ancestor!w := v
}

// The Sophisticated version of eval and link
// using compression and tree balancing.

AND eval3(v) = VALOF // Based on EVAL in p140 of the LT67 paper.
{ // Note that the field Best is called label in the LT67 paper.
  // This seems to be ok.
  LET a, b = ?, ?
  UNLESS Ancestor!v RESULTIS Best!v
  compress(v)
  a, b := Best!(Ancestor!v), Best!v
  // Return the pointer to the vertex having the earlier discovery time.
  RESULTIS Semi!a < Semi!b -> a, b
}

AND link3(v, w) BE
{ // Note that the field Best is called label in the LT67 paper.
  // This function is a translation of LINK on p140 of
  // the LT67 paper.
  // Note that
  // size(v)  is tranlated as v -> Size!v, 0
  // label(v) is tranlated as v -> Best!v, 0
  // semi(v)  is tranlated as v -> Semi!v, 0
  // When link is called v will be the DFS tree parent of w.
  // Both v and w are root vertices of forests consisting of
  // lists of trees linked by child pointers. If we call
  // semi!(Best!v) the best value of any vertex v, the child
  // chains have the property that the best values of the
  // tree roots are monotonically decreasing. Just before
  // link is called the best value of of w may have been
  // reduced, so the child chain from w may need correction
  // to reinstate its monotonicity.
 
  LET s = w

  { // Vertex w was processed just before this call of link(v, w). 
    // This may have caused Semi!(Best!w) to be reduced.
    // The child chain from w may need correction to reinstate
    // monotonicity.
    LET cs  = Child!s            // cs   = child(s)
    LET bcs = cs -> Best!cs, 0   // bcs  = best(child(s))

    TEST cs & 
         Semi!(Best!w) < Semi!bcs        // bcs=0 only if cs=0
    THEN { // The child of s exists and the best value of this
           // vertex is too large so a correction must be made.
           // This involves combining the first two tree in the
           // child chain making the root of the larger tree the
           // root of the combination. The root of the other tree
           // is given an ancestor link to the new root. The size
           // of the root is corrected, if necessary, and s made
           // to point to this root. This operation reduces the
           // length of the child chain by one. This operation is
           // repeated by the REPEAT loop until either s has no
           // child or the best value of s is greater then or
           // equal to the best value of the child. The whole
           // process keeps the trees in the forest balanced.
           LET ccs  = Child!cs           // ccs  = child(child(s))
           LET ss   = Size!s             // sc   = size(s)
           LET scs  = Size!cs            // scs  = size(child(s)
           LET sccs = ccs -> Size!ccs, 0 // sccs = size(child(child(s))
           // Note that ss       is the size of the forest s.
           //      and  ss-scs   is the size of the first tree.
           //      and  scs-sccs is the size of the second tree.

           TEST ss-scs >= scs-sccs // The first two tree sizes.
           THEN { // Make s the new root.
                  Ancestor!cs := s // The first is larger or equal.
                  Child!s := ccs
                }
           ELSE { // Make child!s the new root.
                  Size!cs := ss    // The second is larger.
                  Ancestor!s := cs
                  s := cs
                }
         }
    ELSE { BREAK
         }
  } REPEAT

  // Correct Best!s if necessary.
  Best!s := Best!w

  // Combine the two forests making v the root and giving 
  // it the child chain of the smaller forest. The child
  // chain of the other forest is collapsed by giving all
  // its vertices anscestor links to v. This mechanism
  // keeps the forest balanced.

  IF Size!v < Size!w DO
  { LET t = s
    s := Child!v
    Child!v := t
  }
  Size!v := Size!v + Size!w

  WHILE s DO
  { Ancestor!s := v
    s := Child!s
  }
}

AND dominators(r) BE
{ 
// Step1:   of the LT69 algorithm

  // Perform a DPS over the flow graph setting the parent
  // links and predecessor lists of all the vertices.
  nodes, edges := 0, 0
  dfs(r)
  //writef("nodes = %n  edges = %n*n", nodes, edges)

  FOR i = nodes TO 2 BY -1 DO
  { // Process the nodes in DFS tree reverse pre order.
    LET w = vertex!i  // w is the node with discovery time i.
    LET p = Pred!w    // p is the list of predecessors of
                      // vertex w in the flow graph.
    LET q = ?

// Step2:   of the LT69 algorithm

    WHILE p DO
    { LET v = p!1      // v is the next predecessor of w.
      LET u = eval(v)  // u is the vertex with the earliest
                       //    semidominatorin the Ancestor
                       //    chain from v.

      IF Semi!u < Semi!w DO
        Semi!w := Semi!u
      p := !p          // Look for next predecessor
    }
    // Add w to the Bucket of Semi(w) ready for step 3
    // Bucket!t will hold a list of all vertices having
    // semidominator t 
    Bucket!(vertex!(Semi!w)) := mk2(Bucket!(vertex!(Semi!w)), w)

    p := Parent!w

    link(p, w)         // Add edge (p,w) to the forest

// Step3:   of the LT69 algorithm

    q := Bucket!p

    WHILE q DO
    { LET v = q!1 // For each v in bucket(parent(w))
      LET u = eval(v)
      Dom!v := Semi!u < Semi!v -> u, p // p is the parent of w
      q := !q
    }

    IF Bucket!p DO { freelist(Bucket!p); Bucket!p := 0 }
  }

// Step4:   of the LT69 algorithm

  FOR i = 2 TO nodes DO  // Do step 4 -- nodes in dfs order
  { LET w = vertex!i
    UNLESS Dom!w = vertex!(Semi!w) DO Dom!w := Dom!(Dom!w)
  }
  // The root has no dominator
  Dom!(vertex!1) := 0
}

AND start() = VALOF
{ LET n, e = 0, 0
  LET format = "NODES/N,EDGES/N,SEED/N,TO/K,D=DEBUG/S,F=FLOW/S"
  LET seed = setseed(12345)  // Get previous seed
  LET argv = VEC 50

  stdout := output()
  tostream := stdout
  
  UNLESS rdargs(format, argv, 50) DO
  { writef("Bad arguments for lt*n")
    RESULTIS 20
  }

  debug := FALSE

  IF argv!0 DO n     := !argv!0    // NODES/N
  IF argv!1 DO e     := !argv!1    // EDGES/N
  IF argv!2 DO seed  := !argv!2    // SEED/N
  IF argv!3 DO
  { tostream := findoutput(argv!3) // TO/K
    UNLESS tostream DO
    { writef("trouble with stream %s*n", argv!3)
      RESULTIS 20
    }
  }
  debug := argv!4                  // DEBUG/S
  flow  := argv!5                  // FLOW/S

  IF e < n-1 DO e := n-1 // Ensure that there are enough edges.

  selectoutput(tostream)

  TEST argv!0
  THEN { // Test the algorithms on a user specified graph.
         check(0)   // Inititalise the previous hash value

         // The very simple version is too slow for graphs with
         // more than about 200000 edges.

         IF e<200_000 DO try(1, n, e, seed, flow)
         try(2, n, e, seed, flow)
         try(3, n, e, seed, flow)
       }
  ELSE { // Test the algorithms on a set of graphs.
         // Check that there is sufficient Cintcode memory and that
         // the stack is large enough.
         newline()
         IF rootnode!rtn_memsize < 40_000_000 DO
         { writef("The Cintcode memory of %n words is too small.*n",
                  rootnode!rtn_memsize)
           writef("Enter the system using: cintsys -m 40000000*n")
           GOTO fin
         }
         IF currco!co_size < 500_000 DO
         { writef("The stack size of %n words is too small.*n",
                   currco!co_size)
           writef("Execute the command: stack 500000*n")
           writef("before calling lt*n")
           GOTO fin
         }

         writef("                                  Instruction*
                * Counts*n")
         writef(" Nodes    Edges  Seed     v.simple    simple*
                *  sophisticated*n*n")
         trygraph(  1000,   1500, 1, FALSE)
         trygraph(  1000,   2000, 1, FALSE)
         trygraph(  1000,   2500, 1, FALSE)
         trygraph(  1000,   3000, 1, FALSE)
         trygraph(  1000,   5000, 1, FALSE)
         trygraph(  1000,  10000, 1, FALSE)
         trygraph( 10000,  50000, 1, FALSE)
         trygraph( 10000, 100000, 1, FALSE)
         trygraph(100000, 400000, 1, FALSE)
         newline()
         trygraph(100000, 123289, 1, TRUE)
         newline()
       }

fin:
  IF tostream & tostream ~= stdout DO
  { endwrite()
    selectoutput(stdout)
  }

  RESULTIS 0
}

AND try(t, n, e, seed, f) BE
{ LET mess = ?

  LET newgraph = n=0 -> mkdefaultgraph1,
                 n=1 -> mkdefaultgraph2,
                 mkgraph

  IF f DO newgraph := mkflow

  setseed(seed)

  SWITCHON t INTO
  { DEFAULT:
    CASE 1: // Very simple version
            eval, link := eval1, link1
            mess :=  "very simple"
            ENDCASE
    CASE 2: // Simple version
            eval, link := eval2, link2
            mess :=  "simple"
            ENDCASE
    CASE 3: // Sophisticated version
            eval, link := eval3, link3
            mess :=  "sophisticated"
            ENDCASE
  }

  newline()

  root := newgraph(n, e)

  writef("Finding dominator tree using the %s version of eval*n",
          mess)

  writef("*nInstruction count = %n*n",
          instrcount(dominators, root))

  writef("Some dominators:")
  { LET count = 0
    LET step = nodes < 20 -> 1, nodes/20
    LET i = step
    WHILE i <= nodes DO
    { LET d = i>1 -> Id!(Dom!(vertex!i)), 0
      IF count MOD 5 = 0 DO newline()
      writef(" %i8:%i8", i, d)
      count := count+1
      i := i+step
    }
  }

  { LET h1, h2 = hashgraph(), hashtree()
    writef("*nGraph hash: %n  Dominator Tree hash: %n*n", h1, h2)
    check(h1+h2) // Check the hash values are the same as before
  }

  freespace()
}

AND tryversion(t, n, e, seed, f) = VALOF
{ LET newgraph = f -> mkflow, mkgraph
  LET count = 0

  setseed(seed)

  SWITCHON t INTO
  { DEFAULT:
    CASE 1: // Very simple version
            eval, link := eval1, link1
            ENDCASE
    CASE 2: // Simple version
            eval, link := eval2, link2
            ENDCASE
    CASE 3: // Sophisticated version
            eval, link := eval3, link3
            ENDCASE
  }

  root := newgraph(n, e)

  count := instrcount(dominators, root)

  { LET h1, h2 = hashgraph(), hashtree()
    check(h1+h2) // Check the hash values are the same as before
  }

  freespace()
  RESULTIS count
}

AND trygraph(n, e, seed, f) BE
{ LET count = ?

  check(0)   // Inititalise the previous hash value

  writef("%i6 %i8    %n %s ", n, e, seed, f->"f ","  ")
  TEST e<200_000
  THEN writef(" %9i", tryversion(1, n, e, seed, f))
  ELSE writef("         -")
  writef(" %9i", tryversion(2, n, e, seed, f))
  writef("      %9i", tryversion(3, n, e, seed, f))
  newline()
}

AND edge(i, j) BE
{ LET vi, vj = vertex!i, vertex!j
  Succ!vi := mk2(Succ!vi, vj)
  edges := edges+1
}

AND mkdefaultgraph1() = VALOF
{ // Make the graph used in the paper.
  initspace(13, 21)
  nodes, edges := 0, 0
  FOR i = 1 TO 13 DO mkNode(i)

  edge( 1, 11); edge( 1,  8); edge( 1,  2)
  edge( 2,  6); edge( 2,  3)
  edge( 3,  4)
  edge( 4,  5)
  edge( 5,  4); edge( 5,  1)
  edge( 6,  4); edge( 6,  7)
  edge( 7,  4)
  edge( 8, 12); edge( 8, 11); edge( 8,  9)
  edge( 9, 10)
  edge(10,  9); edge(10,  5)
  edge(11, 12)
  edge(12, 13)
  edge(13, 10)

  RESULTIS vertex!1
}

AND mkdefaultgraph2() = VALOF
{ // Make the graph used in my talk.
  initspace(25, 38)
  nodes, edges := 0, 0
  FOR i = 1 TO 25 DO mkNode(i)

  edge( 1, 14); edge( 1,  2)
  edge( 2, 21); edge( 2,  3)
  edge( 3, 18); edge( 3, 11); edge( 3,  4)
  edge( 4,  5)
  edge( 5,  9); edge( 5,  6)
  edge( 6,  7)
  edge( 7,  1); edge( 7,  8)
  edge( 8,  6)
  edge( 9, 10); edge( 9,  7)
  edge(10,  7)
  edge(11, 15); edge(11, 14); edge(11, 12)
  edge(12, 13); edge(12,  9)
  edge(14, 15)
  edge(15, 16)
  edge(16, 17)
  edge(17, 15); edge(17,  8)
  edge(18, 24); edge(18, 21); edge(18, 19)
  edge(19, 20); edge(19, 15)
  edge(21, 24); edge(21, 22); edge(21, 20)
  edge(22, 23)
  edge(23, 16)
  edge(24, 25)

  RESULTIS vertex!1
}

AND mktree1(n) = n<=0 -> 0, VALOF
{ // Make a random tree with given number of nodes
  LET v = mkNode() // Make a root node

  IF n>1 DO
  { // k = the number of nodes to make in the left branch.
    //LET k = 0 
    //LET k = 1 
    LET k = n/10 
    //LET k = n/3
    //LET k = n/2
    //LET k = randno(n-1)
    LET c = mktree1(k)     // Make the left branch.
    IF c DO edge(v, c)
    c := mktree1(n-k-1)    // Make the right branch.
    IF c DO edge(v, c)
  }
  RESULTIS v
}

AND mktree2(n) = VALOF
{ // Make a random tree with a given number of nodes.
  vertex!1 := 0
  FOR i = 1 TO n DO
  { mkNode()
    IF i>1 DO edge(randno(i-1), i) // Give node i a random parent.
  }
  RESULTIS vertex!1
}

AND mkgraph(n, e) = VALOF
{ // Make a random flow graph with n vertices and e edges.
  initspace(n, e)

  nodes, edges := 0, 0
  mktree2(n)    // First create a random tree with n vertices.

  // Then add additional random edges until there are e edges.
  UNTIL edges>=e DO
  { LET p = randno(n)
    LET q = p + randno(200) - 100
    UNLESS 1<=q<=n LOOP
    edge(p, q)  // Add edge to nearby vertex.
  }

  RESULTIS vertex!1  // return the root.
}

AND mkflow(n) = VALOF
{ // Make a random flow graph with exactly n nodes.
  initspace(n, 2*n)

  vertex!1 := 0
  // First make the nodes
  nodes, edges := 0, 0
  FOR i = 1 TO n DO mkNode()
  clab, blab, glab := 0, 0, 0 // continue, break and goto labels
  loadp := 1   // Position of next instruction to compile
  trnC(n-1)    // Compile C; return in n-1 nodes
  genj("stop")
  RESULTIS vertex!1  // Return the root
}

AND trnC(n) BE
// Compiler n nodes of code
{ LET m = ?
  LET r = randno(100)

  IF n<=0 DO abort(1000)

  IF n=1 DO
  { IF prob(5) & clab DO
    { genfl("continue", clab)      //     continue  Lclab 
      RETURN
    }
    IF prob(5) & blab DO           //     break     Lblab
    { genfl("break", blab)
      RETURN
    }
    IF prob(5) & glab DO
    { genfl("goto", glab)          //     goto      Lglab 
      RETURN
    }
    genf("com")                    //     com
    RETURN
  }

  IF r<25 & n>5 DO // if E then C
                   // => E; jf l1; C; lab l1
  { LET g = glab
    LET L1 = loadp+n-1
    IF prob(10) DO
       glab := loadp + randno(n) - 1
    genf("exp")                     //     E
    genfl("jf", L1)                 //     jf  L1 
    trnC(n-3)                       //     C
    genf("lab")                     // L1:
    glab := g
    RETURN
  }

  IF r<35 & n>7 DO // if E then C else C
                   // => E; jf l; C; jump m; lab l; C; lab m
  { LET g = glab
    LET m = randno(n-6)  // size of C1  1 <= m <= n-6
    LET L1 = loadp + m + 3
    LET L2 = loadp + n - 1
    IF prob(10) DO
      glab := loadp + randno(n) - 1
    genf("exp")                         //     E
    genfl("jf", L1)                     //     jf  L1
    trnC(m)                             //     C
    genjl("jmp", L2)                    //     jmp L2
    genf("lab")                         // L1:
    trnC(n-m-5)                         //     C
    genf("lab")                         // L2:
    glab := g
    RETURN
  }

  IF r<55 & n>8 DO // while E do C
  { LET c, b, g = clab, blab, glab
    LET L1 = loadp + 1
    clab  := loadp + n - 4
    blab  := loadp + n - 1
    IF prob(10) DO  // possibly change goto target
       glab := loadp + randno(n) - 1
    genjl("jmp", clab)                  //        jmp Lclab
    genf("lab")                         // L1:
    trnC(n-6)                           //        C
    genf("lab")                         // Lclab:
    genf("exp")                         //        E
    genfl("jt", L1)                     //        jt  L1
    genf("lab")                         // Lblab:
    clab, blab, glab := c, b, g
    RETURN
  }


  IF r<75 & 10<n<1000 DO // switch E { C1;..;Ck }
  { LET s = loadp    // Position of switch instruction
    LET elab = loadp + n - 1
    genj("switch")
    n := n-1

    UNTIL n<=1 DO
    { LET m = randno(100) + 4   // Choose a case size
      IF m+5>n DO m := n-1      // make this the last case
      edge(s, loadp)            // Compile next case from p to m
      genf("caselab")
      trnC(m-2)
      genjl("endcase", elab)
      n := n-m
    }
    edge(s, loadp)
    genf("endswitch")
    RETURN
  }

  // Otherwise compile:  C1; C2
  m := randno(n-1) // size of C1
  trnC(m)                         //      C1
  trnC(n-m)                       //      C2
}  

AND genf(str) BE
{ edge(loadp, loadp+1)
  loadp := loadp+1
}

AND genfl(str, lab) BE
{ edge(loadp, lab)
  edge(loadp, loadp+1)
  loadp := loadp+1
}

AND genj(str) BE
{ loadp := loadp+1
}

AND genjl(str, lab) BE
{ edge(loadp, lab)
  loadp := loadp+1
}

AND prob(percent) = randno(100)<=percent

AND prnodes(n) BE
{ newline()
  FOR i = 1 TO n DO
  { LET p = vertex!i
    LET q = Succ!p
    writef("%z2:",                 Id!p)
    writef(" P:%z2", Parent!p   -> Id!(Parent!p),   0)
    writef(" D:%z2", Dom!p      -> Id!(Dom!p),      0)
    writef(" S:%z2",               Semi!p)
    writef(" A:%z2", Ancestor!p -> Id!(Ancestor!p), 0)
    writef(" L:%z2", Best!p     -> Id!(Best!p),     0)
    UNLESS Ancestor!p DO
    { // These are only valid for roots of forest trees
      writef(" C:%z2", Child!p    -> Id!(Child!p),    0)
      writef(" Z:%z2", Size!p)
    }
    writef(" -> ")
    WHILE q DO { writef(" %z2", Id!(q!1)); q := !q }
    q := Bucket!p
    IF q DO
    { writef("*nBucket: ")
      WHILE q DO { writef(" %z2", Id!(q!1)); q := !q }
    }
    UNLESS Ancestor!p DO
    { // These are only valid for roots of forest trees
      q := Child!p
//writef("Child pointer of %z2 q=%n*n", Id!p, q)
//abort(1233)
      IF q DO
      { writef("*nClild: ")
        WHILE q DO
        { writef(" %z2", Id!(q!1))
          q := Child!q
          deplete(cos)
//writef("Next child pointer q=%n*n", q)
//        abort(1234)
        }
      }
    }
    newline()
  }
}

AND prres(n) BE
{ newline()
  FOR i = 1 TO n DO
  { LET p = vertex!i
    LET q = Succ!p
    writef("%z2:", Id!p)
    writef(" P:%z2", Parent!p   -> Id!(Parent!p),   0)
    writef(" D:%z2", Dom!p      -> Id!(Dom!p),      0)
    writef(" S:%z2",               Semi!p)
    writef(" ->")
    WHILE q DO { writef(" %z2", Id!(q!1)); q := !q }
    newline()
  }
}

AND prstruct() BE
{ writef("*nN: ")
  FOR i = 1 TO nodes DO writef(" %i2", i)
  writef("*nS: ")
  FOR i = 1 TO nodes DO { LET s = Semi!(vertex!i)
                          writef(" %i2", s)
                        }
  writef("*nB: ")
  FOR i = 1 TO nodes DO { LET v = Best!(vertex!i)
                          writef(" %i2", v -> Id!v, 0)
                        }
  writef("*nZ: ")
  FOR i = 1 TO nodes DO { LET v = vertex!i
                          writef(" %i2", Size!v)
                        }
  writef("*nA: ")
  FOR i = 1 TO nodes DO { LET v = Ancestor!(vertex!i)
                          writef(" %i2", v -> Id!v, 0)
                        }
  writef("*nC: ")
  FOR i = 1 TO nodes DO { LET v = Child!(vertex!i)
                          writef(" %i2", v -> Id!v, 0)
                        }
  newline()
}

AND prforest() BE
{ FOR i = 1 TO nodes DO
  { LET v = vertex!i
    IF Size!v>=1 & Ancestor!v=0 DO
    { IF Child!v & Id!(Child!v)<i DO abort(9999)
      prvtree(v)
      writes("*n")
    } 
  }
  FOR i = 1 TO nodes DO
  { LET v = vertex!i
    Size!v := ABS(Size!v) // Restore original size
  }
}

AND prvtree(t) BE
{ prstree(t)        // Print subtree
  Size!t := -Size!t // Mark this subtree as printed
  t := Child!t
  UNLESS t RETURN
  writes("=>")
} REPEAT

AND prstree(t) BE
{ LET id = Id!t
  LET c = Child!t
  LET first = TRUE
  writef("%i2:A%z2S%z2B%z2",
         id, Ancestor!t->Id!(Ancestor!t),0, Semi!t, Id!(Best!t))
  UNLESS Ancestor!t DO
    writef("Z%nC%z2", Size!t, c->Id!c,0)

  FOR i = 1 TO nodes DO // Find any sub branches
  { LET vi = vertex!i
    IF t=Ancestor!vi DO
    { wrch(first -> '(', ' ')
      first := FALSE
      prstree(vi)
    }
  }
  UNLESS first DO wrch(')')
}

AND hashgraph() = VALOF
{ LET res = 34567
  FOR i = 1 TO nodes DO
  { LET p = vertex!i
    LET q = Succ!p
    res := 31*res + Id!p
    WHILE q DO { res := res*13 + Id!(q!1); q := !q }
  }
  RESULTIS (res>>1) REM 1000000
}

AND hashtree() = VALOF
{ LET res = 34567
  FOR i = 2 TO nodes DO
  { LET p = vertex!i
    res := 31*res + Id!p
    res := 13*res + Id!(Dom!p)
  }
  RESULTIS (res>>1) REM 1000000
}

AND check(hash) BE
{ IF prevhash & hash & prevhash~=hash DO
     writef("##### ERROR: Dominator trees are different ####*n")
  prevhash := hash
} 

