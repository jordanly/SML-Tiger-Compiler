signature FUNCGRAPH=
sig
type nodeID 
type 'a node
type 'a edge = {from: nodeID, to: nodeID}
type 'a graph

exception NoSuchNode of nodeID
exception NoSuchEdge of nodeID * nodeID

val empty: 'a graph

(* add a node*)
val addNode: 'a graph * nodeID * 'a -> 'a graph
(* add a node, and return it immediately w/ the new graph*)
val addNode': 'a graph * nodeID * 'a -> 'a graph * 'a node

(* remove a node (and all of its edges). 
 *  raises NoSuchNode(nodeId) if not present 
 *)
val removeNode: 'a graph * nodeID -> 'a graph
(* remove a node (and all of its edges).
 *  Returns graph unchanged if node is not present
 *)
val removeNode': 'a graph * nodeID -> 'a graph

val remove : 'a graph * 'a node -> 'a graph

(* get a particular node, raises NoSuchNode if not found*)
val getNode: 'a graph * nodeID -> 'a node

(* pull the info out of the node *)
val nodeInfo: 'a node -> 'a

(* Add an edge.  Raises
 *  NoSuchNode if either node is not in the graph
 *)
val addEdge: 'a graph * 'a edge -> 'a graph

(*
 * Add a doubly linked edge to the graph
 *)
val doubleEdge : 'a graph * nodeID * nodeID -> 'a graph

(* remove an edge.  raises NoSuchNode if the specified
 * nodes do not exists, or NoSuchEdge if the edge does not exist 
 *)
val removeEdge: 'a graph * 'a edge -> 'a graph

(* remove an edge. If the edge does not
 * exist, ignores it an returns the graph unchanged.
 * If the nodes involved do not exists, throws NoSuchNode 
 *)
val removeEdge': 'a graph * 'a edge -> 'a graph


(* update the data associated with a node (keeping the edges the same)
 * raises NoSuchNode if the node does not exist
 *)

val changeNodeData: 'a graph * nodeID * 'a -> 'a graph


(* accessors*)

(* get all the nodes---these dont come in any particularly
 * useful graph order
 *)
val nodes: 'a graph -> 'a node list

(* get all of the successors of the nodes *)
val succs:  'a node -> nodeID list
val succs': 'a graph -> 'a node -> 'a node list
(* get all of the predecessors of the nodes *)
val preds:   'a node -> nodeID list
val preds': 'a graph -> 'a node -> 'a node list
(* get all of the adjacent nodes of the nodes *)
val adj:  'a node -> nodeID list
val adj': 'a graph -> 'a node -> 'a node list

(* Convert a node to a node id*)
val getNodeID: 'a node -> nodeID

(* how many successors? *)
val outDegree: 'a node -> int

(* how many predecessors? *)
val inDegree: 'a node -> int

(* How many edges *)
val degree: 'a node -> int

(* fold functions functions *)
val foldNodes: (('a node * 'b) -> 'b) -> 'b -> 'a graph -> 'b
val foldSuccs: ((nodeID * 'b) -> 'b) -> 'b -> 'a node -> 'b
val foldSuccs':'a graph -> (('a node * 'b) -> 'b) -> 'b -> 'a node -> 'b
val foldPreds: ((nodeID * 'b) -> 'b) -> 'b -> 'a node -> 'b
val foldPreds':'a graph -> (('a node * 'b) -> 'b) -> 'b -> 'a node -> 'b

val isAdjacent: 'a node * 'a node -> bool

(* debugging*)

(* print the graph.  Give it a function
 * that says how to convert any given node's data into a
 * string, and it will print everything out 
 *)
val printGraph:  ((nodeID * 'a) -> string) -> 'a graph  -> unit

end
