-- | Various graph algorithms
module GraphAlgorithms (
       dijkstra,
       prim,
       bidelete_edge_from
       ) where

import Data.Set (Set, member, notMember, insert, empty)
import Data.Map (Map, fromList, toList, (!), updateAt, member, keys, findWithDefault, union)
import Data.List (minimumBy,(\\),delete,intersect)
import Data.Graph
import qualified Data.Array (bounds, (!),(//))

import Utilities (argminBy, comb)

type Node = Int

--type Edge = (Node,Node) Data.Graph defines this.

-- | Find a key associated with the smallest value.
argmin :: (Ord a) => Map k a -> k
argmin map = let mv = minimum [val | (key,val) <- Data.Map.toList map]
             in head [key | (key,val) <- Data.Map.toList map, val == mv]

argmin' :: (Ord v) => Map k v -> k
argmin' map = fst $ minimumBy (\x y -> compare (snd x) (snd y)) (Data.Map.toList map)

deleteKeys :: (Ord k) => Map k a -> Set k -> Map k a 
deleteKeys map keys = Data.Map.fromList [(k,v) | (k,v) <- Data.Map.toList map, k `notMember` keys]

fupdate :: (Num a, Ord a) => Node -> Map Edge a -> Map Node a -> Node -> a
fupdate x eCosts vDists y = let vy = (vDists ! y)
                                c = findWithDefault vy (x,y) eCosts
                            in min vy ((vDists ! x) + c)

-- | Dijkstra's algorithm for finding the cost of the minimum
-- cost path to a given destination in a directed graph.
--
-- <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>
dijkstra :: (Num a, Ord a)
         => Map Edge a  -- ^ costs of traversing each edge
         -> Map Node a  -- ^ current known cost of starting at each node
         -> Set Node    -- ^ set of already visited nodes
         -> Node        -- ^ destination node
         -> a           -- ^ the minimum cost
dijkstra eCosts vDists visited dest
         | dest `elem` visited = vDists ! dest
         | otherwise           = let x = argmin (deleteKeys vDists visited)
                                     vDists' = (Data.Map.fromList [(y,fupdate x eCosts vDists y) | (z,y) <- Data.Map.keys eCosts, z==x]) `Data.Map.union` vDists
                                     visited' = Data.Set.insert x visited
                                 in dijkstra eCosts vDists' visited' dest

inf = 1.0/0.0

-- A linear graph with unit edge costs: 1-->2-->3-->4-->5
--
eCosts1 = Data.Map.fromList $ zip [(1,2),(2,3),(3,4),(4,5),(5,1)] [1,1,1,1,1.0]
vDists1 = Data.Map.fromList $ zip [1,2,3,4,5] (0.0:(take 4 $ repeat inf))
answer1 = map (dijkstra eCosts1 vDists1 Data.Set.empty) [1,2,3,4,5]

-- A circular graph (add 5--1) with edges in both directions
--
eCosts2 = Data.Map.fromList $ zip [(1,2),(2,3),(3,4),(4,5),(5,1),(1,5),(5,4),(4,3),(3,2),(2,1)] [1,1,1,1,1,1,1,1,1,1.0]
vDists2 = Data.Map.fromList $ zip [1,2,3,4,5] (0.0:(take 4 $ repeat inf))
answer2 = map (dijkstra eCosts2 vDists2 Data.Set.empty) [1,2,3,4,5]

-- A circular graph, but edges in one direction are much more costly
--
eCosts3 = Data.Map.fromList $ zip [(1,2),(2,3),(3,4),(4,5),(5,1),(1,5),(5,4),(4,3),(3,2),(2,1)] [1,1,1,1,1,10,10,10,10,10.0]
vDists3 = Data.Map.fromList $ zip [1,2,3,4,5] (0.0:(take 4 $ repeat inf))
answer3 = map (dijkstra eCosts3 vDists3 Data.Set.empty) [1,2,3,4,5]

-- just a reminder:
--
-- type Graph = Array Vertex [Vertex]

connected :: Graph
          -> Bool
connected gr = case scc gr of
                    [tr] -> True
                    _    -> False

delete_edges_from :: Graph   -- ^ the graph from which we intend to delete the graph
                  -> [Edge]  -- ^ the edges we wish to delete
                  -> Graph   -- ^ the resulting graph
delete_edges_from gr edges = buildG (Data.Array.bounds gr) [(a,b) | a <- vertices gr, b <- (Data.Array.!) gr a, (a,b) `notElem` edges]

bidelete_edge_from :: Graph   -- ^ the graph from which we intend to delete the graph
                   -> Edge    -- ^ the edge we wish to delete (both directions)
                   -> Graph   -- ^ the resulting graph
bidelete_edge_from gr (a,b) = delete_edges_from gr [(a,b),(b,a)]

add_edge_to :: Graph
            -> Edge
            -> Graph
add_edge_to gr (a,b) = (Data.Array.//) gr [(a,b:((Data.Array.!) gr a))] -- add b to the list of neighbors of a

biadd_edge_to :: Graph
              -> Edge
              -> Graph
biadd_edge_to gr (a,b) = (add_edge_to (add_edge_to gr (a,b)) (b,a))

-- | Prim's algorithm for finding a minimal cost spanning tree
--
-- <https://en.wikipedia.org/wiki/Prim%27s_algorithm>
--
-- Start with a one-vertex sub-graph and incrementally add
-- the least cost edge that connects to a new vertex. Fails
-- immediately if graph is not connected.
prim :: (Num a, Ord a)
     => Map Edge a   -- ^ the costs of the edges
     -> Graph        -- ^ the connected graph gr
     -> Graph        -- ^ the min cost spanning tree
prim costs gr
     | length (scc gr) /= 1 = error "graph is not connected"
     | otherwise            = prim' costs gr (buildG (lb,ub) []) [lb] (delete lb $ vertices gr)
                              where (lb,ub) = Data.Array.bounds gr

prim' :: (Num a, Ord a)
      => Map Edge a   -- ^ the costs of the edges
      -> Graph        -- ^ the connected graph gr
      -> Graph        -- ^ a sub-graph of gr
      -> [Vertex]     -- ^ the vertices visited so far
      -> [Vertex]     -- ^ unvisited vertices
      -> Graph        -- ^ the min cost spanning tree
prim' edge_costs gr tr visited unvisited
      | unvisited == [] = tr  -- we've reached all of the vertices.
      | otherwise       = prim' edge_costs gr (biadd_edge_to tr cheapest) (v2:visited) (delete v2 unvisited)
      where cheapest@(_,v2) = argminBy (edge_costs !) [(a,b) | a <- visited, b <- Data.List.intersect ((Data.Array.!) gr a) unvisited]

