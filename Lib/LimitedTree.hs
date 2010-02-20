module Lib.LimitedTree (MGraph, graph, load , limit, mkMGraph, update) where

import Data.Array (array,assocs,bounds,Array)
import Data.Graph (buildG, edges, Vertex, Graph)
import Data.List (intersect, delete)


-- | A graph with an upper bound to the number of nodes
data MGraph a = MGraph 
	{limit :: Int			-- ^ the maximum index of attach
	,graph :: Graph 		-- ^ the graph of nodes
	,queue :: [Vertex] 		
	,load :: Array Vertex a		-- ^ an array holding a mapping from node names to values
	} deriving Show


leaves :: Graph -> [Vertex]
leaves = map fst . filter (null . snd) . assocs

-- | build a graph with one only node
mkMGraph 	:: Int 		-- ^ limiting bound
		-> a 		-- ^ value of root node 0
		-> MGraph a	-- ^ the graph
mkMGraph l zero = MGraph l (buildG (0,0) []) [] $ array (0,0) [(0,zero)]

-- | update a graph, attaching  a new value to a given node
update 	:: MGraph a 		-- ^ the graph to update
	-> (Vertex,a) 		-- ^ the node to which attach the new one containing the value
	-> Maybe (MGraph a, Vertex)	-- ^ resulting graph and the name of the new node, Nothing is graph full
update (MGraph l g vs as) (v,x) = if b >= l && null ns  then Nothing else Just (u , n) 
	where
		lvs = leaves g
		ns = vs `intersect` delete v lvs
		(n,nb) = if  b < l then (b + 1,n) else (head ns,l)
	 	(0,b) = bounds g
		u = MGraph l (buildG (0,nb) $ (v,n): el snd (edges g)) 
				(delete n vs ++ [n]) (array (0,nb) $ (n,x) : el fst (assocs as))
		el s = filter ((/=) n . s)

