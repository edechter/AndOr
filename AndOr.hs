{-# Language GADTs #-}

module AndOr where

import Prelude
import Control.Monad 

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict  as HashMap

import qualified Data.GraphViz as GraphViz
import qualified Data.Graph.Inductive.Graph as FGL
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import qualified Data.GraphViz.Attributes as Attributes

-- | We implement a graph as an adjacency list. 
type VertexMap a = HashMap a
type Vertex  = Int
type AdjacencyList a w =  VertexMap a (VertexMap a w)
data AONode a = OrNode {label :: a} | AndNode {label :: a} deriving (Show, Eq)
data Graph a w where  
    Graph :: Hashable a => {adjList :: AdjacencyList a w, nodeMap :: VertexMap a a} -> Graph a w 
type AOGraph a w = Graph (AONode a) w

instance (Show a, Show w) => Show (Graph a w) where
    show (Graph g m) = "Graph {gr = " ++ show g ++ ", nodeMap = " ++ show m ++ "}"

empty :: Hashable a => Graph a w
empty = Graph HashMap.empty HashMap.empty

getLinksToNode :: (Eq w, Eq a) => Graph a w -> a -> [(w, a)]
getLinksToNode Graph{adjList=al} v = HashMap.foldrWithKey f [] al
    where f v' innermap vs | v `HashMap.member` innermap = (innermap ! v, v'):vs
                           | otherwise = []

getLinksFromNode :: Eq a => Graph a w -> a -> [(w, a)]
getLinksFromNode Graph{adjList=al} n = case n `HashMap.lookup` al of
    (Just m) ->  map (\(x, y) -> (y, x)) $ HashMap.toList m
    Nothing  -> []

deleteNode ao@Graph{adjList=al, nodeMap=m} n 
    = let al' = HashMap.delete n al
          al'' = HashMap.filter (\m -> n `HashMap.member` m) al'
          m'    = HashMap.delete n m 
      in ao{adjList=al'', nodeMap=m'}

addNode :: (Eq a, Show a) => Graph a w -> a -> Graph a w
-- | Adds a node with a label to the graph. 
-- | If vertex already exists, throws an error. 
addNode ao@Graph{nodeMap=m, adjList=al} node
    = let m' = HashMap.insertWith (error $ "Graph nodeMap already contains node " ++ show node) node node m 
          al' = HashMap.insertWith (error $ "Graph adjList already contains source vertex " ++ show node) node HashMap.empty al
      in ao{nodeMap=m', adjList=al'}

addNodes :: (Eq a, Show a) => Graph a w -> [a] -> Graph a w
addNodes g nodes = foldl addNode g nodes

addEdge :: (Eq a, Show a) => Graph a w -> (a, w, a) -> Graph a w
addEdge g@Graph{adjList = al} (v1, w, v2)
    = let al' = insertAL al v1 v2 w
      in g{adjList=al'}

addEdges :: (Eq a, Show a) => Graph a w -> [(a, w, a)] -> Graph a w
addEdges g es = foldl addEdge g es


addNodeEdge
  :: (Eq a, Show a) => Graph a w 
                    -> (a, w, a) 
                    -> Graph a w
addNodeEdge ao (n1, w, n2) = let ao' = addNodes ao [n1, n2]
                         in addEdge ao' (n1, w, n2)

--addNodeEdges :: Graph a w -> [(a,  w, a)] -> Graph a w
addNodeEdges ao edges = foldl (\g e -> addNodeEdge g e) ao edges
------------------------------
-- Adjacency list functions --
------------------------------

--lookupAL :: AdjacencyList a w -> Vertex -> Vertex -> Maybe w
lookupAL al v1 v2 = do m <- v1 `HashMap.lookup` al
                       v2 `HashMap.lookup` m

--memberAL :: AdjacencyList a w -> Vertex -> Vertex -> Bool
memberAL al v1 v2 = case lookupAL al v1 v2 of 
                        (Just _) -> True
                        otherwise -> False

--insertAL :: AdjacencyList a w -> Vertex -> Vertex -> w -> AdjacencyList a w
insertAL al v1 v2 w = HashMap.insertWith f v1 (HashMap.insert v2 w HashMap.empty) al
    where f w1 w2 = w1 `HashMap.union` w2

---------------------------------------------------
-- Helper functions for Generating And/OR Graphs --
---------------------------------------------------

--addOr :: AOGraph a w -> a -> [(w, a)] -> AOGraph a w 
addOr ao a es = addNodeEdges ao [(OrNode a, w, AndNode b) | (w, b) <- es]

------------------------------
-- Pretty Print --------------
------------------------------

defaultVis = GraphViz.graphToDot GraphViz.nonClusteredParams
graphToDot params g = GraphViz.graphToDot params g

defaultParams = GraphViz.nonClusteredParams { GraphViz.fmtNode = fNode 
                                            , GraphViz.fmtEdge = fEdge
                                            } 
        where fNode (v, (AndNode l)) = [GraphViz.toLabel l, GraphViz.shape GraphViz.BoxShape]
              fNode (v, (OrNode l)) = [GraphViz.toLabel l, GraphViz.shape GraphViz.Ellipse]
              fEdge (_, _, l) = [GraphViz.toLabel l]

--quickToPDF:: (GraphViz.Labellable a, GraphViz.Labellable w) => Graph (AONode a ) w -> FilePath -> IO FilePath
quickToPDF g path = GraphViz.runGraphviz (graphToDot defaultParams g) GraphViz.Pdf path


------------------------------

--hashNode :: (H.Hashable a) => a -> Vertex
--hashNode a = hash a

--getMContext :: Graph a w -> Vertex -> Maybe ([(w, Vertex)], Vertex, a, [(w, Vertex)])
--getMContext ao v = let linksTo = getLinksToNode ao v
--                       linksFrom = getLinksFromNode ao v
--                   in do label <- getNode ao v
--                         return $ (linksTo, v, label, linksFrom)

--decomp :: Graph a w -> Vertex -> (Maybe ([(w, Vertex)], Vertex, a, [(w, Vertex)]), Graph a w)
--decomp ao v = let mcontext = getMContext ao v
--                  rest = deleteVertex ao v
--              in (mcontext, rest)

--mkGraph :: Graph a w -> [(Vertex, a)] -> [(Vertex, Vertex, w)] -> Graph a w 
--mkGraph g vs es = let g' = addVertexs g vs
--                      g'' = addEdges g' es
--                  in g''

---- Instances
--instance FGL.Graph Graph where
--    empty = empty
--    isEmpty ao@Graph{nodeMap = m} =  HashMap.null m
--    match v g = decomp g v 
--    mkGraph vs es = mkGraph empty vs es 
--    labNodes = HashMap.toList . nodeMap 

--instance Attributes.Labellable a => Attributes.Labellable (AONode a) where
--    toLabelValue (AndNode x) = Attributes.toLabelValue x
--    toLabelValue (OrNode x)  = Attributes.toLabelValue x

--------------------------------
---- Testing -------------------
--------------------------------

--test = let g = empty
--           (v1, g')  = addNode g (OrNode "a")
--           (v2, g'') = addNode g' (OrNode "b")
--           g'''      = addEdge g'' (v1, v2, 4)
--       in g'''





--import qualified Data.Array as Array
--import Data.Array ((//), (!))

--type NodeMap a b = HashMap (AONode a b)
--type Graph = HashMap IntSet 
--type EdgeMap w = HashMap (HashMap w)
--data AONode a b = OrNode a | AndNode b deriving Show
--data AOEdge a b w = AOEdge {source :: Vertex, target :: Vertex, weight :: w}
--data Graph a b w = Graph {gr :: Graph, nodeMap :: NodeMap a b, edgeMap :: EdgeMap w} deriving Show

--bounds :: Graph a b w -> Bounds 
--bounds = Array.bounds . gr

--empty :: Graph a b w
--empty = Graph g ns es where g = Graph.buildG (0,0) []
--                              ns = HashMap.empty
--                              es = HashMap.empty



--edgeWeight :: Graph a b w -> (Vertex, Vertex) -> Maybe w
--edgeWeight (Graph _ _ edgeMap) (v1, v2) 
--    = do m <- v1 `HashMap.lookup` edgeMap
--         v2 `HashMap.lookup` m

--addEdge :: Graph a b w -> AOEdge a b w -> Graph a b w
--addEdge ao@(Graph g nm em) e@(AOEdge src trg w) = ao{gr=g', edgeMap=em'}
--    where g' = g // [(src, trg: (g ! src))]
--          em' = HashMap.adjust (\m -> HashMap.insert trg w m) src em 

--addNode :: Graph a b w -> AONode a b w -> (Vertex, Graph)
--addNode ao@{Graph g nm _} node = (v, ao{g=g', nm=nm'})
--    where v = HashMap.findMax nm + 1
--          nm' = HashMap.insert v node nm 
--          g' =  HashMap 




--addNode :: Graph a b w -> AONode a b -> [AOEdge w] -> Graph a b w
--addNode ao n es = let g = gr ao
--                      nm = nodeMap ao
--                      em = edgeMap ao
--                      (minv, maxv) = bounds ao
--                      idx = max + 1
--                  in g // [(idx, trg)]

--or ao@Graph{gr=g, nodeMap=nm, edgeMap = em} label children = Graph $ gr' nodeMap' edgeMap'
--    where gr' = gr // 

--Or "b" [(0.4, And "1" []), (0.6, And "0" [])]







--type Or orNodeType weightType = Gr orNodeType weightType
--data OrNode labelType valType = OrLabel labelType | OrVal valType deriving Show

--a = (1, OrLabel "a")
--g = mkGraph [a] [] :: Or (OrNode String String) ()

--data AONode n m = OrNode (ONode n) | AndNode (ANode m)
--data AOEdge v = OrEdge (OEdge v) | AndEdge AEdge

--data ANode m = ANode {val :: m} 
--data AEdge = AEdge
--data ONode n = ONode {label :: n}
--data OEdge v = OEdge {weight :: v}

--newtype AndOr n m v  = AndOr (Gr (AONode n m) (AOEdge v) )

--instance Graph (AndOr n)  where 
--    empty = AndOr empty
--    isEmpty (AndOr g) = isEmpty g
--    match n (AndOr g) = let (mcontext, g') = match n g in (mcontext, AndOr g')



--genLNodes :: Enum a => a -> Int -> [LNode a]
--genLNodes q i = take i (zip [1..] [q..])

--noEdges :: [LEdge v]
--noEdges = []

--a = (1, "a")
--g = mkGraph [a] noEdges :: AndOr n m v



--data Or n m v = Or {orLabel :: n, orForest :: [(v, And n m v)]} deriving Show
--data And m a = And {andLabel :: m, andForest :: [a]} deriving Show

----data Or n m v = Or {orLabel :: n, orForest :: [(v, And n m v)]} deriving Show
----data And m a = And {andLabel :: m, andForest :: [a]} deriving Show

--type AndOr = Or
--type Name = String

--foldAndOr :: (n -> And m v -> k)
--          -> Or n m v 
--          -> k
--foldAndOr f (Or name xs) 
--    = let (vs, ands) = unzip xs
--          ors = map andForest ands
--          labels = map andLabel ands
--          from_below = map (map (foldAndOr f)) ors
--      in f name $ map (\m v vs -> And m v vs) labels vs from_below


--condition :: (n -> m -> Bool) -> Or n m v -> Or n m v
--condition f (Or n ors) = (Or n ors')
--    where ors' = filter ( \(_, (And m _)) -> f n m ) ors 

--conditionOn :: (Eq n, Eq m) => n -> m -> Or n m v -> Or n m v
--conditionOn n m = condition f
--    where f i j | i == n && m == j = True
--          f _ _ = False 

----attachOr :: Or n m v -> n -> m -> Or n m v
----attachOr name val = 

--sumProduct :: Num v => Or n m v -> v
--sumProduct ao = foldAndOr prod ao 
--  where prod n (v:vs) (ys:yss) = v * (product ys) + prod n vs yss
--        prod n [] [] = 0

--binaryAO :: n -> (v, m) -> (v, m) ->  Or n m v
--binaryAO name (weight1, val1) (weight2, val2)  
--    = Or name [(weight1, And val1 []), (weight2, And val2 [])]

--binaryCoinAO :: (Ord v, Num v) => n -> v -> Or n Bool v
--binaryCoinAO name prob | prob <= 1 = binaryAO name (prob, True) (1-prob, False) 
--                       | otherwise = error "Probability of coin must be less than or equal to 1."


--t = Or "b" [(0.4, And "1" []), (0.6, And "0" [])]

-- utilities
norm xs = map (/ (sum xs)) xs

--vars :: AndOr a -> [Name]
---- | Return the variables appearing in the Or tree. 
--vars ao = case ao of 
--  (AndTree tr) -> varsAnd tr
--  (OrTree  tr) -> varsOr  tr
--  where varsAnd (And ors) = concat . map $ varsOr ors
--        varsOr  (Or n xs) = let go names ys =  

