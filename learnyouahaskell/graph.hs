
type Vertex a = (a,[a])
data Graph a = Graph [Vertex a]


createGraph ::Eq a => [(a,a)] -> Graph a
createGraph = undefined

empty :: Graph a
empty = Graph []

insertVertex :: Eq a => a -> Graph a -> Graph a
insertVertex v g = Vertex v:g

insertEdge :: Eq a => (a,a) -> Graph a -> Graph a
insertEdge = undefined -- insert edge in list of origin
