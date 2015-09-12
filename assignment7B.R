users = read.csv("users.csv")
edges = read.csv("edges.csv")

str(users)
library(igraph)
g = graph.data.frame(edges, directed=FALSE, users) 
plot(g, vertex.size=5, vertex.label=NA)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

# node colour on the basis of gender
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

# node colour on the basis of school
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "B"] = "gray"
V(g)$color[V(g)$school == "AB"] = "blue"
plot(g, vertex.label=NA)

# node colour on the basis of locale
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
