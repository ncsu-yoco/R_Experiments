#facebook
appID <- 822638467823452
appSecret <- "70bad976092c6112a6e44a4530afd7ce"
tagHistoryToken <- "CAACEdEose0cBAHE5wpoWahsrNo1CZAp5uDBU2nPB9RgzDsBvCBZAjgw6viExE8qBozl1s017U4CRPm3OqwNf87Xp9eANITzUYkddoDz0ZA0nFUIzfBtTioXMC3P7TZA6FXZBC1ZADd7QqFsyscxBce5PiCiz8T9IRvS8ZAUALBjrVBnSLbE1LxXAWRgjZCVUUJC6oLAkJfpygZCFAGqkcm09X1LfOZCLCtMoEZD"

fb_conn <- fbOAuth(appID, appSecret, extended_permissions = TRUE)
user <- getUsers("me", token=fb_conn, private_info = TRUE)

save(fb_conn, file="fb_conn")
load("fb_conn")

e <- getUsers("me", token=fb_conn)
my_friends <- getFriends(tagHistoryToken, simplify=TRUE)
my_friends_info <- getUsers(my_friends$id, token=fb_conn, private_info=TRUE)
my_network <- getNetwork(tagHistoryToken, format="adj.matrix")
singletons <- rowSums(my_network)==0 # friends who are friends with me alone

require(igraph)
my_graph <- graph.adjacency(my_network[!singletons,!singletons])
layout <- layout.drl(my_graph,options=list(simmer.attraction=0))
plot(my_graph, vertex.size=2, 
     #vertex.label=NA, 
     vertex.label.cex=0.5,
     edge.arrow.size=0, edge.curved=TRUE,layout=layout)

#news feed classifier
#wordcloud
install.packages("tm.plugin.webmining")
library("tm.plugin.webmining")

install.packages("XML")
library(XML)

g = WebCorpus(GoogleNewsSource(params = list( hl = 'en', ie = 'utf-8', num = 2, output = 'rss' )))
g[[1]]
query = "a"
y = WebCorpus(YahooNewsSource(query, params = list( p = query, ei = 'utf-8', n = 2 )))
y[[1]]

xml.url <- "http://www.w3schools.com/xml/plant_catalog.xml"
# Use the xmlTreePares-function to parse xml file directly from the web
xmlfile <- xmlTreeParse(xml.url)
# the xml file is now saved as an object you can easily work with in R:
class(xmlfile)
# Use the xmlRoot-function to access the top node
xmltop = xmlRoot(xmlfile)
# have a look at the XML-code of the first subnodes:
print(xmltop)[1:2]
