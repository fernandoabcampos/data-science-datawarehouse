library(XML)
library("methods")

t <- xmlParse(file = "PRA2-Data/equipaments.xml")
rootnode <- xmlRoot(result)
rootsize <- xmlSize(rootnode)
print(rootsize)

xmlAncestors(rootnode)
xmlGetAttr(rootnode, "/RDF")#/VCard/fn")



elig_text <- findbyxpath("PRA2-Data/equipaments.xml", elig_path)


elig_path <- "//RDF" 
xmldoc <- xmlParse("PRA2-Data/equipaments.xml")
root <- xmlRoot(xmldoc)
removed <- removeNodes(list(root[[2]]))
root

children <- xmlChildren(root)

xmlName(root)
names(children)

length(children)
ns <- getNodeSet(root, '//RDF/VCard')
getNodeLocation(children)

summary(children)

categories <- getNodeSet(root,"//category")

summary(categories)

vCard = getNodeSet(root, "/RDF/VCard")
vCard
summary(vCard)
xmlAttrs(root)
summary(root)
docName(xmldoc)
class(xmldoc)
summary(xmldoc)
removeChildren(root, "type", "Description", "adr", "country-name", "locality", "nickname", "region", "street-address", "tel", "geo", "latitude", "longitude", "email")


doc = xmlParse("<top><a/><b/><c><d/><e>bob</e></c></top>")
top = xmlRoot(doc)
top
removeNodes(list(top[[1]], top[[3]]))
# a and c have disappeared.
top




xml
result <- xpathSApply(xmldoc, elig_path, xmlValue)











elig_text <- xpathSApply(t, elig_path, xmlValue)

findbyxpath <- function(xmlfile, xpath) {
  xmldoc <- xmlParse(xmlfile)
  result <- xpathSApply(xmldoc, xpath, xmlValue)
  if(length(result) == 0) { # check for empty list, returned if node not found
    return("")
  } else {
    return(result)
  }
}



xmlName() name of the node
xmlSize() number of subnodes
xmlAttrs() named character vector of all attributes
xmlGetAttr() value of a single attribute
xmlValue() contents of a leaf node
xmlParent() name of parent node
xmlAncestors() name of ancestor nodes
getSibling() siblings to the right or to the left
xmlNamespace() 





xmldataframe <- xmlToDataFrame("PRA2-Data/equipaments.xml")



doc = xmlTreeParse("PRA2-Data/equipaments.xml")
summary(doc$doc$children$RDF)
root <- xmlRoot(doc)
xmlSApply(root, xmlValue)
xmlSApply(root, "//VCard", xmlValue)
xmlSApply(root, xmlSize)

xmlName(root)

enter(root)
enter <- function(x) {
  for (node in x){
    print(paste("The year is", node))
  }
  while(xmlName(x) != "category") {
    print(xmlSApply(x, xmlName))
    enter(xmlChildren(x))
  }

}







