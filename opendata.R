install.packages("httr")

library(httr)

covid <- GET('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19InfStateJson?ServiceKey=Ez3Vsw48HZBk6YOcJt3qlv4%2F2CsGfo7RGUn87k1HFZLB6baKOOb0cGtW0p%2FhtmjvMfcGuQdPe9kmSDMhuQTAjg%3D%3D&pageNo=1&numOfRows=10&startCreateDt=20200310&endCreateDt=20200315')

install.packages('XML')
library(XML)

doc <- xmlTreeParse('http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19InfStateJson?ServiceKey=Ez3Vsw48HZBk6YOcJt3qlv4%2F2CsGfo7RGUn87k1HFZLB6baKOOb0cGtW0p%2FhtmjvMfcGuQdPe9kmSDMhuQTAjg%3D%3D&pageNo=1&numOfRows=487&startCreateDt=20200201', useInternalNodes = T, encoding = 'UTF-8')

rootNode <- xmlRoot(doc)
numOfrows <- as.numeric(xpathSApply(rootNode, '//numOfROws', xmlValue))

xmlToDataFrame(nodes = getNodeSet(rootNode), '//item')
