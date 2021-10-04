## Loop zum extrahieren von Links

webseitenliste <- xml_text(xml_children(read_xml("https://www.uster.ch/sitemap.xml")))
webseite_unter <- NULL

for(iii in 1:length(webseitenliste)) {
  webseite_unter <- c(webseite_unter,sitemap_link(webseitenliste[iii]))
}


## Loop zum analysieren der Webseite 

webseitenliste <- xml_text(xml_children(read_xml("https://www.uster.ch/sitemap.xml")))

werteliste <- NULL
webnamenliste <- NULL

for(ii in 1:length(webseitenliste)) {
  werteliste <- c(werteliste,lixwert(webseitenliste[ii]))
  webnamenliste <- c(webnamenliste,webseitenliste[ii])
}


