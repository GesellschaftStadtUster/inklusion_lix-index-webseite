#Bibliothek einlesen
library(jsonlite)
library(rvest)
library(xml2)
library(stringr)
library(digest)

## Funktion Links extrahieren

sitemap_link <- function(site_link, sucher = ".icms-link-int"){
webseite_link <- xml2::read_html(site_link)
link <- html_nodes(x = webseite_link, css = sucher)
link <- html_attr(x = link, "href")
link
}

## sucher herausgefunden
# für alle Links: .icms-container-callout a 
# für unterseiten: .icms-link-int
