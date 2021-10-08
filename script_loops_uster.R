## Bibliotheken
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

## Loop zum extrahieren von Links

webseitenliste <- xml_text(xml_children(read_xml("https://www.uster.ch/sitemap.xml")))
webseite_unter <- NULL

for(iii in 1:length(webseitenliste)) {
  webseite_unter <- c(webseite_unter,sitemap_link(webseitenliste[iii]))
}

webseite_unter_df <- data.frame(webseite_unter)

## Loop zum Verfolständigen des Links sowie Anpassungen

`%likeic%` <- function (x, pattern) { 
  grepl(pattern, x, ignore.case=TRUE)
}

for(iiiii in 1:dim(webseite_unter_df)[1]) {

  if(webseite_unter_df[iiiii,1] %likeic% "uster.ch" == FALSE) {
    webseite_unter_df[iiiii,1] <- str_replace(webseite_unter_df[iiiii,1], "/_rtr", "https://www.uster.ch/_rtr")
    webseite_unter_df[iiiii,1] <- str_replace(webseite_unter_df[iiiii,1], "/_rte", "https://www.uster.ch/_rte")
    webseite_unter_df[iiiii,1] <- str_replace(webseite_unter_df[iiiii,1], "/kontakt", "https://www.uster.ch/kontakt")
    
  }
}

webseite_unter_df[,1] <- str_replace_all(webseite_unter_df[,1], "http:", "https:")



## Loop zum berechnen von LIX, Hash und Zusatzinfo ==============

## Erstellen Abfrageliste
alle_seiten <- data.frame(webseitenliste)
colnames(webseite_unter_df) <- "webseitenliste"
alle_seiten <- rbind(alle_seiten,webseite_unter_df)
alle_seiten[,1] <- str_replace_all(alle_seiten[,1], ",", "")

## Variablen erstellen
webseite_hash_liste <- NULL
lix_liste <- NULL
zusatz_liste <- NULL

## LOOOPPPPPPPPP ===========================
for(iiii in 1:dim(alle_seiten)[1]) {
  ganze_seite <- xml2::read_html(alle_seiten[iiii,1])
  
  # LIX 
  ## Webseite auslesen
  seite <- html_nodes(x = ganze_seite, css = ".icms-wysiwyg")
  seite <- html_text(seite)
  
  ## Tabellen und Aufzählungen entfernen
  inhalt <- str_replace_all(seite, "\r\n\t", ". ")
  inhalt <- str_replace_all(inhalt, "\n", " ")
  inhalt <- str_replace_all(inhalt, "\r", " ")
  inhalt <- str_replace_all(inhalt, "\t", ". ")
  
  ## Korrekte Punktion
  inhalt <- str_replace_all(inhalt, " . ", ". ")
  inhalt <- str_replace_all(inhalt, " . ", ". ")
  #inhalt <- str_replace_all(inhalt, ".. ", ". ")
  
  ## Leerzeichen entfernen
  inhalt <- str_replace_all(inhalt, "  ", " ")
  inhalt <- str_replace_all(inhalt, "  ", " ")
  inhalt <- str_replace_all(inhalt, "  ", " ")
  inhalt <- str_replace_all(inhalt, "  ", " ")
  inhalt <- str_replace_all(inhalt, "  ", " ")
  inhalt <- str_replace_all(inhalt, "  ", " ")
  inhalt <- str_replace_all(inhalt, "  ", " ")
  
  ## Text zusammennehmen
  inhaltzusammen <- ""
  for(i in 1:length(inhalt)) {
    inhaltzusammen <- paste(inhaltzusammen,inhalt[i])
  }
  
  ## Lesbarkeitsindex berechnen
  lix <- textstat_readability(inhaltzusammen, measure = "LIW")
  lix <- as.character(lix)[2]
  lix_liste <- c(lix_liste,lix)
  
  # Hash erstellen --------------------
  webseite_hash <- html_text(ganze_seite)
  webseite_hash_liste <- c(webseite_hash_liste,digest(webseite_hash, "md5", serialize = FALSE))
  
  ## Zusatzinfo
  zusatz <- html_nodes(x = ganze_seite, css = "meta")
  zusatz_inhalt <- html_attr(x = zusatz, "content")
  zusatz_inhalt <- rbind(zusatz_inhalt[1],zusatz_inhalt[2],zusatz_inhalt[3],zusatz_inhalt[4],zusatz_inhalt[5])
  zusatz_bezeichnung <- html_attr(x = zusatz, "property")
  zusatz_bezeichnung <- rbind(zusatz_bezeichnung[1],zusatz_bezeichnung[2],zusatz_bezeichnung[3],zusatz_bezeichnung[4],zusatz_bezeichnung[5])
  zusatz_bezeichnung <- data.frame(zusatz_bezeichnung)
  zusatz_bezeichnung[is.na(zusatz_bezeichnung)] <- "keinWert"
  zusatz <- cbind(zusatz_inhalt,zusatz_bezeichnung)
  zusatz <- as.character(zusatz$zusatz_inhalt[zusatz$zusatz_bezeichnung=="cms:stats-title"])
  zusatz_liste <- c(zusatz_liste,zusatz)
}

datensatz <- data.frame(cbind(alle_seiten[1:402,],lix_liste,webseite_hash_liste,zusatz_liste))
write.csv(datensatz,file="datensatz.csv")

