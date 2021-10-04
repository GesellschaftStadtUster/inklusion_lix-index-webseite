#Bibliothek einlesen
library(jsonlite)
library(rvest)
library(xml2)
library(stringr)

library(quanteda.textstats)

## Funktion zum Auslesen der Webseite Uster.ch =====================

lixwert <- function(quelle, suchcode = ".icms-wysiwyg"){
## Webseite auslesen
webseite <- xml2::read_html(quelle)
seite <- html_nodes(x = webseite, css = suchcode)
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
as.character(lix)[2]
}


