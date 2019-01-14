# gesis_dataday_19
Materialien für den Workshop "Forschungsdaten selbst analysieren mit R", [GESIS DataDay 2019](https://www.gesis.org/angebot/veranstaltungen/gesis-tagungen/dataday2019/)

Um das Jupyter-Notebook für diesen Workshop zu starten, klicken Sie einfach auf den "Launch Binder"-Button. Auf der Seite, die sich dann nach kurzer Zeit öffnen sollte, klicken Sie dann die Datei allbus_evs_examples_dataday19.ipynb an.

[![Binder](https://notebooks.gesis.org/binder/badge_logo.svg)](https://notebooks.gesis.org/binder/v2/gh/jobreu/gesis_dataday_19/master)

Wenn Sie das vollständige Repository herunterladen und lokal speichern wollen, klicken Sie auf den grünen "Clone or download"-Button und wählen dann "Download ZIP" aus.

---

## Voraussetzungen
Um das Notebook bzw. das Script verwenden zu können, benötigen Sie einen Account für den [GESIS DBK](https://dbk.gesis.org/dbksearch/register.asp).

Für R-Neulinge empfiehlt es sich, Grundkenntnisse im Umgang mit R und dem [Tidyverse](https://www.tidyverse.org/) zu erwerben, um mit dem Script (weiter) zu arbeiten.
Kurze, interaktive und kostenlose Einführungen in R bieten bspw. der [swirl](https://swirlstats.com/)-Kurs R Programming oder der DataCamp-Kurs [Introduction to R](https://www.datacamp.com/courses/free-introduction-to-r).

Einen ausführlicheren Überblick mit Fokus aud die Tidyverse-Packages liefert das Buch [R for Data Science](http://shop.oreilly.com/product/0636920034407.do) von Hadley Wickham und Garrett Grolemund. Das Buch ist [online](https://r4ds.had.co.nz/) vollständig und kostenfrei verfügbar. Das gedruckte Buch ist mittlerweile auch in [deutscher Übersetzung](https://www.oreilly.de/buecher/13082/9783960090502-r-f%C3%BCr-data-science.html) erschienen.

Als Einführung speziell auch für den Datenjournalismus ist vor Kurzem das Buch [Practical R for Mass Communication and Journalism](https://www.crcpress.com/Practical-R-for-Mass-Communication-and-Journalism/Machlis/p/book/9781138726918) von Sharon Machlis erschienen. Die Autorin bietet in der Serie [Do More with R](https://www.infoworld.com/video/series/8563/do-more-with-r) zudem einige kostenlose kurze Video Tutorials zu verschiedenen Themen (u.a. auch zur Erstellung von Karten) an.

Für den Einstieg in R gibt es auch zahlreiche Präsenzkurse. Bei GESIS bietet [Jan-Philipp Kolb](https://github.com/Japhilko) am 13. & 14.05.2019 z.B. den Kurs [Einführung die Datenanalyse mit R](https://training.gesis.org/?site=pDetails&child=full&pID=0xE5FEA0B0CD754F68B0754EC1011C08C3) an und ich selbst werde, gemeinsam mit meinem Kollegen Thomas Ebel, am 15. & 16.05.2019 einen Workshop zu [Data Wrangling & Exploration with the Tidyverse in R](https://training.gesis.org/?site=pDetails&child=full&pID=0x33C195D77A9F450183D79276838B4E73) anbieten (beide Kurse finden in Mannheim statt).

## Quellen
Für die Beispiele werden Daten aus der Allgemeinen Bevölkerungsumfrage der Sozialwissenschaften [ALLBUS](https://www.gesis.org/allbus/allbus/) sowie der European Value Study [EVS](https://europeanvaluesstudy.eu/) verwendet.

Vollständige Zitationen für die verwendeten Datensätze:

GESIS - Leibniz-Institut für Sozialwissenschaften (2018): Allgemeine Bevölkerungsumfrage der Sozialwissenschaften ALLBUScompact - Kumulation 1980-2016. GESIS Datenarchiv, Köln. ZA4587 Datenfile Version 1.0.0, doi:10.4232/1.13048

GESIS - Leibniz-Institut für Sozialwissenschaften (2017): Allgemeine Bevölkerungsumfrage der Sozialwissenschaften ALLBUS 2016. GESIS Datenarchiv, Köln. ZA5250 Datenfile Version 2.1.0, doi:10.4232/1.12796

EVS (2018): European Values Study 2017: Integrated Dataset (EVS 2017). GESIS Datenarchiv, Köln. ZA7500 Datenfile Version 1.0.0, doi:10.4232/1.13090

EVS (2016): European Values Study 2008: Integrated Dataset (EVS 2008). GESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0, doi:10.4232/1.12458

Die Erstellung der Karten für die EVS-Daten orientiert sich an einem [Blog-Post von Alexandru Cernat](http://www.alexcernat.com/freesurveydata-european-values-study-2017/).
