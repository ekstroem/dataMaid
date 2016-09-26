# cleanR

Development version of the R package cleanR

To install the development version of cleanR run the following commands
from within R

```{r}
library(devtools)
install_github('ekstroem/cleanR')
```



## To Do / ideas

* Documentation
* Liste / forklare checks
* mulighed for print på skærm
* ~~Væk fra Tufte? (html/standard-pdf)~~ &#10003;
* ~~Kun variable med problemer~~ &#10003;
* ~~Kun figurer~~ &#10003;
* ~~Kun tabeller~~ &#10003;
* Formattering er meget hardcoded mht. bredder osv. Det betyder specielt at basicVisual-plots bliver meget, meget små. Kan vi fikse det på en smart måde?
* stopOverwrite er implementeret for rmd-filerne, men ikke for pdf/html. Jeg har ikke fundet en smart måde at "afmærke" disse filer. Vi kan godt tilføje en kommentar ("%comment") til pdf-filen, men IKKE i slutningen af filen (der er en end-of-file-marker, som jeg ikke tør rode med). Det betyder, at vi skal skrive kommentaren i starten og altså skal det gøres "før" (eller mens) der knittes - ellers vil vi skulle have hele pdf-filen liggende i hukommelsen mens vores funktion kører. For html kan vi godt kommentere i slutningen af filen, men det bliver langsomt (og tungt?) at skulle læse dertil hver gang. Hvad gør vi?
