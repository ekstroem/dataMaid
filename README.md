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
* ~~Liste / forklare checks~~ &#10003;
* mulighed for print på skærm
* ~~Væk fra Tufte? (html/standard-pdf)~~ &#10003;
* ~~Kun variable med problemer~~ &#10003;
* ~~Kun figurer~~ &#10003;
* ~~Kun tabeller~~ &#10003;
* Formattering er meget hardcoded mht. bredder osv. Det betyder specielt at basicVisual-plots bliver meget, meget små. Kan vi fikse det på en smart måde?
* replace (fejl ved overskrivning) er implementeret for rmd-filerne, men ikke for pdf/html. Det er ikke straight-forward for pdf, da vi skal kunne læse en pdf-fils metadata. Dette kan muligvis gøres med en ny pakke (exif), men jeg kan ikke få lov til at installere den vha. devtools. Derfor: replace defaulter nu til ikke at overskrive. Har desuden tilføjet en option for versionsnummer, så brugeren let og hurtigt kan få nye filnavne.
* Check for factor/character-variable: Er det en dato? Jeg kan ikke finde nogen, der har skrevet en funktion, som forsøger at genkende datoer (heller ikke i lubridate). Jeg er ikke sikker på, at jeg synes det er besværet værd at skrive den selv - der findes mange, mange datoformater.
* I clean(): Skal vi ikke forsøge at fikse flere problemer med mærkelige inputs fra brugeren, i stedet for blot at overskrive deres valg/smide errors? (Fx. om objekt er data.frame, om filnavn er meningsfuldt osv. )



