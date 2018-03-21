# dataMaid next version

*   Added information about the directory the makeDataReport command was run from 
*   Fixed a bug with determining the full name of the user running the report, and added the username to the report as well.
*   Minor tweaks related to the label attributes. Fixes #21 - thanks to jomuller.
*   Added data.frame label to the presentation under the summary table. Fixes #23 - thanks to carlbfrederick
*   Fixec a bug with the arguments to makeCodebook - thanks to Jaeoc (#24)

# dataMaid 1.1.0

*   Added makeCodebook() function to use a default set of arguments to produce a codebook-like document.
*   Added the bigPresidentData dataset 
*   Various minor bug fixes

# dataMaid 1.0.0

*   Substantial speed-up from optimizing the plot information saved to the Rmarkdown file and the amount of objects passed on internally.

