# Computer Aided Note And Diagnosis Interface (CANDI)

## Radiograph Annotation Dashboard (RAD) WebApps
There are 3 CANDI RAD implementations, which differ in there input/output interface.  See the accompanying app page (candi.dudleylab.org) for further details.

## Getting started
from inside an app directory, the app can be started from cli with `$ R -e "library(shiny); runApp(\"$PWD\")"`
* `server.R` and `ui.R` are the 2 primary components of the shiny web app
* `global.R` is sourced before server.R and ui.R are executed, and provides interface
functions and folder system references.
* `gc.R` is used to generate code for a debug dashboard by metaprogramming app source
code and generating verbatim text outputs for all ui input / reactive sources and
reactive expressions.

#### Dependencies
dplyr
tibble
magrittr
stringr
rebus
purrr
shinythemes
shinyjs
shiny