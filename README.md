# Annotation Web App for Classification and Segmentation of Radiographs
`server.R` and `ui.R` are the 2 primary components of the shiny web app

the app can be started from cli with `$ R -e "library(shiny); runApp(\"$PWD\")"`

`gc.R` is used to generate code for a debug dashboard by metaprogramming app source
code and generating verbatim text outputs for all ui input / reactive sources and
reactive expressions.

`global.R` is sourced before server.R and ui.R are executed, and provides interface
functions and folder system references.

See a running example of the [community implementation](https://precise.shinyapps.io/radiographicstudyannotator_communal/).  
(don't go crazy though - the community app downloads each requested image from openai, and thus could bust the cap for rshiny free hosting).
