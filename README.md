# Description
This repository contains packaged utilities and demonstration applications to collect user impressions on radiographic images and supporting data.

Further background and running demonstration applications can be found at the companion website: [candi.nextgenhealthcare.org](https://candi.nextgenhealthcare.org/)

# Code Design
## Package Functions
The package core is in `R/` and consists of:

* base R
    * data/image interface functions
    * components for queue management and data processing
* shiny
    * shiny modules handle compartamentalized logic for different user interface components

## Module and application demos
`inst` contains minimal demonstrations of each shiny module as well as common use applications.

# Developer Tutorials
[![Installation and Configuration Tutorial](https://i.ytimg.com/vi/8zhaj6PgGNw/3.jpg)](https://www.youtube.com/watch?v=8zhaj6PgGNw "Installation and Configuration Tutorial")
