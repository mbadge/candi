# Description
This repository contains packaged utilities and demonstration applications to collect user impressions on radiographic images and supporting data.

# Guide
## Installation and Configuration Tutorial
[![Installation and Configuration Tutorial](https://i.ytimg.com/vi/8zhaj6PgGNw/3.jpg)](https://www.youtube.com/watch?v=8zhaj6PgGNw "Installation and Configuration Tutorial")

## Module and Application demos
* **Modules**
   * use `?XyzModule` in R to fetch a example code snippets of shiny modules
   * run `inst/module_demos/*.R` to launch a shiny app with one module
* **Applications**
  * in `inst/examples` there are example applications

# Design
## Use Cases / System Context
![Use Case Schematic](https://i.imgur.com/cW00AYp.png)

## System Components
The package core is in `R/` and consists of:

* base R
    * data/image interface functions
    * components for queue management and data processing
* shiny
    * shiny modules handle compartamentalized logic for different user interface components
    
![Components Schematic](https://i.imgur.com/nHCoWch.png)

## Data Structure / Sample Scope  
CANDI provides infrastructure to operate on one image at a time or on all the images collected for the same case. 
* CNNs conventionally operate on a single image scope
* Radiologists clinically operate on a patient scope
  
![Data Schematic](https://i.imgur.com/nbY57Dt.png)
