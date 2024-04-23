pathfinder: an algorithm for finding the most efficient path between
waypoints - Part II - being less stupid
================
RKOpTris
2024-04-21

## Abstract

This is a continuation of a journey to write an algorithm from scratch
which finds the most efficient path between a set of waypoints with a
variety of paths between them. (I’m adding to this document
intermittently and iteratively and represents a work in progress.)

Welcome to Part Deux!

## Introduction

In the first part of this series we wrote a not-so-clever algorithm to
find the shortest distance from a start/finish waypoint that passed
through a group of other waypoints at least once. It managed it but did
not find the best route, nor did it do it very quickly. Here we explore
how we can improve upon our original design.

### Methods and R code

So let’s load in the needed libraries and the functions we defined in
the Part I

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
source("generate_points.R")
source("generate_paths.R")
source("get_pos.R")
source("visualise_paths.R")
source("get_distance.R")
source("init_points.R")
source("init_paths.R")
```

Did we get here?!
