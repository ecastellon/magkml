## -*- coding:utf-8 -*-
library(purrr)
library(dplyr)
library(magfoo)
library(devtools)
library(testthat)
library(xml2)

setwd("c:/eddy/code/r/magkml")
source("r/kut.r")
source("r/kml.r")

RZ <- rprojroot::is_r_package
PT <- testthat::test_path()

