#!/usr/bin/env Rscript --vanilla

args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(NOT_CRAN = "true", TZ = "America/Denver")
sqs_wrapper()
