# Big Thief Lyric Bot Shiny App
# Author: Zachary Lorico Hertz
# Date: June 2025
# Description: Make a 'lyric bot' shiny app for website.

# Load required libraries ------------------------------------------------
library(here)
library(tidyverse)
library(data.table)

chunks <- read.csv("../files/chunks.csv")
