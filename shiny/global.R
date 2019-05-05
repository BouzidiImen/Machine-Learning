
library(shiny)
library(plotly)
library(shinydashboard)
library(dashboardthemes)

library(plotly)
library(caret)

base <- read.csv("data/dataset.csv",header = T,sep =",",dec = "," )
base=data.frame(base)