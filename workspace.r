library(bnutil)
library(shiny)
library(reshape2)
library(pgBatch)
library(combatapp)
library(ggplot2)

getdata = function() {
  aData = AnnotatedData$new(data = combat_testdf, metadata = combat_testmetadf)
}

setResult = function(annotatedResult){
  print(annotatedResult)
  result = annotatedResult$data
}

bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getDataHandler = getdata
bnMessageHandler$setResultHandler = setResult


bnshiny::startBNTestShiny('combatapp', sessionType='run', bnMessageHandler=bnMessageHandler)
