#' @import shiny
#' @import bnutil
#' @import reshape2
#' @import ggplot2
#' @import pgBatch
#'
#' @export
shinyServerRun = function(input, output, session, context) {

  output$body = renderUI({
    sidebarLayout(
      sidebarPanel(
        checkboxInput("applymode", "Apply saved model", FALSE),
        conditionalPanel(condition = "!input.applymode",
                         checkboxInput("useref", "Use a reference batch", value = FALSE),
                         conditionalPanel(condition = 'input.useref',
                                          selectInput("refbatch", "Select reference batch",choices = list())
                         ),
                         checkboxInput("returnlink", "Return link to Combat model", value = FALSE)
        ),
        conditionalPanel(condition = "input.applymode",
                         selectInput("modlink", "Select factor containing the model link", choices = list())
        ),
        actionButton("done", "Done"),
        verbatimTextOutput("status")
      ),
      mainPanel(
        plotOutput("pca")
      )
    )
  })

  getRunFolderReactive = context$getRunFolder()
  getStepFolderReactive = context$getFolder()
  getDataReactive = context$getData()

  observe({

    getData=getDataReactive$value
    if (is.null(getData)) return()

    getRunFolder = getRunFolderReactive$value
    if(is.null(getRunFolder)) return()

    getStepFolder = getStepFolderReactive$value
    if(is.null(getStepFolder)) return()

    bndata = getData()
    if (!bndata$hasColors){
      stop("Need exactly 1 data color for the batch variable or model link")
    }

    if(length(bndata$colorColumnNames) > 1){
      stop("Need exactly 1 data color for the batch variable or model link")
    }

    df = bndata$data
    df$bv = as.factor(df[[bndata$colorColumnNames]])

    lab = paste("Select reference batch from the values in", bndata$colorLabels)
    updateSelectInput(session, "refbatch", label = lab, choices = levels(df$bv))
    updateSelectInput(session, "modlink", choices = bndata$arrayColumnNames)

    comfit = reactive({
      X0 = acast(df, rowSeq ~ colSeq, value.var = "value")
      bv = acast(df,  rowSeq ~ colSeq, value.var = "bv")[1,]
      rowSeq = acast(df,  rowSeq ~ colSeq, value.var = "rowSeq")[,1]
      colSeq = acast(df,  rowSeq ~ colSeq, value.var = "colSeq")[1,]
      dimnames(X0) = list(rowSeq = rowSeq, colSeq = colSeq)
      cmod = pgCombat$new()
      if(input$useref){
        cmod = cmod$fit(X0, bv, ref.batch = input$refbatch)
      } else {
        cmod = cmod$fit(X0, bv)
      }

      return(cmod)
    })

    comapply = reactive({
      modlink = as.character(df[[input$modlink]][1])
      X0 = acast(df, rowSeq ~ colSeq, value.var = "value")
      bv = acast(df,  rowSeq ~ colSeq, value.var = "bv")[1,]
      rowSeq = acast(df,  rowSeq ~ colSeq, value.var = "rowSeq")[,1]
      colSeq = acast(df,  rowSeq ~ colSeq, value.var = "colSeq")[1,]
      dimnames(X0) = list(rowSeq = rowSeq, colSeq = colSeq)
      load(modlink)
      Xc = aCom$apply(X0, bv)
      dimnames(Xc) = dimnames(X0)
      result = list(X0 = X0, Xc = Xc, batches = bv)
    })

    output$pca = renderPlot({
      if(!input$applymode){
        aCom = comfit()
      } else {
        aCom = comapply()
      }
      iPca = prcomp(t(aCom$X0))
      fPca = prcomp(t(aCom$Xc))
      pcaresi = data.frame(PC1 = iPca$x[,1], PC2 = iPca$x[,2], bv = aCom$batches, stage = "before")
      pcaresf = data.frame(PC1 = fPca$x[,1], PC2 = fPca$x[,2], bv = aCom$batches, stage = "after")
      pcares = rbind(pcaresi, pcaresf)
      prt = ggplot(pcares, aes(x = PC1 , y = PC2, colour = bv)) + geom_point()
      prt = prt + facet_wrap(~stage,scales = "free")
      return(prt)
    })

    output$status = renderText({
      isolate({
        bLink = input$returnlink
      })
      if(input$done >0){
        if(!input$applymode){
          aCom = comfit()
        } else {
          aCom = comapply()
        }
        Xc = aCom$Xc
        dfXc = melt(Xc, value.name = "CmbCor")
        dfXc$rowSeq = as.double(dfXc$rowSeq)
        dfXc$colSeq = as.double(dfXc$colSeq)
        if(!bLink | input$applymode){
          mdf = data.frame(labelDescription = c("rowSeq", "colSeq", "CmbCor"),
                           groupingType = c("rowSeq", "colSeq", "QuantitationType"))
          result = AnnotatedData$new(data = dfXc, metadata = mdf)
        } else {
          modellink = file.path(getRunFolder(), "modellink.RData")
          save(file = modellink, aCom)
          dfXc = data.frame(dfXc, modellink = as.character(modellink))
          mdf = data.frame(labelDescription = c("rowSeq", "colSeq", "CmbCor", "modellink"),
                           groupingType = c("rowSeq", "colSeq", "QuantitationType", "Array"))
          result = AnnotatedData$new(data = dfXc, metadata = mdf)
        }
        context$setResult(result)
        return("Done")
      } else {
        return(".")
      }
    })



  })
}

#' @export
shinyServerShowResults = function(input, output, session, context){
  getFolderReactive = context$getRunFolder()

  output$body = renderUI({
    mainPanel(
      verbatimTextOutput("combatlink")
    )
  })

  observe({
    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()

    output$combatlink = renderText({
      return("...")
    })
  })
}
