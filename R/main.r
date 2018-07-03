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
        checkboxInput("useref", "Use a reference batch", value = FALSE),
        conditionalPanel(condition = 'input.useref',
                         selectInput("refbatch", "Select reference batch",choices = list())
        ),
        checkboxInput("returnlink", "Return link to Combat model", value = FALSE),
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
      stop("Need exactly 1 data color for the batch variable")
    }

    if(length(bndata$colorColumnNames) > 1){
      stop("Need exactly 1 data color for the batch variable")
    }

    df = bndata$data
    df$bv = as.factor(df[[bndata$colorColumnNames]])

    lab = paste("Select reference batch from the values in", bndata$colorLabels)
    updateSelectInput(session, "refbatch", label = lab, choices = levels(df$bv))

    comfit = reactive({
      X0 = acast(df, colSeq ~ rowSeq, value.var = "value")
      bv =acast(df, colSeq ~ rowSeq, value.var = "bv")[,1]
      cmod = pgCombat$new()
      if(input$useref){
        cmod = cmod$fit(t(X0), bv, ref.batch = input$refbatch)
      } else {
        cmod = cmod$fit(t(X0), bv)
      }
      return(cmod)
    })

    output$pca = renderPlot({
        aCom = comfit()
        iPca = prcomp(t(aCom$X0))
        fPca = prcomp(t(aCom$Xc))
        pcaresi = data.frame(PC1 = iPca$x[,1], PC2 = iPca$x[,2], bv = aCom$batches, stage = "before")
        pcaresf = data.frame(PC1 = fPca$x[,1], PC2 = fPca$x[,2], bv = aCom$batches, stage = "after")
        pcares = rbind(pcaresi, pcaresf)
        prt = ggplot(pcares, aes(x = PC1 , y = PC2, colour = bv)) + geom_point()
        prt = prt + facet_wrap(~stage)
        return(prt)
    })

    output$status = renderText({
      isolate({
        bLink = input$returnlink
      })
      if(input$done >0){
        aCom = comfit()
        Xc = aCom$Xc
        dimnames(Xc) = list(rowSeq = 1:dim(Xc)[1], colSeq = 1:dim(Xc)[2])
        dfXc = melt(Xc, value.name = "CmbCor")
        dfXc$rowSeq = as.double(dfXc$rowSeq)
        dfXc$colSeq = as.double(dfXc$colSeq)
        if(!bLink){
          mdf = data.frame(labelDescription = c("rowSeq", "colSeq", "CmbCor"),
                           groupingType = c("rowSeq", "colSeq", "quantitationType"))
          result = AnnotatedData$new(data = dfXc, metadata = mdf)
        } else {
          modellink =file.path(getRunFolder(), "modellink.RData")
          save(file = modellink, aCom)
          dfXc = data.frame(dfXc, modellink = modellink)
          mdf = data.frame(labelDescription = c("rowSeq", "colSeq", "CmbCor", "modellink"),
                           groupingType = c("rowSeq", "colSeq", "quantitationType", "Array"))
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
