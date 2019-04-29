library(shiny)
library(datasets)

fillSelectDS <- function() {
  dsets <- data(package = "datasets")$result[, "Item"]
  #print(dsets)
  nnnnn <- rep(FALSE, length(dsets))
  #print(nnnnn)
  for (ii in 1:length(dsets)) {
    #print(dsets[ii])
    y <- try(vvvData <- get(x = dsets[ii]), silent = TRUE)
    #print(c(class(vvvData),dsets[ii]))
    if(inherits(y,"try-error")) {
      nnnnn[ii] <- FALSE
    }
    else {
      if (!is.null(names(vvvData))) {
        nnnnn[ii] <- TRUE
      }
    }
    if (class(vvvData) == "list" || class(vvvData) == "numeric")
      nnnnn[ii] <- FALSE
    #if (nnnnn[ii] == TRUE)
    #  print(c(class(vvvData),dsets[ii]))
    
  }
  dsets <- dsets[nnnnn == TRUE]
  return(dsets)
}

fillSelectF <- function() {
  dsets2 <- fillSelectDS()
  vvData <- data.frame()
  y <- try(vvData <- get(x = dsets2[1]), silent = TRUE)
  if(inherits(y,"try-error")) {
    return()
  }
  
  return(vvData)
}

shinyUI(fluidPage(
  
  titlePanel("Random Forest Prediction Simulation"),
  
  fluidRow(
    column(5,
           wellPanel(
      selectInput("tDataset","Dataset",fillSelectDS()),
      varSelectInput("tField","Covariate to predict", data = fillSelectF()),
      checkboxInput("cZero","Delete covariates with variance near Zero", value = TRUE),
      checkboxInput("cNA","Delete covariates with high % of NA values", value = TRUE),
      numericInput("nPorcNA","minimum % NA values",90, min = 50, max = 100, step = 0.1),
      checkboxInput("cCorr","Delete covariates with high % of correlación", value = TRUE),
      numericInput("nPorcCorr","minimum % correlation",80, min = 50, max = 100, step = 0.1),
      actionButton("bSubmit","Submit"),
      actionButton("bCancel","Cancel"),
      hr(),
      tags$p(tags$strong(tags$a(id = "LinkHelp", href = "help.html", target = "_blank", "Help")), align = "center"),
      hr(),
      textOutput("tError")
           )
    ),
    column(7,
           wellPanel(
            fluidRow(
              column(12,
                     wellPanel(

                       p(strong("Results"), align = "center"),
                       hr(),
                       textOutput("tNFields"),
                       textOutput("tFieldsnz"),
                       textOutput("tFieldsna"),
                       textOutput("tFieldscorr"),
                       hr(),
                       textOutput("tFields"),
                       textOutput("tTrainTrainObs"),
                       textOutput("tTrainValObs"),
                       textOutput("tTestObs"),
                       hr(),
                       textOutput("tTrainValAcc"),
                       textOutput("tTrainValErr"),
                       textOutput("tTestAcc"),
                       textOutput("tTestErr")
                       
                     )
                     )
            )
            

           )
           )
  )
)
)