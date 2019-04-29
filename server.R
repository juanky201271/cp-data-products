library(shiny)
library(datasets)
library(ggplot2)
library(lattice)
library(caret)
library(randomForest)
library(rpart)
library(plyr)
library(MASS)
library(klaR)
library(gbm)

vInitOutput <- function(output) {
  output$tTestObs <- renderText({" "})
  output$tTrainValAcc <- renderText({" "})
  output$tTrainValErr <- renderText({" "})
  output$tTestAcc <- renderText({" "})
  output$tTestErr <- renderText({" "})
  output$tNFields <- renderText({" "})
  output$tFieldsnz <- renderText({" "})
  output$tFieldsna <- renderText({" "})
  output$tFieldscorr <- renderText({" "})
  output$tFields <- renderText({" "})
  output$tTrainTrainObs <- renderText({" "})
  output$tTrainValObs <- renderText({" "})
  return()
} 

shinyServer(function(input, output, session) {
  observe({
    if (input$tDataset != "") {
      vvDataSet <- renderText({input$tDataset})
      vvData <- data.frame()
      y <- try(vvData <- get(x = vvDataSet()), silent = TRUE)
      if(inherits(y,"try-error")) {
        vInitOutput(output)
        output$tError = renderText("The dataset not exists in datasets package")
        return()
      }
      print(vvDataSet())
      print(names(vvData))
      #print(class(vvData))
      
      updateVarSelectInput(session = session, "tField", data = vvData)
    }
  })

  observeEvent(input$bCancel, {
    stopApp()
  })
    
  observeEvent(input$bSubmit, {
    vInitOutput(output)
    if (is.null(input$tDataset)) {
      output$tError = renderText("You must to choose a name of Dataset (Package: datasets)")
      return()      
    }
    if (is.null(input$tField)) {
      output$tError = renderText("You must to choose a name of covariate. Some datasets haven't any names.")
      return()      
    }
    if (input$tDataset == "") {
      output$tError = renderText("You must to choose a name of Dataset (Package: datasets)")
      return()
    }
    if(input$tField == "") {
      output$tError = renderText("You must to choose a name of covariate")
      return()
    }
        output$tError = renderText("busy! Thank you for to be patient")
        # here is where begin the process
        vDataSet <- renderText({input$tDataset})
        print(vDataSet())
        print(input$tField)
        y <- try(vData <- get(x = vDataSet()), silent = TRUE)
        if(inherits(y,"try-error")) {
          vInitOutput(output)
          output$tError = renderText("The dataset not exists in datasets package")
          return()
        }
        set.seed(1234567890)  

        vf <- grep(input$tField, names(vData))
        if (length(vf) != 1) {
          vInitOutput(output)
          output$tError = renderText("The covariate not exists in the datasets")
          return()
        }
        num <- dim(vData)[2]
        print(num)
        output$tNFields <- renderText(paste("Total number of covariates in the Dataset :", num))
        vDatanz <- vData
        if (input$cZero) {
          nz <- nearZeroVar(vData)
          #print(nz)
          if (length(nz) > 0)
            vDatanz <- vData[, -nz]
          output$tFieldsnz <- renderText({paste("Covariates deleted with variance near Zero :",length(nz))})
        }
        rm(vData)
        vDatana <- vDatanz
        if (input$cNA) {
          vna    <- sapply(vDatanz, function(x) mean(is.na(x))) > as.numeric(input$nPorcNA/100)
          #print(vna)
          vDatana <- vDatanz[, vna==FALSE]
          output$tFieldsna <- renderText({paste("Covariates deleted with high % of NA values :",sum(vna==TRUE))})
        }
        rm(vDatanz)
        vDatacorr <- vDatana
        if (input$cCorr) {
          y <- try(descrCor <-  cor(vDatana[-vf]))
          if(inherits(y,"try-error")) {
            vInitOutput(output)
            output$tError = renderText("Is not possible to use Correlation, you should uncheck")
            return()
          }
          y <- try(highlyCorDescr <- findCorrelation(descrCor, cutoff = as.numeric(input$nPorcCorr/100)))
          if(inherits(y,"try-error")) {
            vInitOutput(output)
            output$tError = renderText("Is not possible to use find Correlation, you should uncheck")
            return()
          }
          #print(highlyCorDescr)
          if (length(highlyCorDescr) > 0)
            vDatacorr <- vDatana[,-highlyCorDescr]
          output$tFieldscorr <- renderText({paste("Covariates deleted with high % of correlation :",length(highlyCorDescr))})
        }
        rm(vDatana)
        num2 <- dim(vDatacorr)[2]
        output$tFields <- renderText({paste("Covariates selected for the simulation",num2)})
        
        vf <- grep(input$tField, names(vDatacorr))
        if (length(vf) != 1) {
          vInitOutput(output)
          output$tError = renderText("The covariate is deleted in the process, try another one")
          return()
        }
        
        vDatacorr[,vf] <- as.factor(vDatacorr[,vf])
        
        y <- try(Spl <- createDataPartition(y = vDatacorr[,vf], p = 0.6, list = FALSE))
        if(inherits(y,"try-error")) {
          vInitOutput(output)
          output$tError = renderText("Is not possible to use Cross Validation, try another covariate")
          return()
        }
        
        vData.train <- vDatacorr[Spl, ]
        vData.2 <- vDatacorr[-Spl, ]
        y <- try(Spl2 <- createDataPartition(y = vData.2[,vf], p = 0.5, list = FALSE))
        if(inherits(y,"try-error")) {
          vInitOutput(output)
          output$tError = renderText("Is not possible to use Cross Validation, try another covariate")
          return()
        }
        
        vData.val <- vData.2[Spl2, ]
        vData.test <- vData.2[-Spl2, ]
        
        output$tTrainTrainObs <- renderText({paste("Training Observations (60%) :", dim(vData.train)[1])})
        output$tTrainValObs <- renderText({paste("Training Validation Observations (20%) :", dim(vData.val)[1])})
        output$tTestObs <- renderText({paste("Testing Observations (20%) :", dim(vData.test)[1])})
        
        vControl <- trainControl(method="cv", number=4, verboseIter = FALSE)

        y <- try(Modfit.rf <- train(formula(paste0(input$tField," ~ .")), method = "rf", data = vData.train, trControl = vControl), silent = TRUE)
        if(inherits(y,"try-error")) {
          vInitOutput(output)
          output$tError = renderText("Is not possible to use Random Forest, try another covariate or Dataset")
          return()
        }
        
        y <- try(Pre.rf <- predict(Modfit.rf, vData.val), silent = TRUE)
        if(inherits(y,"try-error")) {
          vInitOutput(output)
          output$tError = renderText("Is not possible to do some predict (validation), try another covariate or Dataset")
          return()
        }
        
        y <- try(Accu <- confusionMatrix(Pre.rf, vData.val[,vf])$overall[1], silent = TRUE)
        if(inherits(y,"try-error")) {
          vInitOutput(output)
          output$tError = renderText("Is not possible to calculate the accuracy (Validation), try another covariate or Dataset")
          return()
        }
        pAccu <- Accu * 100
        Error <- 1 - Accu
        pError <- Error * 100
        
        output$tTrainValAcc <- renderText({paste("Accuracy Training Validation :",round(pAccu,2),"%")})
        output$tTrainValErr <- renderText({paste("Error Out-of-sample Trainig Validation :",round(pError,2),"%")})


        y <- try(Pre.rf <- predict(Modfit.rf, vData.test), silent = TRUE)
        if(inherits(y,"try-error")) {
          vInitOutput(output)
          output$tError = renderText("Is not possible to do some predict (Testing), try another covariate or Dataset")
          return()
        }
        
        y <- try(Accu <- confusionMatrix(Pre.rf, vData.test[,vf])$overall[1], silent = TRUE)
        if(inherits(y,"try-error")) {
          vInitOutput(output)
          output$tError = renderText("Is not possible to calculate the accuracy (Testing), try another covariate or Dataset")
          return()
        }
        pAccu <- Accu * 100
        Error <- 1 - Accu
        pError <- Error * 100

        output$tTestAcc <- renderText({paste("Accuracy Testing :",round(pAccu,2),"%")})
        output$tTestErr <- renderText({paste("Error Out-of-sample Testing :",round(pError,2),"%")})
        output$tError = renderText("Done!")
        

  })
})