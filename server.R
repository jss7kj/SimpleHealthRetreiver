library(shiny)
library(XML)
library(data.table)
library(reshape2)

shinyServer(function(input,output){
  # Unfortunately Apple stores its health data in attributes, are not consistent across records
  parseHealthXML <-function(xdata){
    dumFun <- function(x){
      xname <- xmlName(x)
      xattrs <- xmlAttrs(x)
      c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)
    }
    dum <- xmlParse(xdata)
    data.frame(t(xpathSApply(dum, "//HealthData/Record", dumFun)), stringsAsFactors = FALSE)
  }
  
  getData = reactive({
    parseHealthXML(input$xmlfile$datapath) ->> dats
    if(nrow(dats) != 1) {
      hdf <<- data.table(type = rep("NA",nrow(dats)),
                         value = rep("NA",nrow(dats)),
                         source = rep("NA",nrow(dats)),
                         unit = rep("NA",nrow(dats)),
                         creationDate = rep("NA",nrow(dats)),
                         startDate=rep("NA",nrow(dats)),
                         endDate=rep("NA",nrow(dats)),
                         min = rep("NA",nrow(dats)),
                         max = rep("NA",nrow(dats)),
                         average =rep("NA",nrow(dats)),
                         recordCount = rep("NA",nrow(dats)))
      
      hdf$type = dats$type
      hdf$value = dats$value
      hdf$source = dats$source
      hdf$unit = dats$unit
      hdf$startDate = dats$startDate
      hdf$endDate = dats$endDate

    } else {
      hdf <<- data.table(type = rep("NA",ncol(dats)),
                         value = rep("NA",ncol(dats)),
                         source = rep("NA",ncol(dats)),
                         unit = rep("NA",ncol(dats)),
                         creationDate = rep("NA",ncol(dats)),
                         startDate=rep("NA",ncol(dats)),
                         endDate=rep("NA",ncol(dats)),
                         min = rep("NA",ncol(dats)),
                         max = rep("NA",ncol(dats)),
                         average =rep("NA",ncol(dats)),
                         recordCount = rep("NA",ncol(dats)))
      withProgress(message="Converting XML...", {
        dprog = 1/ncol(dats)*100
        for (n in 1:ncol(dats)) {
          if(n %% 100 == 0) {
            incProgress(dprog)
          }
          rec = dats[[n]]
          dat = rep(NA,ncol(hdf))
          tryCatch({
            if(!is.null(rec[[1]]$type)) {
              set(hdf,i = n, j = 1L, as.character(rec[[1]]$type))
            }
            if(!is.null(rec[[1]]$value)) {
              set(hdf,i = n, j = 2L, as.character(rec[[1]]$value))
            }
            if(!is.null(rec[[1]]$source)) {
              set(hdf,i = n, j = 3L, as.character(rec[[1]]$source))
            }
            if(!is.null(rec[[1]]$unit)) {
              set(hdf,i = n, j = 4L, as.character(rec[[1]]$unit))
            }
            if(!is.null(rec[[1]]$creationDate)) {
              set(hdf,i = n, j = 5L, as.character(rec[[1]]$creationDate))
            }
            if(!is.null(rec[[1]]$startDate)) {
              set(hdf,i = n, j = 6L, as.character(rec[[1]]$startDate))
            }
            if(!is.null(rec[[1]]$endDate)) {
              set(hdf,i = n, j = 7L, as.character(rec[[1]]$endDate))
            }
            if(!is.null(rec[[1]]$min)) {
              set(hdf,i = n, j = 8L, as.character(rec[[1]]$min))
            }
            if(!is.null(rec[[1]]$max)) {
              set(hdf,i = n, j = 9L, as.character(rec[[1]]$max))
            }
            if(!is.null(rec[[1]]$average)) {
              set(hdf,i = n, j = 10L, as.character(rec[[1]]$average))
            }
            if(!is.null(rec[[1]]$recordCount)) {
              set(hdf,i = n, j = 11L, as.character(rec[[1]]$recordCount))
            }
            
          }, error = function(e) {
            
          })
        }
      })
    }
    hdf$type = gsub(x = hdf$type, pattern = "HKQuantityTypeIdentifier",replacement="")
    hdf$type = gsub(x = hdf$type, pattern = "HKDataType",replacement="")
    hdf$value = as.numeric(hdf$value)
    hdf$min = as.numeric(hdf$min)
    hdf$max = as.numeric(hdf$max)
    hdf$average = as.numeric(hdf$average)
    hdf$recordCount = as.numeric(hdf$recordCount)
    
    sd_c = lapply(hdf$startDate, function (x)
      as.character(as.POSIXlt(strptime(x,format = "%Y%m%d%H%M%S%z")))
    )

    ed_c = lapply(hdf$endDate, function (x)
      as.character(as.POSIXlt(strptime(x,format = "%Y%m%d%H%M%S%z")))
    )
    cd_c = lapply(hdf$creationDate, function (x)
      as.character(as.POSIXlt(strptime(x,format = "%Y%m%d%H%M%S%z")))
    )
    
    hdf$startDate[hdf$startDate == "NA"] = NA
    hdf$endDate[hdf$endDate == "NA"] = NA
    hdf$creationDate[hdf$creationDate == "NA"] = NA
#     if(input$unix.time) {
#       hdf$startDate = lapply(hdf$startDate, function(x) as.numeric(x))
#       hdf$endDate = lapply(hdf$endDate, function(x) as.numeric(x))
#       hdf$creationDate = lapply(hdf$creationDate, function(x) as.numeric(x))
#     } else {
      hdf$startDate = sd_c
      hdf$endDate = ed_c
      hdf$creationDate = cd_c
    # }

    if(input$format.choose == "Long") {
      hdf.melted = melt(hdf,id.vars = c(1,5,6,7))
      return(hdf.melted)
    } else {
      return(hdf)
    }
    
  })
  
  output$health.table = renderDataTable({
    if(is.null(input$xmlfile)) {
      return(NULL)
    } else {
      df = getData()
      if (!input$showall) {
        subset(df,df$type %in% input$select.type)
      } else {
        df
      }
    }
  }, options = list(
    lengthMenu = list(c(5,10,50,100),c('5','10','50','100')),
    pageLength = 5
  ))
  
  output$choose.type = renderUI({
    if(is.null(input$xmlfile)) {
      return(NULL)
    } else {
      selectInput("select.type",label = "Select Type",choices = unique(getData()$type),multiple=TRUE)
    } 
  })
  
  output$downloadbutton = downloadHandler(
    filename = function() { paste("export_",as.numeric(Sys.time()),".csv",sep = "")},
    content = function(file){
      write.csv(data.frame(lapply(getData(), as.character), stringsAsFactors=FALSE), file)
    }
  )
  
  
})