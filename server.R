library(shiny)
library(RODBC)
library(ggplot2)
library(readxl)
library(dplyr)
library(rvg)
library(officer)
#library(mschart)
library(magrittr)
library(dplyr)

server<-function(input,output,session){

  data1<-reactive({
    #query<-paste("Exec sp_Get_AllCatRegScores")
    query<-paste("Exec sp_Get_AllCatRegScores1")
    dbhandle <- odbcDriverConnect('driver={SQL Server};server=WSCPT3IPSBC13\\SQLEXPRESS;database=Validations;UID=sa;PWD=Deogloria1!')
    #dbhandle <- odbcDriverConnect('driver={SQL Server};server=10.144.64.204;Database=Validations;Uid=sa;Pwd=Password99#')
    #dbhandle <- odbcDriverConnect('Driver=FreeTDS;TDS_Version=8.0;Server=197.189.248.58;Port=1445;Database=validations;Uid=DKantu01;Pwd=DEOGLORIA00!!;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;')
    datasql <- sqlQuery(dbhandle, query)
    close(dbhandle)
    datasql
  })
  
  data2<-reactive({
    
    query<-paste("Exec sp_Get_CatAndSubCat")
    dbhandle <- odbcDriverConnect('driver={SQL Server};server=WSCPT3IPSBC13\\SQLEXPRESS;database=Validations;UID=sa;PWD=Deogloria1!')
    #dbhandle <- odbcDriverConnect('driver={SQL Server};server=10.144.64.204;Database=Validations;Uid=sa;Pwd=Password99#')
    #dbhandle <- odbcDriverConnect('Driver=FreeTDS;TDS_Version=8.0;Server=197.189.248.58;Port=1445;Database=validations;Uid=DKantu01;Pwd=DEOGLORIA00!!;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;')
    datasql <- sqlQuery(dbhandle, query)
    close(dbhandle)
    datasql
  })
  
  output$share_ae<-renderPlot({
    cat.label <- input$categoryInput
    reg.label <- input$regionInput
    work <- data1()
    # count the number of observations in work2
    n.row1<-nrow(work)
    # TE appears to be of type 'character'
    unique(work$TE)
    # coerce TE to numeric
    work$TE <- as.numeric(work$TE)
    # Can use character(0) to remove all choices
    if (is.null(cat.label) & is.null(reg.label)){#x <- character(0)
      cat.label <- work$Category
      reg.label <- work$Region
    }
    if (is.null(cat.label) & !is.null(reg.label)){#x <- character(0)
      cat.label <- work$Category
    }
    if (!is.null(cat.label) & is.null(reg.label)){#x <- character(0)
      reg.label <- work$Region
    }

    filtered<- filter(work, work$Category %in% cat.label, work$Region %in% reg.label)

    gg_plot<-ggplot(filtered, aes(x=AE, y=MShre)) +
      geom_point(shape=2) +    # Use hollow circles
      geom_smooth(method=lm, formula = y ~ x + I(x^2), size = 1,  # Add linear regression line
                  se=FALSE)
    gg_plot + ggtitle("AE against SHARE") + labs(x = "AE") + labs(y = "SHARE")
    gg_plot + xlim(0, 100) + ylim(0,100)

    })
  
  

  
  output$share_ee<-renderPlot({
    cat.label <- input$categoryInput
    reg.label <- input$regionInput
    work <- data1()
    # find rows with no missing values
    work2 <- work[complete.cases(work), ]
    # count the number of observations in work2
    n.row2<-nrow(work2)
    # TE appears to be of type 'character'
    unique(work$TE)
    # coerce TE to numeric
    work$TE <- as.numeric(work$TE)
    # Can use character(0) to remove all choices
    if (is.null(cat.label) & is.null(reg.label)){#x <- character(0)
      cat.label <- work$Category
      reg.label <- work$Region
    }
    if (is.null(cat.label) & !is.null(reg.label)){#x <- character(0)
      cat.label <- work$Category
    }
    if (!is.null(cat.label) & is.null(reg.label)){#x <- character(0)
      reg.label <- work$Region
    }

    filtered<- filter(work, work$Category %in% cat.label, work$Region %in% reg.label)
    gg_plot<-ggplot(filtered, aes(x=TE, y=MShre)) +
      geom_point(shape=1) +    # Use hollow circles
      geom_smooth(method=lm,   # Add linear regression line
                  se=FALSE) 
    gg_plot + labs(x = "EE") + labs(y = "SHARE") + labs(title = "EE against SHARE")
    gg_plot + xlim(0, 100) + ylim(0,100)

  })
  
  #Return correlation table
  output$results<-renderTable({
    
    cat.label <- input$categoryInput
    reg.label <- input$regionInput
    
    if(!is.null(cat.label)){output$display1 <- renderText({ toString(unique(cat.label)) })}
    else {output$display1 <- renderText({ toString("All categories") })}
    if(!is.null(reg.label)){output$display2 <- renderText({ toString(unique(reg.label)) })}
    else {output$display2 <- renderText({ toString("All regions") })}

    work <- data1()
    # find rows with no missing values
    #work1 <-work[,1:5]
    work1 <-work[,1:8]
    work2 <- work1[complete.cases(work1), ]
    # TE appears to be of type 'character'
    unique(work$TE)
    # coerce TE to numeric
    work$TE <- as.numeric(work$TE)
    # Can use character(0) to remove all choices
    if (is.null(cat.label) & is.null(reg.label)){#x <- character(0)
      cat.label <- work$Category
      reg.label <- work$Region
    }
    if (is.null(cat.label) & !is.null(reg.label)){#x <- character(0)
      cat.label <- work$Category
    }
    if (!is.null(cat.label) & is.null(reg.label)){#x <- character(0)
      reg.label <- work$Region
    }
    
    filtered<- filter(work, work$Category %in% cat.label, work$Region %in% reg.label)
    filtered1<- filter(work2, work2$Category %in% cat.label, work2$Region %in% reg.label)
    filtered1$TE <- as.numeric(filtered1$TE)
    n.row1<-nrow(filtered)
    n.row2<-nrow(filtered1)
    #table<-cbind(n.row1,cor(filtered$AE,filtered$MShre),cor(filtered1$TE,filtered1$MShre))
    table<-cbind(n.row1,cor(filtered$AE,filtered$MShre),cor(filtered1$TE,filtered1$MShre))
    colnames(table)<-c("Count","Cor(AE,Sh)","Cor(EE,Sh)")
    data.frame(table)
    return(table)
    
  })
  
  

  

  #Create and Download ggplot chart into Powerpoint
  observeEvent(input$go, {
    x <- input$categoryInput
    y <- input$regionInput
    work <- data1()
    # count the number of observations in work2
    n.row1<-nrow(work)
    # TE appears to be of type 'character'
    unique(work$TE)
    # coerce TE to numeric
    work$TE <- as.numeric(work$TE)
    # Can use character(0) to remove all choices
    if (is.null(x) & is.null(y)){#x <- character(0)
      x <- work$Category
      y <- work$Region
    }
    if (is.null(x) & !is.null(y)){#x <- character(0)
      x <- work$Category
    }
    if (!is.null(x) & is.null(y)){#x <- character(0)
      y <- work$Region
    }

    filtered<- filter(work, work$Category %in% x, work$Region %in% y)

    gg_plot<-ggplot(filtered, aes(x=AE, y=MShre)) +
      geom_point(shape=2) +    # Use hollow circles
      geom_smooth(method=lm, formula = y ~ x + I(x^2), size = 1,  # Add linear regression line
                  se=FALSE)
    gg_plot + labs(x = "AE") + labs(y = "SHARE") + labs(title = "AE against SHARE")
    gg_plot + xlim(0, 100) + ylim(0,100)
    
    gg_plot1<-ggplot(filtered, aes(x=TE, y=MShre)) +
      geom_point(shape=1) +    # Use hollow circles
      geom_smooth(method=lm,   # Add linear regression line
                  se=FALSE) 
    gg_plot1 + labs(x = "EE") + labs(y = "SHARE") + labs(title = "EE against SHARE")
    gg_plot1 + xlim(0, 100) + ylim(0,100)
    
    
    doc <- read_pptx()
    doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
    doc <- ph_with_vg(doc, ggobj = gg_plot, type = "body")
    #doc <- ph_with_vg_at(doc, ggobj = gg_plot, left=0.79/2.54, top=1.29/2.54, height=17.05/2.54, width=32.53/2.54)
    #doc <- ph_with_vg(doc, code = barplot(sample(1:20,10),xlab="Day",ylab="Widgets"),type = "body")
    doc <- ph_with_text(doc, type = "title", str = "AE vs. Share Chart")
    doc <- ph_with_text(doc, type = "ftr", str = "Validations")
    #doc <- ph_with_text(doc, type = "ftr", str = toString(unique(x)))
    #doc <- ph_with_text(doc, type = "ftr", str = toString(unique(y)))
    doc <- ph_with_text(doc, type = "dt", str = format(Sys.Date()))
    doc <- ph_with_text(doc, type = "sldNum", str = "slide 1")
    
    doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
    doc <- ph_with_vg(doc, ggobj = gg_plot1, type = "body")
    doc <- ph_with_text(doc, type = "title", str = "EE vs. Share Chart")
    doc <- ph_with_text(doc, type = "ftr", str = "Validations")
    doc <- ph_with_text(doc, type = "dt", str = format(Sys.Date()))
    doc <- ph_with_text(doc, type = "sldNum", str = "slide 2")
    #setwd('C:/Users/DKantu01/Documents/Datasets')
    setwd("C:/Users/DKantu01/OneDrive/Programming/R/Shiny/Validations R Shiny")
    print(doc, target = "Validations_Charts.pptx")

  })
  
  #Deselect all categories and regions
  observeEvent(input$deselectAll,{
      
        updateCheckboxGroupInput(session,"categoryInput","Category",choices=c("Alcoholic Beverages","Automotive","Durables and Electronics","Electronics","Financial","FMCG","Healthcare","Media","Retail","Services","Telecommunications"))
        updateCheckboxGroupInput(session,"regionInput","Region",choices = c("Africa", "Asia", "Australia","Eastern Europe", "Japan","Latin America","Middle East","New Zealand","North America","Western Europe"))
     }
  )
  

  
  subcateg <- reactive({
    df<-as.data.frame(data1())
    cat.label <- input$categoryInput1
    reg.label <- input$regionInput1
    country.label <- input$countryInput1
    
    
    if(cat.label == "All"){cat.label <- unique(df$Category)}
    if(reg.label == "All"){reg.label <- unique(df$Region)}
    if(country.label == "All"){country.label <- unique(df$Country)}
    
    df %>% filter(Category %in% cat.label, Region %in% reg.label, Country %in% country.label) %>% select(Category,SubCategory)
    #df %>% filter(Category %in% cat.label) %>% select(Category,SubCategory)
    
  })
  
  observe({
    x <- subcateg() %>% select(SubCategory) %>% distinct()
    updateSelectInput(session, "subcategoryInput1", "subCategory", choices = c("All",x), selected = "All")
    #updateSelectInput(session, "subcategoryInput1", "subCategory", choices = names(x))
  })
  
  country <- reactive({
    df<-as.data.frame(data1())
    reg.label <- input$regionInput1
    cat.label <- input$categoryInput1
    subcat.label <- input$subcategoryInput1
    
    if(cat.label == "All"){cat.label <- unique(df$Category)}
    if(subcat.label == "All"){subcat.label <- unique(df$SubCategory)}
    if(reg.label == "All"){reg.label <- unique(df$Region)}
    
    df %>% filter(Category %in% cat.label, SubCategory %in% subcat.label, Region %in% reg.label) %>% select(Region,Country)
    #df %>% filter(Region %in% reg.label) %>% select(Region,Country)
  })
  
  observe({
    x <- country() %>% select(Country) %>% distinct()
    updateSelectInput(session, "countryInput1", "Country", choices = c("All",x), selected = "All")
    #updateSelectInput(session, "countryInput1", "Country", choices = names(x))
  })
  

  correl <- reactive({
    cat.label <- input$categoryInput1
    subcat.label <- input$subcategoryInput1
    reg.label <- input$regionInput1
    country.label <- input$countryInput1
    
    work <- data1()
    work1 <-work[,1:8]
    # find rows with no missing values
    work2 <- work1[complete.cases(work1), ]
    if(cat.label == "All"){cat.label <- NULL}
    if(subcat.label == "All"){subcat.label <- NULL}
    if(reg.label == "All"){reg.label <- NULL}
    if(country.label == "All"){country.label <- NULL}
    
    if(!is.null(cat.label)){output$display3 <- renderText({c("Category: ",toString(unique(cat.label))) })}
    else {output$display3 <- renderText({ toString("All categories") })}
    if(!is.null(subcat.label)){output$display4 <- renderText({c("Subcategory: ", toString(unique(subcat.label))) })}
    else {output$display4 <- renderText({ toString("All subcategories") })}
    if(!is.null(reg.label)){output$display5 <- renderText({c("Region: ",toString(unique(reg.label))) })}
    else {output$display5 <- renderText({ toString("All regions") })}
    if(!is.null(country.label)){output$display6 <- renderText({ c("Country: ",toString(unique(country.label))) })}
    else {output$display6 <- renderText({ toString("All countries") })}
  
    # filter Category and SubCategory
    if (is.null(cat.label) & is.null(subcat.label)){#x <- character(0)
      cat.label <- unique(work1$Category)
      subcat.label <- unique(work1$SubCategory)
    }
    if (is.null(cat.label) & !is.null(subcat.label)){#x <- character(0)
      cat.label <- unique(work1$Category)
    }
    if (!is.null(cat.label) & is.null(subcat.label)){#x <- character(0)
      subcat.label <- unique(work1$SubCategory)
    }

    # filter region and country
    if (is.null(reg.label) & is.null(country.label)){#x <- character(0)
      reg.label <- unique(work1$Region)
      country.label <- unique(work1$Country)
    }
    if (is.null(reg.label) & !is.null(country.label)){#x <- character(0)
      reg.label <- unique(work1$Region)
    }
    if (!is.null(reg.label) & is.null(country.label)){#x <- character(0)
      country.label <- unique(work1$Country)
    }
    

    
    #work2 %>% filter(Category %in% cat.label) %>% select(AE,MShre,TE) %>% summarise(correl_AE=cor(AE,MShre),correl_EE=cor(TE,MShre))
    x1<-work1 %>% filter(Category %in% cat.label, SubCategory %in% subcat.label, Region %in% reg.label, Country %in% country.label) %>% select(AE,MShre,TE) %>% summarise(correl_AE=cor(AE,MShre))
    x2<-work2 %>% filter(Category %in% cat.label, SubCategory %in% subcat.label, Region %in% reg.label, Country %in% country.label) %>% select(AE,MShre,TE) %>% summarise(correl_EE=cor(TE,MShre))
    x3<-work1 %>% filter(Category %in% cat.label, SubCategory %in% subcat.label, Region %in% reg.label, Country %in% country.label) %>% select(AE) %>% summarise(count=n())
    x4<-work1 %>% filter(Category %in% cat.label, SubCategory %in% subcat.label, Region %in% reg.label, Country %in% country.label) %>% select(Project) %>% summarise(count1=length(unique(Project)))
    #c(x1,x2,x3)
    table<-cbind(x1,x2,x3,x4)
    colnames(table)<-c("Cor(AE,Sh)","Cor(EE,Sh)","Count of brands","Count of studies")
    data.frame(table)
    #return(table)
   })
  
  output$tab_correl<-renderTable({
    
    correl()
    
  })
  
  # observeEvent(input$go1, {
  #   setwd("C:/Users/DKantu01/OneDrive/Programming/R/Shiny/Validations R Shiny")
  #   write.csv(correl(),file = "Correl_Table.csv")
  # })
  
  
  output$download <- downloadHandler(
    filename = function(){"thename.csv"}, 
    content = function(fname){
      write.csv(correl(), fname)
    }
  )
 

}
