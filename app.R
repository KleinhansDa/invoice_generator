library(tidyverse)
library(magick)
library(shiny)
library(kableExtra)

options(knitr.kable.NA = '')
options(scipen = 999)

path <- '.\\data'
dir_app <- getwd()
details_contractors <- read_csv2(paste0(path,'\\', 'details_contractors.csv'), col_types = cols(phone = col_character(), ac_no = col_character()))
details_clients <- read_csv2(paste0(path,'\\', 'details_clients.csv'))

choices_contractors <- details_contractors[['name']]
choices_clients <- details_clients[['name']]

shinyApp(
  ui = fluidPage(
    titlePanel('Invoice Generator'),
    sidebarLayout(
      sidebarPanel(
        h4('Contractor'),
        selectInput('name_contractor', NULL, choices = choices_contractors),
        h4('Client'),
        selectInput('name_client', NULL, choices = choices_clients),
        hr(),
        h4('Invoice'),
        numericInput('salary', 'Salary (per hour)', 50),
        numericInput('days', 'Hours', 1),
        textInput('item', 'Item', 'item name'),
        textAreaInput('item_desc', 'Description', 'item description'),
        hr(),
        downloadButton("download_invoice", "Generate invoice")
        ),
      mainPanel(
        fluidRow(
          h3('Details'),
          column(3, 
            h4('Contractor'),
            htmlOutput('adress_contractor')
            ),
          column(3, imageOutput('signature', height = "150px")),
          ),
        fluidRow(
          column(3, 
            h4('Client'),
            htmlOutput('adress_client'))
        ),
        hr(),
        fluidRow(
          h3('Positions'),
          column(12, htmlOutput('table'), htmlOutput('table_total'))
        )
        )
      )
    ),
  server = function(input, output) {
    
    # client details
    
    output$adress_client <- renderText({
      line <- line <- details_clients %>% filter(name == input$name_client)
      markdown(
        paste(input$name_client, '</br>', line[['addr_l1']], '</br>', line[['addr_l2']], line[['addr_l3']])
        )
    })
    
    # contractor details
    
    output$adress_contractor <- renderText({
      line <- line <- details_contractors %>% filter(name == input$name_contractor)
      markdown(
        paste(input$name_contractor, '</br>', line[['addr_l1']], '</br>', line[['addr_l2']], line[['addr_l3']])
        )
    })
    
    output$signature <- renderImage({
      tmpfile <- 
        magick::image_read(paste0(path, "\\", input$name_contractor,'.png')) %>%
        image_trim() %>% 
        image_scale('x100') %>% 
        magick::image_write(tempfile(fileext = 'png'), format = 'png')
      
      list(
        src = tmpfile,
        contentType = "image/png"
        )
    }, deleteFile = T)
    
    output$table <- renderText({
      
      items <- 
        tibble(
          hours = as.numeric(input$days),
          rate = as.numeric(input$salary),
          item = input$item,
          item_desc = input$item_desc
          ) %>% 
        mutate(
          id = row_number(),
          amount = round(hours * rate, 2)
          ) 
      
      items %>%
        select(id, item, item_desc, hours, rate, amount) %>%
        mutate(
          rate = paste('€', format(rate, digits = 2, nsmall = 1, big.mark = '.', decimal.mark = ',')),
          amount = paste('€', format(amount, digits = 2, nsmall = 1, big.mark = '.', decimal.mark = ','))
        ) %>% 
        knitr::kable(booktabs = T, col.names = c('#', 'Item', 'Description', 'Hours', 'Rate', 'Amount'), align = c('c', 'l', 'l', 'r', 'r', 'r'), table.attr = "style='width:90%;'") %>% 
        kable_styling(bootstrap_options = c('hover', 'condensed', 'responsive'), position = 'left') %>% 
        row_spec(0, bold = T) %>% 
        column_spec(1, width = '.5cm') %>%
        column_spec(2, width = '2.5cm') %>%
        column_spec(3, width = '3cm') %>%
        column_spec(c(4, 5), width = '1.5cm') %>%
        column_spec(6, width = '2cm')
    })
    
    output$table_total <- renderText({
      
      items <- 
        tibble(
          hours = as.numeric(input$days),
          rate = as.numeric(input$salary),
          item = input$item,
          item_desc = input$item_desc
        ) %>% 
        mutate(
          id = row_number(),
          amount = hours * rate
        ) 
      
      sums <- 
        items %>% 
        group_by() %>%
        summarize(amount = sum(amount)) %>% 
        mutate(id = '', item = '', item_desc = '', hours = '', rate = 'Total')
      
      sums %>% 
        select(id, item, item_desc, hours, rate, amount) %>%
        mutate(amount = paste('€', format(amount, digits = 2, nsmall = 1, big.mark = '.', decimal.mark = ','))) %>% 
        knitr::kable(booktabs = T, col.names = NULL, align = c('r', 'r', 'r', 'r', 'r', 'r'), table.attr = "style='width:90%;'") %>% 
        kable_styling(bootstrap_options = c('hover', 'condensed', 'responsive'), position = 'left') %>% 
        row_spec(1, bold = T, color = "white", background = "black") %>% 
        column_spec(1, width = '.5cm') %>%
        column_spec(2, width = '2.5cm') %>%
        column_spec(3, width = '3cm') %>%
        column_spec(c(4, 5), width = '1.5cm') %>%
        column_spec(6, width = '2cm')
    })
    
    output$download_invoice <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function() {paste0('invoice_', str_to_lower(input$name_client) %>% str_remove(' .*'), '_', format(Sys.Date(), '%y%m'), '.pdf')},
      content = function(file) {
        withProgress(message = 'Knitting', detail = 'one moment please ..', {
            ## End of progression
            #src <- normalizePath("S:/Aaron/R/Apps/Sample Illustrative Lineup Application/summary_PDF.Rmd")
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            #owd <- setwd(tempdir())
            #on.exit(setwd(owd))
            #file.copy(src,"summary_PDF.Rmd", overwrite = TRUE)
            out <- rmarkdown::render(
              #tempReport,
              "report.Rmd",
              #output_dir = dir_app,
              #output_file = paste0(dir_app, '/', file),
              #output_format = 'pdf_document',
              #envir = new.env(parent = globalenv()),
              params = list(
                contractor = list(
                  name = input$name_contractor,
                  phone = details_contractors %>% filter(name == input$name_contractor) %>% pull(phone),
                  mail = details_contractors %>% filter(name == input$name_contractor) %>% pull(mail),
                  salary = input$salary,
                  address_street = details_contractors %>% filter(name == input$name_contractor) %>% pull(addr_l1),
                  address_pcode = details_contractors %>% filter(name == input$name_contractor) %>% pull(addr_l2),
                  address_city = details_contractors %>% filter(name == input$name_contractor) %>% pull(addr_l3),
                  bic = details_contractors %>% filter(name == input$name_contractor) %>% pull(bic),
                  ben_name = details_contractors %>% filter(name == input$name_contractor) %>% pull(ben_name),
                  ac_no = details_contractors %>% filter(name == input$name_contractor) %>% pull(ac_no),
                  bank_ac_name = details_contractors %>% filter(name == input$name_contractor) %>% pull(bank_ac_name),
                  bank_branch_addr = details_contractors %>% filter(name == input$name_contractor) %>% pull(bank_branch_addr),
                  signature = image_read(paste0(dir_app, '\\', path, "\\", input$name_contractor,'.png')) %>% image_trim()
                ),
                client = list(
                  name = input$name_client,
                  address_street = details_clients %>% filter(name == input$name_client) %>% pull(addr_l1),
                  address_pcode = details_clients %>% filter(name == input$name_client) %>% pull(addr_l2),
                  address_city = details_clients %>% filter(name == input$name_client) %>% pull(addr_l3),
                  phone = details_clients %>% filter(name == input$name_client) %>% pull(phone),
                  mail = details_clients %>% filter(name == input$name_client) %>% pull(mail)
                ),
                item = list(
                  days = as.integer(input$days),
                  name = input$item,
                  desc = input$item_desc
                )
              )
            )
            file.rename(out, file)
            })
        #file.copy(file.path(tempdir(), 'report.pdf'), file)
      }
    )
  }
)