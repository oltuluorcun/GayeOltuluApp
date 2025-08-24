library(shiny)
library(shinyalert)
library(DT)
library(shinyjs)
library(shinydashboard)

# Yeni Diyet UI
newDietUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
        .fancy-button {
          background: linear-gradient(45deg, #667eea 0%, #764ba2 100%);
          border: none;
          border-radius: 25px;
          color: white;
          padding: 12px 30px;
          font-size: 16px;
          font-weight: bold;
          box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);
          transition: all 0.3s ease;
          margin-bottom: 20px;
        }
        .fancy-button:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(102, 126, 234, 0.6);
          background: linear-gradient(45deg, #764ba2 0%, #667eea 100%);
        }
        .diet-table-container {
          background: white;
          border-radius: 15px;
          box-shadow: 0 10px 30px rgba(0,0,0,0.1);
          padding: 20px;
          margin-top: 20px;
        }
        .custom-textarea {
          width: 100%;
          min-height: 80px;
          padding: 8px;
          border: 2px solid #667eea;
          border-radius: 4px;
          font-family: inherit;
          font-size: 14px;
          resize: vertical;
        }
        .edit-buttons {
          margin-top: 8px;
          text-align: right;
        }
        .btn-save, .btn-cancel {
          margin-left: 5px;
          padding: 4px 12px;
          border: none;
          border-radius: 4px;
          cursor: pointer;
          font-size: 12px;
        }
        .btn-save {
          background-color: #28a745;
          color: white;
        }
        .btn-cancel {
          background-color: #6c757d;
          color: white;
        }
        .calorie-summary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 15px;
          border-radius: 10px;
          text-align: center;
          font-size: 18px;
          font-weight: bold;
          margin-bottom: 15px;
          box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
        }
        .table-header {
          color: #667eea;
          font-size: 24px;
          font-weight: bold;
          margin-bottom: 15px;
          text-align: center;
        }
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_processing,
        .dataTables_wrapper .dataTables_paginate {
          color: #667eea;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #667eea !important;
        }
      "))
    ),
    
    fluidRow(
      column(12,
             div(class = "text-center",
                 actionButton(ns("ogun_ekle"), "🍽️ Öğün Ekle", class = "fancy-button")
             )
      )
    ),
    
    fluidRow(
      column(12,
             div(class = "diet-table-container",
                 div(class = "table-header", "📋 Günlük Diyet Planı"),
                 conditionalPanel(
                   condition = "output.show_summary",
                   ns = ns,
                   div(class = "calorie-summary",
                       textOutput(ns("calorie_summary"))
                   )
                 ),
                 DTOutput(ns("diet_table"))
             )
      )
    )
  )
}

# Yeni Diyet Server
newDietServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      data = data.frame(
        Saat = character(),
        Öğün = character(),
        Açıklama = character(),
        YaklasikKalori = numeric(),
        stringsAsFactors = FALSE
      ),
      editing_row = NULL,
      original_value = NULL
    )
    
    # Show summary condition
    output$show_summary <- reactive({
      nrow(rv$data) > 0
    })
    outputOptions(output, "show_summary", suspendWhenHidden = FALSE)
    
    # Calorie summary
    output$calorie_summary <- renderText({
      if(nrow(rv$data) > 0) {
        total_calories <- sum(rv$data$YaklasikKalori, na.rm = TRUE)
        paste("🔥 Toplam Kalori:", total_calories, "kcal")
      }
    })
    
    # Öğün ekle butonu
    observeEvent(input$ogun_ekle, {
      shinyalert(
        title = "🍽️ Yeni Öğün Ekle",
        html = TRUE,
        text = tagList(
          tags$style(HTML("
            .swal-content__input {
              margin: 10px 0;
              padding: 10px;
              border: 2px solid #e0e0e0;
              border-radius: 8px;
              font-size: 14px;
            }
            .swal-content__input:focus {
              border-color: #667eea;
              outline: none;
              box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
            }
          ")),
          div(style = "text-align: left; padding: 10px;",
              tags$label("🕐 Saat:", style = "font-weight: bold; color: #667eea;"),
              textInput(ns("meal_time"), NULL, placeholder = "Örn: 08:00"),
              
              tags$label("🍽️ Öğün Adı:", style = "font-weight: bold; color: #667eea;"),
              textInput(ns("meal_name"), NULL, placeholder = "Örn: Kahvaltı"),
              
              tags$label("📝 Açıklama:", style = "font-weight: bold; color: #667eea;"),
              textAreaInput(ns("meal_desc"), NULL, 
                            placeholder = "Yiyecek detaylarını buraya yazın...", 
                            rows = 3),
              
              tags$label("🔥 Yaklaşık Kalori:", style = "font-weight: bold; color: #667eea;"),
              numericInput(ns("meal_kcal"), NULL, value = 0, min = 0)
          )
        ),
        showCancelButton = TRUE,
        confirmButtonText = "➕ Ekle",
        cancelButtonText = "❌ İptal",
        callbackR = function(x) {
          if(x == TRUE) {
            # Add the meal when confirmed
            req(input$meal_time, input$meal_name)
            
            rv$data <- rbind(rv$data, data.frame(
              Saat = input$meal_time,
              Öğün = input$meal_name,
              Açıklama = ifelse(is.null(input$meal_desc) || input$meal_desc == "", 
                                "Açıklama eklenmedi", input$meal_desc),
              YaklasikKalori = ifelse(is.null(input$meal_kcal), 0, input$meal_kcal),
              stringsAsFactors = FALSE
            ))
            
            # Clear inputs
            updateTextInput(session, "meal_time", value = "")
            updateTextInput(session, "meal_name", value = "")
            updateTextAreaInput(session, "meal_desc", value = "")
            updateNumericInput(session, "meal_kcal", value = 0)
            
            showNotification("✅ Öğün başarıyla eklendi!", type = "message", duration = 3)
          }
        }
      )
    })
    
    
    
    # Custom description editing
    observeEvent(input$edit_description, {
      row_num <- as.numeric(input$edit_description)
      if(row_num > 0 && row_num <= nrow(rv$data)) {
        rv$editing_row <- row_num
        rv$original_value <- rv$data[row_num, "Açıklama"]
        
        shinyalert(
          title = paste("📝 Açıklama Düzenle - Satır", row_num),
          html = TRUE,
          text = tagList(
            div(
              style = "text-align: left;",
              tags$label("Açıklama (Enter ile yeni satır):", style = "font-weight: bold; margin-bottom: 10px; display: block;"),
              tags$textarea(
                id = ns("description_textarea"),
                style = "width: 100%; min-height: 100px; padding: 10px; border: 2px solid #667eea; border-radius: 5px; font-family: inherit; resize: vertical;",
                placeholder = "Açıklama yazın... (Enter ile yeni satır)",
                rv$data[row_num, "Açıklama"]
              )
            )
          ),
          showCancelButton = TRUE,
          confirmButtonText = "💾 Kaydet",
          cancelButtonText = "❌ İptal",
          callbackR = function(x) {
            if(x == TRUE && !is.null(rv$editing_row)) {
              # Use JavaScript to get the actual textarea value and trigger an input event
              shinyjs::runjs(paste0("
                var textarea = document.getElementById('", ns("description_textarea"), "');
                if(textarea && textarea.value !== undefined) {
                  Shiny.setInputValue('", ns("temp_description_value"), "', textarea.value, {priority: 'event'});
                } else {
                  // Fallback - try to find textarea by different method
                  var textareas = document.getElementsByTagName('textarea');
                  if(textareas.length > 0) {
                    Shiny.setInputValue('", ns("temp_description_value"), "', textareas[0].value, {priority: 'event'});
                  }
                }
              "))
            } else {
              rv$editing_row <- NULL
              rv$original_value <- NULL
            }
          }
        )
      }
    })
    
    # Handle description update
    observeEvent(input$temp_description_value, {
      if(!is.null(rv$editing_row) && rv$editing_row <= nrow(rv$data)) {
        new_value <- input$temp_description_value
        if(!is.null(new_value) && new_value != "") {
          rv$data[rv$editing_row, "Açıklama"] <- new_value
          showNotification("📝 Açıklama başarıyla güncellendi!", type = "message", duration = 3)
        }
        rv$editing_row <- NULL
        rv$original_value <- NULL
      }
    })
    
    # Cell edit handling (for non-description columns)
    observeEvent(input$diet_table_cell_edit, {
      info <- input$diet_table_cell_edit
      i <- info$row
      j <- info$col + 1  # DT is 0-indexed, R is 1-indexed
      v <- info$value
      
      # Don't allow editing the delete button column and description column
      if(j <= 4 && j != 3) {  # Skip description column (j=3)
        if(j == 4) {  # Kalori column - ensure numeric
          v <- as.numeric(v)
          if(is.na(v)) v <- 0
        }
        rv$data[i, j] <- v
        showNotification("📝 Güncelleme kaydedildi", type = "message", duration = 2)
      }
    })
    
    # Delete row function
    observeEvent(input$delete_row, {
      row_to_delete <- as.numeric(input$delete_row)
      if(row_to_delete > 0 && row_to_delete <= nrow(rv$data)) {
        rv$data <- rv$data[-row_to_delete, ]
        showNotification("🗑️ Öğün silindi", type = "warning", duration = 3)
      }
    })
    
    # Enhanced DT table
    output$diet_table <- renderDT({
      if(nrow(rv$data) == 0) {
        return(datatable(
          data.frame("Henüz öğün eklenmedi..." = "Yukarıdaki butona tıklayarak öğün ekleyebilirsiniz"),
          rownames = FALSE,
          options = list(
            dom = 't',
            ordering = FALSE,
            searching = FALSE,
            info = FALSE
          )
        ))
      }
      
      # Add edit buttons for descriptions and delete buttons
      data_with_buttons <- rv$data
      data_with_buttons$Açıklama <- paste0(
        gsub("\n", "<br>", rv$data$Açıklama),
        '<br><button class="btn btn-info btn-sm" onclick="Shiny.setInputValue(\'',
        ns('edit_description'), '\', ', 1:nrow(rv$data), 
        ', {priority: \'event\'})" style="margin-top: 5px; font-size: 10px;">',
        '✏️ Düzenle</button>'
      )
      data_with_buttons$Sil <- paste0(
        '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'',
        ns('delete_row'), '\', ', 1:nrow(rv$data), 
        ', {priority: \'event\'})" style="border-radius: 50%; width: 30px; height: 30px;">',
        '🗑️</button>'
      )
      
      # Add subtotal row
      subtotal_row <- data.frame(
        Saat = "",
        Öğün = "",
        Açıklama = "TOPLAM",
        YaklasikKalori = sum(rv$data$YaklasikKalori, na.rm = TRUE),
        Sil = "",
        stringsAsFactors = FALSE
      )
      
      final_data <- rbind(data_with_buttons, subtotal_row)
      
      # Convert line breaks to HTML for display
      final_data$Açıklama <- gsub("\n", "<br>", final_data$Açıklama)
      
      datatable(
        final_data,
        rownames = FALSE,
        escape = c(TRUE, TRUE, FALSE, TRUE, FALSE),  # Allow HTML in description and delete buttons
        editable = list(
          target = 'cell',
          disable = list(columns = c(2, 4))  # Disable editing of description and delete columns
        ),
        options = list(
          pageLength = 15,
          dom = 'frtip',
          ordering = TRUE,
          searching = TRUE,
          info = TRUE,
          autoWidth = FALSE,  # Fixed width
          scrollX = TRUE,
          columnDefs = list(
            list(targets = 0, width = "80px", className = "dt-center"),
            list(targets = 1, width = "120px", className = "dt-center"),
            list(targets = 2, width = "300px", className = "dt-left", 
                 render = JS("function(data, type, row, meta) {
                   if(type === 'display' || type === 'type') {
                     return data;
                   }
                   return data.replace(/<br\\s*\\/?>/gi, '\\n');
                 }")),
            list(targets = 3, width = "120px", className = "dt-center"),
            list(targets = 4, width = "60px", className = "dt-center", orderable = FALSE),
            list(targets = nrow(final_data) - 1, className = "summary-row")  # Style subtotal row
          ),
          initComplete = JS("
            function(settings, json) {
              $(this.api().table().container()).find('table').addClass('table-striped table-hover');
              $('.summary-row').css({
                'background-color': '#f8f9fa',
                'font-weight': 'bold',
                'border-top': '2px solid #667eea'
              });
              
              // Fix table width on initialization
              $(this.api().table().container()).css('width', '100%');
              $(this.api().table().node()).css('width', '100%');
            }
          "),
          drawCallback = JS("
            function(settings) {
              // Maintain width after each redraw
              $(this.api().table().container()).css('width', '100%');
              $(this.api().table().node()).css('width', '100%');
            }
          ")
        )
      ) %>%
        formatStyle(
          columns = c("Saat", "Öğün", "Açıklama", "YaklasikKalori"),
          backgroundColor = "#fafafa",
          border = "1px solid #e0e0e0"
        )
    }, server = TRUE)  # Enable server-side processing for editing
  })
}