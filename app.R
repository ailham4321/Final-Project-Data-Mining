#setwd("C:/Users/Asem/Downloads/Coba Dashboard/Final-Project-Data-Mining")

library(shiny)
library(bs4Dash)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(htmltools)
library(plotly)
library(psych)
library(DT)
library(randomForest)
library(caret)
library(pROC)
library(shinyalert)

### Custom

mydateInput <- function(inputId, label, value = NULL, min = NULL, max = NULL,
                        format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", minviewmode="months",
                        width = NULL) {
  
  # If value is a date object, convert it to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  htmltools::attachDependencies(
    tags$div(id = inputId,
             class = "shiny-date-input form-group shiny-input-container",
             style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
             
             controlLabel(inputId, label),
             tags$input(type = "text",
                        # datepicker class necessary for dropdown to display correctly
                        class = "form-control datepicker",
                        `data-date-language` = language,
                        `data-date-weekstart` = weekstart,
                        `data-date-format` = format,
                        `data-date-start-view` = startview,
                        `data-date-min-view-mode` = minviewmode,
                        `data-min-date` = min,
                        `data-max-date` = max,
                        `data-initial-date` = value
             )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (identical(!is.null(x), !is.na(x)))
    if (identical(!is.null(y), !is.na(y)))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

datePickerDependency <- htmlDependency(
  "bootstrap-datepicker", "1.0.2", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/datepicker.css")


dateRangeMonthsInput <- function(inputId, label, start = NULL, end = NULL,
                                 min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
                                 minviewmode="months", # added manually
                                 weekstart = 0, language = "en", separator = " to ", width = NULL) {
  
  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%Y-%m-%d")
  if (inherits(end,   "Date"))  end   <- format(end,   "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  htmltools::attachDependencies(
    div(id = inputId,
        class = "shiny-date-range-input form-group shiny-input-container",
        style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
        
        controlLabel(inputId, label),
        # input-daterange class is needed for dropdown behavior
        div(class = "input-daterange input-group",
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = start
            ),
            span(class = "input-group-addon", separator),
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = end
            )
        )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (identical(!is.null(x), !is.na(x)))
    if (identical(!is.null(y), !is.na(y)))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

# the datePickerDependency is taken from https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmltools::htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See #1346.
  head = "<script>
   (function() {
   var datepicker = $.fn.datepicker.noConflict();
   $.fn.bsDatepicker = datepicker;
   })();
   </script>")


###

df = read.csv("Datasets/Hotel Reservations.csv")
df_test = read.csv("Datasets/test_data.csv")
rf_model = readRDS("rf_model.Rdata")

numerical_columns = c("no_of_adults", "no_of_children", 
                      "no_of_weekend_nights", "no_of_week_nights", 
                      "lead_time", 
                      "no_of_previous_cancellations", "no_of_previous_bookings_not_canceled", 
                      "avg_price_per_room")

categorical_columns = c("type_of_meal_plan", "room_type_reserved", 
                        "market_segment_type", "booking_status",
                        "required_car_parking_space", "repeated_guest",
                        "no_of_special_requests")
categorical_columns_book = c("type_of_meal_plan", "room_type_reserved", 
                        "market_segment_type",
                        "required_car_parking_space",
                        "no_of_special_requests")

df = df %>%
  mutate(arrival_date = lubridate::make_date(arrival_year, arrival_month, arrival_date))%>%
  select(-c(arrival_year, arrival_month))%>%
  mutate(across(all_of(categorical_columns), as.factor))%>%
  mutate(booking_date = arrival_date-lead_time)%>%
  na.omit()



### SQL

### END OF SQL

header = dashboardHeader(title = "Menu", titleWidth = 200)


sidebar = bs4DashSidebar(vertical=F,
                         width=200,
                         sidebarMenu(
                           menuItem("Halaman Utama", 
                                    tabName = "home", 
                                    icon = icon("home")),
                           menuItem("Executive Report", 
                                    tabName = "report", 
                                    icon = icon("th-list"),
                                    menuSubItem("Summary Statistics", tabName = "summary"),
                                    menuSubItem("Key Insights (Linechart)", tabName = "visualization"),
                                    menuSubItem("Interactive Plot", tabName = "interactive_plot")),
                           menuItem("Booking System", 
                                    tabName = "booking", 
                                    icon = icon("book"))
                         )
)


body = bs4DashBody(
  tabItems(
    tabItem(
      tabName = "home",
      fluidRow(
        h1(strong("Selamat Datang di The Hotel"), style="text-align:center; width: 100%;")
      ),
      fluidRow(
        box(title = "Kata Pengantar",
            HTML(
              "
              <div style='text-align: justify;'>
              <p>
              Dataset hotel memiliki atribut 
              <a href='https://creativecommons.org/licenses/by/4.0/'>Lisensi Internasional</a>. Untuk keperluan Tugas Akhir Mata Kuliah Data Mining dan Visualisasi B, kami mempersembahkan data ini dalam bentuk dashboard serta beberapa analisis yang diperlukan dalam bentuk interaktif dan dapat diakses melalui laman <a href='intip.in/RShinyDatmin'>Dashboard Ilham dan Farham</a>. Permasalahan ini dimulai ketikan kanal reservasi hotel online telah secara dramatis mengubah kemungkinan pemesanan dan perilaku pelanggan. Sejumlah besar reservasi hotel dibatalkan karena pembatalan atau tidak muncul. Alasan tipikal untuk pembatalan termasuk perubahan rencana, konflik jadwal, dll. Hal ini seringkali menjadi lebih mudah dilakukan dengan opsi tanpa biaya atau lebih baik lagi dengan biaya rendah yang menguntungkan bagi tamu hotel, tetapi merupakan faktor yang kurang diinginkan dan mungkin mengurangi pendapatan bagi hotel untuk menghadapinya.
              </p>
              <strong>Berikut merupakan metadata dashboard:</strong>
          <ul>
            <li><strong>Booking_ID:</strong> pengidentifikasi unik dari setiap pemesanan</li>
            <li><strong>no_of_adults:</strong> Jumlah dewasa</li>
            <li><strong>no_of_children:</strong> Jumlah anak-anak</li>
            <li><strong>no_of_weekend_nights:</strong> Jumlah malam akhir pekan (Sabtu atau Minggu) yang diinapkan atau dipesan oleh tamu di hotel</li>
            <li><strong>no_of_week_nights:</strong> Jumlah malam biasa (Senin hingga Jumat) yang diinapkan atau dipesan oleh tamu di hotel</li>
            <li><strong>type_of_meal_plan:</strong> Tipe rencana makan yang dipesan oleh pelanggan</li>
            <li><strong>required_car_parking_space:</strong> Apakah pelanggan memerlukan tempat parkir mobil? (0 - Tidak, 1 - Ya)</li>
            <li><strong>room_type_reserved:</strong> Tipe kamar yang dipesan oleh pelanggan</li>
            <li><strong>lead_time:</strong> Jumlah hari antara tanggal pemesanan dan tanggal kedatangan</li>
            <li><strong>arrival_year:</strong> Tahun tanggal kedatangan</li>
            <li><strong>arrival_month:</strong> Bulan tanggal kedatangan</li>
            <li><strong>arrival_date:</strong> Tanggal dari bulan tersebut</li>
            <li><strong>market_segment_type:</strong> Penunjukan segmen pasar</li>
            <li><strong>repeated_guest:</strong> Apakah pelanggan merupakan tamu yang berulang? (0 - Tidak, 1 - Ya)</li>
            <li><strong>no_of_previous_cancellations:</strong> Jumlah pemesanan sebelumnya yang dibatalkan oleh pelanggan sebelum pemesanan saat ini</li>
            <li><strong>no_of_previous_bookings_not_canceled:</strong> Jumlah pemesanan sebelumnya yang tidak dibatalkan oleh pelanggan sebelum pemesanan saat ini</li>
            <li><strong>avg_price_per_room:</strong> Harga rata-rata per hari dari reservasi; harga kamar dinamis. (dalam euro)</li>
            <li><strong>no_of_special_requests:</strong> Jumlah total permintaan khusus yang dibuat oleh pelanggan (mis. lantai tinggi, pemandangan dari kamar, dll)</li>
            <li><strong>booking_status:</strong> Tanda menunjukkan apakah pemesanan dibatalkan atau tidak</li>
          </ul>
          </div>
        "
              )
            , width = 12, collapsible = F)
      ),
      fluidRow(
        h2(strong("Identitas Pembuat"), style="text-align:center; width: 100%;")
      ),
      fluidRow(
        box(
          title = "Abdillah Ilham (5003211069)",
          imageOutput("ilham"),
          width = 6,
          style = "text-align: center; height: 550px;",
          collapsible = F
        ),
        box(
          title = "Farham Ramadhani (5003211165)",
          imageOutput("farham"),
          width = 6,
          style = "text-align: center; height: 550px;",
          collapsible = F
        )
      )
    ),
    tabItem(
      tabName = "summary",
      tabBox(
        title = "Statistika Deskriptif", width = 12,
        tabPanel(
          "Summary",
          fluidRow(
            h1(strong("Summary The Hotel"), style="text-align:center; width: 100%;")
          ),
          fluidRow(
            infoBoxOutput("jumlah_kunjungan", width = 4),
            infoBoxOutput("profit", width = 4),
            infoBoxOutput("jumlah_pembatalan", width = 4)
          ),
          fluidRow(
            box(title = "Numerical Summary",
                DTOutput("numerical_summary", width = "90%"),
                width=12, collapsible = F)
          ),
          fluidRow(
            box(title = "Categorical Summary",
                verbatimTextOutput('categorical_summary'),
                width=12, collapsible = F)
          )
        ),
        tabPanel(
          "Interactive Chi-Square Test",
          fluidRow(
            h1(strong("Chi Square Test"), style="text-align:center; width: 100%;")
          ),
          fluidRow(
            box(
              selectizeInput("input_chi_sq", label = "Pilih Variabel", choices = categorical_columns, selected = "booking_status", multiple=TRUE, options = list(maxItems=2)),
              width = 12,
              collapsible = F,
              verbatimTextOutput('chi_sq_test')
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "visualization",
      fluidRow(
        h1(strong("Dashboard The Hotel"), style="text-align:center; width: 100%;")
      ),
      fluidRow(
        box(tittle="Room Demand",
            plotlyOutput("room_demand"),
            fluidRow(
              box(dateRangeInput("room_demand_date", label="Pilih Tanggal", min = min(df$arrival_date, na.rm = T), max = max(df$arrival_date, na.rm = T), start = max(df$arrival_date, na.rm = T)-60, end = max(df$arrival_date, na.rm = T)), width = 6, collapsible = F),
              box(selectizeInput("room_type_demand", label = "Room Type", choices = unique(df$room_type_reserved), selected = "Room_Type 1", multiple = T), width = 6, collapsible = F)
            ),
            width=12, collapsible = F
        )
      ),
      fluidRow(
        box(title="Room Price based on Arrival Date", 
            plotlyOutput("room_price"),
            dateRangeMonthsInput('room_price_date',label = "Pilih Tanggal",format = "mm/yyyy",start = min(df$arrival_date, na.rm = T), end=max(df$arrival_date, na.rm = T)), 
            width=12, collapsible = F)
      ),
      fluidRow(
        box(tittle="Book Demand",
            plotlyOutput("booking_demand"),
            fluidRow(
              box(dateRangeInput("book_demand_date", label="Pilih Tanggal", min = min(df$booking_date, na.rm = T), max = max(df$booking_date, na.rm = T), start = max(df$booking_date, na.rm = T)-60, end = max(df$booking_date, na.rm = T)), width = 6, collapsible = F),
              box(selectizeInput("room_type_book", label = "Room Type", choices = unique(df$room_type_reserved), selected = "Room_Type 1", multiple = T), width = 6, collapsible = F)
            ),
            width=12, collapsible = F
        )
      )
    ),
    tabItem(
      tabName = "interactive_plot",
      fluidRow(
        h1(strong("Interactive Plot"), style="text-align:center; width: 100%;")
      ),
      tabBox(
        title = "Interactive Plot", width = 12,
        tabPanel(
          "Numerical Plot",
          box(selectInput("input_interactive_numerical_plot", label = "Variabel Numerik", choices = numerical_columns, selected = "avg_price_per_room", selectize = TRUE), width = 12, collapsible = F),
          box(title = "Histogram",
            sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
            plotlyOutput("interactive_histogram"),
            width = 12, collapsible = F
          ),
          box(title = "BoxPlot",
              plotlyOutput("interactive_boxplot"),
              width = 12, collapsible = F
          ),
          box(title = "Histogram",
              fluidRow(
                box(selectInput("input_interactive_numerical_plot1", label = "Variabel Numerik 1", choices = numerical_columns, selected = "avg_price_per_room", selectize = TRUE), width = 6, collapsible = F),
                box(selectInput("input_interactive_numerical_plot2", label = "Variabel Numerik 2", choices = numerical_columns, selected = "lead_time", selectize = TRUE), width = 6, collapsible = F)
              ),
              plotlyOutput("interactive_2d_plot"),
              width = 12, collapsible = F
          )
        ),
        tabPanel(
          "Count Plot",
          box(selectizeInput("input_interactive_count_plot", label = "Variabel Kategorik", choices = categorical_columns, selected = "booking_status", multiple = T, options = list(maxItems=2)), width = 12, collapsible = F),
          box(plotlyOutput("interactive_barplot"), width = 12, collapsible = F),
          box(plotlyOutput("interactive_piechart"), width = 12, collapsible = F)
        ),
        tabPanel(
          "Numerical and Categorical",
          box(
            fluidRow(
              box(selectInput("input_interactive_numerical_categorical1", label = "Variabel Kategorik", choices = categorical_columns, selected = "booking_status", selectize = TRUE), width = 6, collapsible = F),
              box(selectInput("input_interactive_numerical_categorical2", label = "Variabel Numerik", choices = numerical_columns, selected = "booking_status", selectize = TRUE), width = 6, collapsible = F)
            ),
            plotlyOutput("interactive_violin_plot"),
            width = 12, collapsible = F
          )
        )
      )
    ),
    tabItem(
      tabName = "booking",
      fluidRow(
        h1(strong("Sistem Booking The Hotel"), style="text-align:center; width: 100%;")
      ),
      fluidRow(
        box(
          title = "Masukkan Data Booking",
          width = 12,
          collapsible = F,
          numericInput("no_of_adults", "Jumlah Dewasa", value = 1, min = 1, max = 20),
          numericInput("no_of_children", "Jumlah Anak", value = 0, min = 0, max = 20),
          numericInput("no_of_weekend_nights", "Jumlah Menginap Akhir Pekan", value = 0, min = 0, max = 30),
          numericInput("no_of_week_nights", "Jumlah Menginap Weekday", value = 1, min = 1, max = 30),
          numericInput("required_car_parking_space", "Jumlah Parkir Mobil", value = 1, min = 1, max = 10),
          dateInput("booking_date", label = "Pilih Tanggal Booking", value =  min(na.omit(df$booking_date)), min = min(na.omit(df$booking_date)), max = max(na.omit(df$booking_date))),
          dateInput("arrival_date", label = "Pilih Tanggal Menginap", value =  min(na.omit(df$arrival_date)), min = min(na.omit(df$arrival_date)), max = max(na.omit(df$arrival_date))),
          
          selectInput("type_of_meal_plan", "Paket Makan", choices = levels(df$type_of_meal_plan)),
          selectInput("room_type_reserved", "Tipe Kamar", choices = levels(df$room_type_reserved)),
          selectInput("market_segment_type", "Tipe Segmen Pasar", choices = levels(df$market_segment_type)),
          
          numericInput("avg_price_per_room", "Harga Rata-rata per Kamar", value = 100, min = 20, max = 1000),
          numericInput("no_of_special_requests", "Jumlah Permintaan Khusus", value = 0, min = 0, max = 5),
          fluidRow(
            actionButton("predict_button", "Predict")
          )
        )
      )
      
    )
  )
)

ui = bs4DashPage(header = header,
                 sidebar = sidebar,
                 body = body)


server <- function(input, output, session){
  output$ilham = renderImage({
    list(src="Images/ilham.png",height = 480, width = 360)
  }, deleteFile = F)
  output$farham = renderImage({
    list(src="Images/farham.jpeg",height = 480, width = 360)
  }, deleteFile = F)
  output$jumlah_kunjungan = renderInfoBox({
    jumlah_kunjungan_temp = df%>%
      select(c(booking_status))%>%
      filter(booking_status=="Not_Canceled")%>%
      count()
    infoBox(
      "Jumlah Kunjungan", paste(round(jumlah_kunjungan_temp/1000, 2), "Ribu"), icon = icon("ok-circle", lib = "glyphicon"), color = "primary"
    )
  })
  
  output$profit = renderInfoBox({
    profit_temp = df%>%
      mutate(bill = (no_of_week_nights+no_of_weekend_nights)*avg_price_per_room)%>%
      filter(booking_status=="Not_Canceled")%>%
      select(c(bill))%>%
      sum()
    infoBox(
      "Estimasi Profit", paste(round(profit_temp/1000000, 2), "Juta Satuan Mata Uang"), icon = icon("usd", lib = "glyphicon"), color = "success"
    )
  })
  
  output$numerical_summary = renderDataTable({
    as.data.frame(describe(df[, numerical_columns]))%>%
      mutate(across(everything(), round, 2))%>%
      select(-c(vars, n, se, skew, kurtosis))
  })
  
  output$categorical_summary <- renderPrint({
    summary(df[, categorical_columns])
  })
  
  output$jumlah_pembatalan = renderInfoBox({
    jumlah_pembatalan_temp = df%>%
      select(c(booking_status))%>%
      filter(booking_status=="Canceled")%>%
      count()
    infoBox(
      "Jumlah Pembatalan", paste(round(jumlah_pembatalan_temp/1000, 2), "Ribu"), icon = icon("remove-circle", lib = "glyphicon"), color = "danger"
    )
  })
  
  output$room_demand = renderPlotly({
    df%>%
      filter(room_type_reserved==input$room_type_demand)%>%
      filter(arrival_date>= input$room_demand_date[1] & arrival_date<=input$room_demand_date[2])%>%
      count(arrival_date, room_type_reserved)%>%
      plot_ly(x = ~arrival_date, 
              y = ~n, 
              color = ~room_type_reserved,
              colors = "Paired",
              type = 'scatter', mode = 'lines')%>%
      layout(
        title = "Room Demand",
        xaxis = list(title = "Date", tickangle = 45),
        yaxis = list(title = "Room Demand"),
        template = "plotly_dark"
      ) %>%
      config(displayModeBar = T)
  })
  
  output$room_price = renderPlotly({
    df%>%
      filter(arrival_date>= input$room_price_date[1] & arrival_date<=input$room_price_date[2])%>%
      group_by(year = year(arrival_date), month = month(arrival_date)) %>%
      summarise(mean = mean(avg_price_per_room))%>%
      ungroup()%>%
      na.omit()%>%
      mutate(arrival_date = as.Date(paste0(year,"-",month, "-01")))%>%
      select(-c(1,2))%>%
      plot_ly(x = ~arrival_date, 
              y = ~mean, type = 'scatter', mode = 'lines')%>%
      layout(
        title = "Average Price per Room by Month and Year",
        xaxis = list(title = "Month and Year", tickangle = 45),
        yaxis = list(title = "Mean Average Price per Room"),
        template = "plotly_dark"  # Using a dark theme
      ) %>%
      config(displayModeBar = T)
  })
  
  output$booking_demand = renderPlotly({
    df%>%
      filter(room_type_reserved==input$room_type_book)%>%
      filter(booking_date>= input$book_demand_date[1] & booking_date<=input$book_demand_date[2])%>%
      count(booking_date, room_type_reserved)%>%
      plot_ly(x = ~booking_date, 
              y = ~n, 
              color = ~room_type_reserved,
              colors = "Paired",
              type = 'scatter', mode = 'lines')%>%
      layout(
        title = "Booking Demand",
        xaxis = list(title = "Date", tickangle = 45),
        yaxis = list(title = "Booking Demand"),
        template = "plotly_dark"
      ) %>%
      config(displayModeBar = T)
  })
  
  output$interactive_histogram = renderPlotly({
    df%>%
      select(input$input_interactive_numerical_plot)%>%
      plot_ly(x = as.formula(paste("~", input$input_interactive_numerical_plot)), type = 'histogram',
              nbinsx = input$bins,
              marker = list(color = 'rgba(100, 150, 200, 0.7)',
                            line = list(color = 'rgba(100, 150, 200, 1.0)', width = 1))) %>%
      layout(title = paste('Histogram of', input$input_interactive_numerical_plot),
             xaxis = list(title = input$input_interactive_numerical_plot),
             yaxis = list(title = 'Count'),
             bargap = 0.1)
  })
  
  output$interactive_boxplot = renderPlotly({
    df %>%
      select(all_of(input$input_interactive_numerical_plot)) %>%
      plot_ly(y = as.formula(paste("~", input$input_interactive_numerical_plot)), type = 'box',
              marker = list(color = 'rgba(100, 150, 200, 0.7)',
                            line = list(color = 'rgba(100, 150, 200, 1.0)', width = 1))) %>%
      layout(title = paste('Boxplot of', input$input_interactive_numerical_plot),
             yaxis = list(title = input$input_interactive_numerical_plot))
  })
  
  output$interactive_2d_plot = renderPlotly({
    fig <- plot_ly(x = df[[input$input_interactive_numerical_plot1]], y = df[[input$input_interactive_numerical_plot2]])
    fig2 <- subplot(
      fig %>% add_histogram2d()
    )
    
    fig2 %>%
      layout(title = paste('Heatmap of', input$input_interactive_numerical_plot1, 'and', input$input_interactive_numerical_plot2),
             xaxis = list(title = input$input_interactive_numerical_plot1),
             yaxis = list(title = input$input_interactive_numerical_plot2))
  })
  
  output$interactive_barplot = renderPlotly({
    if (length(input$input_interactive_count_plot) == 1) {
      plot_data <- df %>%
        count(!!sym(input$input_interactive_count_plot[[1]])) %>%
        rename(Count = n)
      
      p_bar <- plot_ly(plot_data, x = ~get(input$input_interactive_count_plot[[1]]), y = ~Count, type = 'bar') %>%
        layout(title = paste('Bar plot of', input$input_interactive_count_plot[[1]]),
               xaxis = list(title = input$input_interactive_count_plot[[1]]),
               yaxis = list(title = 'Count'))
    } else if (length(input$input_interactive_count_plot) == 2) {
      plot_data <- df %>%
        count(!!sym(input$input_interactive_count_plot[[1]]), !!sym(input$input_interactive_count_plot[[2]])) %>%
        rename(Count = n)
      
      p_bar <- plot_ly(plot_data, x = ~get(input$input_interactive_count_plot[[1]]), y = ~Count, color = ~get(input$input_interactive_count_plot[[2]]), type = 'bar') %>%
        layout(title = paste('Grouped bar plot of', input$input_interactive_count_plot[[1]], 'and', input$input_interactive_count_plot[[2]]),
               xaxis = list(title = input$input_interactive_count_plot[[1]]),
               yaxis = list(title = 'Count'),
               barmode = 'group')
    }
    
    p_bar
  })
  
  output$interactive_piechart = renderPlotly({
    if (length(input$input_interactive_count_plot) == 1) {
      plot_data <- df %>%
        count(!!sym(input$input_interactive_count_plot[[1]])) %>%
        rename(Count = n)
      
      p <- plot_ly(plot_data, labels = ~get(input$input_interactive_count_plot[[1]]), values = ~Count, type = 'pie') %>%
        layout(title = paste('Pie chart of', input$input_interactive_count_plot[[1]]))
    } else if (length(input$input_interactive_count_plot) == 2) {
      plot_data <- df %>%
        count(!!sym(input$input_interactive_count_plot[[1]]), !!sym(input$input_interactive_count_plot[[2]])) %>%
        rename(Count = n)
      
      inner_data <- plot_data %>%
        group_by(!!sym(input$input_interactive_count_plot[[1]])) %>%
        summarise(Count = sum(Count)) %>%
        mutate(Parent = "")
      
      outer_data <- plot_data %>%
        rename(Inner = !!sym(input$input_interactive_count_plot[[1]]), Outer = !!sym(input$input_interactive_count_plot[[2]]))
      
      sunburst_data <- bind_rows(
        inner_data %>%
          rename(Label = !!sym(input$input_interactive_count_plot[[1]]), Parent = Parent, Value = Count),
        outer_data %>%
          rename(Label = Outer, Parent = Inner, Value = Count)
      )
      
      p <- plot_ly(
        labels = ~sunburst_data$Label,
        parents = ~sunburst_data$Parent,
        values = ~sunburst_data$Value,
        type = 'sunburst',
        branchvalues = 'total'
      ) %>%
        layout(title = paste('Sunburst chart of', input$input_interactive_count_plot[[1]], 'and', input$input_interactive_count_plot[[2]]))
    }
    
    p
  })
  
  output$interactive_violin_plot =  renderPlotly({
    fig <- df %>%
      plot_ly(
        x = as.formula(paste("~", input$input_interactive_numerical_categorical1)),
        y = as.formula(paste("~", input$input_interactive_numerical_categorical2)),
        split = as.formula(paste("~", input$input_interactive_numerical_categorical1)),
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        )
      ) 
    
    fig <- fig %>%
      layout(
        xaxis = list(
          title = input$input_interactive_numerical_categorical1
        ),
        yaxis = list(
          title = input$input_interactive_numerical_categorical2,
          zeroline = F
        )
      )
    
    fig
  })
  
  output$chi_sq_test <- renderPrint({
    validate(
      need(length(input$input_chi_sq)>1, "Masukkan minimal dua variabel")
    )
    
    var1 <- input$input_chi_sq[[1]]
    var2 <- input$input_chi_sq[[2]]
    
    # Check if variables exist in the dataframe
    if (!(var1 %in% names(df)) || !(var2 %in% names(df))) {
      shinyalert(
        title = "Error",
        text = "One or more selected variables do not exist in the dataset.",
        type = "error"
      )
      return(NULL)
    }
    
    # Perform chi-square test
    contingency_table <- table(df[[var1]], df[[var2]])
    chi_sq_result <- chisq.test(contingency_table)
    
    # Display the result
    hasil <- if (chi_sq_result$p.value < 0.05) {
      "Signifikan"
    } else {
      "Tidak Signifikan"
    }
    
    shinyalert(
      title = "Chi Square Test",
      text = paste(var1, "dan", var2, hasil, "pada taraf signifikansi 5%"),
      type = "info"
    )
    
    return(chi_sq_result)
  })
  
  
  
  ### TRY BOOKING SISTEM
  observeEvent(input$predict_button, {
    
    # Check and coerce inputs to the correct types
    temp_rf_data <- data.frame(
      no_of_adults = as.integer(input$no_of_adults),
      no_of_children = as.integer(input$no_of_children),
      no_of_weekend_nights = as.integer(input$no_of_weekend_nights),
      no_of_week_nights = as.integer(input$no_of_week_nights),
      type_of_meal_plan = factor(input$type_of_meal_plan, levels = levels(test_data$type_of_meal_plan)),
      room_type_reserved = factor(input$room_type_reserved, levels = levels(test_data$room_type_reserved)),
      lead_time = as.integer(as.Date(input$arrival_date) - as.Date(input$booking_date)),
      market_segment_type = factor(input$market_segment_type, levels = levels(test_data$market_segment_type)),
      avg_price_per_room = as.numeric(input$avg_price_per_room),
      no_of_special_requests = as.integer(input$no_of_special_requests),
      stringsAsFactors = TRUE
    )
    
    # Print temp_rf_data for debugging
    print(temp_rf_data)
    
    # Check if temp_rf_data has been created correctly
    # Combine with a row from test_data to ensure structure matches
    xtest <- rbind(test_data[1,], temp_rf_data)
    xtest <- xtest[-1,]
    
    # Predict using the Random Forest model
    prediction <- predict(rf_model, xtest)
    
    # Generate prediction message
    if(prediction == "Not_Canceled") {
      prediksinya = "Hati-hati! Pesanan ini berpotensi akan dibatalkan"
    } else {
      prediksinya = "Pesanan berhasil disimpan. Pelanggan kemungkinan tidak membatalkan pesanan"
    }
    
    # Display the result using shinyalert
    shinyalert(
      title = "Informasi",
      text = paste(prediksinya),
      type = "info"
    )
  })
}

shinyApp(ui = ui, server = server)
