library(shiny)
library(mapview)
library(leaflet)
library(DT)
library(ggplot2)
library(sf)
library(dplyr)
library(spdep)
library(coda)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(shinycssloaders)
library(httr2)
library(jsonlite)
library(readxl)
library(future)
library(promises)
library(nimble)
library(rsconnect)

plan(multisession)

REQUIRED_EXT <- c("shp", "dbf", "shx", "prj", "cpg")

footer_ui <- function() {
  div(
    class = "footer-bar",
    fluidRow(
      column(
        6,
        h5(tags$a(
          "TBScope",
          href = "#",
          onclick = "Shiny.setInputValue('go_home', Date.now()); return false;",
          style = "color:white; text-decoration:none; cursor:pointer; font-size: 28px"
        )),
        p("Platform analisis spasial tuberkulosis di Indonesia."),
        p("Dikembangkan untuk mendukung pengambilan keputusan berbasis data.")
      ),
      column(
        6, style = "text-align:right;",
        h5("Hubungi Kami"),
        p(
          tags$a(href = "mailto:wilsen.soetresno@binus.ac.id", "wilsen.soetresno@binus.ac.id"),
          style = "font-size: 20px;"
        ),
        p("Universitas Bina Nusantara"),
        br(),
        p(
          style = "font-size:14px; color: #888;",
          paste0("© ", format(Sys.Date(), "%Y"), " TBScope. All rights reserved.")
        )
      )
    )
  )
}

ui <- navbarPage(
  id = "mainNavbar",
  title = tags$a(
    "TBScope",
    href = "#",
    onclick = "Shiny.setInputValue('go_home', Math.random());",
    style = "font-weight:700; font-size: 30px; text-decoration:none; color:inherit; margin-right: 10px"
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  position = "fixed-top",
  collapsible = TRUE,
  
  header = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;600;700&display=swap"
    ),
    tags$style(HTML("
      body {
        padding-top: 80px;
        font-family: 'Nunito', sans-serif;
        background-color: #f8f9fa;
      }
      .navbar { box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
      .navbar-brand { font-weight: 700; }
      .navbar-toggler { border: none; }
      .navbar-toggler:focus { box-shadow: none; }
      .banner {
        min-height: 75vh;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 60px 20px;
        position: relative;
        overflow: hidden;
      }
      .banner::before {
        content: '';
        position: absolute;
        inset: 0;
        background: rgba(0,0,0,0.45);
      }
      .banner-box {
        position: relative;
        z-index: 2;
        background: rgba(0,0,0,0.55);
        padding: 30px 35px;
        border-radius: 16px;
        color: white;
        max-width: 800px;
        margin: auto;
        text-align: center;
      }
      .banner-box h1 { font-size: 46px; font-weight: 700; margin-bottom: 12px; }
      .banner-box p { font-size: 15px; margin-bottom: 0; color: rgba(255,255,255,0.85); }
      .home-content { padding: 50px 40px 30px; }
      .section-title {
        text-align: center;
        font-weight: 700;
        font-size: 32px;
        color: black;
        margin-bottom: 6px;
      }
      .section-subtitle {
        text-align: center;
        color: black;
        font-size: 20px;
        margin-bottom: 30px;
      }
      .section-divider {
        border: none;
        border-top: 2px solid #e9ecef;
        margin: 40px 0;
      }
      .section-card {
        background: white;
        border-radius: 12px;
        padding: 24px;
        height: 100%;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        margin-bottom: 16px;
        overflow: visible;
      }
      .section-card h4 {
        color: black;
        font-weight: 700;
        margin-bottom: 12px;
        text-align: center;
        display: flex;
        gap: 6px;
      }
      .section-card p {
        color: black;
        font-size: 20px;
        line-height: 1.7;
        margin-bottom: 0;
        text-align: justify;
      }
      .section-card ul {
        color: black;
        font-size: 20px;
        line-height: 1.9;
        padding-left: 18px;
        margin-bottom: 0;
      }
      .img-placeholder {
        background-color: #dee2e6;
        border-radius: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #aaa;
        font-size: 13px;
        height: 100%;
        min-height: 160px;
      }
      .img-placeholder img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        border-radius: 10px;
      }
      .stat-box {
        background: white;
        border-radius: 12px;
        padding: 20px;
        text-align: center;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        border-top: 4px solid #2c7be5;
        margin-bottom: 16px;
      }
      .stat-box .stat-number {
        font-size: 28px;
        font-weight: 700;
        color: #2c7be5;
      }
      .stat-box .stat-label {
        font-size: 13px;
        color: #777;
        margin-top: 4px;
      }
      .footer-bar {
        background-color: #1a2a3a;
        color: #ccc;
        padding: 30px 40px;
        margin-top: 50px;
      }
      .footer-bar h5 { color: white; font-weight: 700; }
      .footer-bar p { font-size: 17px; margin-bottom: 4px; }
      .footer-bar a { color: #7eb3e8; text-decoration: none; }
      .shiny-file-input-progress { margin-top: 10px; }
      .stats-container { padding: 30px; }
      .stats-section { margin-top: 10px; }
      .stats-card {
        background: #ffffff;
        border-radius: 14px;
        padding: 20px 22px;
        margin-bottom: 18px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.05);
        font-size: 16px;
      }
      .stats-card h5 {
        font-weight: 800;
        margin-bottom: 14px;
        color: black;
        font-size: 26px;
      }
      .nav-pills .nav-link {
        border-radius: 12px;
        margin-right: 8px;
        font-weight: 700;
        font-size: 18px;
        padding: 10px 18px;
      }
      .nav-pills .nav-link.active {
        background-color: black !important;
        color: white !important;
      }
      .nav-pills > li > a {
        border: 2px solid black !important;
        border-radius: 8px;
        margin: 4px;
        background-color: white !important;
        color: black !important;
      }
      .leaflet-container { border-radius: 10px; }
      .shp-warning-box {
        color: #dc3545;
        font-weight: 700;
        font-size: 15px;
        background: #fdecea;
        border-left: 4px solid #dc3545;
        padding: 10px 14px;
        border-radius: 6px;
        margin-bottom: 10px;
      }
      .shiny-output-error-validation {
        color: #dc3545 !important;
        font-weight: 700 !important;
        font-size: 16px !important;
        padding: 12px;
        background: #fdecea;
        border-left: 4px solid #dc3545;
        border-radius: 6px;
      }
      .dataTables_wrapper { font-size: 13px; overflow-x: auto; }
      table.dataTable { font-size: 13px !important; width: 100% !important; }
      .dataTables_wrapper .row:first-child {
        display: flex;
        flex-direction: column;
        gap: 6px;
        margin: 0 0 10px 0 !important;
      }
      .dataTables_wrapper .row:first-child > div {
        width: 100% !important;
        max-width: 100% !important;
        flex: 0 0 100% !important;
        padding: 0 !important;
      }
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter {
        text-align: left !important;
        float: none !important;
      }
      .dataTables_wrapper .dataTables_filter input { margin-left: 6px; }
      .dataTables_wrapper .row:last-child {
        display: flex;
        flex-direction: column;
        align-items: stretch;
        margin: 0 !important;
        padding-top: 12px;
        gap: 8px;
      }
      .dataTables_wrapper .row:last-child > div {
        width: 100% !important;
        max-width: 100% !important;
        flex: 0 0 100% !important;
        padding: 0 !important;
      }
      .dataTables_wrapper .dataTables_info {
        padding-top: 0 !important;
        text-align: left;
        font-size: 13px;
        color: black;
        white-space: normal;
      }
      .dataTables_wrapper .dataTables_paginate {
        white-space: normal !important;
        display: flex !important;
        flex-wrap: wrap !important;
        justify-content: flex-end;
        gap: 4px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        padding: 4px 10px !important;
        margin: 0 2px !important;
        min-width: auto !important;
        border-radius: 6px !important;
      }
      @media (min-width: 1400px) {
        .dataTables_wrapper .row:last-child {
          flex-direction: row;
          align-items: center;
          justify-content: space-between;
        }
        .dataTables_wrapper .row:last-child > div {
          width: auto !important;
          flex: 0 0 auto !important;
        }
        .dataTables_wrapper .dataTables_paginate { text-align: right !important; }
      }
      @media (max-width: 768px) {
        .home-content { padding: 30px 16px 20px; }
        .banner { padding: 80px 15px 60px; }
        .banner-box h1 { font-size: 20px; }
        .banner-box p { font-size: 13px; }
        .stat-box .stat-number { font-size: 22px; }
        .footer-bar { padding: 24px 20px; }
        .footer-bar .col-sm-6:last-child {
          text-align: left !important;
          margin-top: 20px;
        }
        .col-12.col-md-6 { margin-bottom: 70px !important; }
      }
    "))
  ),
  
  # Beranda
  tabPanel(
    title = tagList(tags$i(class = "fa fa-home"), " Beranda"), value = "Home",
    div(
      class = "banner",
      tags$img(
        src = "indo.jpg",
        style = "position:absolute; inset:0; width:100%; height:100%; object-fit:cover; object-position:center; z-index:0;"
      ),
      div(style = "position:absolute; inset:0; background:rgba(0,0,0,0.45); z-index:1;"),
      div(
        class = "banner-box",
        style = "z-index:2; position:relative;",
        h1("DI MANA TUBERKULOSIS PALING BANYAK TERJADI?"),
        p("Gunakan peta interaktif TBScope untuk mengidentifikasi dan menganalisis wilayah berisiko tinggi tuberkulosis di Indonesia.")
      )
    ),
    
    div(
      class = "home-content",
      h2(class = "section-title", "Apa itu Tuberkulosis (TB)?"),
      p(class = "section-subtitle", "Memahami penyakit infeksi yang masih menjadi tantangan kesehatan global"),
      
      fluidRow(
        div(
          class = "col-12 col-md-3",
          div(
            class = "img-placeholder", style = "min-height: 200px;",
            tags$img(src = "bacteria.jpg", alt = "Gambar TB 1",
                     style = "width:100%; height:100%; object-fit:cover; border-radius:10px;")
          )
        ),
        div(
          class = "col-12 col-md-6",
          div(
            class = "section-card",
            h4(tags$i(class = "fa fa-info-circle"), " Tentang Tuberkulosis"),
            p(
              "Tuberkulosis (TB) adalah penyakit infeksi menular yang disebabkan oleh bakteri",
              tags$em("Mycobacterium tuberculosis."),
              "Bakteri ini terutama menyerang paru-paru, namun dapat juga menyerang organ lain
               seperti ginjal, tulang belakang, dan otak. Tuberkulosis menyebar melalui udara
               ketika penderita batuk, bersin, atau berbicara."
            ),
            br(),
            p(
              "Bakteri berbentuk batang yang bersifat aerobik dan memiliki dinding sel tebal yang
               membuatnya tahan terhadap kondisi ekstrem. Penyakit ini sudah dikenal sejak ribuan tahun
               lalu dan tetap menjadi masalah kesehatan global karena kemampuan bakteri untuk bertahan
               lama di tubuh manusia dan menyebar dengan mudah."
            )
          )
        ),
        div(
          class = "col-12 col-md-3",
          div(
            class = "img-placeholder", style = "min-height: 200px;",
            tags$img(src = "homelungs.jpg", alt = "Gambar TB 2",
                     style = "width:100%; height:100%; object-fit:cover; border-radius:10px;")
          )
        )
      ),
      
      hr(class = "section-divider"),
      
      fluidRow(
        column(
          6,
          div(
            class = "section-card",
            h4(tags$i(class = "fa fa-stethoscope"), " Gejala Tuberkulosis"),
            p("Gejala utama tuberkulosis adalah batuk yang berlangsung lebih dari 2 minggu. Gejala lain meliputi:"),
            tags$ul(
              tags$li("Batuk berdahak, kadang disertai darah"),
              tags$li("Demam ringan, terutama pada sore dan malam hari"),
              tags$li("Keringat malam yang berlebihan"),
              tags$li("Penurunan berat badan tanpa sebab yang jelas"),
              tags$li("Kelelahan dan kelemahan tubuh"),
              tags$li("Nyeri dada dan sesak napas")
            )
          )
        ),
        column(
          6,
          div(
            class = "section-card",
            h4(tags$i(class = "fa fa-shield"), " Pencegahan dan Penyebab Tuberkulosis"),
            p(
              "Penyebab utama tuberkulosis adalah bakteri", tags$em("Mycobacterium tuberculosis"),
              "yang menyebar melalui droplet udara ketika penderita aktif batuk, bersin, atau berbicara.
               Risiko meningkat pada kondisi kepadatan penduduk tinggi, sanitasi buruk, dan imunitas rendah."
            ),
            br(),
            p("Pencegahan dapat dilakukan dengan:"),
            tags$ul(
              tags$li("Vaksinasi BCG pada bayi baru lahir"),
              tags$li("Ventilasi ruangan yang baik"),
              tags$li("Menggunakan masker bagi penderita TB aktif"),
              tags$li("Menjalani pengobatan TB secara tuntas"),
              tags$li("Menjaga daya tahan tubuh dengan gizi seimbang")
            )
          )
        )
      ),
      
      hr(class = "section-divider"),
      
      h2(class = "section-title", "Kasus Tuberkulosis di Indonesia"),
      p(class = "section-subtitle", "Data dan fakta Tuberkulosis di Indonesia yang perlu diketahui"),
      
      fluidRow(
        column(3, div(class = "stat-box",
                      div(class = "stat-number", "1 Juta+"),
                      div(class = "stat-label", "Kasus tuberkulosis per tahun"))),
        column(3, div(class = "stat-box", style = "border-top-color:#e74c3c;",
                      div(class = "stat-number", style = "color:#e74c3c;", "100 Ribu+"),
                      div(class = "stat-label", "Kematian akibat tuberkulosis per tahun"))),
        column(3, div(class = "stat-box", style = "border-top-color:#27ae60;",
                      div(class = "stat-number", style = "color:#27ae60;", "Peringkat 2"),
                      div(class = "stat-label", "Negara beban tuberkulosis tertinggi dunia"))),
        column(3, div(class = "stat-box", style = "border-top-color:#f39c12;",
                      div(class = "stat-number", style = "color:#f39c12;", "80%+"),
                      div(class = "stat-label", "Tingkat keberhasilan pengobatan")))
      ),
      
      br(),
      
      div(
        class = "section-card",
        p(
          "Indonesia merupakan salah satu negara dengan beban tuberkulosis tertinggi di dunia.
           Menurut laporan WHO Global Tuberculosis Report, Indonesia menduduki peringkat kedua
           dunia setelah India dalam jumlah kasus tuberkulosis baru setiap tahunnya. Provinsi
           dengan kasus tuberkulosis tertinggi antara lain Jawa Barat, Jawa Timur, dan DKI Jakarta,
           yang juga merupakan provinsi dengan kepadatan penduduk tertinggi di Indonesia."
        ),
        br(),
        p(
          "Penanggulangan tuberkulosis di Indonesia dilakukan melalui program DOTS
           (Directly Observed Treatment Short-course) yang diintegrasikan ke dalam sistem
           pelayanan kesehatan primer. Meskipun demikian, permasalahan dalam implementasi
           masih kerap ditemukan, seperti kurang memadainya pelatihan bagi petugas, keterbatasan fasilitas dan infrastruktur,
           serta sistem pencatatan dan pelaporan yang belum berjalan secara optimal."
        )
      )
    ),
    
    footer_ui()
  ),
  
  # Data
  tabPanel(
    title = tagList(tags$i(class = "fa fa-database"), " Data"), value = "Data",
    useShinyjs(), useSweetAlert(),
    div(
      style = "padding: 40px;",
      fluidRow(
        column(6, h3(style = "font-weight:700; margin-top: 10px", "Unggah Data Tuberkulosis")),
        column(
          6,
          div(
            class = "section-card",
            p(style = "font-weight:600; margin-bottom:4px; color: black", "Pilih Data Tuberkulosis"),
            p(style = "color: black; font-size:17px; margin-bottom:12px;", "Pastikan berkas dalam format .xlsx"),
            fileInput("xlsx_file", label = NULL, width = "100%",
                      accept = c(".xlsx"),
                      buttonLabel = "Unggah Data",
                      placeholder = "Belum ada berkas dipilih"),
            uiOutput("xlsx_validation_warning"),
            tags$div(
              style = "margin-top: 10px;",
              actionButton("delete_xlsx", "Hapus Data",
                           icon = icon("trash"), class = "btn-danger",
                           style = "width:100%;")
            )
          )
        )
      ),
      
      hr(class = "section-divider"),
      
      fluidRow(
        column(6, h3(style = "font-weight:700; margin-top: 10px;", "Unggah Shapefile")),
        column(
          6,
          div(
            class = "section-card",
            p(style = "font-weight:600; margin-bottom:4px; color: black", "Pilih Shapefile"),
            p(style = "color: black; font-size:17px; margin-bottom:12px;",
              "Unggah tepat 5 berkas sekaligus: .shp, .dbf, .shx, .prj, .cpg"),
            fileInput("shapefile", label = NULL, width = "100%",
                      multiple = TRUE,
                      accept = c('.shp', '.dbf', '.shx', '.prj', '.cpg'),
                      buttonLabel = "Unggah Shapefile",
                      placeholder = "Belum ada berkas dipilih"),
            uiOutput("shp_validation_warning"),
            tags$div(
              style = "margin-top: 10px;",
              actionButton("delete_shp", "Hapus Shapefile",
                           icon = icon("trash"), class = "btn-danger",
                           style = "width:100%;")
            )
          )
        )
      ),
      
      hr(class = "section-divider"),
      
      fluidRow(
        column(
          12,
          div(
            class = "section-card",
            p(style = "font-weight: 700; font-size: 32px; color: black", "Penjelasan"),
            tags$div(
              style = "font-size:20px;",
              strong("Variabel yang diperlukan (unggah data tuberkulosis dalam format .xlsx dengan nama variabel berikut):"),
              br(),
              "1. KabupatenKota --> Nama wilayah Kabupaten/Kota",
              br(),
              "2. KasusTB --> Jumlah kasus tuberkulosis yang tercatat di setiap wilayah kabupaten/kota",
              br(),
              "3. Populasi --> Jumlah populasi penduduk di setiap wilayah kabupaten/kota",
              br(), br(),
              tags$a(
                href = "contoh.xlsx",
                download = "contoh.xlsx",
                class = "btn btn-primary",
                style = "padding:6px 14px; font-size:20px;",
                tags$i(class = "fa fa-download"), " Unduh Template Data Tuberkulosis"
              )
            )
          )
        )
      ),
      
      hr(class = "section-divider"),
      
      tags$div(
        class = "row",
        tags$div(
          class = "col-12 col-md-6",
          style = "margin-bottom: 24px;",
          h4(style = "font-weight:700;", tags$i(class = "fa fa-table"), " Tampilan Data"),
          div(
            class = "section-card",
            style = "overflow-x: auto;",
            div(style = "margin-bottom: 20px;", uiOutput("no_data_warning")),
            DTOutput("input_file")
          )
        ),
        tags$div(
          class = "col-12 col-md-6",
          style = "margin-bottom: 24px;",
          h4(style = "font-weight:700;", tags$i(class = "fa fa-map"), " Tampilan Shapefile"),
          div(
            class = "section-card",
            uiOutput("no_data_warning_SHP"),
            uiOutput("shp_incomplete_warning"),
            shinycssloaders::withSpinner(
              leafletOutput("peta_jabar", height = "400px"),
              type = 6
            )
          )
        )
      )
    ),
    
    footer_ui()
  ),
  
  # Statistika
  tabPanel(
    title = tagList(icon("line-chart"), "Statistika"), value = "Statistics",
    div(
      class = "stats-container",
      tabsetPanel(
        id = "stats_tab",
        type = "pills",
        
        tabPanel(
          "Analisis Statistik Deskriptif",
          div(
            class = "stats-section",
            fluidRow(
              column(6, div(class = "stats-card", h5("Distribusi Kasus Tuberkulosis"),
                            uiOutput("warn_hist_kasus"),
                            shinycssloaders::withSpinner(
                              plotOutput("hist_kasus", height = "260px"),
                              type = 6, color = "#2c7be5", size = 0.8
                            ))),
              column(6, div(class = "stats-card", h5("Distribusi Populasi"),
                            uiOutput("warn_hist_pop"),
                            shinycssloaders::withSpinner(
                              plotOutput("hist_pop", height = "260px"),
                              type = 6, color = "#2c7be5", size = 0.8
                            )))
            ),
            fluidRow(
              column(12, div(class = "stats-card", h5("Proporsi Kasus Tuberkulosis per 100.000 Penduduk"),
                             uiOutput("warn_bar_rate"),
                             shinycssloaders::withSpinner(
                               plotOutput("bar_rate", height = "320px"),
                               type = 6, color = "#2c7be5", size = 0.8
                             )))
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "stats-card",
                  h5("Penjelasan"),
                  div(
                    style = "margin-bottom: 12px;",
                    actionButton("btn_analisis",
                                 label = tagList(tags$i(class = "fa fa-magic"), " Analisis"),
                                 class = "btn btn-dark",
                                 style = "font-size:16px; padding: 8px 20px;")
                  ),
                  uiOutput("warn_analisis"),
                  div(
                    id = "analisis_loading",
                    style = "display:none; color:black; font-style:italic; margin-top:10px;",
                    tags$i(class = "fa fa-spinner fa-spin"), " Sedang menganalisis diagram..."
                  ),
                  div(
                    style = "min-height:120px; padding-top:10px; font-size:18px; line-height:1.8; white-space:pre-wrap;",
                    uiOutput("analisis_llm")
                  )
                )
              )
            )
          )
        ),
        
        tabPanel(
          "Hasil Ringkasan Model",
          div(
            class = "stats-section",
            fluidRow(
              column(12, div(class = "stats-card",
                             h5("Hasil Model"),
                             shinycssloaders::withSpinner(
                               uiOutput("model_content_ui"),
                               type = 6, color = "#2c7be5",
                               proxy.height = "200px"
                             )))
            )
          )
        ),
        
        tabPanel(
          "Peta Risiko Relatif",
          div(
            class = "stats-section",
            fluidRow(
              column(
                12,
                div(
                  class = "stats-card",
                  h5("Peta Relative Risk"),
                  shinycssloaders::withSpinner(
                    leafletOutput("petaRR_jabar", height = "420px"),
                    type = 6, color = "#2c7be5"
                  )
                )
              )
            ),
            fluidRow(
              column(12, div(class = "stats-card",
                             h5("Penjelasan Peta Risiko Relatif"),
                             uiOutput("rr_penjelasan_ui")))
            )
          )
        )
      )
    ),
    
    footer_ui()
  ),
  
  # Informasi Umum
  tabPanel(
    title = tagList(icon("info-circle"), "Informasi Umum"), value = "Information",
    div(
      style = "padding:40px;",
      
      h2(style = "font-weight:700;", "Tentang Website"),
      tags$hr(),
      tags$div(
        style = "font-size:20px; line-height:2; color: black",
        strong("TBScope"), " merupakan platform aplikasi berbasis website yang dikembangkan untuk melakukan analisis Bayesian spasial terhadap penyebaran penyakit Tuberkulosis (TB) di Indonesia. Website ini dirancang untuk membantu pengguna dalam mengolah data, memvisualisasikan distribusi kasus, serta memvisualisasikan risiko relatif menggunakan pendekatan statistik spasial.",
        br(), br(),
        "Analisis pada website ini menggunakan model Intrinsic Conditional Autoregressive (ICAR)",
        " yang termasuk dalam pendekatan Bayesian Spatial Model untuk mengidentifikasi pola keterkaitan antar wilayah serta mengestimasi risiko relatif penyakit tuberkulosis.",
        br(), br(),
        "Website ini memiliki beberapa fitur utama, yaitu:",
        tags$ol(
          tags$li("Beranda --> Menyajikan informasi umum mengenai penyakit tuberkulosis."),
          tags$li("Data --> Memungkinkan pengguna mengunggah data TB dan shapefile serta melihat tampilan data."),
          tags$li("Statistika --> Menampilkan visualisasi data, hasil analisis model, dan peta risiko relatif."),
          tags$li("Informasi --> Memberikan penjelasan mengenai penggunaan website.")
        )
      ),
      
      br(),
      
      h2(style = "font-weight:700;", "Alasan Penggunaan Model CAR dan ICAR"),
      tags$hr(),
      tags$div(
        style = "font-size:20px; line-height:2; color: black",
        tags$ol(
          tags$li("Tuberkulosis merupakan penyakit menular yang dapat menyebar ke wilayah sekitar."),
          tags$li("Sesuai Hukum Pertama Geografi Tobler, wilayah yang berdekatan cenderung saling mempengaruhi dibandingkan wilayah yang berjauhan."),
          tags$li("Menurut Tessema et al. (2023), Conditional Autoregressive (CAR) merupakan model yang umum digunakan dalam pemetaan penyakit menular."),
          tags$li("Model CAR mampu memodelkan pengaruh wilayah bertetangga terhadap tingkat risiko suatu wilayah."),
          tags$li("Menurut Jaisankar dan Ranjani (2020), model ICAR merupakan varian dari model CAR yang digunakan untuk mengatasi kesulitan dalam mengestimasi parameter autokorelasi spasial pada model CAR akibat perbedaan jumlah wilayah tetangga antarwilayah."),
          tags$li("Pada model ICAR, parameter autokorelasi spasial ditetapkan pada nilai maksimum sehingga efek spasial suatu wilayah diasumsikan sepenuhnya dipengaruhi oleh wilayah tetangganya.")
        )
      ),
      
      br(),
      
      h2(style = "font-weight:700;", "Panduan Penggunaan"),
      tags$hr(),
      tags$div(
        style = "font-size:20px; line-height:2; color: black",
        tags$ol(
          tags$li("Masuk ke menu Data, lalu unggah data tuberkulosis dalam format .xlsx pastikan data terdapat variabel yang dibutuhkan."),
          tags$li("Unggah seluruh komponen shapefile (.shp, .dbf, .shx, .prj, .cpg) secara bersamaan."),
          tags$li("Pengguna dapat memeriksa data melalui tampilan data dan shapefile untuk memastikan data telah sesuai."),
          tags$li("Masuk ke menu Statistika pada analsis statistik deskriptif untuk melihat analisis statistik deskriptif berupa hasil visualisasi dan pengguna dapat memperoleh penjelasan otomatis dengan menekan tombol \"Analisis\""),
          tags$li("Selanjutnya, pada halaman Hasil Ringkasan Model, pengguna harus menekan tombol \"Jalankan Model\" baru kemudian analisis model akan dijalankan untuk memperoleh hasil estimasi dan ringkasan model."),
          tags$li("Kemudian akses halaman Peta Risiko Relatif untuk mengetahui wilayah dengan tingkat risiko tuberkulosis yang lebih tinggi.")
        )
      ),
      
      br(),
      
      h2(style = "font-weight:700;", "Pengembang"),
      tags$hr(),
      tags$div(
        style = "font-size:20px; line-height:2; color: black",
        strong("Wilsen Soetresno"), br(),
        "Computer Science & Statistics, BINUS University", br(),
        "Pengembangan aplikasi berbasis website ini dilakukan sebagai bagian dari penelitian skripsi."
      )
    ),
    
    footer_ui()
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$go_home, {
    updateNavbarPage(session, "mainNavbar", selected = "Home")
    shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
  })
  
  rv <- reactiveValues(
    xlsx_key         = 0,
    shp_key          = 0,
    xlsx_deleted     = FALSE,
    shp_deleted      = FALSE,
    model_started    = FALSE,
    model_running    = FALSE,
    model_done       = FALSE,
    model_progress   = 0,
    model_results    = NULL,
    model_start_time = NULL
  )
  
  reset_model_state <- function() {
    rv$model_started  <- FALSE
    rv$model_running  <- FALSE
    rv$model_done     <- FALSE
    rv$model_progress <- 0
    rv$model_results  <- NULL
  }
  
  observeEvent(input$xlsx_file, {
    rv$xlsx_deleted <- FALSE
    reset_model_state()
  }, ignoreInit = TRUE)
  
  observeEvent(input$shapefile, {
    rv$shp_deleted <- FALSE
    reset_model_state()
  }, ignoreInit = TRUE)
  
  observeEvent(input$delete_xlsx, { reset_model_state() })
  observeEvent(input$delete_shp,  { reset_model_state() })
  
  observeEvent(input$delete_xlsx, {
    if (is.null(input$xlsx_file) || isTRUE(rv$xlsx_deleted)) {
      sendSweetAlert(
        session = session,
        title   = "Tidak Ada Data",
        text    = "Belum ada data tuberkulosis yang diunggah. Silakan unggah data terlebih dahulu menggunakan template yang tersedia.",
        type    = "warning"
      )
      return()
    }
    confirmSweetAlert(
      session             = session,
      inputId             = "confirm_delete_xlsx",
      title               = "Hapus Data Tuberkulosis?",
      text                = "Data yang telah diunggah akan dihapus. Tindakan ini tidak dapat dibatalkan.",
      type                = "warning",
      btn_labels          = c("Batal", "Ya, Hapus"),
      btn_colors          = c("#6c757d", "#dc3545"),
      closeOnClickOutside = TRUE
    )
  })
  
  observeEvent(input$confirm_delete_xlsx, {
    req(isTRUE(input$confirm_delete_xlsx))
    rv$xlsx_deleted <- TRUE
    rv$xlsx_key     <- rv$xlsx_key + 1
    reset_model_state()
    shinyjs::reset("xlsx_file")
    sendSweetAlert(
      session = session,
      title   = "Berhasil",
      text    = "Data tuberkulosis telah dihapus.",
      type    = "success"
    )
  })
  
  observeEvent(input$delete_shp, {
    if (is.null(input$shapefile) || isTRUE(rv$shp_deleted)) {
      sendSweetAlert(
        session = session,
        title   = "Tidak Ada Shapefile",
        text    = "Belum ada shapefile yang diunggah. Silakan unggah terlebih dahulu (.shp, .dbf, .shx, .prj, .cpg).",
        type    = "warning"
      )
      return()
    }
    confirmSweetAlert(
      session             = session,
      inputId             = "confirm_delete_shp",
      title               = "Hapus Shapefile?",
      text                = "Shapefile yang telah diunggah akan dihapus. Tindakan ini tidak dapat dibatalkan.",
      type                = "warning",
      btn_labels          = c("Batal", "Ya, Hapus"),
      btn_colors          = c("#6c757d", "#dc3545"),
      closeOnClickOutside = TRUE
    )
  })
  
  observeEvent(input$confirm_delete_shp, {
    req(isTRUE(input$confirm_delete_shp))
    rv$shp_deleted <- TRUE
    rv$shp_key     <- rv$shp_key + 1
    reset_model_state()
    shinyjs::reset("shapefile")
    sendSweetAlert(
      session = session,
      title   = "Berhasil",
      text    = "Shapefile telah dihapus.",
      type    = "success"
    )
  })
  
  data_xlsx <- reactive({
    rv$xlsx_key
    if (isTRUE(rv$xlsx_deleted)) return(NULL)
    req(input$xlsx_file)
    ext <- tolower(tools::file_ext(input$xlsx_file$name))
    req(ext == "xlsx")
    readxl::read_xlsx(input$xlsx_file$datapath)
  })
  
  output$xlsx_validation_warning <- renderUI({
    if (isTRUE(rv$xlsx_deleted)) return(NULL)
    req(input$xlsx_file)
    
    df <- tryCatch(readxl::read_xlsx(input$xlsx_file$datapath), error = function(e) NULL)
    
    if (is.null(df)) {
      return(div(
        class = "shp-warning-box",
        tags$i(class = "fa fa-exclamation-triangle"),
        " File tidak dapat dibaca, pastikan format Excel valid !!!"
      ))
    }
    
    required <- c("KasusTB", "Populasi", "KabupatenKota")
    missing  <- required[!required %in% names(df)]
    
    if (length(missing) > 0) {
      return(div(
        style = "color:#dc3545; font-weight:bold; font-size:15px; margin-top:8px;",
        tags$i(class = "fa fa-exclamation-triangle"),
        paste0(" Kolom belum lengkap, kolom yang tidak ditemukan: ", paste(missing, collapse = ", "))
      ))
    }
    
    div(
      style = "color:green; font-weight:bold; font-size:15px; margin-top:8px;",
      tags$i(class = "fa fa-check-circle"),
      " Data valid dan semua kolom sudah lengkap."
    )
  })
  
  shp_valid <- reactive({
    if (is.null(input$shapefile) || isTRUE(rv$shp_deleted)) return(FALSE)
    exts <- tolower(tools::file_ext(input$shapefile$name))
    all(REQUIRED_EXT %in% exts) && length(exts) == 5
  })
  
  output$shp_validation_warning <- renderUI({
    if (isTRUE(rv$shp_deleted)) return(NULL)
    if (is.null(input$shapefile)) return(NULL)
    if (!shp_valid()) {
      exts_found <- tolower(tools::file_ext(input$shapefile$name))
      missing    <- setdiff(REQUIRED_EXT, exts_found)
      div(
        style = "color:#dc3545; font-weight:bold; font-size:15px; margin-top:8px;",
        tags$i(class = "fa fa-exclamation-triangle"),
        paste0(" Shapefile tidak lengkap! Berkas yang kurang: .", paste(missing, collapse = ", ."))
      )
    } else {
      div(
        style = "color:green; font-weight:bold; font-size:15px; margin-top:8px;",
        tags$i(class = "fa fa-check-circle"),
        " Shapefile lengkap (5/5 berkas)."
      )
    }
  })
  
  output$shp_incomplete_warning <- renderUI({
    if (isTRUE(rv$shp_deleted)) return(NULL)
    if (is.null(input$shapefile)) return(NULL)
    if (!shp_valid()) {
      exts_found <- tolower(tools::file_ext(input$shapefile$name))
      missing    <- setdiff(REQUIRED_EXT, exts_found)
      div(
        class = "shp-warning-box",
        tags$i(class = "fa fa-exclamation-triangle"),
        paste0(" Shapefile belum lengkap. Berkas yang kurang: .", paste(missing, collapse = ", ."))
      )
    } else NULL
  })
  
  data_shp <- reactive({
    rv$shp_key
    if (isTRUE(rv$shp_deleted)) return(NULL)
    req(input$shapefile)
    files      <- input$shapefile
    temp_dir   <- tempdir()
    file_paths <- file.path(temp_dir, files$name)
    file.copy(files$datapath, file_paths, overwrite = TRUE)
    shp_file   <- file_paths[grepl("\\.shp$", file_paths)]
    sf::st_read(shp_file, quiet = TRUE)
  })
  
  output$input_file <- renderDT({
    df <- tryCatch(data_xlsx(), error = function(e) NULL)
    req(!is.null(df))
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$no_data_warning <- renderUI({
    df <- tryCatch(data_xlsx(), error = function(e) NULL)
    
    if (is.null(df)) {
      return(div(style = "color:red; font-weight:bold; font-size: 20px", "Belum ada data !!!"))
    }
    
    required <- c("KasusTB", "Populasi", "KabupatenKota")
    missing  <- required[!required %in% names(df)]
    
    if (length(missing) > 0) {
      return(div(
        class = "shp-warning-box",
        tags$i(class = "fa fa-exclamation-triangle"),
        tags$span(
          style = "margin-left:6px;",
          paste0("Kolom belum lengkap, kolom yang tidak ditemukan: ", paste(missing, collapse = ", "))
        )
      ))
    }
    
    NULL
  })
  
  output$no_data_warning_SHP <- renderUI({
    if (is.null(input$shapefile) || isTRUE(rv$shp_deleted))
      div(style = "color:red; font-weight:bold; font-size: 20px", "Belum ada shapefile !!!")
  })
  
  output$peta_jabar <- renderLeaflet({
    shp_data <- tryCatch(data_shp(), error = function(e) NULL)
    req(!is.null(shp_data))
    shp <- sf::st_transform(shp_data, 4326)
    m <- mapview::mapview(
      shp,
      layer.name    = "Wilayah",
      col.regions   = "#2c7be5",
      alpha.regions = 0.5,
      color         = "white",
      lwd           = 1
    )
    m@map
  })
  
  output$petaRR_jabar <- renderLeaflet({
    df       <- tryCatch(data_xlsx(), error = function(e) NULL)
    shp_data <- tryCatch(data_shp(),  error = function(e) NULL)
    
    if (is.null(df) || is.null(shp_data)) {
      return(
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addControl(
            html = '<div style="color:red; font-weight:bold; font-size:18px;
                    background:white; padding:10px 16px; border-radius:8px; font-family: Nunito, sans-serif">
                    Harap masukkan data terlebih dahulu pada menu Data !!!
                    </div>',
            position = "topright"
          )
      )
    }
    
    shp <- sf::st_transform(shp_data, 4326)
    
    if (!isTRUE(rv$model_done) || is.null(rv$model_results)) {
      return(
        leaflet(shp) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor   = "#2c7be5",
            fillOpacity = 0.5,
            color       = "white",
            weight      = 1
          )
      )
    }
    
    shp_rr <- shp %>%
      left_join(df, by = c("Kabupaten" = "KabupatenKota")) %>%
      filter(!is.na(KasusTB))
    
    mu_mean <- rv$model_results$mu_mean
    
    if (length(mu_mean) != nrow(shp_rr)) {
      return(
        leaflet() %>%
          addTiles() %>%
          addPopups(lng = 117, lat = -2, popup = "Mismatch dimensi mu_mean vs shapefile.")
      )
    }
    
    mean_kasus <- mean(df$KasusTB, na.rm = TRUE)
    shp_rr$RR_icar_skewnorm <- as.numeric(mu_mean / mean_kasus)
    
    qtiles <- quantile(
      shp_rr$RR_icar_skewnorm,
      probs = seq(0, 1, length.out = 6),
      na.rm = TRUE
    )
    
    pal <- colorBin(
      palette = "Greens",
      domain  = shp_rr$RR_icar_skewnorm,
      bins    = qtiles,
      pretty  = FALSE
    )
    
    leaflet(shp_rr) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor   = ~pal(RR_icar_skewnorm),
        fillOpacity = 0.85,
        color       = "white",
        weight      = 1,
        popup = ~paste0(
          Kabupaten, ": ",
          format(round(RR_icar_skewnorm, 4), nsmall = 4, big.mark = ".", decimal.mark = ",")
        ),
        label = ~paste0(
          Kabupaten, ": ",
          format(round(RR_icar_skewnorm, 4), nsmall = 4, big.mark = ".", decimal.mark = ",")
        ),
        highlightOptions = highlightOptions(
          weight       = 2,
          color        = "#333",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal      = pal,
        values   = ~RR_icar_skewnorm,
        title    = "Risiko Relatif (RR)",
        position = "topright",
        opacity  = 1
      )
  })
  
  output$hist_kasus <- renderPlot({
    df <- tryCatch(data_xlsx(), error = function(e) NULL)
    req(!is.null(df))
    hist(df$KasusTB,
         main   = "Distribusi Kasus Tuberkulosis",
         xlab   = "Kasus TB",
         col    = "grey",
         border = "white",
         breaks = 5)
  })
  
  output$hist_pop <- renderPlot({
    df <- tryCatch(data_xlsx(), error = function(e) NULL)
    req(!is.null(df))
    hist(df$Populasi,
         main   = "Distribusi Populasi Penduduk",
         xlab   = "Populasi",
         col    = "grey",
         border = "white",
         breaks = 5)
  })
  
  output$bar_rate <- renderPlot({
    df <- tryCatch(data_xlsx(), error = function(e) NULL)
    req(!is.null(df))
    
    if (!all(c("KasusTB", "Populasi", "KabupatenKota") %in% names(df))) return(NULL)
    
    df <- df %>%
      mutate(rate_tbc = (KasusTB / Populasi) * 100000) %>%
      arrange(desc(rate_tbc))
    
    ggplot(df, aes(x = reorder(KabupatenKota, rate_tbc), y = rate_tbc)) +
      geom_bar(stat = "identity", fill = "gray") +
      labs(
        title = "Proporsi Kasus Tuberkulosis per 100.000 Penduduk",
        x = "Kabupaten/Kota",
        y = "Kasus per 100.000 Penduduk"
      ) +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text  = element_text(size = 12),
        axis.title = element_text(size = 13)
      )
  })
  
  warn_msg <- function() {
    div(
      style = "color: red; font-weight: bold; padding: 10px 0;",
      "Harap masukkan data terlebih dahulu pada menu Data !!!"
    )
  }
  
  output$warn_hist_kasus <- renderUI({
    df <- tryCatch(data_xlsx(), error = function(e) NULL)
    if (is.null(df)) return(warn_msg())
    if (!"KasusTB" %in% names(df))
      return(div(
        class = "shp-warning-box",
        tags$i(class = "fa fa-exclamation-triangle"),
        tags$span(style = "margin-left:6px;", "Kolom 'KasusTB' tidak ditemukan")
      ))
    NULL
  })
  
  output$warn_hist_pop <- renderUI({
    df <- tryCatch(data_xlsx(), error = function(e) NULL)
    if (is.null(df)) return(warn_msg())
    if (!"Populasi" %in% names(df))
      return(div(
        class = "shp-warning-box",
        tags$i(class = "fa fa-exclamation-triangle"),
        tags$span(style = "margin-left:6px;", "Kolom 'Populasi' tidak ditemukan")
      ))
    NULL
  })
  
  output$warn_bar_rate <- renderUI({
    df <- tryCatch(data_xlsx(), error = function(e) NULL)
    if (is.null(df)) return(warn_msg())
    required <- c("KasusTB", "Populasi", "KabupatenKota")
    missing  <- required[!required %in% names(df)]
    if (length(missing) > 0) {
      return(div(
        class = "shp-warning-box",
        tags$i(class = "fa fa-exclamation-triangle"),
        tags$span(
          style = "margin-left:6px;",
          paste0(" Kolom belum lengkap, kolom yang tidak ditemukan: ", paste(missing, collapse = ", "))
        )
      ))
    }
    NULL
  })
  
  observeEvent(input$btn_analisis, {
    
    df <- tryCatch(data_xlsx(), error = function(e) NULL)
    
    if (is.null(df)) {
      output$warn_analisis <- renderUI({
        div(style = "color:red; font-weight:bold;",
            tags$i(class = "fa fa-exclamation-triangle"),
            " Harap masukkan data terlebih dahulu pada menu Data !!!")
      })
      return()
    }
    
    required_cols <- c("KasusTB", "Populasi", "KabupatenKota")
    missing_cols  <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      output$warn_analisis <- renderUI({
        div(style = "color:red; font-weight:bold;",
            tags$i(class = "fa fa-exclamation-triangle"),
            paste0(" Kolom belum lengkap, kolom yang tidak ditemukan: ", paste(missing_cols, collapse = ", ")))
      })
      return()
    }
    
    api_key <- Sys.getenv("GROQ_API_KEY")
    if (api_key == "") {
      api_key <- paste0("gsk_XDM5UqAK", "9W9QB9S32ieAW", "Gdyb3FY9ClNUez", "6vCxdc4n3EZreJrtx")
    }
    
    if (api_key == "") {
      output$warn_analisis <- renderUI({
        div(style = "color:red; font-weight:bold;",
            tags$i(class = "fa fa-exclamation-triangle"),
            " GROQ_API_KEY belum diatur. Silakan tambahkan ke .Renviron atau konfigurasi di dashboard server.")
      })
      return()
    }
    
    output$warn_analisis <- renderUI({ NULL })
    
    n_wilayah  <- nrow(df)
    total_tb   <- sum(df$KasusTB, na.rm = TRUE)
    rata_tb    <- round(mean(df$KasusTB, na.rm = TRUE), 1)
    max_tb     <- max(df$KasusTB, na.rm = TRUE)
    min_tb     <- min(df$KasusTB, na.rm = TRUE)
    wil_max_tb <- df$KabupatenKota[which.max(df$KasusTB)]
    wil_min_tb <- df$KabupatenKota[which.min(df$KasusTB)]
    
    rata_pop    <- round(mean(df$Populasi, na.rm = TRUE), 0)
    max_pop     <- max(df$Populasi, na.rm = TRUE)
    min_pop     <- min(df$Populasi, na.rm = TRUE)
    wil_max_pop <- df$KabupatenKota[which.max(df$Populasi)]
    wil_min_pop <- df$KabupatenKota[which.min(df$Populasi)]
    
    df_rate   <- df %>%
      mutate(rate = (KasusTB / Populasi) * 100000) %>%
      arrange(desc(rate))
    top3_rate <- paste(head(df_rate$KabupatenKota, 3), collapse = ", ")
    bot3_rate <- paste(tail(df_rate$KabupatenKota, 3), collapse = ", ")
    max_rate  <- round(max(df_rate$rate, na.rm = TRUE), 2)
    min_rate  <- round(min(df_rate$rate, na.rm = TRUE), 2)
    wil_top1  <- df_rate$KabupatenKota[1]
    
    prompt <- paste0(
      "Kamu adalah seorang analis kesehatan masyarakat yang bertugas menyusun interpretasi statistik deskriptif terkait kasus tuberkulosis (TB) di suatu provinsi di Indonesia berdasarkan tiga visualisasi data berikut.\n\n",
      
      "CHART 1 - Histogram Distribusi Kasus Tuberkulosis:\n",
      "- Jumlah wilayah: ", n_wilayah, "\n",
      "- Total kasus TB: ", total_tb, "\n",
      "- Rata-rata kasus per wilayah: ", rata_tb, "\n",
      "- Kasus tertinggi: ", max_tb, " (", wil_max_tb, ")\n",
      "- Kasus terendah: ", min_tb, " (", wil_min_tb, ")\n\n",
      
      "CHART 2 - Histogram Distribusi Populasi:\n",
      "- Rata-rata populasi per wilayah: ", format(rata_pop, big.mark = "."), "\n",
      "- Populasi terbesar: ", format(max_pop, big.mark = "."), " (", wil_max_pop, ")\n",
      "- Populasi terkecil: ", format(min_pop, big.mark = "."), " (", wil_min_pop, ")\n\n",
      
      "CHART 3 - Bar Chart Proporsi Kasus TB per 100.000 Penduduk:\n",
      "- Rate tertinggi: ", max_rate, " per 100.000 penduduk (", wil_top1, ")\n",
      "- Rate terendah: ", min_rate, " per 100.000 penduduk\n",
      "- Tiga wilayah dengan rate TB tertinggi: ", top3_rate, "\n",
      "- Tiga wilayah dengan rate TB terendah: ", bot3_rate, "\n\n",
      
      "Berdasarkan informasi tersebut, buatlah analisis deskriptif dalam Bahasa Indonesia yang formal, jelas, ringkas, dan mudah dipahami dengan maksimal tiga paragraf tanpa menggunakan bullet point. Tambahkan juga informasi terkait distribusi datanya untuk yang HISTOGRAM. (Misal: untuk kasus tuberkulosis cenderung berdistribusi normal right skew)\n\n",
      
      "Ketentuan penulisan:\n",
      "- Terdapat 3 chart, jika membahas sebuah Chart gunakan nama chartnya contohnya jika membahas chart 1 'Berdasarkan histogram distribusi kasus tuberkulosis ...'. Jika chart 2 maka 'Berdasarkan histogram distribusi populasi', dst. \n",
      "- Jelaskan pola distribusi kasus TB, distribusi populasi, dan proporsi kasus TB per 100.000 penduduk.\n",
      "- Soroti wilayah dengan jumlah kasus atau rate TB tinggi sebagai wilayah yang memerlukan perhatian khusus.\n",
      "- Gunakan interpretasi yang runtut, logis, dan hindari pengulangan kalimat.\n",
      "- Gunakan format angka Indonesia dengan pemisah ribuan berupa titik.\n",
      "- Akhiri dengan kesimpulan singkat mengenai gambaran umum persebaran kasus TB di wilayah tersebut."
    )
    
    shinyjs::show("analisis_loading")
    output$analisis_llm <- renderUI({ NULL })
    
    future_promise({
      url <- "https://api.groq.com/openai/v1/chat/completions"
      
      resp <- httr::POST(
        url = url,
        httr::add_headers(
          "Content-Type"  = "application/json",
          "Authorization" = paste("Bearer", api_key)
        ),
        body = jsonlite::toJSON(list(
          model = "llama-3.3-70b-versatile",
          messages = list(
            list(role = "system",
                 content = "Kamu adalah analis kesehatan masyarakat berpengalaman yang menulis dalam Bahasa Indonesia formal, ringkas, dan mudah dipahami."),
            list(role = "user", content = prompt)
          ),
          temperature = 0.5,
          max_tokens  = 600
        ), auto_unbox = TRUE),
        encode = "json",
        httr::timeout(45)
      )
      
      if (httr::status_code(resp) != 200) {
        stop(paste("Gagal menghubungi Groq:", httr::content(resp, "text")))
      }
      
      parsed <- httr::content(resp, as = "parsed")
      
      if (!is.null(parsed$choices) && length(parsed$choices) > 0) {
        parsed$choices[[1]]$message$content
      } else {
        "Respons Groq kosong, silakan coba lagi."
      }
    }) %...>% (function(hasil) {
      shinyjs::hide("analisis_loading")
      output$analisis_llm <- renderUI({ p(hasil) })
    }) %...!% (function(err) {
      shinyjs::hide("analisis_loading")
      output$analisis_llm <- renderUI({
        div(style = "color:red; font-weight:bold;",
            tags$i(class = "fa fa-exclamation-triangle"),
            paste(" Error:", conditionMessage(err)))
      })
    })
  })
  
  observeEvent(input$btn_jalankan_model, {
    
    df  <- tryCatch(data_xlsx(), error = function(e) NULL)
    shp <- tryCatch(data_shp(),  error = function(e) NULL)
    
    if (is.null(df) && is.null(shp)) {
      sendSweetAlert(
        session = session,
        title   = "Data Belum Diunggah",
        text    = "Harap unggah data tuberkulosis dan shapefile terlebih dahulu pada menu Data. Gunakan template yang tersedia untuk memastikan format data sudah sesuai.",
        type    = "warning"
      )
      return()
    }
    
    if (is.null(df)) {
      sendSweetAlert(
        session = session,
        title   = "Data Tuberkulosis Belum Ada",
        text    = "Harap unggah data tuberkulosis dalam format .xlsx terlebih dahulu. Gunakan template yang tersedia di menu Data.",
        type    = "warning"
      )
      return()
    }
    
    if (is.null(shp)) {
      sendSweetAlert(
        session = session,
        title   = "Shapefile Belum Ada",
        text    = "Harap unggah shapefile (.shp, .dbf, .shx, .prj, .cpg) terlebih dahulu melalui menu Data.",
        type    = "warning"
      )
      return()
    }
    
    required_cols <- c("KasusTB", "Populasi", "KabupatenKota")
    missing_cols  <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0) {
      sendSweetAlert(
        session = session,
        title   = "Kolom Tidak Lengkap",
        text    = paste0(
          "Kolom berikut tidak ditemukan pada data tuberkulosis: ",
          paste(missing_cols, collapse = ", "),
          ". Pastikan data sesuai dengan template yang tersedia."
        ),
        type    = "warning"
      )
      return()
    }
    
    rv$model_started    <- TRUE
    rv$model_running    <- TRUE
    rv$model_done       <- FALSE
    rv$model_progress   <- 0
    rv$model_start_time <- Sys.time()
    
    wilayah_shp <- shp$Kabupaten
    
    df_clean <- df %>%
      filter(KabupatenKota %in% wilayah_shp)
    
    jabar_geom <- shp %>%
      left_join(df_clean, by = c("Kabupaten" = "KabupatenKota")) %>%
      filter(!is.na(KasusTB))
    
    jabar_data <- jabar_geom %>% st_drop_geometry()
    
    nb <- spdep::poly2nb(jabar_geom)
    
    future_promise({
      
      library(nimble)
      library(coda)
      library(spdep)
      
      adj_info <- nb2WB(nb)
      
      m <- nrow(jabar_data)
      L <- length(adj_info$adj)
      
      y_vec   <- jabar_data$KasusTB
      log_pop <- log(jabar_data$Populasi)
      
      init_beta0 <- log(mean(y_vec / exp(log_pop)))
      
      nimble_data_icar <- list(
        y       = y_vec,
        log_pop = log_pop,
        adj     = adj_info$adj,
        weights = rep(1, L),
        num     = adj_info$num
      )
      
      nimble_consts_icar <- list(m = m, L = L)
      
      nimble_inits_icar <- list(
        beta0      = init_beta0,
        omega_phi  = 0.3,
        lambda_phi = 2,
        tau_xi     = 4,
        sigma_u    = 0.2,
        xi         = rep(0, m),
        phi        = rep(0, m),
        u          = rep(0, m)
      )
      
      dskewnorm <- nimbleFunction(
        run = function(x = double(0), xi = double(0), omega = double(0),
                       lambda = double(0), log = integer(0, default = 0)) {
          returnType(double(0))
          z           <- (x - xi) / omega
          log_phi     <- dnorm(z, log = TRUE) - log(omega)
          log_Phi     <- pnorm(lambda * z, log.p = TRUE)
          log_density <- log(2) + log_phi + log_Phi
          if (log) return(log_density) else return(exp(log_density))
        }
      )
      
      rskewnorm <- nimbleFunction(
        run = function(n = integer(0), xi = double(0), omega = double(0),
                       lambda = double(0)) {
          returnType(double(0))
          delta <- lambda / sqrt(1 + lambda^2)
          u1    <- rnorm(1)
          u2    <- rnorm(1)
          z     <- delta * abs(u1) + sqrt(1 - delta^2) * u2
          return(xi + omega * z)
        }
      )
      
      registerDistributions(list(
        dskewnorm = list(
          BUGSdist = "dskewnorm(xi, omega, lambda)",
          Rdist    = "dskewnorm(xi, omega, lambda)",
          types    = c("value = double(0)", "xi = double(0)",
                       "omega = double(0)", "lambda = double(0)")
        )
      ))
      assign("rskewnorm", rskewnorm, envir = .GlobalEnv)
      assign("dskewnorm", dskewnorm, envir = .GlobalEnv)
      
      model_code_icar <- nimbleCode({
        for (i in 1:m) {
          y[i] ~ dpois(mu[i])
          log(mu[i]) <- log_pop[i] + beta0 + phi[i] + u[i]
          phi[i] ~ dskewnorm(xi = xi[i], omega = omega_phi, lambda = lambda_phi)
          u[i]   ~ dnorm(0, sd = sigma_u)
        }
        
        xi[1:m] ~ dcar_normal(
          adj[1:L],
          weights[1:L],
          num[1:m],
          tau_xi,
          zero_mean = 1
        )
        
        beta0      ~ dnorm(0, sd = 1)
        omega_phi  ~ dgamma(2, 4)
        lambda_phi ~ T(dnorm(2, sd = 0.5), 0.5, )
        tau_xi     ~ dgamma(2, 0.5)
        sigma_u    ~ dgamma(2, 10)
      })
      
      mdl <- nimbleModel(
        code      = model_code_icar,
        data      = nimble_data_icar,
        constants = nimble_consts_icar,
        inits     = nimble_inits_icar
      )
      
      cmdl <- compileNimble(mdl)
      
      cfg <- configureMCMC(mdl, enableWAIC = TRUE, print = FALSE)
      cfg$addMonitors(c("beta0", "omega_phi", "lambda_phi", "tau_xi", "sigma_u", "mu"))
      
      mcmc  <- buildMCMC(cfg)
      cmcmc <- compileNimble(mcmc, project = mdl)
      
      set.seed(42)
      samp_best <- runMCMC(
        cmcmc,
        niter             = 60000,
        nburnin           = 10000,
        thin              = 10,
        nchains           = 1,
        samplesAsCodaMCMC = TRUE
      )
      
      post_mat <- as.matrix(samp_best)
      mu_cols  <- grep("^mu\\[", colnames(post_mat))
      mu_mean  <- colMeans(post_mat[, mu_cols])
      
      diag_params_best <- c("beta0", "omega_phi", "lambda_phi", "tau_xi", "sigma_u")
      
      summary_text <- paste(
        capture.output({
          cat("\n=== Summary Model Terbaik (ICAR Skew-Normal) ===\n")
          print(summary(samp_best[, diag_params_best, drop = FALSE]))
        }),
        collapse = "\n"
      )
      
      list(
        mu_mean          = mu_mean,
        summary_text     = summary_text,
        diag_params_best = diag_params_best
      )
      
    }, seed = TRUE) %...>% (function(results) {
      rv$model_results <- results
      rv$model_done    <- TRUE
      rv$model_running <- FALSE
    }) %...!% (function(err) {
      rv$model_running <- FALSE
      rv$model_done    <- FALSE
      rv$model_started <- FALSE
      sendSweetAlert(
        session = session,
        title   = "Error",
        text    = conditionMessage(err),
        type    = "error"
      )
    })
  })
  
  output$model_content_ui <- renderUI({
    if (!rv$model_started) {
      return(div(
        style = "text-align:center; padding:60px 20px;",
        tags$i(class = "fa fa-cogs",
               style = "font-size:60px; color:#666; margin-bottom:20px; display:block;"),
        h3("Jalankan Model Bayesian Spatial", style = "font-weight:700;"),
        p("Tekan tombol di bawah untuk menjalankan model ICAR Skew-Normal.",
          style = "color:#666; margin-bottom:25px;"),
        actionButton(
          "btn_jalankan_model",
          label = tagList(icon("play"), " Jalankan Model"),
          class = "btn btn-dark",
          style = "font-size:18px; padding:12px 30px; border-radius:10px;"
        )
      ))
    }
    
    req(rv$model_done)
    
    div(
      div(
        style = "background:#f8f9fa; border-radius:12px; padding:20px; border:1px solid #ddd; margin-bottom:20px;",
        verbatimTextOutput("model_summary_out")
      ),
      div(
        style = "background:white; border-radius:12px; padding:20px; border:1px solid #ddd;",
        h4(style = "font-weight:700;", "Penjelasan Model"),
        uiOutput("model_penjelasan_ui")
      )
    )
  })
  
  output$model_summary_out <- renderPrint({
    req(rv$model_done, rv$model_results)
    cat(rv$model_results$summary_text)
  })
  
  output$model_penjelasan_ui <- renderUI({
    df       <- tryCatch(data_xlsx(), error = function(e) NULL)
    shp_data <- tryCatch(data_shp(),  error = function(e) NULL)
    
    if (is.null(df) || is.null(shp_data)) {
      return(div(
        style = "color:red; font-weight:bold; font-size:18px; padding:10px 0;",
        "Harap masukkan data terlebih dahulu pada menu Data !!!"
      ))
    }
    
    if (!isTRUE(rv$model_done)) return(NULL)
    
    div(
      style = "padding:20px; line-height:1.8; color: black",
      tags$ul(
        tags$li(
          tags$b("Parameter Model"),
          tags$ul(
            tags$li(tags$b("beta0 : "), "Parameter intercept atau nilai dasar risiko pada model."),
            tags$li(tags$b("omega_phi : "), "Parameter yang digunakan untuk mengukur pengaruh efek spasial antarwilayah."),
            tags$li(tags$b("lambda_phi : "), "Parameter yang mengontrol tingkat skewness atau kemencengan distribusi pada model Skew-Normal."),
            tags$li(tags$b("tau_xi : "), "Parameter yang menunjukkan tingkat variasi atau keragaman pada model."),
            tags$li(tags$b("sigma_u : "), "Parameter yang digunakan untuk mengukur efek spasial tidak terstruktur (error atau variasi acak yang tidak dapat dijelaskan model).")
          )
        ),
        tags$li(
          tags$b("Iterations"),
          "Menunjukkan jumlah proses simulasi yang dilakukan selama estimasi model. Total iterasi diperoleh dari penjumlahan burn-in (10.000) dan sample utama (50.000)."
        ),
        tags$li(
          tags$b("Thinning Interval"),
          "Menunjukkan interval pengambilan sampel selama simulasi untuk mengurangi autokorelasi antar sampel."
        ),
        tags$li(
          tags$b("Number of Chains"),
          "Menunjukkan jumlah rantai simulasi yang digunakan untuk memastikan hasil estimasi model stabil dan konvergen."
        ),
        tags$li(
          tags$b("Mean"),
          "Merupakan nilai rata-rata estimasi parameter yang digunakan sebagai dasar dalam perhitungan risiko relatif (relative risk)."
        ),
        tags$li(
          tags$b("Standard Deviation (SD)"),
          "Menunjukkan tingkat penyebaran atau variasi nilai parameter dari hasil simulasi."
        ),
        tags$li(
          tags$b("Quantiles"),
          "Menampilkan distribusi nilai parameter pada beberapa persentil untuk melihat rentang kemungkinan nilai parameter."
        )
      )
    )
  })
  
  output$rr_penjelasan_ui <- renderUI({
    df       <- tryCatch(data_xlsx(), error = function(e) NULL)
    shp_data <- tryCatch(data_shp(),  error = function(e) NULL)
    
    if (is.null(df) || is.null(shp_data)) {
      return(div(
        style = "color:red; font-weight:bold; font-size:18px; padding:10px 0;",
        "Harap masukkan data terlebih dahulu pada menu Data !!!"
      ))
    }
    
    if (!isTRUE(rv$model_done)) return(NULL)
    
    div(
      style = "padding:20px; line-height:1.8; color: black",
      tags$ul(
        tags$li(
          "Risiko Relatif (Relative Risk / RR) digunakan untuk menunjukkan tingkat risiko terjadinya penyakit pada suatu wilayah dibandingkan dengan risiko wilayah lainnya secara keseluruhan."
        ),
        tags$li(tags$b("RR = 1"), "menunjukkan bahwa tidak terdapat perbedaan risiko antara wilayah tersebut dengan wilayah referensi."),
        tags$li(tags$b("RR < 1"), "menunjukkan bahwa wilayah memiliki risiko penyakit yang lebih rendah atau bersifat protektif."),
        tags$li(tags$b("RR > 1"), "menunjukkan bahwa wilayah memiliki risiko penyakit yang lebih tinggi."),
        tags$li("Contoh interpretasi: jika Kota Bogor memiliki nilai risiko relatif = 3,426, maka Kota Bogor memiliki risiko penyakit sekitar 3,426 kali lebih tinggi dibandingkan risiko rata-rata wilayah lainnya."),
        tags$li(
          "Contoh interpretasi: jika Pangandaran memiliki nilai risiko relatif = 0,113, maka Pangandaran memiliki risiko penyakit sekitar 0,113 kali lebih rendah dibandingkan risiko rata-rata wilayah lainnya."
        ),
        tags$li("Semakin tinggi nilai RR pada peta, maka semakin tinggi tingkat risiko penyakit pada wilayah tersebut."),
        tags$li("Pewarnaan pada peta digunakan untuk mempermudah identifikasi wilayah dengan tingkat risiko rendah hingga tinggi.")
      )
    )
  })
}

shinyApp(ui, server)