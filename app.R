# Expenditure tracking dashboard (fictional data)

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# ---- Partners and budgets (total EUR 450,000) ----
partners <- data.frame(
  partner = paste("Partner", 1:9),
  region  = c("Sub-Saharan Africa", "Sub-Saharan Africa", "Sub-Saharan Africa",
              "Sub-Saharan Africa", "Asia", "America",
              "Sub-Saharan Africa", "Sub-Saharan Africa", "Sub-Saharan Africa"),
  budget  = c(55000, 50000, 50000, 50000, 45000, 50000, 50000, 55000, 45000)
)
partner_levels <- partners$partner
region_levels  <- c("Sub-Saharan Africa", "Asia", "America")

# ---- Reported expenditure since January 2026 (edit freely) ----
tx <- tribble(
  ~partner,    ~month,    ~amount,
  # Spent everything at once in the first months
  "Partner 2", "2026-03", 50000,
  "Partner 5", "2026-04", 45000,
  # Part in the first months, part later
  "Partner 1", "2026-03", 25000,
  "Partner 1", "2026-07", 30000,
  "Partner 8", "2026-02", 20000,
  "Partner 8", "2026-09", 35000,
  # Everything at once in summer
  "Partner 4", "2026-06", 50000,
  "Partner 6", "2026-08", 50000,
  # Part in summer, part just now
  "Partner 3", "2026-07", 25000,
  "Partner 3", "2026-10", 25000,
  "Partner 7", "2026-06", 20000,
  "Partner 7", "2026-10", 30000,
  # The exception: not fully spent yet (78%)
  "Partner 9", "2026-07", 20000,
  "Partner 9", "2026-09", 15000
) |>
  arrange(month, partner) |>
  mutate(ref = sprintf("REP-%03d", row_number())) |>
  group_by(partner) |>
  mutate(type = if_else(n() == 1, "Lump sum", "Tranche")) |>
  ungroup()

months <- format(seq(as.Date("2026-01-01"), by = "month", length.out = 10), "%Y-%m")
month_label <- function(m) format(as.Date(paste0(m, "-01")), "%b %Y")
month_choices <- setNames(months, month_label(months))

# ---- Look and feel ----
col_a <- "#b13fa6"  # magenta
col_b <- "#2fb9c0"  # teal
col_c <- "#c9831d"  # orange

css <- "
body { background:#3d4160; color:#fff; }
.container-fluid { padding: 16px 24px; }
.dash-title { font-size:20px; font-weight:700; margin:0; }
.btn-filter { background:#1fb5e8; color:#fff; font-weight:700; border:none;
              text-transform:uppercase; border-radius:3px; }
.btn-filter:hover { background:#17a0cf; color:#fff; }
.card-w { border:1px solid #fff; border-radius:3px; margin-bottom:18px; overflow:hidden; background:#50536f; }
.card-t { padding:8px 10px 0 10px; font-weight:700; font-size:14px; }
.card-b { padding:6px 10px; }
.card-f { background:#fff; color:#555; font-size:10px; padding:5px 10px; }
.pill { display:inline-block; padding:2px 12px; border-radius:12px; font-size:10px;
        font-weight:700; color:#fff; }
.pill-high { background:#b13fa6; }
.pill-medium { background:#2bb3c0; }
.pill-low { background:#e9741b; }
.shiny-output-error-validation { color:#e8e9f2; font-size:12px; padding:20px 0; }
.dataTables_wrapper { color:#fff; font-size:11px; }
table.dataTable thead th, table.dataTable thead td {
  background-color:transparent !important; color:#fff !important; text-align:center;
  text-transform:uppercase; font-size:10px; border-bottom:1px solid rgba(255,255,255,.3) !important; }
table.dataTable tbody td { background-color:transparent !important; color:#fff !important;
  border-top:1px solid rgba(255,255,255,.12) !important; font-size:10px; }
table.dataTable tbody tr { background-color:transparent !important; }
.dataTables_scrollBody { border-bottom:none !important; }
"

theme_dash <- function() {
  theme_minimal(base_size = 11) +
    theme(
      text = element_text(colour = "#e8e9f2"),
      axis.text = element_text(colour = "#e8e9f2"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#ffffff22"),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      legend.position = "top",
      legend.title = element_blank(),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA)
    )
}

card <- function(title, body, source = "Source: fictional data (for illustration only)") {
  div(class = "card-w",
      div(class = "card-t", title),
      div(class = "card-b", body),
      div(class = "card-f", source))
}

pill <- function(label, cls) sprintf('<span class="pill pill-%s">%s</span>', cls, label)

# ---- User interface ----
ui <- fluidPage(
  tags$head(tags$style(HTML(css))),
  div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:14px;",
      p(class = "dash-title", "Project expenditure tracker"),
      actionButton("change_filters", "Change filters", icon = icon("filter"), class = "btn-filter")),
  fluidRow(
    column(6, card("Budget execution by partner",
                   plotOutput("budget_plot", height = 280, width = "100%"))),
    column(6, card("Reported expenditure by month and region",
                   plotOutput("month_plot", height = 280, width = "100%")))
  ),
  fluidRow(
    column(6, card("Partner budget status", DTOutput("status_table"))),
    column(6, card("Reported payments", DTOutput("tx_table")))
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  f <- reactiveValues(partners = partner_levels, month = months[length(months)])
  
  observeEvent(input$change_filters, {
    showModal(modalDialog(
      title = "Change filters",
      selectInput("f_partners", "Partners", choices = partner_levels,
                  selected = f$partners, multiple = TRUE),
      selectInput("f_month", "Expenditure reported up to",
                  choices = month_choices, selected = f$month),
      footer = tagList(modalButton("Cancel"), actionButton("apply", "Apply")),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$apply, {
    req(input$f_partners)
    f$partners <- input$f_partners
    f$month <- input$f_month
    removeModal()
  })
  
  filtered_tx <- reactive({
    tx |>
      filter(partner %in% f$partners, month <= f$month) |>
      left_join(select(partners, partner, region), by = "partner")
  })
  
  summary_df <- reactive({
    spent_by_partner <- filtered_tx() |>
      group_by(partner) |>
      summarise(spent = sum(amount), .groups = "drop")
    
    partners |>
      filter(partner %in% f$partners) |>
      left_join(spent_by_partner, by = "partner") |>
      mutate(
        spent     = coalesce(spent, 0),
        remaining = budget - spent,
        pct_spent = spent / budget,
        level     = case_when(pct_spent >= 0.85 ~ "High",
                              pct_spent >= 0.75 ~ "Medium",
                              TRUE ~ "Low")
      )
  })
  
  # Top left: budget execution by partner (stacked horizontal bars)
  output$budget_plot <- renderPlot({
    split <- filtered_tx() |>
      group_by(partner) |>
      summarise(a = sum(amount[month <= "2026-06"]),
                b = sum(amount[month >  "2026-06"]), .groups = "drop")
    
    d <- partners |>
      filter(partner %in% f$partners) |>
      left_join(split, by = "partner") |>
      mutate(a = coalesce(a, 0), b = coalesce(b, 0), remaining = budget - a - b)
    
    seg_levels <- c("Spent Jan-Jun 2026", "Spent Jul 2026 onward", "Remaining")
    mk <- function(v, lab) data.frame(partner = d$partner, value = v, segment = lab)
    long <- bind_rows(mk(d$a, seg_levels[1]), mk(d$b, seg_levels[2]), mk(d$remaining, seg_levels[3])) |>
      mutate(segment = factor(segment, levels = seg_levels))
    
    ggplot(long, aes(x = value, y = partner, fill = segment)) +
      geom_col(position = position_stack(reverse = TRUE), width = 0.7) +
      scale_y_discrete(limits = rev(partner_levels)) +
      scale_x_continuous(labels = scales::comma) +
      scale_fill_manual(values = setNames(c(col_a, col_b, col_c), seg_levels)) +
      labs(x = "Amount (EUR)", y = NULL) +
      theme_dash()
  }, bg = "transparent", res = 96)
  
  # Top right: expenditure by month, stacked by region
  output$month_plot <- renderPlot({
    validate(need(nrow(filtered_tx()) > 0, "No expenditure reported up to this month."))
    
    d <- filtered_tx() |>
      mutate(region = factor(region, levels = region_levels))
    
    ggplot(d, aes(x = amount, y = month, fill = region)) +
      geom_col(width = 0.7) +
      scale_y_discrete(limits = rev(months), labels = month_label) +
      scale_x_continuous(labels = scales::comma) +
      scale_fill_manual(values = setNames(c(col_a, col_b, col_c), region_levels), drop = FALSE) +
      labs(x = "Reported expenditure (EUR)", y = NULL) +
      theme_dash()
  }, bg = "transparent", res = 96)
  
  # Bottom left: partner status table with pills
  output$status_table <- renderDT({
    summary_df() |>
      mutate(level = pill(level, tolower(level))) |>
      select(partner, region, budget, spent, pct_spent, level) |>
      datatable(rownames = FALSE, escape = FALSE, class = "compact",
                colnames = c("Partner", "Region", "Budget", "Spent", "% spent", "Execution level"),
                options = list(scrollY = "230px", scrollCollapse = TRUE, paging = FALSE,
                               dom = "t", ordering = FALSE,
                               columnDefs = list(list(className = "dt-center", targets = "_all")))) |>
      formatCurrency(c("budget", "spent"), currency = "\u20ac", digits = 0) |>
      formatPercentage("pct_spent", digits = 0)
  })
  
  # Bottom right: payments table with pills
  output$tx_table <- renderDT({
    validate(need(nrow(filtered_tx()) > 0, "No payments reported up to this month."))
    
    filtered_tx() |>
      arrange(desc(month), partner) |>
      mutate(month = month_label(month),
             type = pill(type, if_else(type == "Lump sum", "medium", "high"))) |>
      select(ref, month, partner, region, amount, type) |>
      datatable(rownames = FALSE, escape = FALSE, class = "compact",
                colnames = c("Report ref", "Month", "Partner", "Region", "Amount", "Payment type"),
                options = list(scrollY = "230px", scrollCollapse = TRUE, paging = FALSE,
                               dom = "t", ordering = FALSE,
                               columnDefs = list(list(className = "dt-center", targets = "_all")))) |>
      formatCurrency("amount", currency = "\u20ac", digits = 0)
  })
}
shinyApp(ui, server)

