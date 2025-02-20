
`%notin%` <- Negate(`%in%`)
library(readxl)
library(tidyverse)
full_theme <- function(){
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, 
                                     vjust = 1, hjust = 1),
          axis.title = element_text(size = 14),
          plot.caption = element_text(size = 12),
          legend.title=element_text(size=14),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 15)
    )
}
load("pharm_new.RData")
#Function for making pretty y-axis breaks
integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
    }
    return(fxn)
}


oxyhydro <- c("Hydromorphone", "Oxycodone")

abc_oh <- pharm_new |> 
    filter(generic_match %in% oxyhydro &
               unit == "TAB") |> 
    select(province, the_year, generic_match, quant,
           unit, substance_data, subs_quant, 
           facility, incident_type, incident_subtype, grouped_cat) |> 
          mutate(prov_drug = paste(province, generic_match, sep = "\n"))

prov_pick <- sort(unique(abc_oh$province))
op_lev <- data.frame(Prov = rep(prov_pick, length(oxyhydro)),
                     Drug = rep(oxyhydro, length(prov_pick))) |> 
    unite("lev", Prov:Drug, sep = "\n", remove = F) |> 
    arrange(Drug, Prov)
op_lev <- crossing(prov_pick, oxyhydro) |> 
    unite("lev", prov_pick:oxyhydro, sep = "\n", remove = F) |> 
    arrange(oxyhydro, prov_pick)

abc_oh$prov_drug <- factor(abc_oh$prov_drug,
                           levels = op_lev$lev, ordered = T)
the_years <- unique(abc_oh$the_year)
abc_oh$incident_type[abc_oh$incident_type == "Loss"] <- "Lost"
library(shiny)
library(shinythemes)
library(bslib)

group_options <- c("100-499","500-999","1000-1499", "1500-1999","2000+")
light <- bslib::bs_theme(version = 5, bootswatch = "flatly")
dark <- bslib::bs_theme(version = 5, bootswatch = "darkly")
source("explanation.R")

####--------------------------------UI--------------------------------------####
# Define UI for application that draws a histogram
ui <- navbarPage("Opioids Lost/Stolen at Pharmacies 2018-2023",
    tabPanel("Opioid Tablets",
    # Sidebar with a slider input for number of bins
    card(
    sidebarLayout(
        sidebarPanel(width = 2,
            checkboxGroupInput("provs", "Select Provinces",
                        choices = unique(abc_oh$province),
                        selected = c("AB", "BC", "ON")),
            checkboxGroupInput("grouped", "Select Grouped Categories",
                        choices = sort(unique(abc_oh$grouped_cat)),
                        selected = group_options),
            radioButtons("incidents", "Select Incident Types",
                        choices = sort(unique(abc_oh$incident_type)),
                        selected = c("Lost")),
            radioButtons("percap", "Per 100 000?",
                         choices = c("Yes", "No"),
                         selected = "No")
        ),
  ####---------------------------------Panel 2------------------------------####
        # Show a plot of the generated distribution
           mainPanel(plotOutput("mplot"),
                              width = 10)
    )
        ),
        card(
            tags$h1("About this data")
           )
        ), 
    tabPanel("Total Opioid Tablets Lost/Stolen",
                    sidebarPanel(width =2,
                        checkboxGroupInput("prov2", "Select Provinces",
                                           choices = unique(abc_oh$province),
                                           selected = c("AB", "BC")),
                        checkboxGroupInput("group2", "Select Grouped Categories",
                                           choices = sort(unique(abc_oh$grouped_cat)),
                                           selected = group_options),
                        radioButtons("mpills", "By Mass or Number of Pills",
                                     choices = c("Mass", "Number of Pills"),
                                     selected = "Number of Pills"),
                        radioButtons("percap2", "Per 100 000 People?",
                                     choices = c("Yes", "No"),
                                     selected = "No")
                        
                    
                    ),
             mainPanel(plotOutput("mplot2", height = 800), width = 10)
    ),
  ####----------------------All Incident Line graph-------------------------####
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    t_incidents <- reactive(input$incidents)
    f_var <- reactive({
        abc_oh |>
            filter(province %in% input$provs,
                   grouped_cat %in% input$grouped,
                   incident_type %in% t_incidents()) |> 
            group_by(province, generic_match, 
                     the_year, grouped_cat) |> 
            count(name = "the_count") |> 
            ungroup() |> 
            mutate(grouped_cat = fct_rev(grouped_cat)) |> 
            left_join(pop)
    })
  ####----------------------Opioid Tablets Incidents----------------------####
    output$mplot <- renderPlot({
        inc_plot <- f_var()
        validate(
            need(nrow(inc_plot) >0.9, "Please select some outputs")
        )
        if(input$percap == "Yes"){
            inc_plot <- inc_plot |> 
                mutate(the_count = the_count/population * 100000)
            the_title = paste("Oxycodone & Hydromorphone Tablets ", 
            t_incidents(), " per 100 000 People", sep = "")
            y_ax = "Incidents per 100 000 People"
            } else {
                the_title = paste("Oxycodone & Hydromorphone Tablets ", 
                t_incidents(), " by Year", sep = "")
                y_ax = "Number of Incidents"
            }
        max_y = 
            inc_plot |> 
                group_by(province, the_year, generic_match) |> 
                summarise(max_count = sum(the_count)) |> 
                pull(max_count) |> 
                max()
        max_y = max_y + max_y * 0.1
        ggplot(inc_plot, aes(x = the_year, y = the_count,
                            fill = grouped_cat)) +
            geom_col() +
            scale_x_date(date_labels = "%Y", breaks = the_years) +
            scale_y_continuous(expand = c(0,0), limits = c(0, max_y)) +
            labs(x = "Year", y = y_ax, fill = "Quantity Reported",
                 title = the_title, 
                 caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies") +
            theme_bw() +
            facet_grid(province ~generic_match) +
            full_theme()
    })
    output$explain <- renderText({
        paste("I recommend only using grouped categories >100, ")
    })
    ####----------------------Total Oxycodone & ----------------------####
    
    
    
    p_weight <- reactive({req(input$mpills)})
    pill_filter <- reactive({
        df = abc_oh |>
            filter(province %in% input$prov2,
                   grouped_cat %in% input$group2) 
        if(p_weight() == "Number of Pills" | is.null(p_weight())){
            df = df |> 
                group_by(province, prov_drug, the_year,
                         incident_type, substance_data) |> 
                summarise(total = sum(quant)) |> 
                ungroup()
        } else {
            df = df |>
                group_by(province, prov_drug, the_year, 
                         incident_type, substance_data)  |> 
                summarise(total = sum(subs_quant)) |> 
                ungroup()
        }
        df = df |> 
            left_join(pop)
        return(df)
    })
    
    output$mplot2 <- renderPlot({
        mwplot <- pill_filter()
        validate(
            need(nrow(mwplot) >0.9, "Please select some outputs")
        )
        if(input$percap2 == "Yes"){
            mwplot <- mwplot |>
                mutate(total = total/population * 100000)
            y_ax1 <- "per 100 000 people"
        } else {
            y_ax1 <- ""
        }
        if(p_weight() == "Number of Pills"){
            the_title <- paste0("Number of Tablets Lost/Stolen from Medical Centres ",
                           y_ax1)
            y_ax <- paste0("Number of Tablets ", y_ax1)
            
        } else {
            the_title <- paste0("Total MG Lost/Stolen ", y_ax1)
            y_ax <- paste0("Total Mass (mg) ", y_ax1)
        }
        max_y2 <- mwplot |> 
            group_by(prov_drug, incident_type, the_year) |> 
            summarise(max_count = sum(total)) |> 
            pull(max_count) |> 
            max()
        max_y2 = max_y2 + (max_y2 * 0.1)
       ggplot(mwplot, aes(x = the_year, y = total,
                                  fill = substance_data)) +
            geom_col() +
            scale_x_date(date_labels = "%Y", breaks = the_years) +
            scale_y_continuous(labels = scales::comma,
                                  expand = c(0,0),
                                  limits = c(0, max_y2),
                               breaks = integer_breaks(n = 6)) +
            labs(x = "Year", y = y_ax, fill = "Quantity Reported",
                 title = the_title,
                 caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies") +
           theme_bw() +
           facet_grid(incident_type ~ prov_drug, scales = "free_x") +
           full_theme() +
           theme(legend.position = "bottom")


    })
}

# Run the application 
shinyApp(ui = ui, server = server)
