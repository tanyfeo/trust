library(shiny)
library(haven)
library(dplyr)
library(DT)
library(ggplot2)

ESS10 <- read_sav("./ESS10.sav")
bg <- filter(ESS10, cntry == 'BG')

bg_trust_all <- select(bg, ppltrst, pplfair, pplhlp, gndr)
bg_trust_all$gndr <- ifelse(bg_trust_all$gndr == 1, "Male", "Female")

# Separate datasets for each variable and gender
ppltrst_all <- data.frame(value = na.omit(bg_trust_all$ppltrst))
ppltrst_male <- data.frame(value = na.omit(filter(bg_trust_all, gndr == "Male")$ppltrst))
ppltrst_female <- data.frame(value = na.omit(filter(bg_trust_all, gndr == "Female")$ppltrst))

pplhlp_all <- data.frame(value = na.omit(bg_trust_all$pplhlp))
pplhlp_male <- data.frame(value = na.omit(filter(bg_trust_all, gndr == "Male")$pplhlp))
pplhlp_female <- data.frame(value = na.omit(filter(bg_trust_all, gndr == "Female")$pplhlp))

pplfair_all <- data.frame(value = na.omit(bg_trust_all$pplfair))
pplfair_male <- data.frame(value = na.omit(filter(bg_trust_all, gndr == "Male")$pplfair))
pplfair_female <- data.frame(value = na.omit(filter(bg_trust_all, gndr == "Female")$pplfair))



# Summary statistics
summary_stats_ppltrst <- bg_trust_all %>%
  group_by(gndr) %>%
  summarize(
    Mean = format(round(mean(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    Median = format(round(median(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    SD = format(round(sd(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    Min = format(round(min(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    Max = format(round(max(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2)
  ) %>%
  rename(Gender = gndr)

summary_stats_pplhlp <- bg_trust_all %>%
  group_by(gndr) %>%
  summarize(
    Mean = format(round(mean(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    Median = format(round(median(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    SD = format(round(sd(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    Min = format(round(min(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    Max = format(round(max(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2)
  ) %>%
  rename(Gender = gndr)

summary_stats_pplfair <- bg_trust_all %>%
  group_by(gndr) %>%
  summarize(
    Mean = format(round(mean(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    Median = format(round(median(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    SD = format(round(sd(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    Min = format(round(min(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    Max = format(round(max(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2)
  ) %>%
  rename(Gender = gndr)


summary_stats_ppltrst_male <- bg_trust_all %>%
  filter(gndr == "Male") %>%
  summarize(
    Mean = format(round(mean(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    Median = format(round(median(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    SD = format(round(sd(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    Min = format(round(min(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    Max = format(round(max(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2)
  )

summary_stats_pplhlp_male <- bg_trust_all %>%
  filter(gndr == "Male") %>%
  summarize(
    Mean = format(round(mean(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    Median = format(round(median(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    SD = format(round(sd(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    Min = format(round(min(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    Max = format(round(max(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2)
  )

summary_stats_pplfair_male <- bg_trust_all %>%
  filter(gndr == "Male") %>%
  summarize(
    Mean = format(round(mean(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    Median = format(round(median(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    SD = format(round(sd(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    Min = format(round(min(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    Max = format(round(max(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2)
  )

summary_stats_ppltrst_female <- bg_trust_all %>%
  filter(gndr == "Female") %>%
  summarize(
    Mean = format(round(mean(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    Median = format(round(median(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    SD = format(round(sd(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    Min = format(round(min(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2),
    Max = format(round(max(as.numeric(ppltrst), na.rm = TRUE), 2), nsmall = 2)
  )

summary_stats_pplhlp_female <- bg_trust_all %>%
  filter(gndr == "Female") %>%
  summarize(
    Mean = format(round(mean(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    Median = format(round(median(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    SD = format(round(sd(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    Min = format(round(min(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2),
    Max = format(round(max(as.numeric(pplhlp), na.rm = TRUE), 2), nsmall = 2)
  )

summary_stats_pplfair_female <- bg_trust_all %>%
  filter(gndr == "Female") %>%
  summarize(
    Mean = format(round(mean(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    Median = format(round(median(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    SD = format(round(sd(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    Min = format(round(min(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2),
    Max = format(round(max(as.numeric(pplfair), na.rm = TRUE), 2), nsmall = 2)
  )


# Creating variables for the text, that is shown in the describing tab
variables_text_1 <- "This tab provides information about the variables and the dataset used in the analysis."

variables_text_2 <- "1. Dataset Description"
variables_text_3 <- "The dataset used in this analysis is the European Social Survey (ESS) round 10. The ESS is a comprehensive survey conducted every two years to gather data on various social and political aspects of European countries. ESS round 10 focuses on understanding social attitudes, behaviors, and beliefs across different countries in Europe. It provides a rich source of information for studying social trust and its associated variables."

variables_text_4 <- "2. Country: Bulgaria"
variables_text_5 <- "Bulgaria has been chosen as the focus country for studying social trust. This choice is particularly interesting considering the context of post-communist countries. Social trust can be understood from two perspectives: as an individual characteristic or as a societal property. In this project, we emphasize the latter perspective, specifically exploring the success and well-being theory, which suggests that adult life experiences shape an individual's sense of social trust (Kim, 2020).
Social trust in Bulgaria experienced a decline in the early 2010s, primarily associated with the fall of communism (Pehlivanova, 2009). At that time, Bulgaria was even referred to as 'a low-trusted society,' and the lack of social trust was linked to lower levels of self-assessed quality of life and well-being (Tilkidjiev, 2011). However, recent studies have shifted the focus to social trust in relation to political issues, creating a scientific gap in current research. Therefore, this project aims to investigate social trust in Bulgaria and contribute to filling the gap in recent studies."

variables_text_6 <- "3. Main Variables of Social Trust"
variables_text_7 <- " round 10 considers three main measures of social trust, which are crucial for understanding its dynamics and determinants. These variables provide insights into different aspects of social trust and its implications for social cohesion and well-being. The three main variables of social trust are:
"
0
table_with_variables <- data.frame(
  Variable = c("Trust in People (ppltrst)", "Helpfulness of People (pplhlp)", "Fairness of People (pplfair)"),
  Description = c("On the scale from 0 to 10 people had to decide whether people can be trusted or not (10 = most people can be trusted).", "On the scale from 0 to 10 people had to decide whether to help other people or not (10 = the highest readiness to help).", "On the scale from 0 to 10 most people try to take advantage of you or most people try to be fair (10 = most people are fair)")
)

# UI
ui <- fluidPage(
  #design      
  tags$head(
    tags$style(HTML("
        :root {
          --form-control-color: rebeccapurple;
        }
        *,
        *:before,
        *:after {
          box-sizing: border-box;
        }
        html {
          min-height: 100vh;
          background: #000000;
        }
        .container-fluid {
          background: #000000;
        }
        
        .well {
          background: #000000;
          color: #01c4e7 !important;
          filter: drop-shadow(0 0 0.75rem #01c4e7);
          border: none !important;
        }
        
        .selectize-input {
          background: #000000 !important;
          border: 1px solid #01c4e7 !important;
          border-color: #01c4e7 !important;
          filter: drop-shadow(0 0 0.1rem #01c4e7);
          color: #01c4e7 !important;
        }
        
        .selectize-input::after {
          border-color: #01c4e7 transparent transparent transparent !important;
        }
        .dropdown-active::after {
           border-color: transparent transparent #01c4e7 transparent !important;
        }
        .selectize-dropdown {
          background: #000000 !important;
          border: 1px solid #01c4e7 !important;
          filter: drop-shadow(0 0 0.3rem #01c4e7);
        }
        .selectize-dropdown-content {
          color: #01c4e7 !important;
        }
        .selectize-dropdown-content > .active {
          background: #01c4e7 !important;
          font-weight: black;
          color: #000000 !important;
        }
        input[type=\"radio\"] {
          -webkit-appearance: none;
          appearance: none;
          background-color: #fff;
          margin: 0.2em 0 0 0;
          font: inherit;
          color: #000000;
          background: #000000;
          border: 1px solid #01c4e7;
          width: 1em;
          height: 1em;
          border-radius: 50%;
        }
        input[type=\"radio\"]:checked {
          display: flex;
          align-items: center;
        }
        input[type=\"radio\"]:checked::before {
          content: \"\";
          width: 0.7em;
          height: 0.7em;
          background: #01c4e7;
          border-radius: 50%;
          margin: 0 auto;
        }
        input[type=\"radio\"]:focus {
          display: none;
        }
        input[type=\"checkbox\"] {
          -webkit-appearance: none;
          appearance: none;
          margin: 0.2em 0 0 0;
          font: inherit;
          color: #000000;
          background: #000000!important;
          border: 1px solid #01c4e7;
          width: 1em;
          height: 1em;
          border-radius: 10%;
        }
        input[type=\"checkbox\"]:checked {
          display: flex;
          align-items: center;
        }
        input[type=\"checkbox\"]:checked::before {
          content: \"\";
          width: 0.7em;
          height: 0.7em;
          background: #01c4e7;
          border-radius: 10%;
          margin: 0 auto;
        }
        input[type=\"checkbox\"]:focus {
          display: none;
        }
        h2 {
          text-align: center;
          margin-bottom: 30px !important;
          color: #01c4e7 !important;
        }
        h4, h3 {
          color: #01c4e7 !important;
        }
        a {
          color: #01c4e7 !important;
        }
        .nav-tabs>li.active>a {
          color: #000000 !important;
          border: none: !important;
          /* box-shadow: 0px -5px 7px 0px #01c4e7; */
        }
        .nav-tabs>li.active>a:hover {
          background: #FFFFFF !important;
        }
        .nav-tabs {
          border: none !important;
        }
        .shiny-plot-output > img {
          margin: 15px 0;
        }
        .shiny-plot-output {
          margin-bottom: 30px;
        }
        .tab-pane {
          padding: 15px;
          background: #FFFFFF;
          border-bottom-left-radius: 5px;
          border-bottom-right-radius: 5px;
          border-top-right-radius: 5px;
          /* border: 1px solid #01c4e7 !important;
          border-top: none !important; */
          box-shadow: 0 0 11px 0px #01c4e7;
        }
        tr .selected > td {
          background-color: #01c4e7 !important;
          drop-shadow: none !important;
        }
      ")
    )
  ),
  
  titlePanel("Social Trust in Bulgaria"),
  sidebarLayout(
    sidebarPanel(
      # selecting variables
      selectInput(
        "variable",
        "Select Variable",
        choices = c("Trust in People", "Helpfulness of People", "Fairness of People"),
        selected = "Trust in People"
      ),
      
      # selecting gender
      radioButtons(
        "gender",
        "Select Gender",
        choices = c("Overall", "Male", "Female"),
        selected = "Overall"
      ),
      
      # show/hide summary table
      checkboxInput("show_table", "Show Summary Table", value = FALSE)
    ),
    
    mainPanel(
      #tab with graph and descriptives
      tabsetPanel(
        tabPanel("Graph and Table",
                 plotOutput("graph"),
                 DT::dataTableOutput("summary_table")
        ),
        #tab with variables and dataset
        tabPanel("Variables and Dataset", 
                 h3("Variables and Dataset"),
                 p(variables_text_1),
                 h4(variables_text_2),
                 p(variables_text_3),
                 h4(variables_text_4),
                 p(variables_text_5),
                 h4(variables_text_6),
                 p(
                   a(href="https://www.europeansocialsurvey.org/", target='_blank', 'ESS'), 
                   variables_text_7
                 ),
                 DT::dataTableOutput("variables_table")
        )
      )
    )
  )
)



# Server
server <- function(input, output) {
  #graphs
  output$graph <- renderPlot({
    variable <- input$variable
    
    if (input$gender == "Overall") {
      if (variable == "Trust in People") {
        ggplot(data = na.omit(bg_trust_all), aes(x = factor(ppltrst), fill = gndr)) +
          geom_bar(stat = "count", width = 0.7) +
          scale_x_discrete(name = "Trust Level", labels = c("No trust", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Complete trust")) +
          scale_fill_manual(name = "Gender", values = c("Female" = "lightpink", "Male" = "lightblue")) +
          theme_minimal() +
          labs(title = "Trust in People by Gender", y = "Count")
      } else if (variable == "Helpfulness of People") {
        ggplot(data = na.omit(bg_trust_all), aes(x = factor(pplhlp), fill = gndr)) +
          geom_bar(stat = "count", width = 0.7) +
          scale_x_discrete(name = "Helpfulness Level", labels = c("Not helpful", "Very helpful")) +
          scale_fill_manual(name = "Gender", values = c("Female" = "lightpink", "Male" = "lightblue")) +
          theme_minimal() +
          labs(title = "Helpfulness of People by Gender", y = "Count")
      } else if (variable == "Fairness of People") {
        ggplot(data = na.omit(bg_trust_all), aes(x = factor(pplfair), fill = gndr)) +
          geom_bar(stat = "count", width = 0.7) +
          scale_x_discrete(name = "Fairness Level", labels = c("Not fair", "1", "2", "3", "4", "5", "6", "7", "8", "9","Very fair")) +
          scale_fill_manual(name = "Gender", values = c("Female" = "lightpink", "Male" = "lightblue")) +
          theme_minimal() +
          labs(title = "Fairness of People by Gender", y = "Count")
      }
    } else if (input$gender == "Male") {
      if (variable == "Trust in People") {
        ggplot(data = ppltrst_male, aes(x = factor(value))) +
          geom_bar(stat = "count", width = 0.7, fill = "lightblue") +
          scale_x_discrete(name = "Trust Level", labels = c("No trust", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Complete trust")) +
          theme_minimal() +
          labs(title = "Trust in People (Males)", y = "Count")
      } else if (variable == "Helpfulness of People") {
        ggplot(data = pplhlp_male, aes(x = factor(value))) +
          geom_bar(stat = "count", width = 0.7, fill = "lightblue") +
          scale_x_discrete(name = "Helpfulness Level", labels = c("Not helpful", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Very helpful")) +
          theme_minimal() +
          labs(title = "Helpfulness of People (Males)", y = "Count")
      } else if (variable == "Fairness of People") {
        ggplot(data = pplfair_male, aes(x = factor(value))) +
          geom_bar(stat = "count", width = 0.7, fill = "lightblue") +
          scale_x_discrete(name = "Fairness Level", labels = c("Not fair", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Very fair")) +
          theme_minimal() +
          labs(title = "Fairness of People (Males)", y = "Count")
      }
    } else if (input$gender == "Female") {
      if (variable == "Trust in People") {
        ggplot(data = ppltrst_female, aes(x = factor(value))) +
          geom_bar(stat = "count", width = 0.7, fill = "lightpink") +
          scale_x_discrete(name = "Trust Level", labels = c("No trust","1", "2", "3", "4", "5", "6", "7", "8", "9", "Complete trust")) +
          theme_minimal() +
          labs(title = "Trust in People (Females)", y = "Count")
      } else if (variable == "Helpfulness of People") {
        ggplot(data = pplhlp_female, aes(x = factor(value))) +
          geom_bar(stat = "count", width = 0.7, fill = "lightpink") +
          scale_x_discrete(name = "Helpfulness Level", labels = c("Not helpful", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Very helpful")) +
          theme_minimal() +
          labs(title = "Helpfulness of People (Females)", y = "Count")
      } else if (variable == "Fairness of People") {
        ggplot(data = pplfair_female, aes(x = factor(value))) +
          geom_bar(stat = "count", width = 0.7, fill = "lightpink") +
          scale_x_discrete(name = "Fairness Level", labels = c("Not fair", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Very fair")) +
          theme_minimal() +
          labs(title = "Fairness of People (Females)", y = "Count")
      }
    }
  })
  #summary table
  output$summary_table <- DT::renderDataTable({
    if (input$show_table) {
      if (input$gender == "Overall") {
        if (input$variable == "Trust in People") {
          datatable(summary_stats_ppltrst, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
        } else if (input$variable == "Helpfulness of People") {
          datatable(summary_stats_pplhlp, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
        } else if (input$variable == "Fairness of People") {
          datatable(summary_stats_pplfair, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
        }
      } else if (input$gender == "Male") {
        if (input$variable == "Trust in People") {
          datatable(summary_stats_ppltrst_male, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
        } else if (input$variable == "Helpfulness of People") {
          datatable(summary_stats_pplhlp_male, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
        } else if (input$variable == "Fairness of People") {
          datatable(summary_stats_pplfair_male, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
        }
      } else if (input$gender == "Female") {
        if (input$variable == "Trust in People") {
          datatable(summary_stats_ppltrst_female, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
        } else if (input$variable == "Helpfulness of People") {
          datatable(summary_stats_pplhlp_female, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
        } else if (input$variable == "Fairness of People") {
          datatable(summary_stats_pplfair_female, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
        }
      }
    }
  })

  output$variables_table <- DT::renderDataTable({
    datatable(table_with_variables, options = list(pageLength = -1, ordering = FALSE, info = FALSE, searching = FALSE, paging = FALSE))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
