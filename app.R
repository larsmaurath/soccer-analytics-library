library(shiny)
library(tidyverse)
library(networkD3)
library(htmlwidgets)
library(shinyjs)

# Load data and format

nodes <- read_csv("data/analytics_nodes.csv", col_types = cols()) %>%
    rowwise() %>%
    mutate(first_topic = str_split(topic, ", ")[[1]][[1]]) %>%
    mutate(first_author = str_split(author, ", ")[[1]][[1]]) %>%
    mutate(publication_date = as.Date(publication_date, "%d/%m/%Y")) %>%
    ungroup %>%
    mutate(nodesize = 1) %>%
    mutate(link = paste0("<a href='", link,"' target='_blank'>Link</a>"))

authors <- separate_rows(nodes, author, sep = ", ") %>%
    select(author) %>%
    arrange(author)

years <- nodes %>%
    mutate(year = as.numeric(substring(publication_date, 1, 4))) %>%
    select(year) %>%
    arrange(desc(year))

ui <- shinyUI(
    fluidPage(
        useShinyjs(),
        titlePanel("Soccer Analytics Library"),
        
        sidebarLayout(
            sidebarPanel(
                selectInput("aggregate", 
                            label = h4("Aggregate By"), 
                            choices = list("Topic" = "topic", "Author" = "author"), 
                            selected = 1),
                
                h3("Filter"),
                
                selectInput("type_filter",
                            label = h4("By Type"),
                            choices = setNames(c("All", unique(nodes$type)), c("All", unique(nodes$type))),
                            selected = 1),
                
                selectInput("author_filter",
                            label = h4("By Author"),
                            choices = setNames(c("All", unique(authors$author)), c("All", unique(authors$author))),
                            selected = 1),
                
                sliderInput("date_filter", label = h4("By Publication Year"),
                            min = min(years), 
                            max = max(years), 
                            value = c(min(years), max(years)),
                            sep = ""),
                
                actionButton("reset_input", label = "Reset"),
                
                width = 2
            ),
            
            mainPanel(
                forceNetworkOutput("force"),
                DT::dataTableOutput("table"),
                width = 10
            )
        )
    )
)

server <- shinyServer(function(input, output, session) {
    
    observeEvent(input$reset_input, {
        updateSelectInput(session, "aggregate", selected = "topic")
        updateSelectInput(session, "type_filter", selected = "All")
        updateSelectInput(session, "author_filter", selected = "All")
        updateSelectInput(session, "date_filter", selected = "All")
        updateSelectInput(session, "date_filter", selected = "All")
        runjs('Shiny.onInputChange("id", null)')
    })
    
    nodes_filtered <- reactive({
        
        table <- nodes
        
        if(input$type_filter != "All"){
            table <- table %>% filter(type == input$type_filter)
        }
        
        if(input$author_filter != "All"){
            table <- table %>% 
                rowwise() %>%
                filter(input$author_filter %in% str_split(author, ", ")[[1]]) %>%
                ungroup()
        }
        
        table <- table %>% 
            filter(as.numeric(substring(publication_date, 1, 4)) >= input$date_filter[[1]]) %>%
            filter(as.numeric(substring(publication_date, 1, 4)) <= input$date_filter[[2]])
        
        aggregation_nodes <- table %>%
            separate_rows(!!rlang::sym(input$aggregate), sep = ", ") %>%
            group_by(!!rlang::sym(input$aggregate)) %>%
            summarize(nodesize = 50*n(), .groups = 'drop') %>%
            mutate(title = !!rlang::sym(input$aggregate)) %>%
            mutate(!!paste0("first_", input$aggregate) := !!rlang::sym(input$aggregate))
        
        table <- bind_rows(table, aggregation_nodes) %>%
            mutate(id = row_number() - 1)
        
    })
    
    links_filtered <- reactive({
        
        links <- separate_rows(nodes_filtered(), !!rlang::sym(input$aggregate), sep = ", ") %>%
            rowwise() %>%
            mutate(target = list(which(.$title == !!rlang::sym(input$aggregate)))) %>%
            mutate(target = ifelse(length(target) > 0, target[[1]], NA)) %>%
            mutate(target = ifelse(!is.na(target), .$id[[target]], NA)) %>%
            ungroup() %>%
            filter(!is.na(target)) %>%
            mutate(value = 1) %>%
            rename(source = id) %>%
            select(source, target, value)
        
        links
    })
    
    output$force <- renderForceNetwork({
        
        if(!nrow(nodes_filtered())){
            src <- c("No data to show. Please change filter settings or reset graph.")
            target <- c("No data to show. Please change filter settings or reset graph.")
            networkData <- data.frame(src, target)
            
            fn <- simpleNetwork(networkData)
        } else {
            nodes_filtered <- nodes_filtered() %>%
                mutate(title = if_else(is.na(author), paste0("<b style='font-size:30px'>", title, "</b>"), title))
            
            links <- links_filtered()
            
            
            fn <- forceNetwork(Links = data.frame(links), Nodes = data.frame(nodes_filtered), Source = "source",
                               Target = "target", Value = "value", NodeID = "title", linkDistance = 300,
                               Nodesize = "nodesize", fontSize = 5, Group = paste0("first_", input$aggregate), opacity = 1, opacityNoHover = 1,
                               charge = -500, zoom = TRUE, clickAction = 'Shiny.onInputChange("id", d.name)')
        }
        
        onRender(fn,
                 'function(el,x) {
                    d3.selectAll(".node text").remove()
                    d3.selectAll(".node")
                      .append("foreignObject")
                      .attr("width", 200)
                      .attr("height", 100)
                      .html(function(d) { return d.name; })
                  }'
        )
    })
    
    focus_table <- reactive({
        if(!length(input$id)){
            table <- nodes_filtered()
        } else if(grepl("<", input$id, fixed = TRUE)){
            table <- nodes_filtered() %>% filter(first_topic == gsub(".*>(.+)<.*", "\\1", input$id))
        } else{
            table <- nodes_filtered() %>% filter(title == input$id)
        }
        
        table
    })
    
    dt_object <- reactive({
        
        if(!nrow(focus_table())){
            return(DT::datatable(data.frame(. = "No data to show. Please change filter settings or reset graph."), 
                                 options = list(dom = 't'),
                                 rownames = FALSE))
        }
        
        table <- focus_table() %>%
            filter(!is.na(author)) %>%
            select(title, type, publication_date, link, abstract)
        
        DT::datatable(
            cbind(' ' = '&oplus;', table),
            escape = FALSE,
            options = list(
                columnDefs = list(
                    list(visible = FALSE, targets = c(0, 6)),
                    list(orderable = FALSE, className = 'details-control', targets = 1)
                )
            ),
            callback = JS("
                    table.column(1).nodes().to$().css({cursor: 'pointer'});
                    var format = function(d) {
                        return '<div style=\"background-color:#eee; padding: .5em;\"> <b>Abstract:</b> ' +
                        d[6] + '</div>';
                    };
                    table.on('click', 'td.details-control', function() {
                        var td = $(this), row = table.row(td.closest('tr'));
                        if (row.child.isShown()) {
                            row.child.hide();
                            td.html('&oplus;');
                        } else {
                            row.child(format(row.data())).show();
                            td.html('&CircleMinus;');
                        }
                    });"
            )
        )
    })
    
    output$table <- DT::renderDataTable(dt_object())
    
    
    
})

shinyApp(ui = ui, server = server)