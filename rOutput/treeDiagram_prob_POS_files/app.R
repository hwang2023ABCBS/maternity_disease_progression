
# Dynamic tree of POS for Maternity  --------------------------------------
### Step 1: Install and Load Necessary Packages

library(shiny)
library(tidyverse)
library(data.table)
library(gt)
library(igraph)
library(DT)
library(arrow)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinyWidgets)
library(DiagrammeR)
library(here)

# load(file = '~/Maternity_diease_progression/Maternity_disease_progression/envir_Objects.RData')
### Step 2: Bring table
# library(pins)
# board <- board_connect()
# df <- pin_read(board, "")
POS_transition_lagged_AddStatics_AddTransitionIndex <- read.csv(here('rOutput','POS_transition_lagged_AddStatics_AddTransitionIndex.csv'))
df <- POS_transition_lagged_AddStatics_AddTransitionIndex






### Step 3: Create the Shiny App

### UI--------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      pickerInput("age_group", "Age_Group", choices = unique(df$Age_Group),multiple=TRUE, selected = "Age [19-30]"),
      pickerInput("gender", "Gender", choices = unique(df$Gender),multiple=TRUE, selected ="F"),
      pickerInput("race", "Race", choices = unique(df$Race),multiple=TRUE, selected ="Hispanic"),
      pickerInput("all_svi_cat", "All_SVI_Cat", choices = unique(df$All_SVI_Cat),multiple=TRUE, selected ="High"),
      pickerInput("rural_county_cat","Rural_County_Cat", choices = unique(df$Rural_County_Cat),multiple=TRUE,selected ="Urban"),
      # pickerInput("trimester","Trimester", choices = unique(df$TRIMESTER), multiple = TRUE)
    ),
    mainPanel(
      grVizOutput("POS_breakup_tbl", width = '100%', height = '1000px')
    )
  )
)

### SERVER-------------
server <- function(input, output) {
  # Create reactive expressions for each input control
  Age_Group_selected <- reactive({input$age_group})
  gender_selected <- reactive({input$gender})
  race_selected <- reactive({input$race})
  all_svi_cat_selected <- reactive({input$all_svi_cat})
  rural_county_cat_selected <- reactive({input$rural_county_cat})
  # trimester_selected <- reactive({input$trimester})
  
  # get 'top5_intermediate_POS', Healthy to top5 POS
  # start from this table : 'df'
  
  filtered_df <- reactive({
    df %>% 
      filter(Age_Group %in% Age_Group_selected(), 
             Gender %in% gender_selected(), 
             Race %in% race_selected(), 
             All_SVI_Cat %in% all_svi_cat_selected(), 
             Rural_County_Cat %in% rural_county_cat_selected() 
             # TRIMESTER %in% trimester_selected()
             )
    
  })
  
  
  
  output$POS_breakup_tbl <- renderGrViz({
    
    temp <- filtered_df() %>% dplyr::filter(prev_POS=='Healthy' & curt_POS!='Healthy')
    top5_intermediate_POS <- names(sort(table(temp$curt_POS), decreasing = TRUE)[1:5])
    top5_intermediate_POS
    
    
    
    
    # get 'top5_final_POS', each intermediate POS to next top5 POS.
    # split each top 5 level1 POS into top5 level 2 POS down the road. 
    ReturnTop5Level2_POS_fun <- function(df, top5_intermediate_POS) {
      top5_level2_POS_next <- list()
      for (POS in top5_intermediate_POS) {
        temp_tbl <- df %>% filter(prev_POS == as.character(POS) & curt_POS != as.character(POS))
        top5_level2_POS_next[[POS]] <- names(sort(table(temp_tbl$curt_POS), decreasing = TRUE)[1:5])
      }
      return(top5_level2_POS_next)
    }
    
    
    # return 2nd level POS at once by calling the function
    top5_final_POS <- ReturnTop5Level2_POS_fun(df ,top5_intermediate_POS)
    top5_final_POS
    
    
    
    
    # function
    
    Compute_Probs_fun <- function(POS0, list){
      temp <- df %>% 
        filter(prev_POS==as.character(POS0) & curt_POS!=as.character(POS0)) %>% 
        mutate(curt_POS= ifelse(curt_POS %in% list,curt_POS, "Others"))
      # get probs for healthy to top5 level1 transitions
      janitor::tabyl(temp$curt_POS) 
    }
    
    # Healthy to top 5 intermediate POS
    list0 <- top5_intermediate_POS
    POS0='Healthy'
    Prob_0 <- Compute_Probs_fun(POS0,list0)
    Prob_0 <- Prob_0 %>% 
      rename('pathString'='temp$curt_POS','prob'='percent') 
    
    # level 2 : 
    # from 'OFFICE' to top5_final_POS
    list1 <- top5_final_POS[[1]]
    POS0=top5_intermediate_POS[[1]]
    Prob_1 <- Compute_Probs_fun(POS0,list1)
    Prob_1 <- Prob_1 %>% 
      rename('pathString'='temp$curt_POS','prob'='percent') %>% 
      mutate(pathString= paste0(top5_intermediate_POS[[1]],"/",pathString))
    
    
    # from 'OP' to top5_final_POS
    list1 <- top5_final_POS[[2]]
    POS0=top5_intermediate_POS[[2]]
    Prob_2 <- Compute_Probs_fun(POS0,list1)
    Prob_2 <- Prob_2 %>% 
      rename('pathString'='temp$curt_POS','prob'='percent') %>% 
      mutate(pathString= paste0(top5_intermediate_POS[[2]],"/",pathString))
    
    
    
    
    # from 'OP+OFFICE' to top5_final_POS
    list1 <- top5_final_POS[[3]]
    POS0=top5_intermediate_POS[[3]]
    Prob_3 <- Compute_Probs_fun(POS0,list1)
    Prob_3 <- Prob_3 %>% 
      rename('pathString'='temp$curt_POS','prob'='percent') %>% 
      mutate(pathString= paste0(top5_intermediate_POS[[3]],"/",pathString))
    
    
    
    # from 'ER' to top5_final_POS
    list1 <- top5_final_POS[[4]]
    POS0=top5_intermediate_POS[[4]]
    Prob_4 <- Compute_Probs_fun(POS0,list1)
    Prob_4 <- Prob_4 %>% 
      rename('pathString'='temp$curt_POS','prob'='percent') %>% 
      mutate(pathString= paste0(top5_intermediate_POS[[4]],"/",pathString))
    
    # from 'IP' to top5_final_POS
    list1 <- top5_final_POS[[5]]
    POS0=top5_intermediate_POS[[5]]
    Prob_5 <- Compute_Probs_fun(POS0,list1)
    Prob_5 <- Prob_5 %>% 
      rename('pathString'='temp$curt_POS','prob'='percent') %>% 
      mutate(pathString= paste0(top5_intermediate_POS[[5]],"/",pathString))
    
    
    
    # make a df for tree diagram 
    df_4tree_prob_POS <- bind_rows(Prob_0,Prob_1,Prob_2,Prob_3,Prob_4,Prob_5) %>% 
      select(-n) %>% 
      mutate(prob= round(prob,3))
    df_4tree_prob_POS
    
    
    # pass this to make tree 'df_4tree_prob_POS'
    # Tree starts here
    ## draw tree with probs-----
    
    library(data.tree)
    # step1
    # prob_data <- readr::read_csv("~/Survival analysis/data/DataKwery Tree Diagram - Sheet1.csv")
    # reference 
    # https://www.datakwery.com/post/tree-diagrams-in-r/
    # prob_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQWc07o1xTNCJcGhw-tWYAnD3xPCjS0_jE4CIBR-rp5ff3flVGJQf2K24bJ5FE-DauQvLrtB8wWJNuc/pub?gid=0&single=true&output=csv")
    # this link got blocked,downloaded using my computer and loaded into my project.
    
    # prob_data <- df_4tree_prob_disease
    prob_data <- df_4tree_prob_POS
    
    # step2
    prob_data <- prob_data %>%  mutate(tree_level = str_count(string = pathString, pattern = "/") + 1,
                                       tree_group = str_replace(string = pathString, pattern = "/.*", replacement = ""),
                                       node_type = "decision_node"
    )
    
    max_tree_level <- max(prob_data$tree_level, na.rm = T) 
    
    
    # step3
    parent_lookup <- prob_data %>% distinct(pathString, prob) # get distinct probabilities to facilitate finding parent node probability
    
    for (i in 1:(max_tree_level -  1)) { # loop through all tree layers to get all immidiate parent probabilities (to calculate cumulative prob)
      
      names(parent_lookup)[1] <-paste0("parent",i)
      names(parent_lookup)[2] <-paste0("parent_prob",i)
      
      for (j in 1:i) {
        
        if (j == 1)  prob_data[[paste0("parent",i)]] <- sub("/[^/]+$", "", prob_data$pathString)
        else if (j  > 1) prob_data[[paste0("parent",i)]] <- sub("/[^/]+$", "", prob_data[[paste0("parent",i)]])
      }
      
      prob_data <- prob_data %>% left_join(parent_lookup, by = paste0("parent",i))
      
    }
    
    
    prob_data$overall_prob <- apply(prob_data %>% select(contains("prob"))  , 1, prod, na.rm = T)  # calculate cumulative probability   
    
    
    # step4
    terminal_data <- prob_data %>%  filter(tree_level == max_tree_level) %>% # create new rows that will display terminal/final step calulcations on the tree
      mutate(node_type = 'terminal',
             pathString = paste0(pathString, "/overall"),
             prob = NA,
             tree_level = max_tree_level + 1)
    
    start_node <- "Healthy" # name the root node
    
    prob_data = bind_rows(prob_data, terminal_data) %>%  # bind everything together 
      mutate(pathString = paste0(start_node,"/",pathString),
             overall_prob = ifelse(node_type == 'terminal', overall_prob, NA),
             prob_rank = rank(-overall_prob, ties.method = "min", na.last = "keep"))
    
    prob_data = bind_rows(prob_data, data.frame(pathString = start_node, node_type = 'start', tree_level = 0)) %>% # add one new row to serve as the start node label
      select(-contains("parent"))
    
    
    # step5
    make_my_tree <- function(mydf, display_level = NULL, show_rank = FALSE, direction = "LR") {
      
      if (!is.null(display_level) ) {
        mydf <- mydf %>% filter(tree_level <= display_level)
        
      }
      
      mytree <- as.Node(mydf) 
      
      GetEdgeLabel <- function(node) switch(node$node_type, node$prob)
      
      GetNodeShape <- function(node) switch(node$node_type, start = "box", node_decision = "circle", terminal = "none")
      
      
      GetNodeLabel <- function(node) switch(node$node_type, 
                                            terminal = ifelse(show_rank  == TRUE, paste0("Prob: ", node$overall_prob,"\nRank: ", node$prob_rank),
                                                              paste0("Prob: ", node$overall_prob)),
                                            node$node_name)
      
      SetEdgeStyle(mytree, fontname = 'helvetica', label = GetEdgeLabel)
      
      SetNodeStyle(mytree, fontname = 'helvetica', label = GetNodeLabel, shape = GetNodeShape)
      
      SetGraphStyle(mytree, rankdir = direction) 
      
      plot(mytree)
      
    }
    
    # step6
    result_plot_POS <- make_my_tree(prob_data, show_rank = TRUE)
    # make_my_tree(prob_data)
    # make_my_tree(prob_data, display_level = 1)
    # make_my_tree(prob_data, display_level = 2)
    
    result_plot_POS
    
  })  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
















































