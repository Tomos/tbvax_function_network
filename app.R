# Script written by Tomos O. Prys-Jones
# The script creates a network showing which functions are called from other functions

# Loading packages:
{
  rm(list=ls())
  library(here) 
  library(Matrix) 
  library(here) 
  library(tools) 
  library(assertthat) 
  library(stringi) 
  library(xml2) 
  library(digest) 
  library(deSolve) 
  library(data.table) 
  library(fst) 
  library(minpack.lm) 
  library(lubridate) 
  library(pkgnet)
  library(igraph)
  library(visNetwork)
  library(shiny)
  library(log4r)
  # Seed is important to create the same network structure each time:
  set.seed(123)
}

# List tarball (there should only be one)
tarball <- list.files(path = here("rpackage"), pattern = "\\.tar\\.gz$")

# Building tbvax
install.packages(paste0(here(), "/rpackage/", tarball), repos = NULL, type = "source")
library(tbvax)

# My own custom functions to look for sourcing all tbvax R scripts and looking for certain strings within these files:
#source(here("../TPJ_functions/utils.R"))
#Rfiles <- list.files(here("..", "output_testing_auto", "tbvax-3.4.7-core", "tbvax", "R"), pattern = "\\.R$", full.names = T)
#grep_function_list(Rfiles, "depr.fit.model")
#tbvax_files <- Rfiles[grep("TBVx", Rfiles)]
#source_Rscript_list(tbvax_files)

# Here the functions you want to include are outlined 
# This is structured by the two highest order functions, set.paths() and run()
# Set paths only calls a couple of functions so is included in its own list
# Run is separate, as it calls many downstream functions
# Each of these downstream functions has its own list, which contains the overarching equation first
# Followed by all the downstream functions.

set_paths_funcs <- c("set.paths", "update.paths", "setup.model.log")
run_funcs <- c("run")
sample_fitted_parameters_funcs <- c("sample.fitted.parameters", "selected.parameters")
modify_input_csv_funcs <- c("modify.input.csv", "constant.parameters")
read_model_parameters_funcs <- c("read.model.parameters", "parse.run.spec", "create.aging.matrix", "init.constants", 
                                 "set.node.attrs", "read.population", "calc.birthrate", "get.demography", 
                                 "create.approx.func.from.rownames", "deathrate.from.data", "create.contacts.matrices", "initialize.incidence")
set_baseline_in_model_parameters_funcs <- c("set.baseline.in.model.parameters", "matrix.from.popdf")
update_params_funcs <- c("update.parameters") 
initialize_model_params_funcs <- c("initialize.model.parameters",
                                   "fractions.by.stage.at.birth", "calc.all.indices.for.dim", "fractions.by.stage.at.start", 
                                   "initialize.progression", "parse.time.series", "init.parameters.from.xml",
                                   "init.parameters.from.xml", "initialize.treatment", "initialize.TB.transmission", "optimize.params", 
                                   "initialize.TB.infectivity", "optimize.params", "initialize.aggregated.TBHIVptr", "seed.initial.population")
write_update_xml_funcs <- c("write.updated.xml")
run_model_funcs <- c("run.model", "run.deSolve", "generate.prevalence.output", "generate.all.flows.output", "calc.names.for.dim", "generate.prev.output.df",
                     "generate.all.flows.output", "generate.single.dim.flows.output", "new.demography.from.model.run", "get.deathrate.allcauses", "get.bgdeathrate", 
                     "calc.vac.flows")
get_target_hits_funcs <- c("get.target.hits", "eval.output.vs.targets")
write_stocks_and_flows_funcs <- c("merge.stocks.and.flows")
write_merged_stocks_and_flows_funcs <- c("write.merged.stocks.and.flows", "create.filename", "write.output")
write_stocks_and_flows_funcs <- c("write.stocks.and.flows")
write_targets_funcs <- c("write.targets")
write_econ_output_funcs <- c("write.econ.output")

# All the functions lists are merged into a larger list:
active_func_list_structure <- list(set_paths_funcs,
                                   run_funcs,
                                   sample_fitted_parameters_funcs,
                                   modify_input_csv_funcs,
                                   read_model_parameters_funcs,
                                   set_baseline_in_model_parameters_funcs,
                                   update_params_funcs,
                                   initialize_model_params_funcs,
                                   write_update_xml_funcs, 
                                   run_model_funcs,
                                   get_target_hits_funcs,
                                   write_stocks_and_flows_funcs,
                                   write_merged_stocks_and_flows_funcs,
                                   write_targets_funcs,
                                   write_econ_output_funcs)

# The hilevel_funcs_list just contains the first order functions that are called from run()
hilevel_funcs_list <- c(unlist(lapply(active_func_list_structure, function(x) x[1])))
# Exclude run() and set.paths()
hilevel_funcs_list <- hilevel_funcs_list[!(hilevel_funcs_list %in% c(set_paths_funcs, run_funcs))]

# This is a list of all functions without the structure that you have from placing a list() within a list(): 
active_func_list <- c(unlist(active_func_list_structure))
  

# tbvax function network using pkgnet:
fnc.report.pn <- pkgnet::FunctionReporter$new()
fnc.report.pn$set_package("tbvax") 

# Sub-setting the edges and nodes to only include the functions within the function list (created above)
# This was done as tbvax contains other functions that may not be called in this version of the package
tbvax.edges <- fnc.report.pn$edges
tbvax.edges <- tbvax.edges[tbvax.edges$SOURCE %in% active_func_list,]
tbvax.edges <- tbvax.edges[tbvax.edges$TARGET %in% active_func_list,]

tbvax.nodes <- fnc.report.pn$nodes
tbvax.nodes <- tbvax.nodes[tbvax.nodes$node %in% active_func_list]


# Adding additional edges to the tbvax.edges df, so that we can order the functions that are called by run()
# This is done to make the network diagram more intuitive
# These edges can be given a different color to highlight their difference form the function call edges:
hilevel_funcs_list2 <- c(hilevel_funcs_list[2:(length(hilevel_funcs_list))], hilevel_funcs_list[1])
hilevel_df <- data.frame(SOURCE = as.character(hilevel_funcs_list), TARGET = as.character(hilevel_funcs_list2))  
tbvax.edges$visibility <- "visible"
hilevel_df$visibility <- "invisible"
tbvax.edges <- rbind(tbvax.edges, hilevel_df, hilevel_df, hilevel_df)

# Convert tbvax.edges to an igraph:
g <- graph_from_data_frame(tbvax.edges, directed = TRUE, vertices = tbvax.nodes)

# To see the edge attributes:
#edge_attr(g)

# Modify the igraph layout:
layout <- layout_with_sugiyama(g) 

# Change the size of the nodes here:
V(g)$size <- rep(15,length(V(g)$name))
top_funcs <- c("run", "set.paths")
V(g)$size[V(g)$name %in% top_funcs] <- 50
V(g)$size[V(g)$name %in% hilevel_funcs_list] <- 30

# The 'info' vertex attribute will contain the information from each of the tbvax .Rd files on what the function does: 
V(g)$info <- NA

# The .Rd files are read here. 
# The 'man' directory contains all of these .Rd files and can be found after unpacking tbvax once built from the tar.gz file:
rd_list <- list.files(here("man"), pattern = "\\.Rd$", full = TRUE)
rd_list_short <- list.files(here("man"), pattern = "\\.Rd$", full = FALSE)

# Loop to add the information in the .Rd files to each of the vertex info attributes
for(i in 1:length(rd_list)){
  #i <- 98
  focal_rd <- rd_list[i]
  focal_rd_short <- rd_list_short[i]
  focal_name <- gsub(".Rd", "", focal_rd_short)
  Rd2txt(focal_rd, out = here("temp.txt"))
  rd <- readLines("temp.txt")
  rd <- paste(rd, collapse = "\n")
  V(g)$info[V(g)$name == focal_name] <- rd
  
}

# Checking an individual vertex info:
#cat(V(g)$info[10]) 
# To check the vertex attributes:
#vertex_attr(g)

# Coloring the edges based on being a function edge, i.e. a function calling a function, aka 'visible'
# or the edges that order the first order functions called by run(), in this case 'invisible' 
E(g)$color <- ifelse(E(g)$visibility == "visible", "skyblue3", "darkred")
E(g)$width <- ifelse(E(g)$visibility == "visible", 3, 10)

# Function that selects the nodes that link the node representing the final function called by run() to the node for the first function called by run()

criteria_function <- function(g, edge_id) {
  # Get the names of the start and end nodes for the edge
  start_node_name <- V(g)[ends(g, edge_id)[1]]$name
  end_node_name <- V(g)[ends(g, edge_id)[2]]$name
  # Get the criteria values from the last row of hilevel_df
  source_criteria <- hilevel_df$SOURCE[nrow(hilevel_df)]
  target_criteria <- hilevel_df$TARGET[nrow(hilevel_df)]
  # Compare node names with the criteria
  return(start_node_name == source_criteria && end_node_name == target_criteria)
}
  
# Apply this function to each edge and store the result
satisfying_edges <- sapply(E(g), function(e) criteria_function(g, e))
# All edges that link the final run() subfunction to the first run() subfunction are labeled as 'tester'
E(g)$visibility[satisfying_edges] <- "tester"

# Modifying the opacity of the edges based on the edge visibility tags, i.e. 'visible' for function edges, 
# 'invisible' for edges ordering the first order functions called by run(),
# 'tester' for the edge linking the final first order function with the first first order function 
E(g)$color <- sapply(seq_along(E(g)$color), function(idx) {
  hex <- E(g)$color[idx]
  visibility <- E(g)$visibility[idx]
  
  rgb <- col2rgb(hex)
  if (visibility == "visible") {
    # If visibility is "visible", set to 100% opacity
    sprintf("rgba(%d,%d,%d,1.0)", rgb[1,], rgb[2,], rgb[3,])
  } else if (visibility == "tester") {
    # If visibility is "tester", set to 0% opacity (fully transparent)
    sprintf("rgba(%d,%d,%d,0.0)", rgb[1,], rgb[2,], rgb[3,])
  } else {
    # Otherwise, set to 30% opacity (semi-transparent)
    sprintf("rgba(%d,%d,%d,0.1)", rgb[1,], rgb[2,], rgb[3,])
  }
})



# Convert igraph object to visNetwork
vis_g <- visNetwork::visIgraph(g)

# Customize the visNetwork graph
vis_g <- vis_g %>%
  visNetwork::visNodes(title = V(g)$name, label = V(g)$number) %>%  # Custom text on hover
  visNetwork::visEdges(arrows = "to", 
                       width = "width",
                       color = list(color = E(g)$color), 
                       shadow = FALSE) %>%
  visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visNetwork::visLayout(randomSeed = 123) #Ensure consistent layout

# Render the interactive graph
#vis_g


# UI
ui <- fluidPage(
  column(7, visNetworkOutput("network_plot", height = "800px")),
  column(5, verbatimTextOutput("info_text"))
  #textOutput("info_text")  # Placeholder for node information
)


# Utility function to clean up text
clean_text <- function(text) {
  # Replace escape characters with nothing
  gsub("_\b", "", text)
}

server <- function(input, output) {
  
  output$network_plot <- renderVisNetwork({
    vis_g
  })
  
  output$info_text <- renderPrint({
    selected_node <- input$network_plot_selected
    if (!is.null(selected_node)) {
      node_info <- V(g)$info[V(g)$name == selected_node]
      cleaned_text <- clean_text(node_info)
      cat(cleaned_text)
    } else {
      cat("Select a node to see details")
    }
  })
}


shinyApp(ui = ui, server = server)


