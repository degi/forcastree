require(xml2)
# require(xlsx)

##################################
# Read the XML species data file
##################################
read_species_file <- function(file) {
  xml_data <- read_xml(file)
  sp_list <- xml_children(xml_children(xml_data))
  
  df_out <- data.frame()
  for(sp in sp_list) {
    var <- xml_name(xml_children(sp))
    val <- xml_text(xml_children(sp))
    df <- data.frame(var, val)
    df <- setNames(data.frame(t(df[,-1])), df[,1])
    
    df_out <- dplyr::bind_rows(df_out, df)
  }
  # df_conv <- apply(df_out, 2, function(x) as.numeric(as.character(x)))
  excp_field <- c("Label", "Description", "Tapped")
  suppressWarnings(
    df_conv <- sapply(df_out[ , -which(names(df) %in% excp_field)], function(x) as.numeric(as.character(x)))
  )
  df <- as.data.frame(df_conv)
  if(ncol(df) == 1) df <- as.data.frame(t(df_conv))
  # nna <- names(which(colSums(is.na(df)) > 0))
  df <- cbind(df, df_out[excp_field])
  
  return(df)
}

#file <- "www/default_species.trs"
#############################
# convert xls to xml
#############################

create_xml_species <- function(spesies_df) {
  # f_in <- read.xlsx2("species_icraf_phil.xlsx", 1)
  # Create an XML node
  root_node <- xml_new_root("sexi")
  xml_attr(root_node, "version") <- "2.1"
  # Add child nodes to the root node
  sp_list_node <- xml_add_child(root_node, "SpeciesList")
  cols <- colnames(spesies_df)
  for(r in c(1:nrow(spesies_df))) {
    sp_node <- xml_add_child(sp_list_node, "species")
    xml_attr(sp_node, "SpeciesID") <- r
    for(cn in cols) {
      v <- spesies_df[r, cn]
      if(is.na(v)) next
      xml_add_child(sp_node, cn, v)
    }
  }
  return(root_node)
}

##################
# function plots
##################
eq_dbh <- function(t, dbh_max, k, c) {
  dbh_max * (1 - exp(-k * t)) ^ c
}

eq_dbh_inc <- function(dbh, dbh_max, k, c) {
  dbh * c * k * (((dbh/dbh_max) ^ (-1/c)) - 1)
}

eq_dbh_inc_max <- function(dbh_max, k, c) {
  dbh_max * k * ((1 - (1/c)) ^ (c-1)) 
}

eq_dbh_k <- function(dbh_max, dbh_inc_max, c) {
  dbh_inc_max/(dbh_max * ((1 - (1/c)) ^ (c-1)))
}

eq_allometry <- function(dbh, a, b) {
  a * (dbh ^ b) 
}

# f_in$db_inc_max <- eq_dbh_inc_max(as.numeric(f_in$DbhMaximum), 
#                                   as.numeric(f_in$DbhK), 
#                                   as.numeric(f_in$DbhC))
# 
# library(RColorBrewer)
# 
# n <- nrow(f_in)
# col <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2"), brewer.pal(12, "Set3"))
# 
# plot(1, type = "n",  xlab="Time (years)", ylab="DBH (cm)", 
#      xlim = c(0, 100), ylim = c(0, 250))
# for(r in c(1:n)) {
#   dbh_max <- as.numeric(f_in[r, "DbhMaximum"])
#   k <- as.numeric(f_in[r, "DbhK"])
#   c <- as.numeric(f_in[r, "DbhC"])
#   curve(eq_dbh(x,dbh_max, k, c) * 100, from=1, to=100, add = T, col = col[r])
# }
# 
# plot(1, type = "n",  xlab="DBH (cm)", ylab="DBH Increment (cm)", 
#      xlim = c(0, 100), ylim = c(0, 10))
# for(r in c(1:n)) {
#   dbh_init <- as.numeric(f_in[r, "DbhInitial"])
#   dbh_max <- as.numeric(f_in[r, "DbhMaximum"])
#   k <- as.numeric(f_in[r, "DbhK"])
#   c <- as.numeric(f_in[r, "DbhC"])
#   curve(eq_dbh_inc(x/100,dbh_max, k, c) * 100, from=dbh_init, to=100, add = T, col = col[r])
# }


