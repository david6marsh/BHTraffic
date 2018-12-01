#BHTransits
# function definitions

#read the site positions and group them
readANPRSites <- function( siteFile = "Data/banes_anpr_data/sites.csv"){
  read.csv(siteFile, stringsAsFactors = F) %>% 
    mutate(group1 = case_when(
      location_id %in% c(1,2) ~ "North",
      location_id %in% c(3,4) ~ "East",
      location_id == 5 ~ "Bathampton",
      location_id %in% c(6,7,8) ~ "South",
      location_id %in% c(9,10,11) ~ "West"
    ))
}

#read observations from the ANPR readers
#with a maximum of observations, and then filter to < beforeDate
readANPRObs <- function(numobs = 700000,
                        beforeDate = "2017-11-02",
                        obsFile = "Data/banes_anpr_data/observations.csv"){
  read.csv(obsFile, stringsAsFactors = F,
           nrows = numobs) %>% 
    #convert times, but ignore fractions of second 
    mutate(time = as.POSIXct(t, "%Y-%m-%d %H:%M:%S", tz="Europe/London")) %>% 
    select(-t) %>% 
    filter(time < as.POSIXct(paste(beforeDate,"00:00:00.0"), "%Y-%m-%d %H:%M:%S", tz="Europe/London"))
}

getTransitIDs <- function(ioseq){
  #ioseq is a vector, a subset from obs1$in_out
  #start a new transit ID at each instance of a 1
  tid <- data.frame(io = ioseq) %>% 
    mutate(rnum = 1:length(io),
           tid = ifelse(io == 2, NA_integer_, rnum),
           #rank the 1s and fill down
           tid2 = dense_rank(tid)) %>% 
    fill(tid2)
  return(tid$tid2)
  #still need to throw away any transits with 1 element, and just keep the first two
  #(in case of repeated 2s)
}

# visit ID is like transitID, but based on site_ide not in-out
getVisitIDs <- function(sseq){
  #sseq is a vector of site_ids
  # first visit with first site
  #start a new visit at each in, or
  # after each out, allowing that lag creates an NA at the start, so replace it with FALSE
  vid <- data.frame(site = sseq) %>% 
    mutate(rnum = 1:length(site),
           entry = ifelse((rnum == 1)|(site %in% in1$id)|(na.omit(lag(site) %in% out1$id)), rnum, NA_integer_),
           #rank the 1s and fill down
           visit = dense_rank(entry)) %>% 
    fill(visit)
  return(vid$visit)
}

#from a set of pairs and nodes calculate the frequencies and plot a Sankey
#omit links with less than minPct share (5 converted to 5%)
trSankey <- function(seqs, 
                     nodes,
                     minPct = NA,
                     fontSize = 12){
  #get frequencies for link strength
  slink <- seqs %>% 
    ungroup() %>% 
    select(sid, prev_sid, BSDirect) %>% 
    group_by(sid, prev_sid, BSDirect) %>% 
    summarise(n=n())
  
  #filter if necessary to just the largest
  total_n <- sum(slink$n)
  if (!is.na(minPct)){
  slink <- slink %>% 
    filter(n >= total_n * minPct/100)}

  sankeyNetwork(Links = slink, 
                Nodes = nodes %>% 
                  select(sloc, group1) %>% 
                  distinct() %>% 
                  arrange(sloc), 
                Source = "prev_sid", Target = "sid",
                Value = "n", units = "vehicles",
                NodeID = "sloc", NodeGroup = "group1",
                LinkGroup = "BSDirect",
                fontSize = fontSize)
}

trChord <- function(x, ene = eneBase, max_time = 30,
                    by_type = FALSE,
                    invisible_type = c(),
                    colours = c("green", "skyblue3", "navyblue", "grey"),
                    transp = 0.5){
  # draw chord with with this time limit
  # if by_type, draw a separate chord per type
  # and make types in the list invisible
  # the transp is used to align transparency in chord diagram and legend
  
  # add dummy type if needed
  if (!by_type) {x$type = "ALL"}
  
  #remove the single observations or long transits from the data
  #and group into transits
  obs1v <- x %>% 
    anti_join(ene %>% filter(n==1 || ttime > max_time),
              by = c("vehicle_id", "transitID")) %>% 
    group_by(vehicle_id, type, transitID)  
  
  #calculate global, grouped frequencies
  transits <- obs1v %>%
    mutate(entry_time = first(time),
           # resplit group1 North
           group1 = if_else(group1=="North", description, group1)) %>% 
    select(vehicle_id, type, transitID, entry_time, in_out, group1) %>% 
    spread(in_out, group1, sep="") %>% 
    rename( entry = in_out1, exit = in_out2) 
  
  #for chord diagram select the Bathampton - North, and set line type and width
  lty_df <- data.frame(c("Bathampton", "Swainswick"), c("Swainswick", "Bathampton"), c(1,1))
  lwd_df <- data.frame(c("Bathampton", "Swainswick"), c("Swainswick", "Bathampton"), c(1,1))
  bord_df <- data.frame(c("Bathampton", "Swainswick"), c("Swainswick", "Bathampton"), c(1,1))
  
  grid_col <- c(Swainswick = "brown4", East = "sienna2", Bathampton = "red", Lansdown = "skyblue2",
                West = "navyblue", South = "royalblue2")
  
  circos.par(start.degree = -20,
             gap.after = c(10,2,2,10,2,2))
  
  if (by_type) {
    transf <- transits %>% 
      group_by(entry, exit, type) %>% 
      summarise(n = n()) %>% 
      mutate( type_col = case_when(
        type == "CARS" ~ colours[1],
        type == "LCVs" ~ colours[2],
        type == "HCVs" ~ colours[3],
        TRUE ~ colours[4]),
        type_vis = if_else(type %in% invisible_type, FALSE, TRUE)
      )
    #must still only pass 3 columns to chordDiagram
    cd <- chordDiagram(transf %>% select(entry, exit, n),
                       col = transf$type_col,
                       link.visible = transf$type_vis,
                       order = c("Bathampton","South", "West", "Lansdown", "Swainswick", "East"),
                       directional = 1, direction.type = "arrows", link.arr.type = "big.arrow",
                       grid.col = grid_col,
                       transparency = transp)
    legend(x = "bottomleft", inset = 0.05, bty = "n",
           legend = c("CARS", "LCVs", "HCVs", "Other"),
           fill = add_transparency(colours, transp),
           border = "white")}
  else {
    transf <- transits %>% 
      group_by(entry, exit) %>% 
      summarise(n = n())
    
    cd <- chordDiagram(transf,
                       order = c("Bathampton","South", "West", "Lansdown", "Swainswick", "East"),
                       directional = 1, direction.type = "arrows", link.arr.type = "big.arrow",
                       link.lty = lty_df, link.lwd = lwd_df, link.border = bord_df,
                       grid.col = grid_col)
  }
  circos.clear()  
  
  return(transf)
}


#take list of colour names
#return list with given transparency
