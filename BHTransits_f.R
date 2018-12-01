#BHTransits
# function definitions

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
