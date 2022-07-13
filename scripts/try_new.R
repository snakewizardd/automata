library(dplyr)
library(ggplot2)
library(gganimate)

grid <- matrix(0,
               nrow=19,
               ncol=23) %>% as.data.frame()

positions <- c(1:23)

colnames(grid) <- positions

input_row <- ceiling(length(colnames(grid))/2)
input_col <- ceiling(nrow(grid)/2)

#R-Pentomino
grid[input_row-1,input_col+1] <- 1
grid[input_row-1,input_col] <- 1
grid[input_row,input_col] <- 1
grid[input_row,input_col-1] <- 1
grid[input_row+1,input_col] <- 1

#Need to vectorize this....

#Per SO Feedback
result_list <- c()

capture_index <- NULL

for(cycle in 1:72){

for(row in 1:nrow(grid)){
  
  for(col in 1:ncol(grid)){
    
    input_row <- row
    input_col <- col 
    
    neighbor_1 <- grid[input_row-1,input_col-1] 
    neighbor_2 <- grid[input_row-1,input_col] 
    neighbor_3 <- grid[input_row-1,input_col + 1] 
    
    neighbor_4 <- grid[input_row,input_col-1] 
    neighbor_5 <- grid[input_row,input_col+1] 
    
    neighbor_6 <- grid[input_row+1,input_col-1] 
    neighbor_7 <- grid[input_row+1,input_col] 
    neighbor_8 <- grid[input_row+1,input_col + 1] 
    
    
    neighbor_sum <- sum(neighbor_1,neighbor_2,neighbor_3,
                        neighbor_4,neighbor_5,neighbor_6,
                        neighbor_7,neighbor_8)
    
    capture_index <- rbind(capture_index,data.frame(row,col,
                                                    current_state = grid[row,col],
                                                    neighbor_sum))
    
  }
  
}

capture_index[is.na(capture_index)] <- 0

process <- capture_index %>% 
  mutate(is_alive = current_state == 1) %>%
  mutate(living_survive = ifelse(is_alive == TRUE & neighbor_sum %in% c(2,3),
                                 'STAY ALIVE','NOTHING')) %>%
  mutate(dead_born = ifelse(is_alive == FALSE & neighbor_sum ==3,
         'BE BORN','NOTHING')) 

process$neighbor_sum[is.na(process$neighbor_sum)] <- 0
process$dead_born[is.na(process$dead_born)] <- 'NOTHING'

#Next Steps
process <- process %>% mutate(next_state = 
                          ifelse(living_survive == 'STAY ALIVE',1,
                          ifelse(dead_born == 'BE BORN',1,0)))
                          
                                
for(i in 1:nrow(process)){
  
  grid[process[i,'row'],process[i,'col']] <- process[i,'next_state']
  
}   

result_list[[cycle]] <- grid

}



df <- bind_rows(lapply(result_list, function(x) {
  tidyr::pivot_longer(x, everything(), names_to = 'x') %>% 
    mutate(y = rep(23:1, each = 19))
}), .id = 'time')


gganimate_recording <- ggplot(df, aes(x, y, fill = factor(value))) +
  geom_tile(color = 'gray80') +
  scale_fill_manual(values = c('white', 'black')) +
  coord_equal() +
  theme_void() +
  theme(legend.position = 'none') +
  transition_states(as.numeric(df$time), transition_length = 0)

animate(gganimate_recording, nframes = 33, renderer = gifski_renderer("./animations/fix_pentamino.gif"))
