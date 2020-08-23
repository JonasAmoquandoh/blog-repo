ScoreHeatMap<-function(home,away,homeXg,awayXg){
  
  adjustedHome<-as.character(sub("_", " ", home))
  adjustedAway<-as.character(sub("_"," ",away))
  
  df<-ScoreGrid(homeXg,awayXg)
  
  df %>% 
    as_tibble(rownames = all_of(away)) %>%
    pivot_longer(cols = -all_of(away), 
                 names_to = home, 
                 values_to = "Probability") %>%
    mutate_at(vars(all_of(away), home), 
              ~forcats::fct_relevel(.x, "10+", after = 10)) %>% 
    ggplot() + 
    geom_tile(aes_string(x=all_of(away), y=home, fill = "Probability")) +   
    scale_fill_gradient2(low = "red", mid="white", high = muted("blue"))+
    theme(plot.margin = unit(c(1,1,1,1),"cm"),
          plot.title = element_text(size=20,hjust = 0.5,face="bold",vjust =4),
          plot.caption = element_text(hjust=1.1,size=10,face = "italic"), 
          plot.subtitle = element_text(size=12,hjust = 0.5,vjust=4),
          axis.title.x=element_text(size=14,vjust=-0.5,face="bold"),
          axis.title.y=element_text(size=14, vjust =0.5,face="bold"))+
    labs(x=adjustedAway,y=adjustedHome,
         caption="xG Source: FiveThirtyEight")+
    ggtitle(label = "Expected Scores", subtitle = paste(adjustedHome, "vs",adjustedAway,"xG:",homeXg,"-",awayXg))
  
}
