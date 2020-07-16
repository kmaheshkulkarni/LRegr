library(dplyr)
selclm<- Tdata %>% select(weight, DM.Cost)

lmly<- function(data= NULL, val1 = NULL, weight1 = NULL, weight2= NULL){
  # group <- gl(val1, val2, labels = c("Weight","DM Cost"))
  wt<- c(weight1, weight2)
  rw <- sqrt(wt)    ## root weights
  X_tilde <- weight1 * val1    ## weighted model matrix (with intercept)
  y_tilde <- weight2 * val2 
  xy<- data.frame(X_tilde, y_tilde)
  print("lm D9")
  lm.D9 <- lm(wt ~ val1)
  print(lm.D9)
  # print("lm D90")
  # lm.D90 <- lm(val1 ~ val2 - 1)
  # print(lm.D90)
  print("Anova")
  print(anova(lm.D9))
  print("Summary")
  # summary(lm.D9)
  opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
  par(opar)
  plot(lm.D9)
  # corr_scatterly(data=xy,x= ~X_tilde,y= ~y_tilde,corr_coef=lm.D9,xname="Weight",yname="DM Cost")
}

lreg <- function(data=NULL, x=NULL, y=NULL, w1 = NULL, w2 = NULL, xname="", yname=""){
  DT1<- data %>% filter(data$weight >= w1 &  data$weight <= w2)
  print(DT1)
  fit<- lm(weight ~ DM.Cost, data = DT1)
  print("")
  print("Anova")
  print(anova(fit))
  print("")
  print("Summary")
  print(summary(fit))
   DT1 %>% 
     plot_ly(x = ~DM.Cost) %>%
     add_markers(y = ~weight, marker = list(size = 5, opacity = 0.8), showlegend = TRUE, name = "") %>%
      add_lines(x = ~DM.Cost, y = fitted(fit), line = list(color = 'rgb(250,128,114)', width = 3), showlegend = TRUE,
                name = "Best Fit Line") %>%
      layout(xaxis = list(title = paste0(unique(xname))),
                     yaxis = list(title = paste0(unique(yname)), showlegend = TRUE)) %>%
      layout(legend = list(orientation = "h",   # show entries horizontally
                                   xanchor = "bottom",  # use center of legend as anchor
                                   x = 0.40, y = -0.3,
                                   bordercolor = "#333",
                                   borderwidth = 2)) %>% layout(paste0('LM =', summary(fit)))%>%
      config(displaylogo = FALSE, collaborate = FALSE)
}

lmly<- function(val1 = NULL, weight1 = NULL, weight2= NULL){
  wt<- c(weight1, weight2)
  rw <- sqrt(wt)    ## root weights
  print("Root Weights")
  print(rw)
  print("lm D9")
  D9 <- lm(wt ~ rw)
  print(D9)
  print("Anova")
  print(anova(D9))
  print("Summary")
  print(summary(lm.D9))
  opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
  par(opar)
  # corr_scatterly(data=xy,x= ~X_tilde,y= ~y_tilde,corr_coef=lm.D9,xname="Weight",yname="DM Cost")
}
