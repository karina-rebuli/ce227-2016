require(shiny)

shinyServer( function(input, output) {
  
  ## Plot it
  output$distributionsPlot <- renderPlot({
    
    ## Validações
    N <- as.numeric(input$N)
    Theta <- as.numeric(input$Theta)/100
    inf <- as.numeric( input$limits.pri[1] )/100
    sup <- as.numeric( input$limits.pri[2] )/100
    n <- as.numeric(input$n)
    if( n > N ) n <- N
    
    ## Calcula alpha e beta da priori
    mode.pri <- mean(inf, sup)
    ## Tryals
    alpha <- seq(1, 1e3, length.out = 1e5)
    beta <- ( (1-mode.pri)*alpha + 2*mode.pri - 1 ) / (mode.pri)
    p <- pbeta( sup, shape1 = alpha, shape2 = beta) - pbeta( inf, shape1 = alpha, shape2 = beta)
    err <- abs(0.95-p)
    
    ## Best estimate
    alpha <- alpha[which( err == min(err) )]
    beta <- beta[which( err == min(err) )]
    
    ## Amostra
    k <- sum( rbinom( size = 1, n = n, prob = Theta ) )
    
    ## Posteriori
    alpha.star = alpha + k
    beta.star = beta + (n-k)
    mode.post <- ( alpha.star - 1 )/( beta.star + alpha.star - 2)
    
    ## Plot it
    col.priori <- "cornflowerblue"
    col.posteriori <- "red3"
    txt.mode.prio <- paste("Moda priori:", formatC( mode.pri, digits = 2, format = "f" ) )
    txt.mode.post <- paste("Moda posteriori:", formatC( mode.post, digits = 2, format = "f" ) )
    y.theta <- dbeta(mode.post, alpha.star, beta.star)
    
    curve( dbeta(x, alpha.star, beta.star)
           , col = col.posteriori
           , ylab = ""
           , xlab = "Proporção de votos do candidato"
           , ylim = c(0, dbeta(mode.post, alpha.star, beta.star)*1.2) )
    
    curve( dbeta(x, alpha, beta), col = col.priori, add = T )
    legend("topright", c("Priori", "Posteriori")
           , col = c(col.priori, col.posteriori)
           , lty = 1, bty = "n")
    mtext( bquote("Moda da priori: " ~ .(mode.pri)), side = 3, line = 3, adj = 0 )
    mtext( bquote("Moda da posteriori: " ~ .(mode.post)), side = 3, line = 2, adj = 0 )
    mtext( bquote("Amostra: n = " ~ .(n) ~ ", " ~ .(k) ~ "votantes no candidato" )
           , side = 3, line = 1, adj = 0 )
    segments( x0 = Theta, x1 = Theta, y0 = 0, y1 = y.theta
              , col = gray(0, 0.7))
    text( x = Theta, y = y.theta+0.3, labels = bquote(theta ~ " = " ~ .(Theta)) )
  })  
})