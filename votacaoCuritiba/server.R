## --------------------------------------------------------------------------------
require(shiny)

shinyServer( function(input, output) {
  
  
  ## --------------------------------------------------------------------------------
  ## Plot it
  output$distributionsPlot <- renderPlot({
    
    
    ## --------------------------------------------------------------------------------
    ## Validações
    N <- as.numeric(input$N)
    Theta <- as.numeric(input$Theta)/100
    inf <- as.numeric( input$limits.pri[1] )/100
    sup <- as.numeric( input$limits.pri[2] )/100
    n <- as.numeric( input$n )
    if( n > N ) n <- N
    
    ## --------------------------------------------------------------------------------
    ## Calcula alpha e beta da priori
    
    prioriBeta <- function( p, int, conf ){
      
      # Optimizing in beta
      betapars.f <- function( beta, p, int, conf ){
        
        # Given mode and beta, let's evaluate alpha
        alfa <- ( ( beta-2 )*p + 1 )/( 1-p )
        
        # Then with some alpha and beta, let's evaluate how far we are from the desired confidence
        err <- ( diff( pbeta( int, shape1=alfa, shape2=beta) ) - conf )^2
        
        return(err)
        
      }
      
      res <- optimize( betapars.f, interval=c(1,50), p=p, conf=conf,int=int )
      pars <- c( ( (res$minimum-2)*p + 1)/(1-p), res$minimum )
      
      return( list( alpha = pars[1], beta = pars[2] ) )
      
    }
    
    mode.pri <- mean( c(inf, sup) )
    
    priori.pars <- prioriBeta( p = mode.pri, int = c(inf, sup), conf = 0.95 )
    alpha <- priori.pars$alpha
    beta <- priori.pars$beta
    
    ## --------------------------------------------------------------------------------
    ## Amostra
    k <- sum( rbinom( size = 1, n = n, prob = Theta ) )
    mode.lik <- k/n
    
    
    ## --------------------------------------------------------------------------------
    ##
    ## Posteriori Analítica
    ##
    alpha.star = alpha + k
    beta.star = beta + (n-k)
    mode.post <- ( alpha.star - 1 )/( beta.star + alpha.star - 2)
    
    
    ## --------------------------------------------------------------------------------
    ##
    ## Posteriori por aproximação
    ##
    ## [theta|y] propto ( theta^k * (1-theta)^(n-k) ) * ( theta^(alpha-1) * (1-theta)^(beta-1) )
    
    ## 1) Analytical
    ## mean = (alpha* - 1) / (alpha* - beta* - 2 ) (posterior mode)
    ## Using pre calculated values. Notice if we were using this method, we didn't had
    ## alpha* and beta* - but so it was just to use n and k like above.
    mean.app <- (alpha.star - 1)/(alpha.star + beta.star - 2 )
    
    ## variance = (-H)^(-1)
    ## hessian = (-1)*(alpha*-1)*(theta0^(-2)) + (-1)*(beta*-1)*((1-theta0)^(-2))
    hessian <- ( -1 )*( alpha.star-1 )*( mean.app^(-2) ) + ( -1 )*( beta.star-1 )*( (1-mean.app)^(-2) )
    sd.app <- sqrt( 1/( (-1)*(hessian) ) )
    
    ## 2) Numerical
    fun.mean <- function( theta0, alpha, beta, n, k ){
      ( alpha + k - 1 )*log( theta0 ) + ( beta + n - k - 1 )*log( 1 - theta0 )
    }
    op <- optim( mode.pri, fn = fun.mean
                 , method = "Brent", control=list(fnscale=-1)
                 , lower = 1e-3, upper = 1-1e-3
                 , alpha = alpha, beta = beta, n = n, k = k       
                 , hessian = TRUE )
    mean.app.num <- op$par
    sd.app.num <- sqrt( 1/( (-1)*(op$hessian) ) )
    
    
    ## --------------------------------------------------------------------------------
    ##
    ## Posterior por amostragem
    ##
    ## [ theta | y ] propto [ ( theta^k * (1-theta)^(n-k) ) ] * [ ( theta^(alpha-1) * (1-theta)^(beta-1) ) ]
    f.post <- function(theta, alpha, beta, n, k ){
      ( theta^k * (1-theta)^(n-k) ) * ( theta^(alpha-1) * (1-theta)^(beta-1) )
    }
    x <- seq( 0, 1, by = 1e-4)[-1][-1e4]
    fx <- f.post(x, alpha, beta, n, k )
    samples <- sample( x = x, prob = fx, size = 1e4, replace = TRUE )
    
    ## Sampling mode
    ## What about if it has more than 1 max value?
    tb <- data.frame( table(samples) )
    tb$samples <- as.numeric( levels(tb$samples ) )
    mode.post.samp <- tb$samples[ which( tb$Freq == max(tb$Freq) ) ]
    
    
    ## --------------------------------------------------------------------------------
    ## Plot it
    
    par( mar = c(3,3,7,3) )    
    
    col.priori <- "cornflowerblue"
    col.lik <- "darkslateblue"
    col.lik2 <- 1
    col.posteriori <- "red3"
    col.post.approx <- "darkmagenta"
    col.post.approx.num <- "darkorchid1"
    col.post.samples <- "limegreen"
    
    txt.mode.prio <- paste("Moda priori:", formatC( mode.pri, digits = 2, format = "f" ) )
    txt.mode.lik <- paste("Moda verossimilhança:", formatC( mode.lik, digits = 2, format = "f" ) )
    txt.mode.post <- paste("Moda posteriori analítica:", formatC( mode.post, digits = 2, format = "f" ) )
    txt.mode.post.approx <- paste("Média posteriori por aproximação:", formatC( mean.app, digits = 2, format = "f" ) )
    txt.mode.post.approx.num <- paste("Média posteriori por aproximação numérica:"
                                      , formatC( mean.app.num, digits = 2, format = "f" ) )
    txt.mode.post.samp <- paste("Moda posteriori por amostragem:", formatC( mode.post.samp, digits = 2, format = "f" ) )
    y.theta <- dbeta(mode.post, alpha.star, beta.star)
    
    ## Posterior
    ## Analytical
    curve( dbeta(x, alpha.star, beta.star)
           , col = col.posteriori
           , ylab = "", main = ""
           , xlab = "Proporção de votos do candidato"
           , ylim = c(0, dbeta(mode.post, alpha.star, beta.star)*2), lwd = 2 )
    
    ## By sampling
    if( "samp" %in% input$plots ) lines( density(samples), col = col.post.samples, lty = 2 )    
    
    ## By Laplace approximation
    if( "app" %in% input$plots ) curve( dnorm(x, m=mean.app, sd=sd.app), col = col.post.approx, add = T )  
    if( "app.num" %in% input$plots ) curve( dnorm(x, m=mean.app.num, sd=sd.app.num), col = col.post.approx.num, add = T )    
    
    ## Prior
    if( "pri" %in% input$plots ) curve( dbeta(x, alpha, beta), col = col.priori, add = T )    
    
    ## Likelihood
    alpha.lik <- k+1
    beta.lik <- n-k+1
    if( "lik" %in% input$plots ) curve( dbeta(x, alpha.lik, beta.lik), col = col.lik, add = T )
    
    ## Likelihood with prior as data
    alpha.lik2 <- k+alpha+1
    beta.lik2 <- (n-k)+(beta)+1
    mode.lik2 <- (alpha.lik2-1)/(alpha.lik2+beta.lik2-2)
    if( "lik2" %in% input$plots ) curve( dbeta(x, alpha.lik2, beta.lik2), col = col.lik2, add = T, lty = 2)
    
    ## Legend
    leg.show <- c( "pri", "lik", "lik2", TRUE, "app", "app.num", "samp" )
    leg.labs <- c("Priori", "Verossimilhança", "Verossimilhança modificada", "Posteriori Analítica"
                  , "Posteriori por Aproximação (Analítico)", "Posteriori por Aproximação (Numérico)"
                  , "Posteriori por Amostragem")
    leg.cols <- c(col.priori, col.lik
                  , col.lik2 ## --
                  , col.posteriori, col.post.approx, col.post.approx.num
                  , col.post.samples) ## --
    leg.ltys <- c(1,1,2,1,1,1,2)
    
    legend("topleft"
           , legend = leg.labs[ leg.show %in% c(input$plots, TRUE) ]
           , col = leg.cols[ leg.show %in% c(input$plots, TRUE) ]
           , lty = leg.ltys[ leg.show %in% c(input$plots, TRUE) ]
           , bty = "n")
    
    ## Results
    mtext( bquote("Moda da priori: " ~ .(mode.pri) ~ 
                    ", Beta(" ~ alpha ~ "=" ~ .(alpha) ~ 
                    "," ~ beta ~ "=" ~ .(beta) ~ ")" ), side = 3, line = 6, adj = 0 )
    mtext( bquote("Moda das verossimilhanças: " ~ .(mode.lik) ~ " / (mod): " ~ .(mode.lik2) ), side = 3, line = 5, adj = 0 )
    mtext( bquote("Moda posteriori analítica: " ~ .(mode.post) ), side = 3, line = 4, adj = 0 )
    mtext( bquote("Médias das posterioris por aproximação: (ana)" ~ .(mean.app) ~ " / (num) " ~ .(mean.app.num) )
           , side = 3, line = 3, adj = 0 )
    mtext( bquote("Moda posteriori por amostragem:" ~ .(mode.post.samp) ), side = 3, line = 2, adj = 0 )
    mtext( bquote("Amostra: n = " ~ .(n) ~ "," ~ .(k) ~ "votantes no candidato" )
           , side = 3, line = 1, adj = 0 )
    segments( x0 = Theta, x1 = Theta, y0 = 0, y1 = y.theta
              , col = gray(0, 0.7))
    text( x = Theta, y = y.theta+0.3, labels = bquote(theta ~ " = " ~ .(Theta)) )
    
  })
  
})