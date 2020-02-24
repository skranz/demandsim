# Consumer Types

# Simulate consumer types
library(dplyr)
library(tidyr)
library(rmdtools)

example = function() {
  prod_df = expand_grid(brand=paste0("b",1:2), type=paste0("t",1:2))
  prods = new_prods(prod_df, base_price_min = 1, base_price_max = 1)
  m_df = new_markets(prods, m = 100,submarkets=2, avail_factor = 1, price_spread = 0)
  
  cons = replicate(4, new_consumer_type(prods,n_consumer =  1000, price_a=3, sigma_eps = 2),simplify = FALSE)

    
  sigma_eps = 1; sigma_cat=0.5; sigma_prod=0.5; V0 = 5
  cons = list(
    new_consumer_type(prods,n_consumer =  1000, price_a=3, prod_add=c(10,10,0,0), sigma_eps=sigma_eps, sigma_cat=sigma_cat, sigma_prod=sigma_prod, V0=V0),
    new_consumer_type(prods,n_consumer =  1000, price_a=3, prod_add=c(0,0,10,10), sigma_eps=sigma_eps, sigma_cat=sigma_cat, sigma_prod=sigma_prod, V0=V0)
  )

  ct = cons[[1]]
  #res = simulate_demand(m_df, cons, V0=-1)
  
  # Pricing experiment
  library(dplyrExtras)
  em_df = m_df %>% mutate_rows(submarket %in% c(2) & prod == 1, price = price*0.8)
  res = simulate_demand(em_df, cons)
  res
  
  library(ggplot2)
  ggplot(res, aes(x=q, fill=as.factor(submarket))) + geom_histogram(alpha=0.5,position = position_identity()) + facet_wrap(~prod)    

  
  elast = res %>%
    group_by(market) %>%
    mutate(p1 = price[1]) %>%
    group_by(basemarket, prod) %>%
    summarize(
      eta_i_1 = ((q[2]-q[1])/(p1[2]-p1[1])) * (p1[1] / q[1])
    )
  elast
  
  library(ggplot2)
  ggplot(filter(elast, prod!=1), aes(x=eta_i_1, fill=as.factor(prod))) + geom_histogram(alpha=0.5, ,position = position_identity()) #+ facet_wrap(~prod)    
  range(elast$eta)
  
  ggplot(filter(elast, prod==1), aes(x=eta, fill=as.factor(prod))) + geom_histogram(alpha=0.5, ,position = position_identity()) #+ facet_wrap(~prod)    
  
}


simulate_demand = function(m_df, cons) {
  restore.point("simulate_demand")
  m = max(m_df$market)
  cat("\n")
  i = 1
  m_df$q = 0
  m_df$n_consumer = 0
  for (i in seq_along(cons)) {
    cat(".")
    ct = cons[[i]]
    res = simulate_ct_demand(m_df=m_df, m=m,ct=ct)
    m_df$q = m_df$q+res$q
    m_df$n_consumer = m_df$n_consumer+res$n_consumer
  }
  m_df
}

simulate_ct_demand = function(m_df,m=max(m_df$market), ct) {
  restore.point("simulate_ct_demand")
  
  V = ct$u_df$V

  if (ct$price_fun == "linear") {
    Vp = V - ct$price_a * m_df$price
  }
  
  n = NROW(m_df)
  n_prod = n / m
  n_consumer = ct$n_consumer
  
  missing_penalty = range(Vp)*2 + 10*ct$sigma_eps
  Vp = Vp - m_df$missing * missing_penalty  
  
  V0 = rep(ct$V0, n_consumer)
  market = 1
  res = bind_rows(lapply(1:m, function(market) {
    
    eps_mat = matrix(rnorm(n_prod*n_consumer,0,ct$sigma_eps), n_prod, n_consumer)
    rows = (n_prod*(market-1)+1):(n_prod*market)
    U = eps_mat + Vp[rows]
    U = rbind(V0, U)
    choices = max.col(t(U))-1
    as_tibble(list(market=market, prod=1:n_prod, q=tabulate(choices,n_prod), n_consumer=n_consumer))
  }))
  res
}

new_markets = function(prods,m,submarkets=1, avail_factor = 1, price_spread = 0.2) {
  restore.point("new_markets")
  
  res = prods$prod_df
  m_df = bind_rows(lapply(1:m, function(market) {
    res$market = market
    res
  }))
  
  
  n = NROW(m_df)
  n_prod = prods$n_prod
  m_df$submarket= rep(rep(1:submarkets,each=n_prod), length=n)
  m_df$basemarket = ceiling(m_df$market / submarkets)
  
  m_df$price = m_df$base_price * runif(n, 1-(price_spread/2), 1+(price_spread/2))
  m_df$missing = m_df$avail*avail_factor < runif(n)
  if (submarkets > 1) {
    offset = (m_df$submarket-1) * prods$n_prod
    m_df$price = m_df$price[(1:n)-offset] 
    m_df$missing = m_df$missing[(1:n)-offset] 
  }
  m_df
}

new_prods = function(prod_df = tibble(prod=1:10), base_price_min=0.8, base_price_max = 1.2) {
  
  vars = setdiff(colnames(prod_df), c("prod","base_price","avail"))
  class = sapply(prod_df[vars], class)
  vars_cat = vars[class %in% c("character","factor")]
  vars_num = setdiff(vars, vars_cat)  
  vals_cat = lapply(prod_df[vars_cat], unique)
  n_cat = sapply(vals_cat, length)
  
  
  n_prod = NROW(prod_df)
  prod_df$prod = 1:n_prod
  prod_df$avail = 1
  prod_df$base_price = runif(n_prod, base_price_min, base_price_max)
  
  nlist(prod_df, n_prod, vars_num, vars_cat, vals_cat, n_cat)
}

new_consumer_type = function(prods,n_consumer=100, sigma_prod = 1, sigma_eps = 1, mean_cat = 0, sigma_cat=1, beta_num=1, price_fun=c("linear", "gutenberg")[1], price_a = 1,price_b=2, price_kink_perc=0.2, num_ref_prod = 5, ref_prod = NULL, prod_add=0, V0=0, ...) {
  restore.point("new_consumer_type")
  
  u_df = prods$prod_df
  
  n = prods$n_prod
  V = rnorm(n,0,sigma_prod) + prod_add
  
  vars_cat = prods$vars_cat
  

  sigma_cat = rep(sigma_cat, length(vars_cat))
  if (is.null(names(sigma_cat))) names(sigma_cat) = vars_cat

  mean_cat = rep(mean_cat, length(vars_cat))
  if (is.null(names(mean_cat))) names(mean_cat) = vars_cat

  var_cat = vars_cat[1]
  for (var_cat in vars_cat) {
    n = prods$n_cat[var_cat]
    V_cat = rnorm(n,mean_cat[var_cat],sigma_cat[var_cat])
    rows = match(u_df[[var_cat]], prods$vals_cat[[var_cat]])
    V = V+V_cat[rows]
  }
  u_df$V = V  

  vars_num = prods$vars_num
  beta_num = rep(beta_num, length(vars_num))
  if (is.null(names(beta_num)))
    names(beta_num) = vars_num
  
  if (is.null(ref_prod)) {
    ref_prod = u_df %>%
      top_n(num_ref_prod, V+rnorm(n, sigma_eps)) %>%
      pull(prod)
  }
  
  nlist(
    n_consumer,
    ref_prod,
    sigma_prod,
    sigma_cat,
    sigma_eps,
    beta_num,
    price_fun,
    price_a,
    price_b,
    price_kink_perc,
    V0,
    u_df
  )
}
