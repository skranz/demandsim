# Consumer Types

example = function() {
  prod_df = expand_grid(brand=paste0("b",1:2), type=paste0("t",1:2))
  prods = new_prods(prod_df, base_price_min = 1, base_price_max = 1)
  m_df = new_markets(prods, m = 100,submarkets=2, avail_factor = 1, price_spread = 0)

  cons = replicate(4, new_consumer_type(prods,n_cons =  1000, price_a=3, sigma_eps = 2),simplify = FALSE)


  sigma_eps = 1; sigma_cat=0.5; sigma_prod=0.5; V0 = 5
  cons = list(
    new_consumer_type(prods,n_cons =  1000, price_a=3, prod_add=c(10,10,0,0), sigma_eps=sigma_eps, sigma_cat=sigma_cat, sigma_prod=sigma_prod, V0=V0),
    new_consumer_type(prods,n_cons =  1000, price_a=3, prod_add=c(0,0,10,10), sigma_eps=sigma_eps, sigma_cat=sigma_cat, sigma_prod=sigma_prod, V0=V0)
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
  m_df$n_cons = 0
  for (i in seq_along(cons)) {
    cat(".")
    ct = cons[[i]]
    res = simulate_ct_demand(m_df=m_df, m=m,ct=ct)
    m_df$q = m_df$q+res$q
    m_df$n_cons = m_df$n_cons+res$n_cons
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
  n_cons = ct$n_cons

  missing_penalty = range(Vp)*2 + 10*ct$sigma_eps
  Vp = Vp - m_df$missing * missing_penalty

  V0 = rep(ct$V0, n_cons)
  market = 1
  res = bind_rows(lapply(1:m, function(market) {

    eps_mat = matrix(rnorm(n_prod*n_cons,0,ct$sigma_eps), n_prod, n_cons)
    rows = (n_prod*(market-1)+1):(n_prod*market)
    U = eps_mat + Vp[rows]
    U = rbind(V0, U)
    choices = max.col(t(U))-1
    as_tibble(list(market=market, prod=1:n_prod, q=tabulate(choices,n_prod), n_cons=n_cons))
  }))
  res
}
