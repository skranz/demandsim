# Consumer Types

example = function() {
  prod_df = expand_grid(brand=paste0("b",1:2), type=paste0("t",1:2))
  prods = def_prods(prod_df, sigma_eps_j=0)
  m_df = def_markets(prods, m = 100,submarkets=2)

  psi_abs = psi_linear(a=1)
  lambda_ji = NULL
  cons = list(
    def_consumer(prods,N = 1000, psi_abs=psi_abs, eps_ji=c(10,10,0,0), lambda_ji=NULL),
    def_consumer(prods,N = 1000, psi_abs=psi_abs, eps_ji=c(0,0,10,10), lambda_ji=NULL)
  )

  ct = cons[[1]]
  #res = simulate_demand(m_df, cons, V_0=-1)

  # Pricing experiment
  library(dplyrExtras)
  em_df = m_df %>% mutate_rows(submarket %in% c(2) & prod == 1, price = price*0.1)
  res = simulate_demand(em_df, cons)
  res

  library(ggplot2)
  ggplot(res, aes(x=price, fill=as.factor(submarket))) + geom_histogram(alpha=0.5,position = position_identity()) + facet_wrap(~prod)

  ggplot(res, aes(x=q, fill=as.factor(submarket))) + geom_histogram(alpha=0.5,position = position_identity()) + facet_wrap(~prod)


  elast = res %>%
    group_by(market) %>%
    mutate(p1 = price[1]) %>%
    group_by(basemarket, prod) %>%
    summarize(
      eta = ((q[2]-q[1])/(p1[2]-p1[1])) * (p1[1] / q[1])
    )
  elast

  library(ggplot2)
  ggplot(filter(elast, prod!=1), aes(x=eta, fill=as.factor(prod))) + geom_histogram(alpha=0.5, ,position = position_identity()) #+ facet_wrap(~prod)
  range(elast$eta)

  ggplot(filter(elast, prod==1), aes(x=eta, fill=as.factor(prod))) + geom_histogram(alpha=0.5, ,position = position_identity()) #+ facet_wrap(~prod)

}


simulate_demand = function(m_df, cons) {
  restore.point("simulate_demand")
  M = max(m_df$market)
  cat("\n")
  i = 1
  m_df$q = 0
  m_df$N = 0
  for (i in seq_along(cons)) {
    cat(".")
    ct = cons[[i]]
    res = simulate_ct_demand(m_df=m_df, M=M,ct=ct)
    m_df$q = m_df$q+res$q
    m_df$N = m_df$N+res$N
  }
  m_df
}

simulate_ct_demand = function(m_df,M=max(m_df$market), ct) {
  restore.point("simulate_ct_demand")

  V_ji = ct$V_ji

  n = NROW(m_df)
  J = n / M
  N = ct$N

  V_0 = rep(ct$V_0i, N)
  m = 1
  res = bind_rows(lapply(1:M, function(m) {
    simulate_market_ct_demand(m, m_df, ct, J,N, V_ji, V_0)
  }))
  res
}

simulate_market_ct_demand = function(m, m_df, ct,J, N=ct$N, V_ji=ct$V_ij, V_0=ct$V_0i) {
  restore.point("simulate_market_ct_demand")


  # Individual specific shocks
  eps_jmin = matrix(rnorm(J*N,0,ct$sigma_eps_jmin), J, N)

  # Add product specific shocks
  rows = (J*(m-1)+1):(J*m)
  cur_m_df = m_df[rows,]

  V_ji = V_ji + cur_m_df$eps_j
  if (ct$sigma_eps_jmi > 0) {
    V_ji = V_ji + rnorm(J,0,sigma_eps_jmi)
  }
  if (!is.null(ct$psi_abs)) {
    V_ji = V_ji + ct$psi_abs$price_util_fun(cur_m_df)
  }

  if (any(cur_m_df$missing)) {
    missing_penalty = diff(range(V_ji))*2 + diff(range(eps_jmin))*2
    V_ji = V_ji - cur_m_df$missing * missing_penalty
  }


  # To do: need to compute utility from numeric variables...

  U = eps_jmin + V_ji
  U = rbind(V_0, U)
  choices = max.col(t(U))-1
  as_tibble(list(market=m, prod=1:J, q=tabulate(choices,J), N=N))
}
