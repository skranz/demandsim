# Consumer Types

example = function() {
  prod_df = expand_grid(brand=paste0("b",1:2), type=paste0("t",1:2))
  prods = def_prods(prod_df, sigma_eps_j=1)
  m_df = def_markets(prods, m = 100,submarkets=2)

  psi_abs = psi_linear(a=0)
  lambda_ji = NULL
  N
  cons = list(
    def_consumer(prods,N = 1000, psi_abs=psi_abs, eps_ji=c(10,10,0,0), lambda_ji=NULL),
    def_consumer(prods,N = 1000, psi_abs=psi_abs, eps_ji=c(10,10,0,0), lambda_ji=NULL)
  )

  ct = cons[[1]]

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

#' Define products
def_prods = function(prod_df = tibble(prod=1:10), vars_cat, vars_num, sigma_eps_j=0, price_base_min=0.8, price_base_max = 1.2) {

  vars = setdiff(colnames(prod_df), c("prod","price_base","avail"))
  class = sapply(prod_df[vars], class)

  if (missing(vars_cat))
    vars_cat = vars[class %in% c("character","factor")]

  if (missing(vars_num))
    vars_num = setdiff(vars, vars_cat)

  if (length(vars_num) > 0) {
    warning("Cannot yet work with numeric product characteristics, they will be ignored.")
    vars_num = NULL
  }

  vals_cat = lapply(prod_df[vars_cat], unique)
  n_cat = sapply(vals_cat, length)


  J = NROW(prod_df)
  prod_df$prod = 1:J
  prod_df$avail = 1

  if (!has.col(prod_df, "price_base"))
    prod_df$price_base = runif(J, price_base_min, price_base_max)

  if (!has.col(prod_df, "eps_j")) {
    if (sigma_eps_j==0) {
      prod_df$eps_j = 0
    } else {
      prod_df$eps_j = rnorm(J,0, sigma_eps_j)
    }
  }


  nlist(prod_df, J, vars_num, vars_cat, vals_cat, n_cat)
}


def_markets = function(prods,m,submarkets=1, avail_factor = 1, price_spread = 0.2, sigma_eps_jm=0) {
  restore.point("new_markets")

  res = prods$prod_df
  m_df = bind_rows(lapply(1:m, function(market) {
    res$market = market
    res
  }))


  n = NROW(m_df)
  J = prods$J
  m_df$submarket= rep(rep(1:submarkets,each=J), length=n)
  m_df$basemarket = ceiling(m_df$market / submarkets)

  m_df$price = m_df$price_base * runif(n, 1-(price_spread/2), 1+(price_spread/2))
  m_df$missing = m_df$avail*avail_factor < runif(n)

  if (sigma_eps_jm == 0) {
    m_df$eps_jm = 0
  } else {
    m_df$eps_jm = rnorm(m,0,sigma_eps_jm)
  }

  if (submarkets > 1) {
    offset = (m_df$submarket-1) * prods$J
    m_df$price = m_df$price[(1:n)-offset]
    m_df$missing = m_df$missing[(1:n)-offset]
    m_df$eps_jm = m_df$eps_jm[(1:n)-offset]
  }
  m_df
}


#' Define a new consumer type
def_consumer = function(prods,N=100,
  lambda_ji = lambda(prods), psi_abs = psi_linear(), psi_ref = NULL, sigma_eps_ji=1, sigma_eps_jmi=0, sigma_eps_jmin=1, ref_prod = NULL, V_0i=0, eps_ji=NULL) {
  restore.point("new_cons_type")

  J = prods$J

  if (is.null(eps_ji)) {
    eps_ji = rnorm(J,0,sigma_eps_ji)
  } else {
    sigma_eps_ji = NA
  }

  V_ji = eps_ji

  vars_cat = prods$vars_cat
  var_cat = vars_cat[1]
  if (!is.null(lambda_ji)) {
    sigma_cat = lambda_ji$sigma_cat
    mean_cat = lambda_ji$mean_cat
    for (var_cat in vars_cat) {
      n = prods$n_cat[var_cat]
      V_cat = rnorm(n,mean_cat[var_cat],sigma_cat[var_cat])
      rows = match(u_df[[var_cat]], prods$vals_cat[[var_cat]])
      V_ji = V_ji+V_cat[rows]
    }
  }

  if (!is.null(psi_ref)) {
    if (is.null(ref_prod)) {
      ref_prod = sort(V_ji+sigma_eps,decreasing = TRUE,index.return)$ix[num_ref_prod]
    }
  }
  nlist(
    N,
    lambda_ji,
    psi_abs,
    psi_ref,
    sigma_eps_ji,
    sigma_eps_jmi,
    sigma_eps_jmin=1,
    ref_prod,
    V_0i,
    V_ji
  )
}


#' Define how categorical and numerical product attributes linearly affact utility
lambda = function(prods, mean_cat=0, sigma_cat=1, mean_num=0, sigma_num=1) {
  vars_cat = prods$vars_cat
  vars_num = prods$vars_num

  sigma_cat = rep(sigma_cat, length(vars_cat))
  if (is.null(names(sigma_cat))) names(sigma_cat) = vars_cat

  mean_cat = rep(mean_cat, length(vars_cat))
  if (is.null(names(mean_cat))) names(mean_cat) = vars_cat

  sigma_num = rep(sigma_num, length(vars_num))
  if (is.null(names(sigma_num))) names(sigma_num) = vars_num

  mean_num = rep(mean_num, length(vars_num))
  if (is.null(names(mean_num))) names(mean_num) = vars_num

  nlist(mean_cat, sigma_cat, mean_num, sigma_num)
}


# Reference based price function according to simplified Gutenberg-Model
psi_gutenberg_kinked = function( a = 1,b=2, kink_perc=0.2, num_ref_prod = 5) {
  nlist(price_fun, a, b, kink_perc, num_ref_prod)
}

psi_linear = function(a=1) {
  price_util_fun = function(m_df,...) {
    -a*m_df$price
  }
  nlist(price_type="linear", price_util_fun, par=nlist(a))
}


