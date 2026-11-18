HB_unit <- function(pop, samp, covars,
                    rho_grid = seq(0.05, 0.95, length = 40),
                    A = 2000) {
  
  ## For a single fixed model space, list length is 1, forcing model prior weights to naturally collapse to 1.00
  HBMA_unit(
    pop = pop,
    samp = samp,
    covars_list = list(covars),
    rho_grid = rho_grid,
    A = A
  )
}