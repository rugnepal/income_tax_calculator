options(scipen = 999)



single_2079_80 <- c(".01" = 500000, ".10" = 700000, ".20" = 800000, ".30" = 1500000, ".36" = Inf)
single_2078_79 <- c(".01" = 400000, ".10" = 500000, ".20" = 700000, ".30" = 2000000, ".36" = Inf)

single_2078_79_ssf <- c("0" = 400000, ".10" = 500000, ".20" = 700000, ".30" = 2000000, ".36" = Inf)
single_2079_80_ssf <- c("0" = 500000, ".10" = 700000, ".20" = 800000, ".30" = 1500000, ".36" = Inf)

rate <- \(x) names(x) |> as.numeric()

female_rebate <- \(x) tail(x, 1)[[3]] * 0.1 

#
# switch("10",
#        quote(10) = "ten", "1000" = "thousand")
#
#
# slab <- \(x, ssf) {
#   val <- paste(x, ssf, sep = "-")
#
#   switch(val,
#     "2078/79" = single_2078_79,
#     "2079/80" = single_2079_80,
#     val = single_2078_79_ssf
#   )
# }
#   # paste(x,ssf, sep="-")

#
# slab("2078/79", 1000)



income_tax <- \(income = 0,
  year = 0,
  cit = 0,
  ssf = 0,
  life = 0,
  medical = 0
) {
  # slab <- switch(as.character(year),
  #   "2078/79" = single_2078_79,
  #   "2079/80" = single_2079_80
  # )

  # if (year == "2078/79")  slab <- ifelse(ssf == 0, single_2078_79, single_2078_79_ssf)
  # else slab <- ifelse( ssf == 0, single_2079_78, single_2078_79_ssf)

  # slab <- ifelse( ssf == 0, single_2078_79, single_2078_79_ssf)

  if (year == "2078/79" & ssf == 0) {
    slab <- single_2078_79
  } else if (year == "2078/79" & ssf != 0) {
    slab <- single_2078_79_ssf
  } else if (year == "2079/80" & ssf == 0) {
    slab <- single_2079_80
  } else if (year == "2079/80" & ssf != 0) slab <- single_2079_80_ssf

  deducible <- cit - ssf - life - medical

  taxable_inc <- income - deducible


  # cit <- ifelse(income / 3 > 300000, 300000, income / 3)

  # taxable_inc <- switch(as.character(cit),
  # "Y" = income - cit,
  # "N" = cit)

  
  brackets <- diff(c(0, pmin(taxable_inc, unname(slab))))
  
  cal <- brackets * rate(slab)

  # inc <- ifelse(rates)

  table <- data.frame(taxable_income = brackets, 
                      rate = rate(slab) * 100, tax_liability = cal)

  table

  table[which(table$taxable_income != 0), ]
  # cit
}


income_tax(1000000, "2078/79", ssf = 10000)
