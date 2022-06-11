library(shiny)
library(metathis)
library(DT)
library(shinyvalidate)
options(scipen = 999)

single_2079_80 <- c(".01" = 500000, ".10" = 700000, ".20" = 1000000, ".30" = 2000000, ".36" = Inf)
single_2079_80_ssf <- c("0" = 500000, ".10" = 700000, ".20" = 800000, ".30" = 1500000, ".36" = Inf)
single_2078_79 <- c(".01" = 400000, ".10" = 500000, ".20" = 700000, ".30" = 2000000, ".36" = Inf)
single_2078_79_ssf <- c("0" = 400000, ".10" = 500000, ".20" = 700000, ".30" = 2000000, ".36" = Inf)

couple_2079_80 <- c(".01" = 600000, ".10" = 800000, ".20" = 1100000, ".30" = 2100000, ".36" = Inf)
couple_2079_80_ssf <- c("0" = 600000, ".10" = 800000, ".20" = 1100000, ".30" = 2100000, ".36" = Inf)
couple_2078_79 <- c(".01" = 450000, ".10" = 550000, ".20" = 750000, ".30" = 1700000, ".36" = Inf)
couple_2078_79_ssf <- c("0" = 450000, ".10" = 550000, ".20" = 750000, ".30" = 1700000, ".36" = Inf)

rate <- \(x) names(x) |> as.numeric()
max_cit <- \(x) ifelse((x /3) < 300000, x /3, 300000)
max_ssf <- \(x) 500000 - x
max_life <- \(x) ifelse(x == "2078/79", 25000, 40000)
female_rebate <- \(x) as.numeric(tail(x, 1)[[3]]) * 0.1 
net_tax <- \(x) as.numeric(x[x$taxable_income == "Total", ][,3]) - as.numeric(x[x$rate == "Female Rebate (10%)", ][,3])

last_row_tax <- \(x) x[nrow(x) -1, "tax_liability"] |> as.numeric()

fmt_cur <- \(x) formatC(x, format = "f", big.mark = ",", drop0trailing = TRUE)

total_taxable <- \(
  disab = F,
  income = 0,
  year = 0,
  cit = 0,
  ssf = 0,
  life = 0,
  medical = 0) {
  
  if (disab == F) disab_wave  <-  0
  else disab_wave <- ifelse(income >= 200000, 200000, income)
  
  deducible <- cit + ssf + life + medical
  taxable_inc <- income - deducible - as.numeric(disab_wave) 
  
  
  deduce <- rbind(list(names = "Total Income", val = income), 
        list(names = "Deduction", val = deducible), 
        list(names = "Disability Discount", val = disab_wave), 
        list(names = "Taxable Income", val = taxable_inc)) |> data.frame()
  
  if(disab == F) deduce <- deduce[deduce$names != "Disability Discount", ]
  
  return(deduce)
  
  # class(deduce)
}

# total_taxable("Yes", 1000, "2078/79")


income_tax <- \(
  disab = F,
  sex = F,
  mstatus = F,
  income = 0,
  year = 0,
  cit = 0,
  ssf = 0,
  life = 0,
  medical = 0, 
  mexpense = 0
) {
  
  if (disab == F) disab_wave  <-  0
  else disab_wave <- ifelse(income >= 200000, 200000, income)
  
  if (year == "2078/79" & ssf == 0 & mstatus == F) slab <- single_2078_79
  else if (year == "2078/79" & ssf != 0 & mstatus == F) slab <- single_2078_79_ssf
  else if (year == "2079/80" & ssf == 0 & mstatus == F) slab <- single_2079_80
  else if (year == "2079/80" & ssf != 0 & mstatus == F) slab <- single_2079_80_ssf
  else if (year == "2078/79" & ssf == 0 & mstatus) slab <- couple_2078_79
  else if (year == "2078/79" & ssf != 0 & mstatus) slab <- couple_2078_79_ssf
  else if (year == "2079/80" & ssf == 0 & mstatus) slab <- single_2079_80
  else if (year == "2079/80" & ssf != 0 & mstatus) slab <- single_2079_80_ssf
  
  deducible <- disab_wave + cit + ssf + life + medical
  taxable_inc <- income - deducible
  
  if (taxable_inc != 0) { 
  
  brackets <- diff(c(0, pmin(taxable_inc, unname(slab))))
  cal <- brackets * rate(slab)
  table <- data.frame(taxable_income = brackets, 
                      rate = rate(slab) * 100, tax_liability = cal)
  table <- table[which(table$taxable_income != 0), ]
  table <- rbind(table, c("Total", "", colSums(table[,2:3])[-1]))
  
  mexp_per <- mexpense * 0.15
  

if (sex) {
  table <- rbind(table, c("", "Female Rebate (10%)", female_rebate(table)))
  table <- rbind(table, c("", "Net Tax Liability", net_tax(table)))

  if (mexpense != 0) {
    table <- rbind(table, c("", "Medical Tax (15%)", mexp_per))
    table <- rbind(table, c("", "Total Tax", last_row_tax(table) - mexp_per))
  }
}


if (mexpense != 0) {
  table <- rbind(table, c("", "Medical Tax (15%)", mexp_per))
  table <- rbind(table, c("", "Total Tax", last_row_tax(table) - mexp_per))
}

  

  
  }
  
  else{

    table <- data.frame(taxable_income = 0, 
                        rate = head(rate(slab) * 100, 1), tax_liability = 0)

  }
  
  return(table)
  
}

# income_tax("Yes", "Male", 1000, "2078/79")
# 
# 
# income_tax(
#   disab = T,
#   sex = "Male",
#   mstatus = F,
#   income = 10,
#   year = "2078/79",
#   cit = 0,
#   ssf = 0,
#   life = 0,
#   medical = 0,
#   mexpense = 5000
# )
