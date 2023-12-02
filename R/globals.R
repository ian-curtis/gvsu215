# declare the global variables for the package

globalVariables(unique(c(
  # infer_anova:
  ".data", "df",
  # infer_chisq:
  ".data", "method",
  # infer_prop1:
  "conf.high", "conf.low", "estimate", "na", "se",
  # infer_reg:
  "p.value", "statistic",
  # pctile:
  "name", "value",
  # tbl_one:
  "n",
  # tbl_two:
  "n", "Total"
)))
