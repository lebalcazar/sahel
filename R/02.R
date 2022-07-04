

x <- read_csv("/media/luisbalcazar/ADATA HD720/01 CIR/KBA_data_Afr_Mex/Amacuzac/DATA/PNNCDR/met4cqu1983-2016/1010.csv",
              col_names = FALSE, na = c("-1", "-9999"))
x

x |> 
  group_by(X3) |> 
  summarise(pmean = sum(X4, na.rm = T)) |> 
  ggplot() +
  geom_col(aes(x = X3, y = pmean))

