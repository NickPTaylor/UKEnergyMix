# load libraries ---------------------------------------------------------------
library(decctools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

# setup time range of past day -------------------------------------------------
end  <- Sys.Date()
start <- end - 5

# get grid mix -----------------------------------------------------------------
df_grid_mix <- get_grid_mix(start, end)

# pre process data -------------------------------------------------------------
df_proc <- tbl_df(df_grid_mix)
df_proc <- df_proc  %>% 
  select(datetime, Gas, Coal, Nuclear, Wind) %>% 
  gather(Type, Output, -datetime) %>% 
  rename(Time = datetime) 

# plot graph -------------------------------------------------------------------
g <- ggplot(df_proc, aes(x = Time, y = Output, color = Type)) +
  geom_line() +
  scale_x_datetime(
    breaks = date_breaks("3 hours"), labels = date_format("%d:%m %H:%M")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Electricity output by fuel type") + ylab("Output (MW)")

print(g)
