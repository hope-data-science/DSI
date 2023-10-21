
library(pacman)
p_load(tidyverse,tidyfst)

read_csv("data/ins166_info.csv") %>% 
	select(-cnci)->data

data %>% 
	filter(class == "Mathematics-driven") %>% 
	select(- (1:2)) %>% 
	summarise_all(mean) %>% 
	mutate(ins = "Baseline") -> total_info

data %>% 
	filter(ins == "Zhejiang Univ Finance & Econ, Sch Data Sci" |
				 	ins == "Univ Texas Austin, Dept Stat & Data Sci" |
				 	ins == "Shanxi Univ, Shanxi Key Lab Math Tech & Big Data Anal Dis Cont") %>% 
	select(-class) %>% 
	bind_rows(total_info) %>% 
	rename(
		DSI = ins,
		`Citation impact` = percentile,
		`Interdisciplinary degree` = div,
		`Team size` = au_no,
		`Funding rate` = funded_rate,
		`Institutional collaboration` = ins_col,
		`International collaboration` = con_col
	) %>% 
	pivot_longer(cols = -1) %>% 
	pivot_wider(names_from = DSI,values_from = value) %>% 
	select(1,4,5,3,2) %>% 
	mutate_if(is.numeric,\(x) round(x,2)) %>% 
	setNames(c("Feature","Target DSI","Baseline","Local model","Foreign model")) %>% 
	write_clip()

data %>% 
	filter(class == "Mathematics-driven") %>% 
	slice_max(percentile,n = 20) %>% 
	pull(ins)
