
library(pacman)
p_load(tidyverse,tidyfst,ggstatsplot,patchwork,janitor)

read_csv("data/ins166_info.csv") %>% 
	select(-cnci)->data

data %>% 
	rename(
		Type = class,
		DSI = ins,
		`Citation impact` = percentile,
		`Interdisciplinary degree` = div,
		`Team size` = au_no,
		`Funding rate` = funded_rate,
		`Institutional collaboration` = ins_col,
		`International collaboration` = con_col
	) %>% 
	transmute(Type,DSI,`Funding rate`,`Team size`,`Interdisciplinary degree`,
						`Institutional collaboration`,`International collaboration`)-> data1



data1 %>% select(-DSI) %>% 
	group_by(Type) %>% 
	summarise_if(is.numeric,\(x){
		mean(x) %>% round(2) -> mx
		sd(x) %>% round(2)-> sdx
		str_glue("{mx} ({sdx})")
	}) %>% 
	ungroup() %>% 
	bind_rows(
		data1 %>% 
			summarise_if(is.numeric,\(x){
				mean(x) %>% round(2) -> mx
				sd(x) %>% round(2)-> sdx
				str_glue("{mx} ({sdx})")
			}) %>% 
			mutate(Type = "Total")
	) %>% 
	pivot_longer(cols = -Type) %>% 
	pivot_wider(names_from = "Type",values_from = "value") %>% 
	write_clip()

data %>% 
	rename(
		Type = class,
		DSI = ins,
		`Citation impact` = percentile,
		`Interdisciplinary degree` = div,
		`Team size` = au_no,
		`Funding rate` = funded_rate,
		`Institutional collaboration` = ins_col,
		`International collaboration` = con_col
	) %>% 
	transmute(Type,DSI,`Citation impact`,`Funding rate`,`Team size`,`Interdisciplinary degree`,
						`Institutional collaboration`,`International collaboration`)-> data2

ggcorrmat(data = data2,cor.vars = 3:8,
					# p.adjust.method = "none",
					# matrix.type = "full",
					type = "nonparametric")

ggsave("output/cor.png",width = 6,height = 6)

# lm(`Citation impact` ~.,data = data2 %>% select(-DSI,-Type)) -> lm_model
# summary(lm_model)
# ggcoefstats(lm_model) 
# ggsave("output/lm.png",width = 5,height = 5)


type_wanted = "Computer-driven" # Clinical-driven Computer-driven Mathematics-driven

data2 %>% 
	filter(Type == type_wanted) %>% 
	slice_max(`Citation impact`,n = 5) %>% 
	select(-1) %>% 
	adorn_totals(name = "Top average")%>%
	mutate(across(where(is.numeric), 
								~ replace(., n(), .[n()]/(n()-1)))) %>%
	as_tibble %>% 
	bind_rows(
		data2 %>% 
			filter(Type == type_wanted) %>% 
			select(-1) %>% 
			adorn_totals(name = "Total average")%>%
			mutate(across(where(is.numeric), 
										~ replace(., n(), .[n()]/(n()-1)))) %>%
			tail(1)
	) %>% 
	print(width = Inf)


