
library(pacman)
p_load(tidyverse,tidyfst,clipr,ggsignif)

import_fst("data/data_ins_cluster_cnci.fst") %>% 
	as_tibble()-> data

data %>% 
	select(UT,percentile,class,ins) %>% 
	summarise(percentile = mean(percentile),.by = c(class,ins)) %>% arrange(class,-percentile)-> data_ap
fwrite(data_ap,"data/class_ins_pct.csv")


data_ap %>%
	mutate(class = fct_reorder(class,percentile)) %>%
	ggplot(aes(class,percentile)) +
	geom_boxplot() +
	geom_signif(
		test = "t.test",
		comparisons = list(c("Computer-driven","Clinical-driven"),
											 c("Mathematics-driven","Clinical-driven"),
											 c("Computer-driven","Mathematics-driven")),
		step_increase = .1,
		# "Computer-driven","Clinical-driven","Mathematics-driven"
							map_signif_level = T) +
	geom_hline(yintercept = 50,linetype = "dashed") +
	# labs(x = NULL,y = "Average Percentile")+
	labs(x = "DSI Type",y = "Citation Impact")+
	theme_bw() +
	theme(
		axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
		axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
	)

ggsave("output/03_class_ap.png",width = 4,height = 4)
data_ap %>% 
	summarise(median = median(percentile),avg = mean(percentile),sd = sd(percentile),.by = class)

t.test(data_ap %>% filter(class == "Clinical-driven") %>% pull(percentile),
			 data_ap %>% filter(class == "Computer-driven") %>% pull(percentile))

# data_ap %>% 
# 	summarise(median = median(percentile),avg = mean(percentile),sd = sd(percentile),.by = class) %>% 
# 	mutate(class = fct_reorder(class,avg)) %>% 
# 	ggplot(aes(class,avg)) +
# 	geom_pointrange(aes(ymin = avg - sd,ymax = avg + sd)) +
# 	geom_hline(yintercept = 50,linetype = "dashed") +
# 	labs(x = NULL,y = "Average Percentile")+
# 	theme_bw() +
# 	theme(
# 		axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
# 	)


data_ap %>% 
	slice_max(percentile,n = 5,by = class) %>% 
	arrange(class,-percentile) %>% 
	print(n = Inf)

data_ap %>% 
	setNames(c("Type","DSI Name","Citation impact")) %>% 
	write_csv("output/Supplementary Table 2 - DSI citation impact.csv")


