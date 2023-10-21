
library(pacman)
p_load(tidyfst,tidyverse,factoextra,ggradar)

read_csv("data/ut_esi.csv") %>% 
	na.omit() %>% 
	janitor::clean_names() %>% 
	filter(publication_date %in% 2013:2022,document_type %in% c("Article","Review")) %>% 
	rename(UT = accession_number) -> incites_data

import_fst("data/data_sci_ins_paper.fst") %>% 
	as_tibble()-> data

data %>% 
	inner_join(incites_data %>% select(UT,research_area)) -> all_data

all_data

all_data %>% 
	count_dt(research_area) %>% 
	select(research_area) -> top_ra

all_data %>% 
	count_dt(research_area) %>% 
	add_prop() %>% 
	mutate(cum_prop = cumsum(prop))

all_data %>% 
	inner_join(top_ra) %>% 
	count(ins,research_area) %>% 
	complete_dt(ins,research_area,fill = 0) %>% 
	group_by(ins) %>% 
	mutate(prop = n/sum(n)) %>% 
	ungroup() %>% 
	wider_dt(ins,name = "research_area",value = "prop") %>% 
	column_to_rownames(var = "ins")-> scaled_data

# find the best k for kmeans clustering, result is 3
fviz_nbclust(scaled_data, kmeans, method = "silhouette") -> find_k_fig
ggsave("output/to_find_k.png")

kmeans(scaled_data,centers = 3) -> km
# fviz_cluster(km, data = scaled_data,main = NULL,
# 						 ggtheme = theme_bw(),geom = "text",repel = T)
# ggsave("output/cluster_pca.png",width = 20,height = 12)
fviz_cluster(km, geom = "point",data = scaled_data,
						 show.clust.cent = F,
						 ggtheme = theme_bw(),main = NULL) -> pca_fig
ggsave("output/cluster_pca.png")

find_k_fig + pca_fig + plot_annotation(tag_levels = "a",tag_prefix = "(",tag_suffix = ")")
ggsave("output/cluster_process.png",width = 9,height = 5)

km

km$centers %>% 
	as_tibble() %>% 
	mutate(class = c("Computer-driven","Clinical-driven","Mathematics-driven"),
				 .before = 1) %>% 
	ggradar(legend.position = "bottom",group.point.size = 3) +
	theme(
		axis.text = element_text(hjust=0.5, vjust = 1),
		plot.margin = margin(0, 5, 0, 5, 'cm')) +
	coord_cartesian(clip = "off")
# https://stackoverflow.com/questions/71790703/ggradar-how-to-increase-space-for-long-axis-labels
ggsave("output/02_radar.png",width = 9,height = 5)

# km$centers %>%
# 	as_tibble() %>%
# 	mutate(class = c("Clinical-driven","Mathematics-driven","Computer-driven"),
# 				 .before = 1) %>%
# 	# ggradar(legend.position = "bottom") +
# 	ggradar(legend.position = "none") +
# 	facet_wrap(~class)

km$cluster %>% 
	enframe() %>% 
	setNames(c("ins","cluster")) %>% 
	mutate(class = case_when(
		cluster == 1 ~ "Computer-driven",
		cluster == 2 ~ "Clinical-driven",
		cluster == 3 ~ "Mathematics-driven",
	)) -> ins_class

all_data %>% inner_join(ins_class) %>% 
	select(-cluster) -> all_data2
all_data2 %>% 
	count(ins,class) %>% 
	arrange(class,-n) %>% 
	slice_max(n,n = 3,by = class) %>% 
	summarise(ins = str_c(ins,collapse = "; "),.by = class) %>% 
	inner_join(
		ins_class %>% count(class)
	) %>% 
	transmute(class,n,ins) %>% 
	arrange(-n)

ins_class %>% 
	# slice_head(n = 3,by = class) %>% 
	summarise(ins = str_c(ins,collapse = "; "),.by = class) %>% 
	inner_join(
		ins_class %>% count(class)
	) %>% 
	transmute(class,n,ins) %>% 
	arrange(-n) %>% 
	write_excel_csv("output/02_display_cluster.csv")

export_fst(all_data2,"data/data_ins_cluster.fst")

## 
import_fst("data/data_ins_cluster.fst") %>% 
	count(class,ins,sort = T) %>% 
	arrange(class,-n) %>% 
	select(-n) %>% 
	setNames(c("Type","DSI Name"))  %>% 
	write_csv("output/Supplementary Table 1 - DSI List with classification.csv")



