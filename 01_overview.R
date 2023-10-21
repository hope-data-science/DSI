
source("code/io.R")

read_csv("data/ut_esi.csv") %>% 
	na.omit() %>% 
	janitor::clean_names() %>% 
	filter(publication_date %in% 2013:2022,document_type %in% c("Article","Review"))-> data

data %>% 
	distinct(accession_number,publication_date) %>% 
	count(publication_date) %>% 
	rename(Year = publication_date) %>% 
	mutate(Year = as.factor(Year)) %>% 
	ggplot(aes(Year,n)) +
	geom_col(fill = "white",color = "black") +
	geom_text(aes(label = n),vjust = -.4) +
	labs(x = NULL,y = "No. of publications") +
	expand_limits(y = 12500) +
	theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
ggsave("output/01_overall_no.png",width = 6,height = 4)

data %>% 
	count_dt(research_area) %>% 
	add_prop() %>% 
	mutate(cum_prop = cumsum(prop))


data %>% 
	count_dt(research_area) %>% 
	add_prop()%>% 
	select(-prop) %>% 
	mutate(rank = min_rank(-n),.before = 1) %>% 
	setNames(c("Rank","ESI Category","Paper No.","Percentage")) %>% 
	fwrite("output/01_esi_prop.csv")

