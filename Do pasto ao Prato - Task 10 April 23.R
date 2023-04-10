#################################################################################
#	Rethriving information from "Compras Governamentais"
# The following script allows for the count of meatpeakers in
#	each Brazilian state that has supplied the government.
#
#  This script is part of the task application. 
#	Data Scientist in the Do Pasto ao Prato.
#
#	Script by Gabriel Santos 10 April 2023
#		any question, email me: ssantos.gabriel@gmail.com
##################################################################################


#=================================================================================
# 			Install and load auxiliary packages 
#Install.packages("Rcurl") if not available. The same applies for each package below.
#=================================================================================
library(RCurl)
library(tidyverse)
library(rjson)

rm(list=ls())	#Make sure we will work with a clear environment


#=================================================================================
#	Rethriving data from the Brazilian database "Compras governamentais"
# Create an object to store data from each API request. 
# Because data extraction is currently manual, each entry should reflect one category.
# Please note format ".json" (others available, but extraction is format specific) 
#					and parameters "id_cnae=1011201" as explained in the activity
#==================================================================================
result_A <- fromJSON(file="https://compras.dados.gov.br/fornecedores/v1/fornecedores.json?id_cnae=1011201")
result_B <- fromJSON(file="https://compras.dados.gov.br/fornecedores/v1/fornecedores.json?id_cnae=1011205")
result_C <- fromJSON(file="https://compras.dados.gov.br/fornecedores/v1/fornecedores.json?id_cnae=1013901")
result_D <- fromJSON(file="https://compras.dados.gov.br/fornecedores/v1/fornecedores.json?id_cnae=1013902")


#=================================================================================
#Brute force standardization
#=================================================================================
## Because data structure follows:
#" list: 
#  - list 1 (metadata rethrive)
#  - list 2: fornecedores
#  - list 3: total number of fornecedores"
# We can bind list of all suppliers by once using do.call(rbind,object[[2]][[1]])
#---------------------------------------------------------------------------------
# NOTE: The soluction is far from ideal but it does the job. A set of manual validation tasks was performed to guarantee data accuracy.
# Note that each supplier is repeated to accommodate the company link in "_links". 
#=================================================================================
df_result_A<-do.call(rbind, result_A[[2]][[1]])%>%as_tibble()%>%unnest()	#as_tibble and unnest are necessary to make sure data follow a simple data.frame. 
df_result_B<-do.call(rbind, result_B[[2]][[1]])%>%as_tibble()%>%unnest()
df_result_C<-do.call(rbind, result_C[[2]][[1]])%>%as_tibble()%>%unnest()
df_result_D<-do.call(rbind, result_D[[2]][[1]])%>%as_tibble()%>%unnest()

#Merge all cnae's in a single data frame
df_result<-rbind(df_result_A,df_result_B,df_result_C,df_result_D)%>%as_tibble()%>%unnest()

#=================================================================================
### Validation: 
#Check if the data was extracted properly.
#=================================================================================
df_result%>%distinct(cnpj,id_cnae,.keep_all=TRUE)%>%group_by(id_cnae)%>%summarise(N=n())
## Compare with:
browseURL("https://compras.dados.gov.br/fornecedores/v1/fornecedores.html?id_cnae=1011201")
##(note html format instead of .json)

#=================================================================================
# Summary data (Table 1 in report)
# Select only in activity suppliers "ativo=TRUE" 
#=================================================================================
df_result%>%					
distinct(cnpj,id_cnae,.keep_all=TRUE)%>%	# Select distinct cnpjs, id_cnae, and uf
group_by(id_cnae,ativo)%>%			# group id_cnae, actity status and uf to summarise
summarise(Ativo=n())%>%				# Sum all suppliers according to activity status 
mutate(Total=sum(Ativo))%>%			# Calculate total suppliers (on and off) 
mutate(Category=case_when(		# Translate id_cnae to a friender classification
				id_cnae=="1011201" ~ "FRIGORÍFICO - ABATE DE BOVINOS (1011201)",
				id_cnae=="1011205" ~ "MATADOURO - ABATE DE RESES, EXCETO ABATE DE SUÍNOS (1011205)",
				id_cnae=="1013901" ~ "FABRICAÇÃO DE PRODUTOS DE CARNE (1013901)",
				id_cnae=="1013902" ~ "PREPARAÇÃO DE SUBPRODUTOS DO ABATE (1013902)"))%>%
filter(ativo=="TRUE")%>%select(-ativo)	#Reduce information to clarify main message

#=================================================================================
# Plot (Figure 1 in report)
#=================================================================================
df_result%>%
filter(ativo=="TRUE")%>%		#Filter in activity suppliers
distinct(cnpj,id_cnae,uf)%>%		# Select distinct cnpjs, id_cnae and uf
group_by(id_cnae,uf)%>%			# group id_cnae and uf to summarise
summarise(N=n())%>%			# count total suppliers category by uf
mutate(Category=case_when(		# Translate id_cnae to a friender classification
				id_cnae=="1011201" ~ "FRIGORÍFICO - ABATE DE BOVINOS (1011201)",
				id_cnae=="1011205" ~ "MATADOURO - ABATE DE RESES, EXCETO ABATE DE SUÍNOS (1011205)",
				id_cnae=="1013901" ~ "FABRICAÇÃO DE PRODUTOS DE CARNE (1013901)",
				id_cnae=="1013902" ~ "PREPARAÇÃO DE SUBPRODUTOS DO ABATE (1013902)"))%>%
ggplot(.,aes(x=uf,y=sort(N),group=as.factor(Category)))+
geom_col(aes(fill = Category))+	#stacked columns
labs(x="Brazilian states (UF)",y= "# Brazilian government suppliers",
title="Part 1 - Technical skills: ",
fill="Category \n(id_cnae)") +
guides(fill=guide_legend(nrow=2,byrow=TRUE))+
scale_fill_brewer(palette="Paired")+
theme_bw(base_size=18)+
theme(legend.position="bottom")



#=================================================================================
# Export Cleaned Data to Excel
# Use the openxlsx package
#=================================================================================

openxlsx::write.xlsx(df_result,"Data_task1.xlsx")
