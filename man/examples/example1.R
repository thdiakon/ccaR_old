DATASET<-readxl::read_excel(system.file('extdata','cca.xlsx', package = 'ccaR'))

cca_table<-cca(DATASET)



