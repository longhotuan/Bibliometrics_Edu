#### import libraries ####
# data wrangling

library(tidyverse)
library(reshape)
library(data.table)
library(xlsx)
library(gridExtra)
library(grid)
library(chron)
library(devtools)
library(rscopus)
library(rlist)
library(rgeos)
library(future)
library(parallel)
library(doParallel)

# data visualization 

library(GGally)
library(RColorBrewer)
library(proj4)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(corrplot)
library(mice)
library(VIM)
library(ggmosaic)
library(esquisse)
library(bibliometrix)
library(ggwordcloud)
library(colorspace)
library(rworldmap)
library(countrycode)

##### import data ----
#** From Scopus website (csv) ####

WN_scopus <- read.csv("WM_scopus.csv", header = TRUE)
WN_scopus2 <- read_csv("WM_scopus.csv")
colnames(WN_scopus) <- colnames(WN_scopus2)
rm(WN_scopus2)

#choose the useful columns

WN_scopus <- WN_scopus[,c(2, 4:6, 13, 14, 16, 18, 19, 20, 30:33, 39:41, 43, 44)]
# transform data

WN_scopus$Publisher <- as.factor(WN_scopus$Publisher)
WN_scopus$`Language of Original Document` <- as.factor(WN_scopus$`Language of Original Document`)
WN_scopus$`Document Type` <- as.factor(WN_scopus$`Document Type`)
WN_scopus$`Access Type` <- as.factor(WN_scopus$`Access Type`)
WN_scopus$`Abbreviated Source Title` <- as.factor(WN_scopus$`Abbreviated Source Title`)
WN_scopus <- WN_scopus %>% filter(Year < 2020)

#** From R affi ####

WN_affi <- read.delim(file = "WM_scopus_affiliation.txt", sep = "\t")
WN_affi <- WN_affi[,-1]
colnames(WN_affi) <- c("Affiliation", "City", "Country", "Entry Number")


WN_affi <- WN_affi[!is.na(WN_affi$Country),]

WN_affi_mis <- WN_affi[0,]

for (i in 1:ncol(WN_affi_mis)){
    if (is.factor(WN_affi_mis[,i])){
        WN_affi_mis[,i] <- as.numeric(as.character(WN_affi_mis[,i]))
    }
    WN_affi_mis[1,i] <- sum(is.na(WN_affi[,i]))
}      

#** From R df ####

WN_df <- read.delim("WM_scopus_df.txt", sep = "\t", stringsAsFactors = FALSE)
WN_df <- WN_df %>% select(eid, `dc.title`, `prism.publicationName`, `prism.coverDate`, `prism.doi`, `citedby.count`, subtypeDescription,
                          openaccessFlag)
colnames(WN_df) <- c("EID", "Title", "Publisher", "Cover Date", "DOI", "Cited by", "Document Type", "Access Type")

# separate year in df data

for (i in 1:nrow(WN_df)){
    x <- 1+ str_count(WN_df$`Cover Date`,"-")
    WN_df$Year[i] <- str_split_fixed(WN_df$`Cover Date`[i], "-", n = x)[1]
}

for(i in 1:nrow(WN_df)){
    if (str_count(WN_df$Year[i], " ") != 0){
        x <- 1 + str_count(WN_df$Year[i], " ")
        WN_df$Year[i] <- str_split_fixed(WN_df$Year[i], " ", n = x)[x] 
    }
}
# WN_df <- WN_df %>% filter(str_detect(Year,"2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019"))
WN_df <- WN_df[,-4]
WN_df <- WN_df[, c(2,3,8,6,4,5,7,1)]


##### Main info ####
#** Document types & languages----

# Document types
WM_scopus <- WN_scopus
WN_scopus <- WN_scopus %>% filter(str_detect(Year,"2009|2010|2011|2012|2013|2014|2015|2016|2017|2018|2019"))


WN_DT <- as.data.frame(summary(WN_scopus$`Document Type`)) %>%
    rownames_to_column(var = "rowname")
write.csv(WN_DT, "WN_DT.csv", row.names = FALSE)

# Percentage stacked bar plot 
WN_DT2 <- WN_scopus %>% select(`Document Type`, Year, `Access Type`) %>% group_by(Year, `Document Type`) %>% 
    dplyr::summarise(n = n())
WN_DT2 <- WN_DT2 %>% 
    group_by(Year) %>% # be careful with the plyr package
    mutate(percentage = n*100/sum(n))

ggsave("WN_DT2.tiff", WN_DT2 %>% ggplot() +
           geom_bar(aes(y=percentage, x=Year,fill = `Document Type`), stat = 'identity')+
           theme_bw() +
           xlab("Year") +
           ylab("Percentage of the publications") +
           theme(text=element_text(family = "Arial")) +
           theme(axis.text.x = element_text(size = 14)) +
           theme(axis.text.y = element_text(size = 14)) +
           theme(axis.title = element_text(size = 14)) +
           theme(legend.text = element_text(size = 12)) +
           theme(legend.title = element_text(size = 12)) +
           # scale_fill_brewer(palette = "Set3")+
           scale_x_continuous(breaks = c(2009:2019)),
       units = 'cm', height = 20, width = 30, dpi = 300
)


# language of the document 
WN_scopus$`Language of Original Document` <- as.factor(WN_scopus$`Language of Original Document`)
WN_Lang <- as.data.frame(summary(WN_scopus$`Language of Original Document`)) %>% rownames_to_column(var = "rowname")
colnames(WN_Lang)[2] <- "n"
WN_Lang$Percentage <- WN_Lang$n*100/sum(WN_Lang$n)

write.csv(WN_Lang, "WN_Lang.csv", row.names = FALSE)

#** Open Access ----

WN_scopus$`Access Type` <- as.factor(WN_scopus$`Access Type`)
WN_AT <- as.data.frame(summary(WN_scopus$`Access Type`)) %>% rownames_to_column(var = "rowname")
colnames(WN_AT)[2] <- "n"
WN_AT$Percentage <- WN_AT$n/sum(WN_AT$n)
write.csv(WN_AT, "WN_AT.csv", row.names = FALSE)  

#** Number and top of keywords and keywords plus ----

WN_KW <- sum(str_count(WN_scopus$`Author Keywords`, ";"), na.rm = TRUE)+
    nrow(WN_scopus[complete.cases(WN_scopus$`Author Keywords`),])
WN_KWP <- sum(str_count(WN_scopus$`Index Keywords`, ";"), na.rm = TRUE)+
    nrow(WN_scopus[complete.cases(WN_scopus$`Index Keywords`),])

KW <- function(x){
    keyword <- strsplit(x, "; ")
    for (i in 1:length(keyword)){
        keyword[i] <- as.data.frame(matrix(as.data.frame(keyword[i])))
    }
    keyword2 <- rbindlist(keyword)
    colnames(keyword2)[1]<- "keyword"
    keyword2<- keyword2[complete.cases(keyword2),]
    keyword2$keyword <- str_to_title(keyword2$keyword)
    keyword3 <- keyword2 %>%
        dplyr::group_by(keyword) %>% 
        dplyr::summarise(n=n()) %>% 
        dplyr::arrange(desc(n)) 
    return(keyword3)
}

WN_scopus$`Author Keywords` <- as.character(WN_scopus$`Author Keywords`)
WN_scopus$`Index Keywords` <- as.character(WN_scopus$`Index Keywords`)

WN_TopKW <- KW(WN_scopus$`Author Keywords`)
WN_TopKWP <- KW(WN_scopus$`Index Keywords`)
write.csv(WN_TopKW[1:50,], "WN_TopKW.csv", row.names = FALSE)
write.csv(WN_TopKWP[1:50,], "WN_TopKWP.csv", row.names = FALSE)

ggsave("WN_TopKW.tiff", WN_TopKW[1:20,] %>% 
           ggplot(aes(label = keyword, size =n
                      ,color = rainbow_hcl(20)
           )) +
           geom_text_wordcloud_area(shape = "star") +
           scale_size_area(max_size = 15) +
           theme_minimal(), 
       units = 'cm', height = 20, width = 20, dpi = 300
)
ggsave("WN_TopKWP.tiff", WN_TopKWP[2:21,] %>% 
           ggplot(aes(label = keyword, size =n
                      ,color = rainbow_hcl(20)
           )) +
           geom_text_wordcloud_area(shape = "star") +
           scale_size_area(max_size = 15) +
           theme_minimal(), 
       units = 'cm', height = 20, width = 20, dpi = 300
)

#** Citation ----
WN_scopus$`Cited by`[is.na(WN_scopus$`Cited by`)] <- 0
WN_cite <- sum(WN_scopus$`Cited by`)
WN_cite_Docu <- WN_cite/nrow(WN_scopus)

WN_TC <- WN_scopus %>% 
    dplyr::arrange(desc(`Cited by`)) %>% 
    dplyr::slice(1:100)

write.csv(WN_TC[1:20,], "WN_TC.csv", row.names = FALSE)

#** Authors (not good due to repeated names)---- 

WN_AU <- sum(str_count(WN_scopus$Authors, ","), na.rm = TRUE)+ # Total number of author 
    nrow(WN_scopus[complete.cases(WN_scopus$Authors),])
WN_AU2 <- sum(str_count(WN_scopus$`Author(s) ID`, ";"), na.rm = TRUE) # less than 200 

single_authored <- 0
for (i in 1:nrow(WN_scopus)){ # single-authored documetn
    if (str_count(WN_scopus$Authors[i], ",") == 1){
        single_authored <- single_authored+1  
    }
}

Docu_AU <- nrow(WN_scopus)/WN_AU # document per author
Au_Docu <- WN_AU/nrow(WN_scopus) # Author per document

# top 50 authors

AU <- function(x,y){
    author <- strsplit(x, ",")
    for (i in 1:length(author)){
        author[i] <- as.data.frame(matrix(as.data.frame(author[i])))
    }
    author2 <- data.table::rbindlist(author)
    colnames(author2)[1]<- "author"
    author2<- author2[complete.cases(author2),]
    author3 <- author2 %>%
        dplyr::group_by(author) %>% 
        dplyr::summarise(n=n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        dplyr::slice(1:y)
    return(author3)
}

WN_scopus$Authors <- as.character(WN_scopus$Authors)

WN_TopAU <- AU(WN_scopus$Authors, 50)
write.csv(WN_TopAU, "WN_TopAU.csv", row.names = FALSE)
ggsave("WN_TopAU.tiff", ggplot(WN_TopAU, aes(x=reorder(author, n),y = n)) +
           geom_bar(stat = "identity",
                    position = position_stack(reverse = TRUE), 
                    fill = "tomato") +
           coord_flip() +
           theme_bw() +
           xlab("Authors") +
           ylab("Number of articles") +
           theme(text=element_text(family = "Arial")) +
           theme(axis.text.x = element_text(size = 9)) +
           theme(axis.text.y = element_text(size = 9)) + 
           theme(axis.title.y = element_blank()),
       units = 'cm', height = 20, width = 20, dpi = 300
)
ggsave("WN_TopAU_20.tiff", ggplot(WN_TopAU[1:20,], aes(x=reorder(author, n),y = n)) +
           geom_bar(stat = "identity",
                    position = position_stack(reverse = TRUE), 
                    fill = "tomato") +
           coord_flip() +
           theme_bw() +
           xlab("Authors") +
           ylab("Number of articles") +
           theme(text=element_text(family = "Arial")) +
           theme(axis.text.x = element_text(size = 14)) +
           theme(axis.text.y = element_text(size = 14)) +
           theme(axis.title = element_text(size = 14)) +
           theme(axis.title.y = element_blank()),
       units = 'cm', height = 20, width = 30, dpi = 300
)

#** Top countries and affiliations -----

# Top country 

WN_partners <- WN_affi %>% 
    dplyr::group_by(Country) %>% 
    dplyr::summarise(n=n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::slice(1:20)

ggsave("WN_Topcountry.tiff", ggplot(WN_partners, aes(x=reorder(Country, n),y = n)) +
           geom_bar(stat = "identity",
                    position = position_stack(reverse = TRUE), 
                    fill = "tomato") +
           coord_flip() +
           theme_bw() +
           xlab("Country") +
           ylab("Number of articles") +
           theme(text=element_text(family = "Arial")) +
           theme(axis.text.x = element_text(size = 14)) +
           theme(axis.text.y = element_text(size = 14)) + 
           theme(axis.title.y = element_blank()),
       units = 'cm', height = 20, width = 20, dpi = 300
)

#** World map ----

# quantiles
WN_country <- WN_affi %>% 
    dplyr::group_by(Country) %>% 
    dplyr::summarise(n=n()) %>% 
    dplyr::arrange(desc(n))


WN_country$Code <- countrycode(WN_country$Country, 'country.name', 'iso3c')
sPDF <- joinCountryData2Map(WN_country, joinCode = "ISO3", nameJoinColumn = "Code")

tiff(filename = "WN_worldmap_log.tiff",units = 'px', height = 1500, width = 2500, res = 300, pointsize = 8)
mapParams <- mapCountryData(sPDF, nameColumnToPlot = "n", 
                            addLegend=FALSE,
                            numCats = 9, mapRegion = "world", catMethod = "logFixedWidth",
                            colourPalette = brewer.pal(9,"PuBuGn"),
                            borderCol = "grey",
                            mapTitle = "",
                            missingCountryCol = "white")
do.call(addMapLegend, c(mapParams, 
                        legendWidth=0.5, 
                        legendMar = 3,
                        legendLabels ="all",
                        legendIntervals = "data"))
dev.off()


#** Publication years ----

WN_PU_year <- WN_scopus %>% select(Year,`Document Type`, `Access Type`) %>% 
    dplyr::group_by(Year) %>% 
    dplyr::summarise(n=n()) %>% 
    dplyr::arrange(Year)

WN_PU_year$cum <- ave(WN_PU_year$n,FUN = cumsum)
# WN_PU_year$Year <- as.factor(WN_PU_year$Year)

ggsave("WN_PU_year.tiff", WN_PU_year %>% 
           ggplot(aes(x=Year, y=cum))+
           geom_point(size = 2) +
           labs(x = "Years", y = "Cumulative publications", fill = NULL, title = NULL) +
           theme_classic()+
           theme(axis.text.x = element_text(size = 14)) +
           theme(axis.text.y = element_text(size = 14)) +
           theme(axis.title = element_text(size = 14)),
       units = 'cm', height = 15, width = 15*1.5, dpi = 300
)

#** Top Journal  ----

WN_journal <- WN_scopus %>% select(`Source title`) %>% 
    dplyr::group_by(`Source title`) %>% 
    dplyr::summarise(n=n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    slice(1:20)

write.csv(WN_journal, "WN_journal.csv", row.names = FALSE)
ggsave("WN_Topjournal.tiff", ggplot(WN_journal[1:10,], aes(x=reorder(`Source title`, n),y = n)) +
           geom_bar(stat = "identity",
                    position = position_stack(reverse = TRUE), 
                    fill = "tomato") +
           coord_flip() +
           theme_bw() +
           xlab("Journals") +
           ylab("Number of publications") +
           theme(text=element_text(family = "Arial")) +
           theme(axis.text.x = element_text(size = 14)) +
           theme(axis.text.y = element_text(size = 14)) +
           theme(axis.title = element_text(size = 14)) +
           theme(axis.title.y = element_blank()),
       units = 'cm', height = 20, width = 30, dpi = 300
)

#### Top authors all ----

WN_scopus$Authors <- as.character(WN_scopus$Authors)
AU2 <- function(x){
    author <- strsplit(x, ", ")
    for (i in 1:length(author)){
        author[i] <- as.data.frame(matrix(as.data.frame(author[i])))
    }
    author2 <- data.table::rbindlist(author)
    colnames(author2)[1]<- "author"
    author2<- author2[complete.cases(author2),]
    author3 <- author2 %>%
        dplyr::group_by(author) %>%
        dplyr::summarise(n=n()) %>%
        dplyr::arrange(desc(n))
    return(author3)
}
AU_name <- function(x, y){
    # Find the overlapped name of the y-most productive authors
    
    x1 <- vector("list", y)
    for (i in 1:y){
        x1[[i]] <- as.character(x$author[i])
        for (j in 1:nrow(x)){
            if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) != "van" & 
                str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) != "de"){# if no "Van"
                if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) == # if the first names are the same
                    str_to_lower(str_split_fixed(x$author[i], ' ',2)[1])){
                    if( i !=j){
                        x1[[i]] <- list.append(x1[[i]], as.character(x$author[j]))
                    }
                }
            } else {
                if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[2]) == # if having "Van"
                    str_to_lower(str_split_fixed(x$author[i], ' ',2)[2])){# if the first names are the same
                    if( i !=j){
                        x1[[i]] <- list.append(x1[[i]], as.character(x$author[j]))
                    }
                }
            }
        }
    }
    
    # replace the one with no problems
    
    for (i in length(x1):1){
        if (length(x1[[i]]) == 1){
            x1 <- x1[-i]
        }
    }
    # choose the names that were from the same authors
    x2 <- list()
    for (i in 1:length(x1)){
        for (j in 2:length(x1[[i]])){
            if (str_to_lower(str_split_fixed(x1[[i]][1], "\\.",2)[1]) == # before the dot is the same indicates the same authors
                str_to_lower(str_split_fixed(x1[[i]][j], "\\.",2)[1])){
                x1[[i]]<-append(x1[[i]][1],x1[[i]][j]) # only choose the overlapped ones
                x2 <- list.append(x2,x1[[i]])
            }
        }
    }
    # replace the ones that are the same authors
    for (i in length(x2):1){
        for(j in length(x2):1){
            if (str_to_lower(str_split_fixed(x2[[i]][1], "\\.",2)[1]) ==
                str_to_lower(str_split_fixed(x2[[j]][1], "\\.",2)[1])
                & j<i){
                x2 <- x2[-i]
            }
        }
    }
    
    # replace the overlapped name
    
    x$author <- as.character(x$author)
    
    for (i in 1:nrow(x)){
        for(j in 1:length(x2)){
            if (length(x2[[j]]) == 2){
                if(nchar(x2[[j]][1]) > nchar(x2[[j]][2])){
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][1], x2[[j]][2])
                } else {
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][2], x2[[j]][1])
                }
            } else {
                for(k in 2:length(x2[[j]])){
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][k], x2[[j]][1])
                }
            }
        }
    }
    
    # rerank these names
    
    x <-aggregate(n ~ author, data=x, sum) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::filter(author != "Jr.")
    
    return(x)
}
AU_list <- function(x, y){
    # Find the overlapped name of the y-most productive authors
    
    x1 <- vector("list", y)
    for (i in 1:y){
        x1[[i]] <- as.character(x$author[i])
        for (j in 1:nrow(x)){
            if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) != "van" & 
                str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) != "de"){# if no "Van"
                if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) == # if the first names are the same
                    str_to_lower(str_split_fixed(x$author[i], ' ',2)[1])){
                    if( i !=j){
                        x1[[i]] <- list.append(x1[[i]], as.character(x$author[j]))
                    }
                }
            } else {
                if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[2]) == # if having "Van"
                    str_to_lower(str_split_fixed(x$author[i], ' ',2)[2])){# if the first names are the same
                    if( i !=j){
                        x1[[i]] <- list.append(x1[[i]], as.character(x$author[j]))
                    }
                }
            }
        }
    }
    
    # replace the one with no problems
    
    for (i in length(x1):1){
        if (length(x1[[i]]) == 1){
            x1 <- x1[-i]
        }
    }
    # choose the names that were from the same authors
    x2 <- list()
    for (i in 1:length(x1)){
        for (j in 2:length(x1[[i]])){
            if (str_to_lower(str_split_fixed(x1[[i]][1], "\\.",2)[1]) == # before the dot is the same indicates the same authors
                str_to_lower(str_split_fixed(x1[[i]][j], "\\.",2)[1])){
                x1[[i]]<-append(x1[[i]][1],x1[[i]][j]) # only choose the overlapped ones
                x2 <- list.append(x2,x1[[i]])
            }
        }
    }
    # replace the ones that are the same authors
    for (i in length(x2):1){
        for(j in length(x2):1){
            if (str_to_lower(str_split_fixed(x2[[i]][1], "\\.",2)[1]) ==
                str_to_lower(str_split_fixed(x2[[j]][1], "\\.",2)[1])
                & j<i){
                x2 <- x2[-i]
            }
        }
    }
    
    # replace the overlapped name
    
    x$author <- as.character(x$author)
    
    for (i in 1:nrow(x)){
        for(j in 1:length(x2)){
            if (length(x2[[j]]) == 2){
                if(nchar(x2[[j]][1]) > nchar(x2[[j]][2])){
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][1], x2[[j]][2])
                } else {
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][2], x2[[j]][1])
                }
            } else {
                for(k in 2:length(x2[[j]])){
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][k], x2[[j]][1])
                }
            }
        }
    }
    
    # rerank these names
    # 
    # x <-aggregate(n ~ author, data=x, sum) %>%
    #     dplyr::arrange(desc(n)) %>%
    #     dplyr::filter(author != "Jr.")
    # 
    return(x)
}
WN_TopAU2 <- AU2(WN_scopus$Authors)
# WN_TopAU2 <- WN_TopAU2 %>% filter(n >= 10) # doesn't count the ones less than 10 publications
WN_TopAU2 <- AU_name(WN_TopAU2, 50) 
write.csv(WN_TopAU2, "WN_TopAU.csv", row.names = FALSE)

ggsave("WN_Topauthor_2_20.tiff", ggplot(WN_TopAU2[1:20,], aes(x=reorder(author, n),y = n)) +
           geom_bar(stat = "identity",
                    position = position_stack(reverse = TRUE),
                    fill = "tomato") +
           coord_flip() +
           theme_bw() +
           xlab("Authors") +
           ylab("Number of publications (-)") +
           theme(text=element_text(family = "Arial")) +
           theme(axis.text.x = element_text(size = 14)) +
           theme(axis.text.y = element_text(size = 14)) +
           theme(axis.title = element_text(size = 14)) +
           theme(axis.title.y = element_blank()),
       units = 'cm', height = 20, width = 30, dpi = 300
)

#### Country year####

CY <- read_csv("CY.csv")
CY_10 <- CY %>% filter(str_detect(Year,"2009|2010|2011|2012|2013|2014|2015|2016|2017|2018")) 
CY_10_LA <- CY_10 %>% filter(!str_detect(Country, "United States|Canada|Global|China"))
CY_10_LA <- aggregate(Publication ~ Year, data=CY_10_LA, sum)
CY_10_LA$Country <- "Latin America and Caribbean"
CY_10_NA <- CY_10 %>% filter(str_detect(Country, "United States|Canada|Global|China")) 
CY_final <- bind_rows(CY_10_LA,CY_10_NA)
CY_final$Country <- as.factor(CY_final$Country)
CY_final$Year <- as.factor(as.character(CY_final$Year))

ggsave("WN_Country_year.tiff", ggplot(CY_final, aes(x = as.factor(Year), y = Publication, color= Country, group = Country))+
           geom_point(size = 3)+
           geom_line(size = 1.25)+
           theme_bw() +
           xlab("Year") +
           ylab("Number of publications (-)") +
           theme(text=element_text(family = "Arial")) +
           theme(axis.text.x = element_text(size = 14)) +
           theme(axis.text.y = element_text(size = 14)) +
           theme(axis.title = element_text(size = 14)) +
           theme(legend.title = element_blank()) +
           theme(legend.text = element_text(size = 14)),
    units = 'cm', height = 20, width = 30, dpi = 300
)
                       
                       
#### Pie chart ####
WN_pie <- read.csv("Pie.csv")

ggsave("WN_pie.tiff", ggplot(WN_pie, aes(x ="", y = Publications, fill = Regions))+
    geom_bar(width = 1, stat = 'identity') +
    coord_polar("y", start = 0) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          # axis.ticks = element_blank()
          ),
    units = 'cm', height = 20, width = 30, dpi = 300
)
