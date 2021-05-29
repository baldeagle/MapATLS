#Code is run by ctrl+enter.  You can step through the file this way.
#Hashtags indicate comments

#Run this the first time to install the packages
install.packages("tidyverse")
install.packages("readr")
install.packages("tidyr")
install.packages("tidygraph")
install.packages("ggraph")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("fuzzyjoin")


#Normally you start here
require(tidyverse)
require(readr)
require(tidyr)
require(tidygraph)
require(ggraph)
require(RColorBrewer)
require(ggplot2)
require(fuzzyjoin)

#This should read the data in.  You could also use the 'import dataset' over on the top right hand side.  
#They need to be named like this for the other code to work though.
#The <- assigns some data to the thing that is being pointed at.  It is how you know R isn't a real programming language
egs <- read_csv2('edges.csv')
nms <- read_csv2('nodes.csv')

#This reads the points needed for a level (copied in from your SS_upgrades sheet)
finallvls <- read_csv2('finallevls.csv')
b2p <- read_csv2('buildingtopoints.csv')


#This line makes the graph object, named g
g <- tbl_graph(nodes = nms, edges = egs, directed = F)

#This creates a function to make the system graph in a SG/R hybrid approach
sysgraph <- function(gr) {ggraph(gr, layout='auto') + geom_edge_link() + geom_node_point(aes(size=level,colour=as.factor(level),x=x,y=y)) +geom_node_text(aes(label = nm), repel = TRUE) +
  labs(edge_width = "Letters") +  theme_graph()}

#This creates a function to make the system graph in a SG/R hybrid approach
corpgraph <- function(gr) {ggraph(gr, layout='auto') + geom_edge_link() + geom_node_point(aes(size=level,colour=as.factor(level),x=x,y=y)) +geom_node_text(aes(label = corp), repel = TRUE) +
    labs(edge_width = "Letters") +  theme_graph()}

#This creates a function to make the system graph in a SG/R hybrid approach
finalgraph <- function(gr) {ggraph(gr, layout='auto') + geom_edge_link() + geom_node_point(aes(size=FinalLevel,colour=as.factor(level),x=x,y=y)) +geom_node_text(aes(label = corp), repel = TRUE) +
    labs(edge_width = "Letters") +  theme_graph()}

#I ran a sysgraph and used the export to make it a picture I could refer back to.  Then I ran the corpgraph for most of the work
#You plot the graph by calling the function sysgraph() with the object g, like this
sysgraph(g)

#Ok, this function basically counts the number of lines coming out from a system, where the system on the other end is the same corp
#I had trouble figuring out the building value, so I assumed lvl 4 buildings everywhere.  If you want to change it, you change the point total after 'nrow()*' on the 5th line of the function.
#You will need to call the function again for it to take effect
#the %>% is like a unix pipe and moved data to the next bit of logic.  
evalbuiltsys <- function(gr){gr %>%
  activate(edges) %>% mutate(matched=.N()$corp[from]==.N()$corp[to]) %>% 
  activate(nodes) %>%  
  mutate(nbpoints = map_local_dbl(order = 1, .f = function(neighborhood, ...) {
    as_tibble(neighborhood, active = 'edges') %>%  filter(matched==T) %>% nrow()*4200
  })) %>% mutate(totalpoints=sysbasepoints+nbpoints) %>% as.data.frame() %>% select(nm,corp, totalpoints) %>% 
    fuzzy_left_join(.,finallvls,by=c('totalpoints'='lowerbound','totalpoints'='upperbound'),match_fun=list(`>=`, `<=`)) %>% select(nm,corp,FinalLevel)
}



#This chunk here is the evaluation chunk, you'll be running it a lot


#This calls the evalbuiltsys fuction we just made, and figures out the final levels
#You will do this a lot as you figure out where systems go to whom
final <- evalbuiltsys(g)
final %>% group_by(corp,FinalLevel) %>% summarise(n=n()) %>% pivot_wider(names_from = corp,values_from = n) %>% arrange(desc(FinalLevel))

#This shows the network graph with the final levels and corp labels
g %>% select(-any_of(c('finalLevel'))) %>% left_join(final) %>% finalgraph()

#This shows the galaxy graph with the final levels and corps.  Sorry, no lines here, because lines are a little more complicated
finalg <- left_join(g,final) %>% as.tibble()
ggplot()+geom_point(data=finalg,aes(x=x,y=y,size=FinalLevel,colour=corp))+coord_fixed(ratio =1,expand = TRUE, clip = "on") + scale_y_reverse()+geom_text(data=finalg,aes(x=x,y=y,label=corp),hjust=0, vjust=0)





#This chunk allows you to update corp assignment in excel or something


#Now we get to the fun part, where you can change the corp names of systems
#This will past the current corp assignements to your clipboard, where you can use ctrl+v to paste into excel or something
g %>% as.tibble() %>% select(nm,level,corp) %>% write_clip()
 
#After you've changed the corp assignments, you highlight just the data (not extra blank lines below, yes to header/title line) 
#hit ctrl+c to copy the data
#then you run this line to read from the clipboard
g <- g %>% select(-any_of('corp')) %>% left_join(read_clip_tbl())
