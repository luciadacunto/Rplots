library(ggplot2)
library(sqldf)

#read data
df = read.csv('piechartdata.csv')

# add labels to data
df = sqldf("select Summary,
  CASE WHEN Gender==1 THEN 'Female'
       WHEN Gender==2 THEN 'Male'
  END gender,
  CASE WHEN Response==1 THEN '1) rarely' 
       WHEN Response==2 THEN '2) infrequently' 
       WHEN Response==3 THEN '3) occasionally' 
       WHEN Response==4 THEN '4) frequently' 
       WHEN Response==5 THEN '5) not sure' 
  END response from df")

# add percent calculation (needed to show in the piechart)
df["percent"] <- NA
df$percent <- paste(round(df$Summary / sum(df$Summary) * 200,1), "%", sep="")

# add position for the percent labels
s1 <- df$Summary[1:(length(df$Summary)/2)]
s2 <- df$Summary[(length(df$Summary)/2+1):length(df$Summary)]

# make a barplot
p = ggplot(data=df, aes(x=factor(1), y=Summary, fill = factor(response))) 
p = p + geom_bar(width = 1)

# add percent labels
p = p + geom_text(aes(y = c(cumsum(s1)-s1/2, cumsum(s2)-s2/2), label = df$percent),size=5)

# separate plot in facets
p = p + facet_grid(facets=. ~ gender)

# transform barplot in piechart
p = p + coord_polar(theta="y") 

# embelishment
p = p + scale_x_discrete("",breaks=NULL) + scale_y_continuous("",breaks=NULL)
p = p + xlab('') + ylab('') + labs(fill='Response')
p = p + theme(legend.title=element_text(size=20), legend.text=element_text(size=20), strip.text.x = element_text(size = 20, colour = "black", angle = 0), panel.grid=element_blank())
p = p + scale_x_discrete("",breaks=NULL) + scale_y_continuous("",breaks=NULL)

p