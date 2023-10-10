#Importing Bonds Csv:

bonds <- read.csv('Homework1_Bonds.csv')
#Basic Info/Label Adjustment:
head(bonds)
summary(bonds)
bonds$Gov_Type <- factor(bonds$Gov_Type, labels=c('CCD','CITY','CNTY','HHD','ISD','OSD','WD'))

#Question Number 1:
#How many Bonds passed: 
table(bonds$Result) #Just for my information
P <- sum(bonds$Result=='Carried')
P
D <- sum(bonds$Result=='Defeated' | bonds$Result =='Cancelled' | bonds$Result == 'NR')
D
#Proportion Table
barplot(prop.table(table(bonds$Result,bonds$Gov_Type),2), 
        main = 'Bond passage by type of Government'
        ,ylab = 'Bond Status', xlab = 'Type of Government',
        col=c('thistle','lightpink1','powderblue','palegreen1'))
        legend('bottomright',c('NR','Defeated','Carried','Cancelled'), pch=c(20,20)
               ,col=c('thistle','lightpink1','powderblue','palegreen1'),inset=.03, cex=.8)



#Question Number 2:

##Creating/Displaying Votes_Total
bonds$votesTotal <- bonds$Votes_For+bonds$Votes_Against
bonds$votesTotal
#Finding Max value
maxVotes <- max(bonds$votesTotal)
MV <- bonds[bonds$votesTotal == maxVotes,]
#Where, When, Why
cat('The highest turnout for a single bond was', maxVotes, 'voters' ,' 
Where:', MV$Gov_Name, '
When:', MV$Election_Date, ' 
For the purpose of:', MV$Purpose_Detail)


#Question Number 3:
App10 <- bonds[bonds$Result == 'Carried' & bonds$votesTotal>10,]
App10$App10Total<-App10$Votes_For+App10$Votes_Against
Percent <- 100*(App10$Votes_For/(App10$App10Total))
App10$Percent<-round(Percent,2)
Percent
boxplot(App10$Percent,main='Percent of Votes In Favor'
        ,xlab='Percent', pch=20, horizontal=TRUE, col='indianred')
fivenum(App10$Percent)



#Question Number 4:
plot(App10$Percent,App10$Amount, main = 'Margin of passage in relation to cost',
     xlab = 'Percent of Yes votes', ylab = 'cost', pch=20, col='royalblue3')
cat('The correlation coefficient of', cor(App10$Percent,App10$Amount), 'demonstrates a very weak negative
correlation between the margin by which the bond passed and its cost, therefore 
we can state that the cost has no relationship to the margin by which a bond passes')



