pkmn <- read_excel("Pokemon.xlsx")
pkmn
# this displays the table as shown in the excel file.
head(pkmn)
#this displays the top record of the pokemon datasets
tail(pkmn)
#this displays the bottom record of the pokemon datasets
?duplicated # to search for help on duplicated data
duplicated(pkmn$Name) # This displays pokemon which are duplicated. It is displayed in Logic i.e TRUE or FALSE
pkmn$pkmn_dupes <- duplicated(pkmn$Name)
#with the code above, a new column "pkmn_dupes" has been added to the table. 
head(pkmn)# This returns a table with an added column
pkmn <- pkmn[pkmn$pkmn_dupes == "FALSE",] # this line removes all duplicates
table(pkmn$Type) # Table of all types of pokemon
summary(pkmn$Attack) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#5.00   55.00   75.00   77.32   95.00  190.00 
# This gives the statistics of the column Attack
summary(pkmn[pkmn$Type == "POISON", ]$Attack)
# This displays the stat of poisonous pokemon and their attack rate.
highattack <- pkmn[order(-pkmn$Attack),]
highattack# displays pokemon that attacks the best. displayed in descending order
highattack[1:6,] # top 6 
highattack <- pkmn[order(-pkmn$Attack),][1:2,]
highattack
# Pokemon with the highest attack are Mega Mewtwo and Mega Heracros with 190 and 185 respectively
highdefense <- pkmn[order(-pkmn$Defense),][1:2,]
highdefense 
# Pokemon with highest defense are Shuckle and mega Aggron
highspeed <- pkmn[order(-pkmn$Speed),][1:2,]
highspeed # Pokemon with the highest speed
toppkmn <- rbind(highattack, highdefense, highspeed)
toppkmn
# Toppokemon overall with highest attack, defense and speed are
# Mega Mewtwo
# Mega Heracros
# shuckle
# Mega Aggron
# speed forme
# Ninjask
summary(pkmn[pkmn$Type == "PSYCHIC", ]$Attack)
# Min.    1st Qu.  Median    Mean 3rd Qu.     Max. 
# 20.00   45.00    55.00     67.74   85.00   190.00 
# statistics for Psychic pokemon
psychic <- pkmn[pkmn$Type == "PSYCHIC",]
psychic
highattackpsychic <- psychic[order(-psychic$Attack),][1:2,]
highattackpsychic
# This displays psychic pokemon with the highest attack(Top two)
# which are Mega Mewtwo and Deoxys Attack forme 
highdefensepsychic <- psychic[order(-psychic$Defense),][1:2,]
highdefensepsychic
# Psychic Pokemon with 2 highest defense are Deoxys- Defense Forme and Lugia 
highspeedpsychic <- psychic[order(-psychic$Speed),][1:2,]
highspeedpsychic
# Top 2 psychic pokemon with highest speed are Deoxys- Speed Forme and Mega Alakazam
toppsychic <- rbind(highattackpsychic, highdefensepsychic, highspeedpsychic)
toppsychic
# This syncs up all psychic pokemon with highest attack, defense and speed
# A tibble: 6 × 11
`#`                  Name    Type Total    HP Attack Defense
*  <chr>                 <chr>   <chr> <dbl> <dbl>  <dbl>   <dbl>
  1  150.1         Mega Mewtwo X PSYCHIC   780   106    190     100
2  386.1  Deoxys- Attack Forme PSYCHIC   600    50    180      20
3  386.2 Deoxys- Defense Forme PSYCHIC   600    50     70     160
4    249                 Lugia PSYCHIC   680   106     90     130
5  386.3   Deoxys- Speed Forme PSYCHIC   600    50     95      90
6  065.1         Mega Alakazam PSYCHIC   590    55     50      65
# ... with 4 more variables: `Special Attack` <dbl>, `Special
#   Defense` <dbl>, Speed <dbl>, pkmn_dupes <lgl>
> 
grass <- pkmn[pkmn$Type == "GRASS",]
grass
slowgrass <- grass[order(grass$Speed),][1:6,]
slowgrass
# Slowest Grass pokemon are Ferroseed, Foongus, Ferrothorn, Oddish, Sunkern and Sunflora
summary(pkmn$Total)
# This displays the stats of all types of pokemon.
min(pkmn$Total)
#The worst type of pokemon has 180 in total which are the least on the stats table
median(pkmn$Total)
#The average or medicre pokemon has 453 total
 max(pkmn$Total)
# According to this data, the pokemon with the highest total of 780 is the best pokemon that should be used or fit for battle.
?which.max
pkmn
which.min(pkmn$Total)
maxi <- pkmn[pkmn$Total == "780",]
maxi # Pokemon with max total and overall best according to stats is Mega Mewtwo and types are psychic and fighting
mini <- pkmn[pkmn$Total == "180",]
mini # This is the least pokemon according to stats. The Sunkern pokemon is the least in total and might notbe fit for battle
mediocre <- pkmn[pkmn$Total == "453",]
mediocre # The average can not be derived because it was divided and it is assumed that it is an even number. The average total of pokemon is 453
# My ideal pokemon is Mega Metwo
fighting <- pkmn[pkmn$Type == "FIGHTING",]
fighting
highattackfighting <- fighting[order(-fighting$Attack),][1:2,]
highattackfighting # the highest fighting attack pokemon is Mega mewtwo and mega heracross
highdefensefighting <- fighting[order(-fighting$Defense),][1:2,] 
highdefensefighting # the highest fighting defense pokemon are cobalion and chestnaught
highspeedfighting <- fighting[order(-fighting$Speed),][1:2,]
highspeedfighting # the highest fighting and fastest pokemon are Mega mewtwo and Meloetta- Pirouette Forme
# To make up my team i will use Mega Mewtwo, Meloetta- Pirouette Forme for my primary attack, becausethey have the best attack
# For speed i will use Cobalion Fighting for defense








