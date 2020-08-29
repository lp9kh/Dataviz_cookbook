#install.packages("tidyverse")
#install.packages("gcookbook")
library("tidyverse")
library("gcookbook")
library("ggplot2")
library("dplyr")
library(wesanderson)
library(hexbin)
library(MASS)


#CHAPTER 2
#quickly exploring data
plot(mtcars$wt, mtcars$mpg)

ggplot(mtcars, aes(x=wt, y=mpg))+
  geom_point()

ggplot(data= NULL, aes(x=mtcars$wt, y=mtcars$mpg))+
  geom_point()

#creating a line graph
#YOU CAN AVOID ALL OF THIS
plot(pressure$temperature, pressure$pressure, type= "l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col ="red")
points(pressure$temperature, pressure$pressure/2, col ="red")
#BY DOING THIS --> geom_point
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()+geom_point()

#creating a bar graph
#YOU CAN AVOID ALL OF THIS
table(mtcars$cyl)
barplot(table(mtcars$cyl))
#BY DOING THIS (also convert time to a factor)  --> geom_col
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_col()
#each value of cyl
ggplot(mtcars, aes(x=cyl)) + geom_bar()
#bar graph of counts
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

#creating in histogram
#YOU CAN AVOID ALL OF THIS
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10)
#BY DOING THIS  --> geom_histogram
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)

#creating in box plot
#YOU CAN AVOID ALL OF THIS
plot(ToothGrowth$supp,  ToothGrowth$len)
boxplot(len ~ supp +  dose, data=ToothGrowth)
#BY DOING THIS  --> geom_boxplot
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len) + geom_boxplot()
       
       plotting a function curve
       #YOU CAN AVOID ALL OF THIS
       myfun<- function(xvar) {
         1/(1 + exp(-xvar + 10))
       }
       curve(myfun(x), from =0, to =20)
       #add a line
       curve(1-myfun(x), add = TRUE, col="red")
       #BY DOING THIS  --> stat_function
       ggplot(data.frame(x = c(0, 20)), aes(x=x)) + stat_function(fun=myfun, geom="line")
       
       #CHAPTER 3 BAR GRAPHS
       ggplot(pg_mean, aes(x=group, y=weight)) + geom_col()
       #HOW TO ADD COLOR
       ggplot(pg_mean, aes(x=group, y=weight)) + geom_col(fill= "lightblue", colour = "black")
       
       #grouping bars together
       ggplot(cabbage_exp, aes(x= Date, y= Weight, fill = Cultivar))+ 
         geom_col(position = "dodge")
       #using scale_fill_brewer
       ggplot(cabbage_exp, aes(x= Date, y= Weight, fill = Cultivar))+ 
         geom_col(position = "dodge", colour = "black")+
         scale_fill_brewer(palette = "Pastel1")
       
       #bar graph of counts --> geom_bar
       ggplot(diamonds, aes(x=cut))+
         geom_bar()
       
       #using colors in a bar graph
       upc<- uspopchange %>%
         arrange(desc(Change)) %>%
         slice (1:10)
       ggplot(upc, aes(x=Abb, y=Change, fill = Region)) + geom_col()  
       
       #nota bene: setting colors OUTSIDE aes() mapping colors WITHIN aes()
       #function -->reorder 
       ggplot(upc, aes(x = reorder(Abb, Change), y= Change, fill = Region)) + 
         geom_col(colour = "black") +
         scale_fill_manual(values = c("#669933", "#FFCC66")) +
         xlab("State")
       
       #coloring negative and positive bars differently
       climate_sub <- climate %>%
         filter(Source == "Berkeley" & Year >= 1900) %>%
         mutate(pos = Anomaly10y >= 0)
       climate_sub  
       #primo tentativo
       ggplot(climate_sub, aes(x = Year, y= Anomaly10y, fill = pos)) + geom_col(position = "identity") 
       #rifinitura
       ggplot(climate_sub, aes(x = Year, y= Anomaly10y, fill = pos)) +
         geom_col(position = "identity", colour = "black", size = 0.25) + 
         scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE)
       
       #adjusting bar width and spacing
       #narrower bars
       ggplot(pg_mean, aes(x=group, y=weight)) + geom_col(width=0.5)
       #wider bars
       ggplot(pg_mean, aes(x=group, y=weight)) + geom_col(width=1)
       #grouped bar graph w narrow bars
       ggplot(cabbage_exp, aes(x= Date, y= Weight, fill = Cultivar))+ 
         geom_col(width=0.5, position = "dodge")
       #grouped bar graph w narrow bars + space between bars
       ggplot(cabbage_exp, aes(x= Date, y= Weight, fill = Cultivar))+ 
         geom_col(width=0.5, position = position_dodge(0.7))
       
       #stacked bar graph
       ggplot(cabbage_exp, aes(x= Date, y= Weight, fill = Cultivar))+ 
         geom_col()+
         guides(fill= guide_legend(reverse=TRUE))
       #reverse the stacking order of the bars
       ggplot(cabbage_exp, aes(x= Date, y= Weight, fill = Cultivar))+ 
         geom_col(position= position_stack(reverse= TRUE))+
         guides(fill= guide_legend(reverse=TRUE))
       #polishing the graph
       ggplot(cabbage_exp, aes(x= Date, y= Weight, fill = 
                                 Cultivar)) +
         geom_col(colour = "black") +
         scale_fill_brewer(palette = "Pastel1")
       
       #making proportional(100%) stacked bar graph(100%)  --> fill
       ggplot(cabbage_exp, aes(x= Date, y= Weight, fill = 
                                 Cultivar)) + 
         geom_col(colour = "black", position = "fill") +
         scale_y_continuous(labels = scales::percent) +
         scale_fill_brewer(palette = "Pastel1")
       
       #using group_by()
       ce<- cabbage_exp %>%
         group_by(Date) %>%
         mutate(percent_weight = Weight / sum(Weight) *100)
       ce
       #transforming data by group
       ggplot(ce, aes(x= Date, y= percent_weight, fill = 
                        Cultivar)) + 
         geom_col()
       #adding labels --> geom_text
       #labels below the top
       ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
         geom_col() +
         geom_text(aes(label = Weight), vjust = 1.5, colour = "white")
       #labels above the top
       ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y= Weight)) +
         geom_col() +
         geom_text(aes(label = Weight), vjust = -0.2)
       #bar label of counts  
       ggplot(mtcars, aes(x= factor(cyl))) +
         geom_bar() +
         geom_text(aes(label = ..count..), stat= "count",
                   vjust = 1.5, colour="white")
       
       #adjust y limits to be a lil higher
       ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y= Weight)) +
         geom_col() +
         geom_text(aes(label = Weight), vjust = -0.2) + 
         ylim(0, max(cabbage_exp$Weight) * 1.05)
       
       #map y positions slightly above bar top
       ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y= Weight)) +
         geom_col() +
         geom_text(aes(y = Weight + 0.1, label = Weight))
       
       #labels on grouped bars
       ggplot(cabbage_exp, aes(x = Date, y= Weight, fill = Cultivar)) +
         geom_col(position = "dodge") +
         geom_text(
           aes(label = Weight), colour = "white", size = 3,
           vjust = 1.5, position = position_dodge(.9)
         )
       
       #labels on stacked bars
       ce<- cabbage_exp %>%
         arrange(Date, rev(Cultivar))
       #sort by date and cultivar 
       ce<- ce %>%
         group_by(Date) %>%
         mutate(label_y = cumsum(Weight))
       ce
       #plot
       ggplot(ce, aes(x = Date, y= Weight, fill = Cultivar)) +
         geom_col() +
         geom_text(aes(y = label_y, label = Weight), vjust = 1.5, colour = "white")
       
       #put labels in the middle
       #labels on stacked bars
       ce<- cabbage_exp %>%
         arrange(Date, rev(Cultivar))
       #sort by date and cultivar 
       ce<- ce %>%
         group_by(Date) %>%
         mutate(label_y = cumsum(Weight) - 0.5 * Weight)
       #plot in the middle
       ggplot(ce, aes(x = Date, y= Weight, fill = Cultivar)) +
         geom_col() +
         geom_text(aes(y = label_y, label = Weight), colour = "white")
       #more polished version
       ggplot(ce, aes(x = Date, y = Weight, fill =
                        Cultivar)) +
         geom_col(colour = "black") +
         geom_text(aes(y = label_y, label = paste(format(Weight, nsmall = 2), "kg")), size=4) +
         scale_fill_brewer(palette = "Pastel1")
       
       #making a Cleveland Dot Plot --> geom_point
       tophit <- tophitters2001[1:25, ]
       #plot
       ggplot(tophit, aes(x = avg, y = name))+geom_point()
       #order by batting average
       ggplot(tophit, aes(x = avg, y = reorder(name,avg))) +
         geom_point(size=3) + 
         theme_bw() + 
         theme(
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.y = element_line(colour = 
                                               "grey60",linetype = "dashed"),
         )
       #swap the axes
       ggplot(tophit, aes(x = reorder(name,avg), y = avg)) +
         geom_point(size=3) + 
         theme_bw() + 
         theme(
           panel.grid.major.y = element_blank(),
           panel.grid.minor.y = element_blank(),
           panel.grid.major.x = element_line(colour = 
                                               "grey60",linetype = "dashed"),
           axis.text.x = element_text(angle = 60, hjust = 1)
         )
       
       #sort the names
       nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
       #turn name into factorsw levels in the order of nameorder
       tophit$name <- factor(tophit$name, levels = nameorder)
       #geom_segment
       ggplot(tophit, aes(x = avg, y = name)) +
         geom_segment(aes(yend = name), xend = 0, colour = "grey50") +
         geom_point(size = 3, aes(colour = lg)) +
         scale_colour_brewer(palette = "Set1", limits = c("NL", "AL")) +
         theme_bw() + 
         theme(
           panel.grid.major.y = element_blank(),
           #no horizontal grid lines
           legend.position = c(1, 0.55),
           #put legend inside plot area
           legend.justification = c(1, 0.5)
         )
       #separate by using facets
       ggplot(tophit, aes(x = avg, y = name)) +
         geom_segment(aes(yend = name), xend = 0, colour = "grey50") +
         geom_point(size = 3, aes(colour = lg)) +
         scale_colour_brewer(palette = "Set1", limits = c("NL", "AL")) +
         theme_bw() + 
         theme(
           panel.grid.major.y = element_blank()) + 
         facet_grid(lg ~ ., scales = "free_y", space="free_y")
       
       #CHAPTER 4 LINE GRAPHS
       ggplot(BOD,aes(x = Time, y = demand))+ geom_line()
       #make a copy of the data
       BOD1<-BOD 
       BOD1$Time <- factor(BOD1$Time)
       #line graph w a factor on x axis
       ggplot(BOD1,aes(x = Time, y = demand, group =1))+ geom_line()
       #linegraph w manually set range
       ggplot(BOD,aes(x = Time, y = demand))+ geom_line() + ylim(0, max(BOD$demand))
       #linegraph w manually set range
       ggplot(BOD,aes(x = Time, y = demand))+ geom_line() + expand_limits(y=0)
       
       
       #adding points to a line graph
       ggplot(worldpop, aes(x=Year, y = Population)) + geom_line()+ geom_point()
       #same plot but w a log y-axis
       ggplot(worldpop, aes(x=Year, y = Population)) + geom_line()+ geom_point() + scale_y_log10()
       
       #making a line graph w multiple lines
       #mapping to colour
       ggplot(tg, aes(x=dose, y=length, colour = supp)) + geom_line()
       #mapping to linetype
       ggplot(tg, aes(x=dose, y=length, linetype = supp)) + geom_line()
       
       #convert "dose" to a factor to treat it as categorical value
       ggplot(tg, aes(x=factor(dose), y = length, colour = supp, group = supp)) + geom_line()
       
       #using shape and fill
       #make the points a lil larger
       ggplot(tg, aes(x=dose, y = length, shape = supp)) + geom_line() + geom_point(size=4)
       
       #use a point w a color fill 
       ggplot(tg, aes(x=dose, y = length, fill = supp)) + geom_line() + geom_point(size=4, shape = 21)
       
       #dodging the points
       ggplot(tg, aes(x=dose, y = length, shape = supp)) + geom_line(position = position_dodge(0.2)) + geom_point(position = position_dodge(0.2), size=4)
       
       
       #changing the appearance of lines
       ggplot(BOD,aes(x = Time, y = demand))+ geom_line(linetype="dashed", size =1, colour = "blue")
       
       #using a palette from RColorBrewer
       ggplot(tg, aes(x=dose, y = length, colour = supp)) + geom_line() + scale_colour_brewer(palette = "Set1")
       
       #if both lines have the same properties, you need to specify a variable to use a grouping
       ggplot(tg, aes(x=dose, y = length, group = supp)) + geom_line(colour= "darkgreen", size = 1.5)
       
       #since supp is mapped to colour, it will automatically be used for grouping
       ggplot(tg, aes(x=dose, y = length, colour = supp)) + geom_line(linetype="dashed") + geom_point(shape = 22, size = 3, fill = "white")
       
       #changing appearance of points
       ggplot(BOD, aes(x=Time, y = demand)) + geom_line() + geom_point(size = 4, shape = 22, colour = "darkred", fill="pink")
       
       #points with a white fill
       ggplot(BOD, aes(x=Time, y = demand)) + geom_line() + geom_point(size = 4, shape = 21, fill="white")
       
       #line graph w manually specified fills of b/w and a slight dodge
       #save the position_didge specification because we'll use it multiple times
       pd <- position_dodge(0.2)
       #result
       ggplot(tg, aes(x=dose, y = length, fill = supp)) + geom_line(position = pd) + 
         geom_point(shape = 21, size = 3, position = pd) + 
         scale_fill_manual(values = c ("black", "white")
         )
       
       #graph with a shaded area
       #convert the sunspot.year data set into a data frame for this example
       sunspotyear <- data.frame(
         Year = as.numeric(time(sunspot.year)),
         Sunspots = as.numeric(sunspot.year)
       )
       #default colors
       ggplot(sunspotyear, aes(x=Year, y = Sunspots)) + geom_area()
       #add outline and change colours
       ggplot(sunspotyear, aes(x=Year, y = Sunspots)) + geom_area(colour = "black", fill="blue", alpha =.2)
       #avoid the outline by not specifying the colour and then a layer
       ggplot(sunspotyear, aes(x=Year, y = Sunspots)) + geom_area( fill="blue", alpha =.2) + geom_line()
       
       
       #making a stacked area graph
       ggplot(uspopage, aes(x=Year, y = Thousands, fill= AgeGroup)) + geom_area()
       #reversed legend order, lines and a different palette
       ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
         geom_area(colour = "black", size = .2, alpha = .4) +
         scale_fill_brewer(palette = "Blues") + 
         geom_line(position = "stack", size = .2)
       
       #making a proportional stacked area graph
       ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
         geom_area(position = "fill", colour = "black", size = .2, alpha = .4) +
         scale_fill_brewer(palette = "Blues") 
       #add labels in %
       ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
         geom_area(position = "fill", colour = "black", size = .2, alpha = .4) +
         scale_fill_brewer(palette = "Blues") +
         scale_y_continuous(labels = scales::percent)
       
       #adding a confidence region
       #1grab a subset of the climate data
       climate_mod <- climate %>%
         filter(Source == "Berkeley") %>%
         select(Year, Anomaly10y, Unc10y)
       #2shaded region
       ggplot(climate_mod, aes(x = Year, y= Anomaly10y)) +
         geom_ribbon(aes(ymin = Anomaly10y - Unc10y,
                         ymax = Anomaly10y + Unc10y),
                     alpha = 0.2) +
         geom_line()
       #3adding dots
       ggplot(climate_mod, aes(x = Year, y= Anomaly10y)) +
         geom_line(aes(y = Anomaly10y - Unc10y), colour = "grey50", linetype = "dotted" )+
         geom_line(aes(y = Anomaly10y + Unc10y), colour = "grey50", linetype = "dotted" )+
         geom_line()
       
       
       #CHAPTER 5 SCATTER PLOTS 
       
       #basic scatter plot
       ggplot(heightweight, aes(x=ageYear, y=heightIn))+ geom_point()
       #different shapes
       ggplot(heightweight, aes(x=ageYear, y=heightIn))+ geom_point(shape=21)
       #smaller points
       ggplot(heightweight, aes(x=ageYear, y=heightIn))+ geom_point(size =1.5)
       
       
       #gouping points together
       ggplot(heightweight, aes(x=ageYear, y=heightIn, shape= sex, colour = sex))+ geom_point()
       #differenciate colors and shapes
       ggplot(heightweight, aes(x=ageYear, y=heightIn, shape= sex, colour = sex)) + 
         geom_point() +
         scale_shape_manual(values = c(1,2)) + 
         scale_colour_brewer(palette = "Set1")
       
       #using different point shapes
       ggplot(heightweight, aes(x=ageYear, y=heightIn))+ geom_point(shape=3)
       #manually change shape levels
       ggplot(heightweight, aes(x=ageYear, y=heightIn, shape = sex)) +
         geom_point(size=3) +
         scale_shape_manual(values = c(1, 4)) 
       
       #add a column in the dataset
       #child weighs < 100 or >= 100 pounds. We'll save this modified dataset as "hw"
       hw <- heightweight %>%
         mutate(weightgroup =ifelse(weightLb <100, "<100", ">100"))
       #specify shapes w fill and color (also specify NA)
       ggplot(hw, aes(x=ageYear, y=heightIn, shape = sex, fill=weightgroup)) +
         geom_point(size=2.5) +
         scale_shape_manual(values = c(21, 24)) +
         scale_fill_manual(values = c(NA, "black"), guide = guide_legend(override.aes = list(shape= 21)))
       
       
       #mapping a continuos variable to color or size
       #colour
       ggplot(heightweight, aes(x=ageYear, y=heightIn, colour = weightLb)) +
         geom_point()
       #size
       ggplot(heightweight, aes(x=ageYear, y=heightIn, size = weightLb)) +
         geom_point()
       
       #size mapping to diameter of point vs area
       range(height$weightLb)
       size_range <- range(heightweight$weightLb) / max(heightweight$weightLb) * 6
       size_range
       #diameter of points
       ggplot(heightweight, aes(x=ageYear, y=heightIn, size = weightLb)) +
         geom_point()+
         scale_size_continuous(range = size_range)
       
       #area of points
       ggplot(heightweight, aes(x=ageYear, y=heightIn, size = weightLb)) +
         geom_point()+
         scale_size_area()
       
       #fill continuous variable to map 
       ggplot(heightweight, aes(x=ageYear, y=heightIn, fill = weightLb)) +
         geom_point(shape = 21, size = 2.5)+
         scale_fill_gradient( low = "black", high = "white")
       #using legend will result in a discret legend
       ggplot(heightweight, aes(x=ageYear, y=heightIn, fill = weightLb)) +
         geom_point(shape = 21, size = 2.5)+
         scale_fill_gradient( low = "black", high = "white", breaks = seq(70, 170, by= 20),
                              guide= guide_legend())
       
       #OVERPLOTTING solutions
       #make the points semi-transparent
       #bin the data into rectangles/hexagons
       #use box plots
       
       #overplotting w 54k points 
       diamonds_sp<- ggplot(diamonds, aes(x= carat, y = price))
       diamonds_sp + geom_point()
       #make the points semi-transparent (90%)
       diamonds_sp + geom_point(alpha= .1)
       #make the points semi-transparent (99%)
       diamonds_sp + geom_point(alpha= .01)
       
       #bin the data into rectangles
       #option1
       diamonds_sp + stat_bin2d()
       #polished
       diamonds_sp + stat_bin2d(bins=50) +
         scale_fill_gradient(low = "lightblue", high = "red", limits = c (0,6000))
       
       #bin the data into rectangles
       diamonds_sp + stat_binhex() +
         scale_fill_gradient(low = "lightblue", high = "red", limits = c (0,8000))
       
       #bin the data into rectangles
       diamonds_sp + stat_binhex() +
         scale_fill_gradient(low = "lightblue", high = "red", limits = c (0,5000))
       
       #overplotting w discrete data
       cw_sp <- ggplot(ChickWeight, aes(x= Time, y = weight))
       cw_sp + geom_point()
       #jitter the data 
       cw_sp + geom_jitter() 
       #or
       cw_sp + geom_point(position = "jitter")
       #polish
       cw_sp + geom_point(position = "jitter"(width = .5, height =0))
       
       #REMEMBER TO GROUP THE DATA 
       cw_sp + geom_boxplot(aes(group = Time))
       #BECAUSE OTHERWISE
       cw_sp + geom_boxplot()
       
       #Adding fitted regression model lines
       hw_sp<-ggplot(heightweight, aes(x=ageYear, y=heightIn))
       hw_sp + geom_point() + stat_smooth( method = lm)
       #by default stat_smooth has 95% confidence level
       #bring it up to 99%
       hw_sp + geom_point() + stat_smooth( method = lm, level = 0.99)
       #no confidence region
       hw_sp + geom_point() + stat_smooth( method = lm, se= FALSE)
       #change the colour
       hw_sp + geom_point( colour = "grey60") + stat_smooth( method = lm, se= FALSE, colour = "black")
       
       #LOESS locally weighted polynomial (che sarebbe il default curve per la regressione)
       hw_sp + geom_point( colour = "grey60") + stat_smooth()
       #equivalente a 
       hw_sp + geom_point( colour = "grey60") + stat_smooth(method = loess)
       
       #load MASS for the biopsy data set
       <!-- library(MASS) -->
         <!-- biopsy_mod <- biopsy %>% -->
         <!--   mutate(classn = recode(class, benign = 0, malignant = 1)) -->
         <!-- biopsy_mod   -->
         
         <!-- ggplot(biopsy_mod, aes(x=V1, y=classn)) + -->
         <!--  geom_point(position = position_jitter(width =0.3, height = 0.06), alpha = 0.4, shape = 21, size = 1.5) + -->
         <!--   stat_smooth(method = glm, method.args = list(family =binomial)) -->
         
         #LOESS fit lines for each group
         hw_sp<-ggplot(heightweight, aes(x=ageYear, y=heightIn, colour = sex)) +
         geom_point() +
         scale_color_brewer(palette = "Set1")
       hw_sp + geom_smooth()
       
       #extrapolated linear fit lines
       hw_sp + geom_smooth(method = lm, se= FALSE, fullrange = TRUE)
       
       #adding fitted lines from an existing model
       
       
       
       
       
       
       
       