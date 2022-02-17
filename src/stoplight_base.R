
plot(0:1,0:1,type="n",axes=F,ann=F)
polygon(x=c(0.4,0.4,0.6,0.6),
        y=c(0.1,0.9,0.9,0.1),col="grey20")
# prepare "circle data"
radius = 0.0750
center_x = 0.5
center_y = 0.75
theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle
# draw the circle
polygon(x = radius * cos(theta) + center_x, y = radius * sin(theta) + center_y,col="red")
       
center_y = 0.5
theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle
# draw the circle
polygon(x = radius * cos(theta) + center_x, y = radius * sin(theta) + center_y,col="yellow")

center_y = 0.25
theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle
# draw the circle
polygon(x = radius * cos(theta) + center_x, y = radius * sin(theta) + center_y,col="green")

