-- My Pattern matching prowess
coords sizex 0 = zip [0..sizex] (take sizex $repeat 0)
coords sizex sizey = zip [0..sizex] (take sizex $repeat sizey ) ++ coords sizex (sizey-1)

