scale_fill_distiller(guide = guide_colorbar("IP Auths Per Member",
                                            title.position = "top",
                                            barwidth = 15,
                                            barheight = .7,
                                            ticks.colour = "#222222"),
                     
                     labels = pht::ph_fnum,
                     #labels = function(x) round(10**x,0),
                     palette = "OrRd",
                     direction = 1,
                     na.value = "white",
                     #breaks =  log(c(.01, .1, 1, 2, 4, 8))
) +
  labs(title = "COVID-19 Heatmap - Recent IP Auths",
       subtitle = "Since June 14th, where are the auths concentrated?",
       caption = "Counties in white have not had an increase in auths or have less than 500 Members")