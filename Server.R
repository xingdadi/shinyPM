#-------------last modified on 12/18/2013

#-------------obtain the data from website
mps <- "http://www.chapm25.com/city/beijing.html" 
mps.doc <- htmlParse(mps, encoding = "utf-8");
mps.tabs <- readHTMLTable(mps.doc, stringsAsFactors = FALSE)
h = length(names(mps.tabs))
PM25_now = mps.tabs[[h]];
idx_now = order(PM25_now[,1]);
PM25_order = PM25_now[idx_now, ];
#-----------transform data into numeric values
pm_char = PM25_order[,2];
end_info = nchar(pm_char)-6;
pm_value = as.numeric( substr(pm_char, 1, end_info) );

write.table(file="pm_data_now.txt", pm_value, sep = " ", row.names=FALSE, col.names=FALSE);
write.table(file="time_start.txt", Sys.time(), row.names=FALSE, col.names=FALSE);

#------------- 35 station info
station_info = read.table(file = "utf_code.txt", sep="", header = TRUE); 

#------------- BJ zipcode info
zipcode_data = read.table(file = "zipcode.txt", header = TRUE, sep = "");

shinyServer(function(input, output) {
  
  output$main_plot <- renderPlot({    
    invalidateLater(1000*60*2)

    z_val = read.table(file = "time_start.txt", sep = "\n");
    z_val = as.character(z_val[1,1])
    time_start = strptime(z_val, "%Y-%m-%d %H:%M:%S", tz = "EST5EDT"); 
     
    print(as.character(time_start));
    time_gap = as.numeric(difftime(Sys.time(), time_start, unit = "mins"))
    print(time_gap);

    if(time_gap>=15)
    {
	mps.doc <- htmlParse(mps, encoding = "utf-8");
	mps.tabs <- readHTMLTable(mps.doc, stringsAsFactors = FALSE)
	h = length(names(mps.tabs))
	PM25_now = mps.tabs[[h]];
	idx_now = order(PM25_now[,1]);
	PM25_order = PM25_now[idx_now, ];
	#-----------transform data into numeric values
	pm_char = PM25_order[,2];
	end_info = nchar(pm_char)-6;
	pm_value = as.numeric( substr(pm_char, 1, end_info) );

      write.table(file="pm_data_now.txt", pm_value, sep = " ", row.names=FALSE, col.names=FALSE);
      time_now = Sys.time();
      write.table(file="time_start.txt", time_now, row.names=FALSE, col.names=FALSE)

    }
    
    #---------------combine PM2.5, longitude, latitude
    x = as.numeric( as.matrix(station_info[,3]) );  #longitude
    y = as.numeric( as.matrix(station_info[,2]) );   #latitude
    z = as.numeric( as.matrix(read.table(file="pm_data_now.txt", sep = "",header = FALSE) ) );  #PM2.5

    data_use = cbind(x, y, z) ;
    coords <- as.matrix(cbind(x, y));

    x.res <- 20
    y.res <- 20
    idx = which(z != "NA");
    surf <- mba.surf(data_use[idx, ], no.X = x.res, no.Y = y.res, h = 5, m = 2, extend = FALSE)$xyz.est;

    #--------zip code prediction
    zipcode_info = zipcode_data$zipcode
    longitude_info = zipcode_data$longitude
    latitude_info = zipcode_data$latitude

    code_now = as.numeric(input$n_zipcode);          #-------input the zipcode
    idx_now = which(zipcode_info == code_now);
    longitude_now = longitude_info[idx_now];
    latitude_now = latitude_info[idx_now];

    long_idx = which.min( abs(surf$x-longitude_now) );
    latd_idx = which.min( abs(surf$y-latitude_now) );

    pm_pred_now = surf$z[long_idx, latd_idx];

    #------zipcode pred plot
    main_label = paste( "PM2.5 density", as.character(as.POSIXlt(Sys.time(), "GMT")+3600*8) );
    sub_label = paste( as.character(round(pm_pred_now, 1)), "ug/m^3 @", as.character(code_now) );
    text_label = paste(as.character(round(pm_pred_now, 1)),"(", as.character(code_now), ")", sep ="");
    image.plot(surf, xaxs = "r", yaxs = "r", main = main_label, sub = sub_label, xlab = "longitude", ylab = "latitude", xlim = c(min(x), max(x)), ylim = c(min(y), max(y)));
    points(longitude_now, latitude_now, pch = 2, cex = sqrt(pm_pred_now)/10)
    text(longitude_now+0.04, latitude_now+0.04, label = text_label)

    if (input$individual_obs) {
      points(x, y, pch = 19);
    }

    if (input$whole_area) {

      x.res_new <- input$rs_adjust
      y.res_new <- input$rs_adjust     
      surf_new <- mba.surf(data_use[idx, ], no.X = x.res_new, no.Y = y.res_new, h = 5, m = 2, extend = FALSE)$xyz.est;

      long_idx_new = which.min( abs(surf_new$x-longitude_now) );
      latd_idx_new = which.min( abs(surf_new$y-latitude_now) );

      pm_pred_new = surf_new$z[long_idx_new, latd_idx_new];

      main_label = paste( "PM2.5 density", as.character(as.POSIXlt(Sys.time(), "GMT")+3600*8) );
      sub_label = paste( as.character(round(pm_pred_new, 1)), "ug/m^3 @", as.character(code_now) );
      text_label_new = paste(as.character(round(pm_pred_new, 1)),"(", as.character(code_now), ")", sep ="");
      image.plot(surf_new, xaxs = "r", yaxs = "r", main = main_label, sub = sub_label, xlab = "longitude", ylab = "latitude");
      points(longitude_now, latitude_now, pch = 2, cex = sqrt(pm_pred_new)/10);
      text(longitude_now+0.04, latitude_now+0.04, label = text_label_new);
      
    }

   if (input$map_area){

      BJmap = get_map(location = c(lon = longitude_now, lat = latitude_now), zoom = 10, maptype = 'roadmap')   
      #BJmap = get_map(location = c(lon = 116.407526, lat = 39.95), zoom = 10, maptype = 'roadmap')
      #longitude_now = 116.5514;
      #latitude_now = 39.9538;
      print( ggmap(BJmap)+ggtitle( paste(main_label,"\n",sub_label) )+geom_text(data = NULL, x=longitude_now+0.01, y=latitude_now+0.01, label=text_label, size = 4, fontface=2, colour = "black")+geom_point(data = NULL, x=longitude_now, y =latitude_now, colour = "red", size = 3.5, shape = 16) );      
   
    }

  })

})

