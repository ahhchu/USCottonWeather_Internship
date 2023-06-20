File data/daymet/daymet_v4_daily_hi_dayl_1980.nc (NC_FORMAT_NETCDF4):

     6 variables (excluding dimension variables):
        float lat[x,y]   (Chunking: [284,584])  (Compression: level 4)
            units: degrees_north
            long_name: latitude coordinate
            standard_name: latitude
        float lon[x,y]   (Chunking: [284,584])  (Compression: level 4)
            units: degrees_east
            long_name: longitude coordinate
            standard_name: longitude
        short yearday[time]   (Chunking: [1])  (Compression: level 4)
            long_name: day of year (DOY) starting with day 1 on Januaray 1st
        float time_bnds[nv,time]   (Chunking: [2,1])  (Compression: level 4)
        short lambert_conformal_conic[]   (Contiguous storage)  
            grid_mapping_name: lambert_conformal_conic
            longitude_of_central_meridian: -100
            latitude_of_projection_origin: 42.5
            false_easting: 0
            false_northing: 0
            standard_parallel: 25
             standard_parallel: 60
            semi_major_axis: 6378137
            inverse_flattening: 298.257223563
        float dayl[x,y,time]   (Chunking: [284,584,1])  (Compression: level 4)
            _FillValue: -9999
            long_name: daylength
            units: s
            missing_value: -9999
            coordinates: lat lon
            grid_mapping: lambert_conformal_conic
            cell_methods: area: mean

     4 dimensions:
        x  Size:284 
            units: m
            long_name: x coordinate of projection
            standard_name: projection_x_coordinate
        y  Size:584 
            units: m
            long_name: y coordinate of projection
            standard_name: projection_y_coordinate
        time  Size:365   *** is unlimited *** 
            standard_name: time
            calendar: standard
            units: days since 1950-01-01 00:00:00
            bounds: time_bnds
            long_name: 24-hour day based on local time
        nv  Size:2 (no dimvar)

    7 global attributes:
        start_year: 1980
        source: Daymet Software Version 4.0
        Version_software: Daymet Software Version 4.0
        Version_data: Daymet Data Version 4.0
        Conventions: CF-1.6
        citation: Please see http://daymet.ornl.gov/ for current Daymet data citation information
        references: Please see http://daymet.ornl.gov/ for current information on Daymet references
