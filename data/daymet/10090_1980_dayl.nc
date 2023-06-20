File 10090_1980_tmax.nc (NC_FORMAT_CLASSIC):

     4 variables (excluding dimension variables):
        double lat[x,y]   
            units: degrees_north
            long_name: latitude coordinate
            standard_name: latitude
            _CoordinateAxisType: Lat
            coordinates: y x 
            grid_mapping: lambert_conformal_conic
        short lambert_conformal_conic[]   
            grid_mapping_name: lambert_conformal_conic
            longitude_of_central_meridian: -100
            latitude_of_projection_origin: 42.5
            false_easting: 0
            false_northing: 0
            standard_parallel: 25
             standard_parallel: 60
            semi_major_axis: 6378137
            inverse_flattening: 298.257223563
            _CoordinateTransformType: Projection
            _CoordinateAxisTypes: GeoX GeoY
        double lon[x,y]   
            units: degrees_east
            long_name: longitude coordinate
            standard_name: longitude
            _CoordinateAxisType: Lon
            coordinates: y x 
            grid_mapping: lambert_conformal_conic
        float tmax[x,y,time]   
            long_name: daily maximum temperature
            units: degrees C
            missing_value: -9999
            _FillValue: -9999
            coordinates: time y x 
            grid_mapping: lambert_conformal_conic
            cell_methods: area: mean time: maximum

     3 dimensions:
        y  Size:6 
            units: km
            long_name: y coordinate of projection
            standard_name: projection_y_coordinate
        x  Size:3 
            units: km
            long_name: x coordinate of projection
            standard_name: projection_x_coordinate
        time  Size:365   *** is unlimited *** 
            standard_name: time
            calendar: standard
            units: days since 1950-01-01 00:00:00
            bounds: time_bnds
            long_name: 24-hour day based on local time
            _CoordinateAxisType: Time

    13 global attributes:
        tileid: 10090
        start_year: 1980
        source: Daymet Software Version 4.0
        Version_software: Daymet Software Version 4.0
        Version_data: Daymet Data Version 4.0
        Conventions: CF-1.6
        citation: Please see http://daymet.ornl.gov/ for current Daymet data citation information
        references: Please see http://daymet.ornl.gov/ for current information on Daymet references
        History: Translated to CF-1.0 Conventions by Netcdf-Java CDM (CFGridWriter2)
Original Dataset = /daymet_data/V4/CFNetCDF/1980/10090_1980/tmax.nc; Translation Date = 2023-06-20T17:46:51.082Z
        geospatial_lat_min: 21.984757222267
        geospatial_lat_max: 22.0424081350116
        geospatial_lon_min: -160.112608500377
        geospatial_lon_max: -160.053412123355
