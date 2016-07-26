function Vs = interpctd(ctd, xn, zn, vns, xres, zres, interptype, extraptype)
  % set interpolation inputs
  % xn = 'dist' ;
  % zn = 'elev' ;
  % vn = {'sa', 'ta', 'oa'} ;
  % xres = 0.1 ; % kilometers 
  % zres = 0.1 ;
  % interptype = 'natural'
  % extraptype = 'linear'
  
  % define interpolation grid
  surfacebound = 3.0 ; % based on max observed WSE
  zbound = -15.8 ; % based on minimum z in bathymetry raster

  % correct ctd distances
  riverdist = [0.30 400
               1.10 1100
               1.20 1100
               1.50 4800
               2.40 2600
               2.50 2600
               3.10 3800
               3.20 3800
               3.50 3800
               4.00 4300
               4.60 4800
               5.30 5700
               6.40 6900
               7.30 7900
               7.40 7900
               8.70 9300
               9.50 10100
               10.0 10800
               10.1 10800] ;
  udist = unique(ctd.(xn)) ;
  for i = 1:size(udist, 1)
    distmask = ctd.(xn) == udist(i) ;
    diff = abs(udist(i) - riverdist(:, 1)) ;
    whichmin = diff == min(diff) ;
    ctd.(xn)(distmask) = riverdist(whichmin, 2) ;
  end
  ctd.(xn) = ctd.(xn)/1000 ;
  % make the grid
  zgrid = zbound:zres:surfacebound ;
  xgrid = min(ctd.(xn)):xres:max(ctd.(xn)) ;
  [Xs, Zs] = meshgrid(xgrid, zgrid) ;

  % create the prediction functions and interpolate
  predict = struct() ;
  Vs = struct ;
  for i = 1:length(vns)
    predict.(vns{i}) = scatteredInterpolant([ctd.(xn), ctd.(zn)], ...
      ctd.(vns{i}), interptype, extraptype) ;
    Vs.(vns{i}) = predict.(vns{i})(Xs, Zs) ;
  end
  Vs.(xn) = Xs*1000 ;
  Vs.(zn) = Zs ;
  Vs.interpolation = interptype ;
  Vs.extrapolation = extraptype ;
end
