function Vs = interpctd(ctd, xn, zn, vns, xres, zres, interptype, extraptype)
  % set interpolation inputs
  % xn = 'dist' ;
  % zn = 'elev' ;
  % vn = {'sa', 'ta', 'oa'} ;
  % xres = 0.1 ; % kilometers 
  % zres = 0.1 ;
  % interptype = 'natural'
  % extraptype = 'none'
  
  % define interpolation grid
  surfacebound = 2.6 ; % based on max observed WSE
  zbound = -15.8 ; % based on minimum z in bathymetry raster

  % correct ctd distances
  riverdist = [0.30 400
               1.20 1100
               2.50 2600
               3.50 3800
               4.00 4300
               4.60 4800
               5.30 5700
               6.40 6900
               7.30 7900
               8.70 9300
               9.50 10100
               10.0 10800] ;
  for i = 1:size(riverdist, 1)
    ctd.(xn)(ctd.(xn) == riverdist(i, 1)) = riverdist(i, 2)/1000 ;
  end

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
