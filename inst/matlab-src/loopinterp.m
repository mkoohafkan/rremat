%%
fpath = 'C:/Repository/rremat/inst/extdata/ctd' ;
fname = dir(fpath) ;
fname = {fname.name} ;
fname = fname(3:end) ;
for i = 1:length(fname)
  load([fpath '/' fname{i}]) ;
  ctd_interp = interpctd(ctd, 'dist', 'elev', {'sa' 'ta' 'oa'}, ...
    0.1, 0.1, 'natural', 'nearest') ;
  save([fpath '_interp/' fname{i}(1:end-4) '_interp.mat'], 'ctd_interp') ;
end
