#' Generate the volume lookup table
#'
#' Generate the volume lookup table from the bathymetry raster and zones 
#' shapefile. Computes total volume and volume of each depth category: 
#' littoral, surface limnetic, subsurface limnetic, and profundal.
#'
#' @param bathymetry_path File path to the bathymetry raster. Geodatabases are
#'   supported.
#' @param zones_path File path to the zones shapefile. Geodatabases are
#'   supported.
#' @param wse Vector of water surface elevations at which to compute volume.
#' @param scratchfolder Folder to use as a scratch workspace. Must be an actual 
#'   folder; geodatabases are not supported.
#' @return The volume lookup table (dataframe).
#'
#' @importFrom dplyr right_join
#' @importFrom magrittr "%>%"
#' @export
generate_volume_table = function(bathymetry_path, zones_path, wse, 
  scratchfolder = tempdir()){
  if(!require(arcpyr))
    stop("library 'arcpyr' not installed.")
  arcpy.initialize()
  sa.initialize()
  bathymetry_path = file.path(dirname(bathymetry_path), 
    basename(bathymetry_path))
  zones_path = file.path(dirname(zones_path), basename(zones_path))
  # load bathymetry
  PythonInR::pyExec(paste0("bathymetry = Raster('", bathymetry_path, "')"))
  alltbl = NULL
  for(w in wse){
    PythonInR::pyExec(paste("wse =", w))
    for(z in wse){
      print(paste0("z = ", z, ", w = ", w))
      if(z > w)
        next
      PythonInR::pyExec(paste("depth =", z))
      # count total number of cells at given depth
      PythonInR::pyExec("total = (bathymetry < depth)")
      # calculate littoral habitat
      PythonInR::pyExec("littoral = total & ((wse - bathymetry) < 2)")
      # count surface limnetic cells
      PythonInR::pyExec("surface = total & ~(littoral) & ((wse - depth) < 2)")
      # count profundal cells
      PythonInR::pyExec("profundal = total & ((wse - depth) > 5)")
      # count sub-surface limnetic cells
      PythonInR::pyExec("subsurface = total & ~(profundal | surface | littoral)")
      # output zonal statistics
      get_tbl = function(rasname){
        dbase.path = file.path(scratchfolder, paste0(rasname, ".dbf"))
        csv.path = file.path(scratchfolder, paste0(rasname, ".csv"))
        on.exit({
          PythonInR::pyExec(paste0("Delete_management('", dbase.path, "')"))
          PythonInR::pyExec(paste0("Delete_management('", csv.path, "')"))
        })
        PythonInR::pyExec(paste0("ZonalStatisticsAsTable('", zones_path, 
          "', 'gridcode', ", rasname, ", '", dbase.path, "', 'DATA', 'SUM')"))
        PythonInR::pyExec(paste0("TableToTable_conversion('", dbase.path, 
          "', '", dirname(csv.path), "', '", basename(csv.path), "')"))
        res = read.csv(csv.path)[c("gridcode", "SUM")]
        names(res) = c("dist", paste0("count.", rasname))
        res
      }
      vtbl = get_tbl("total") %>% right_join(get_tbl("littoral")) %>% 
        right_join(get_tbl("surface")) %>% right_join(get_tbl("profundal")) %>%
        right_join(get_tbl("subsurface")) 
      vtbl["elev"] = z 
      vtbl["wse"] = w 
      alltbl = rbind.data.frame(alltbl, vtbl)
    }
  }
  # get raster resolution
  PythonInR::pyExec("bathyres = Describe(bathymetry.children[0])")
  cellvol = c(xres = as.numeric(pyExecp("bathyres.meanCellWidth")),
              yres = as.numeric(pyExecp("bathyres.meanCellHeight")),
              zres = unique(abs(diff(wse))))
  structure(alltbl[c("dist", "elev", "wse", "count.total", "count.littoral", 
                     "count.surface", "count.subsurface", "count.profundal")],
            resolution = cellvol)
}

#devtools::load_all("arcpyr")
#arcpy.initialize()
#sa.initialize()
#library(dplyr)
#bathymetry_path = "C:/GIS workspace/RRE/habitat.gdb/bathymetry_NGVD_meters"
#zones_path = "C:/GIS workspace/RRE/habitat.gdb/markerzones_poly_dissolve"
#wse = seq(-15.8, 2.6, by = 0.1)