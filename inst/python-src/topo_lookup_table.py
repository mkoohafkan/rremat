import arcpy
arcpy.CheckOutExtension("Spatial")
import arcpy.sa
import time
import tempfile
arcpy.env.workspace = r'C:/GIS workspace/RRE/topotables/scratch.gdb'
bathymetry_path = r'C:/GIS workspace/RRE/habitat.gdb/bathymetry_NGVD_meters'
zones_path = r'C:/GIS workspace/RRE/habitat.gdb/markerzones_meters'
table_folder = r'C:/GIS workspace/RRE/topotables'
bathyorig = arcpy.sa.Raster(bathymetry_path)
bathymetry = arcpy.sa.Int(bathyorig*10.0 + ((bathyorig > 0)*2 - 1)*0.5)
zones = arcpy.sa.Raster(zones_path)

def processtable(raster_obj, zone_file, w, out_folder, out_file):
  temp_name = "/" + next(tempfile._get_candidate_names()) + ".dbf"
  arcpy.sa.ZonalStatisticsAsTable(zone_file, 'VALUE', raster_obj, out_folder + temp_name, "DATA", "SUM")
  arcpy.AddField_management(out_folder + temp_name, "elev", 'TEXT')
  arcpy.CalculateField_management(out_folder + temp_name, "elev", "'" + str(w*0.1) + "'", "PYTHON")
  arcpy.TableToTable_conversion(out_folder + temp_name, out_folder, out_file)
  arcpy.Delete_management(out_folder + temp_name)

def computeplane(elev):
  total       =  (bathymetry == elev)
  # zonal statistics table names
  total_name = 'topo_count_' + str(elev) + '.csv'
  # compute zonal statistics
  processtable(total, zones, elev, table_folder, total_name)

def main():
  topofac = range(-158, 27, 1)
  for i in topofac:
    computeplane(i)

if __name__ == '__main__':
  start = time.clock()
  main()
  end = time.clock()
  print round((end - start)/3600, 2), "hours"