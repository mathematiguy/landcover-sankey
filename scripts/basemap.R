basemap <-
  leaflet() %>%
  setView(174.5, -41.5, 8) %>% 
  #fitBounds(166.70047, -34.45676, 178.52966, -47.06345) %>% 
  addFullscreenControl() %>% 
  addSearchOSM(options=searchOSMOptions(zoom=12, hideMarkerOnCollapse=T)) %>% 
  addEasyButtonBar(
    easyButton(
      icon="fa-globe",
      title="Zoom to NZ", 
      onClick=JS("function(btn, map) {
                 map.fitBounds([[-34.45676, 166.70047],[-47.06345, 178.52966]]); 
                 map.addTo(zoomTo)
                 }")),
        easyButton(
          icon="fa-crosshairs", 
          title="Zoom to my location",
          onClick=JS("function(btn, map) {
                     map.locate({setView: T, maxZoom: 10});
                     }"))
      ) %>% 
  addProviderTiles(
    "Esri.WorldGrayCanvas",
    group="WorldGreyCanvas", 
    options=providerTileOptions(zIndex=-900, attribution="\u00A9 ESRI WorldGrayCanvas")) %>%
  addProviderTiles(
    "Esri.OceanBasemap",
    group="OceanBasemap",
    options=providerTileOptions(zIndex=-900, attribution="\u00A9 ESRI OceanBasemap")) %>%
  addProviderTiles(
    "Esri.WorldImagery", 
    group="WorldImagery", 
    options=providerTileOptions(zIndex=-900, attribution="\u00A9 ESRI WorldImagery")) %>%
  addProviderTiles(
    "OpenStreetMap.Mapnik", 
    group="OpenStreetMap ",
    options=providerTileOptions(zIndex=-900, attribution="\u00A9 OpenStreetMap")
  ) %>%
  addTiles(urlTemplate="http://koordinates-tiles-a.global.ssl.fastly.net/services;key=f6dd767eadb84f7faf2948204092a9dd/tiles/v4/layer=1231/EPSG:3857/{z}/{x}/{y}.png", 
           group="Topo50", 
           options=tileOptions(zIndex=-900), 
           attribution="\u00A9 LINZ. Topo50") %>% 
  addTiles(urlTemplate="http://koordinates-tiles-a.global.ssl.fastly.net/services;key=f6dd767eadb84f7faf2948204092a9dd/tiles/v4/layer=1229/EPSG:3857/{z}/{x}/{y}.png", 
           group="Topo50", 
           options=tileOptions(zIndex=-900), 
           attribution="\u00A9 LINZ. Topo50") %>% 
  addTiles(urlTemplate="http://tiles-a.data-cdn.linz.govt.nz/services;key=44f9a1d429af401da1dbc1f78b70d64e/tiles/v4/set=4702/EPSG:3857/{z}/{x}/{y}.png", 
           group="AerialImagery", 
           options=tileOptions(zIndex=-900), 
           attribution="\u00A9 LINZ. NZ Aerial Imagery") %>% 
  addTiles(urlTemplate="http://tiles-a.data-cdn.linz.govt.nz/services;key=c3d6ad4563ad4c9da2da8345d30a8ebf/tiles/v4/layer=93652/EPSG:3857/{z}/{x}/{y}.png", 
           group="SatelliteImagery", 
           options=tileOptions(zIndex=-900), 
           attribution="\u00A9 LINZ NZ 10m Satellite Imagery (2017)") %>% 
  addWMSTiles(
    baseUrl="http://maps.scinfo.org.nz/cached/",
    layers=c("landscape_eco_painted_relief"), 
    options=WMSTileOptions(
      format="image/png",
      transparent=T, 
      zIndex=-900),
    group="LandcoverTerrain", 
    attribution="\u00A9 Manaaki Whenua Landcare Research") %>%
  addProviderTiles(
    "Esri.WorldTerrain", 
    group="WorldTerrain",
    options=providerTileOptions(zIndex=-900, attribution="\u00A9 ESRI WorldTerrain")) %>%
  addLayersControl(
    baseGroups=c("WorldGreyCanvas","OceanBasemap","WorldImagery", "SatelliteImagery", "AerialImagery", "LandcoverTerrain","WorldTerrain","OpenStreetMap ","Topo50"), 
    position="topleft", 
    options=layersControlOptions(autoZIndex=F)) %>% 
  addScaleBar(
    position="bottomleft",
    options=scaleBarOptions(metric=T, imperial=F))