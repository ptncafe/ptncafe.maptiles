
const map = new maplibregl.Map({
  container: "map",
  //style: 'http://localhost:9812/maps/osm/style.json',
  style: "styles/map_navigation_styles.json",
  center: [106.65921008113624, 10.781224400204039], // starting position [lng, lat]

  zoom: 12,
  maxZoom: 22,
  minZoom: 10,
});

map.on("styleimagemissing", (e) => {
    console.warn("styleimagemissing", e);
    const id = e.id; // id of the missing image

    
});
