
    const map = new maplibregl.Map({
        container: 'map',
        //style: 'http://localhost:9812/maps/osm/style.json',
        style: 'http://127.0.0.1:5500/tegola/styles/map_navigation_styles.json',
        center: [106.65921008113624,10.781224400204039], // starting position [lng, lat]

        zoom: 8
    });
