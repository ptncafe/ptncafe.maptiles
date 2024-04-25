
    const map = new maplibregl.Map({
        container: 'map',
        style: 'http://127.0.0.1:5500/mbtiles/styles/map_navigation_styles.json',
        center: [106.65921008113624,10.781224400204039], // starting position [lng, lat]

        zoom: 12,
        maxZoom: 14,
        minZoom: 10,
    });
