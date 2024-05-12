
    const map = new maplibregl.Map({
        container: 'map',
        style: 'http://127.0.0.1:5500/tegola_openmaptiles/web/style/style.json',
        center: [106.65921008113624,10.781224400204039], // starting position [lng, lat]

        zoom: 15,
        maxZoom: 22,
        minZoom: 4,
    });
